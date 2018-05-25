package com.kantefier.ora4s

import scala.reflect.macros.whitebox


object MacroImpl {
  import scala.language.experimental.macros

  class TableMacro(val c: whitebox.Context) {
    import c.universe._

    val tableAnnotName: String = classOf[DbTable].getSimpleName
    val fieldAnnotName: String = typeOf[Field].typeSymbol.name.toString

    val tableClassName: String = typeOf[Table].typeSymbol.name.toString
    val tableObjectName: String = typeOf[TableCompanion[_]].typeSymbol.name.toString

    private val isEnumClass: c.Tree => Boolean = tpt =>
      c.typecheck(tpt, mode = c.TYPEmode, silent = true).tpe <:< typeOf[enumeratum.values.ValueEnumEntry[_]]

    private val isStringEnum: c.Tree => Boolean = tpt =>
      c.typecheck(tpt, mode = c.TYPEmode, silent = true).tpe <:< typeOf[enumeratum.values.StringEnumEntry]

    private val isIntEnum: c.Tree => Boolean = tpt =>
      c.typecheck(tpt, mode = c.TYPEmode, silent = true).tpe <:< typeOf[enumeratum.values.IntEnumEntry]


    /**
      * Holds information about fields/parameters and generates parsers
      */
    case class ParamInfo(paramName: String, dbName: String, tableTypeTree: c.Tree) {
      /**
        * Returns ResultSet method call for this parameter type
        */
      private def typeToParser(tpt: Tree, inRecursion: Boolean = false): c.Expr[String] => c.Tree = nameExpr =>
        tpt match {
          case tq"Int" =>
            q"rs.getInt($nameExpr)"

          case tq"Long" =>
            q"rs.getLong($nameExpr)"

          case tq"String" =>
            q"rs.getString($nameExpr)"

          case tq"Array[Byte]" =>
            q"rs.getBytes($nameExpr)"

          // Optional enum
          case tq"Option[$enumClass]" if isEnumClass(enumClass) =>
            if(isIntEnum(enumClass))
              q"""scala.util.Try(${TermName(enumClass.toString())}.withValue(rs.getInt($nameExpr))).toOption.filterNot(_ => rs.wasNull())"""
            else if(isStringEnum(enumClass))
              q"""scala.util.Try(${TermName(enumClass.toString())}.withValue(rs.getString($nameExpr))).toOption.filterNot(_ => rs.wasNull())"""
            else
              c.abort(enumClass.pos, s"Class $enumClass should extend StringEnumEntry or IntEnumEntry")

            // Optional non-enum value
          case tq"Option[$valueType]" =>
            if(inRecursion)
              c.abort(tpt.pos, "Nested Options are not supported")
            else
              q"Option(${typeToParser(valueType, inRecursion = true)(nameExpr)}).filterNot(_ => rs.wasNull())"

          case enumClass if isEnumClass(enumClass) =>
            if(isIntEnum(enumClass))
              q"""${TermName(enumClass.toString())}.withValue(rs.getInt($nameExpr))"""
            else if(isStringEnum(enumClass))
              q"""${TermName(enumClass.toString())}.withValue(rs.getString($nameExpr))"""
            else
              c.abort(enumClass.pos, s"Class $enumClass should extend StringEnumEntry or IntEnumEntry")

          case other =>
            c.abort(other.pos, s"Cannot find suitable ResultSet getter method for type ${other.toString()}")
        }

      /**
        * Generate parser for this parameter
        */
      def mkParserPart: c.Tree = {
        val getterMethod = typeToParser(tableTypeTree)
        q"${TermName(paramName)} = ${getterMethod(c.Expr(q"""colNamePrefix + $dbName"""))}"
      }
    }

    /**
      * Macro implementation entry point
      */
    def impl(annottees: c.Expr[Any]*): c.Expr[Any] = {
      // retrieve table name from annotation
      val tableName = c.prefix.tree match {
        case q"new $annotName($param)" if annotName.toString() == tableAnnotName =>
          c.eval[String](c.Expr(param))

        case _ =>
          c.abort(c.enclosingPosition, "Table name should be specified!")
      }

      val result = annottees.map(_.tree) match {

        //matches a case class with companion object
        case (classDef @ q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }")
          :: q"$objMods object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf => ..$objDefs }"
          :: Nil if mods.hasFlag(Flag.CASE)
            && parents.exists(_.toString == tableClassName)
            && objParents.exists(_.toString == s"$tableObjectName[$tpname]") =>

          val paramInfos = collectParamInfo(paramss.head)
          val fieldsList = paramInfos.map(_.dbName)
          val newObjDefs =
            objDefs ++
            Seq(q"override val tableName: String = $tableName",
              q"override val fields: List[String] = $fieldsList",
              genParserFromRsDef(tpname.toString, paramInfos))

          q"""
             $classDef
             $objMods object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf =>
                ..$newObjDefs
             }
           """

          // err: not a case class
        case (classDef @ q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }")
          :: q"$objMods object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf => ..$objDefs }"
          :: Nil =>
          c.abort(classDef.pos, s"Can't generate code for $tpname: make it a case class first.")


          // err: no companion object
        case (classDef @ q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }")
          :: Nil =>
          c.abort(classDef.pos, s"Can't generate code for $tpname: it should have a companion object.")

          // err: something else?
        case _ =>
          c.abort(c.enclosingPosition, "Something went wrong.")

      }

      c.Expr[Any](result)

    }

    /**
      * Generate parser method for companion object
      */
    private def genParserFromRsDef(className: String, paramInfos: List[ParamInfo]): c.Tree = {
      val fieldParsers: List[c.Tree] = paramInfos.map(_.mkParserPart)

      q"""
         override def parseFromResultSet(colNamePrefix: String): java.sql.ResultSet => ${TypeName(className)} = { rs: java.sql.ResultSet =>
            ${TermName(className)}(..$fieldParsers)
         }
       """
    }

    /**
      * Collect info about fields from Field() annotations
      */
    private def collectParamInfo(params: List[c.Tree]): List[ParamInfo] = params.flatMap {
      case q"$mods val $tname: $tpt = $expr" if mods.annotations.nonEmpty =>
        mods.annotations.head match {
          case q"new $annotName" if annotName.toString == fieldAnnotName =>
            ParamInfo(tname.toString, tname.toString, tpt) :: Nil

          case q"new $annotName($fieldNameTree)" if annotName.toString == fieldAnnotName =>
            ParamInfo(tname.toString, c.eval[String](c.Expr(fieldNameTree)), tpt) :: Nil

          case _ =>
            Nil
        }

      case q"$mods val $tname: $tpt = $expr" =>
        Nil
    }
  }
}