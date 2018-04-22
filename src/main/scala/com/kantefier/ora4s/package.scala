package com.kantefier

import java.sql.{PreparedStatement, ResultSet}
import scala.annotation.StaticAnnotation
import scala.language.experimental.macros


package object ora4s {
  type ResultSetParser[T] = ResultSet => T

  class DbTable(name: String) extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro MacroImpl.TableMacro.impl
  }
  class Field(dbName: String = "") extends StaticAnnotation

  trait Table
  trait TableCompanion[T <: Table] extends BasicTableCompanion[T] {
    val fields: List[String] = null
    def parseFromResultSet(colPrefix: String): ResultSetParser[T] = null

    def prefixedFields(prefix: String): List[String] = fields.map(fld => s"$prefix.$fld as $prefix$fld")
    def prefixedFieldsStr(prefix: String): String = prefixedFields(prefix).mkString(", ")
    lazy val fieldsWithoutId: List[String] = fields.filter(_ != "id")
    lazy val parseFromResultSet: ResultSetParser[T] = parseFromResultSet("")
  }

  trait BasicTableCompanion[T <: Table] {
    val tableName: String = null
  }

  implicit class PreparedStatementOps(ps: PreparedStatement) {
    def bindValues(l: List[BoundValue]): PreparedStatement = {
      l.foreach(_.perform(ps))
      ps
    }
  }


  /* ******************************************************* */
  /** *********        Parser DSL        ******************* */
  /* ******************************************************* */

  /**
    * Magically adds two methods to a function (sic!)
    */
  implicit class ResultSetParserOps[T](rsp: ResultSetParser[T]) {
    // Combine two parsers to get a Tuple
    def add[T2](otherRsp: ResultSetParser[T2]): TupleParser[T, T2] =
      TupleParser(rsp, otherRsp)

    // Apply transformation T => T2 to result T
    def map[T2](f: T => T2): TransformParser[T, T2] =
      TransformParser(rsp, f)
  }

  /**
    * Uses two parsers to parse ResultSet into Tuple
    */
  case class TupleParser[T1, T2](parser1: ResultSetParser[T1], parser2: ResultSetParser[T2]) extends ResultSetParser[(T1, T2)] { self =>
    def apply(rs: ResultSet): (T1, T2) =
      parser1(rs) -> parser2(rs)

    def map[T3](f: ((T1, T2)) => T3): TransformParser[(T1, T2), T3] =
      TransformParser(self, f)
  }

  /**
    * Parser with transformation of a result
    */
  case class TransformParser[T, R](parser: ResultSetParser[T], transform: T => R) extends ResultSetParser[R] {
    def apply(rs: ResultSet): R =
      transform(parser(rs))
  }
}