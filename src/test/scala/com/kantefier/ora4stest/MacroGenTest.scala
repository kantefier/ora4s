package com.kantefier.ora4stest

import org.scalatest.FreeSpec

class MacroGenTest extends FreeSpec {
  "DbTable macro annotation" - {
    "shouldn't compile for wrong class definition" - {
      "if it's not a case class" in {
        assertDoesNotCompile(
          """
            |import com.kantefier.ora4s._
            |
            |@DbTable("qwerty")
            |class Table_T(@Field str: String) extends Table
            |object Table_T extends TableCompanion[Table_T]
          """.stripMargin)
      }

      "if it doesn't extend Table" in {
        assertDoesNotCompile(
          """
            |import com.kantefier.ora4s._
            |
            |@DbTable("qwerty")
            |case class Table_T(@Field str: String)
            |object Table_T extends TableCompanion[Table_T]
          """.stripMargin)
      }

      "if it doesn't have a companion" in {
        assertDoesNotCompile(
          """
            |import com.kantefier.ora4s._
            |
            |@DbTable("qwerty")
            |case class Table_T(@Field str: String) extends Table
          """.stripMargin)
      }

      "if its companion object doesn't extend TableCompanion" in {
        assertDoesNotCompile(
          """
            |import com.kantefier.ora4s._
            |
            |@DbTable("qwerty")
            |case class Table_T(@Field str: String) extends Table
            |object Table_T
          """.stripMargin)
      }

    }
    "should compile for accepted parameter types" - {
      "Int field" in {
        assertCompiles(
          """
            |import com.kantefier.ora4s._
            |
            |@DbTable("qwerty")
            |case class Table_T(
            | @Field asd: Int
            | ) extends Table
            |object Table_T extends TableCompanion[Table_T]
          """.stripMargin
        )
      }

      "String field" in {
        assertCompiles(
          """
            |import com.kantefier.ora4s._
            |
            |@DbTable("qwerty")
            |case class Table_T(
            | @Field asd: String
            | ) extends Table
            |object Table_T extends TableCompanion[Table_T]
          """.stripMargin
        )
      }
      
      "Long field" in {
        assertCompiles(
          """
            |import com.kantefier.ora4s._
            |
            |@DbTable("qwerty")
            |case class Table_T(
            | @Field asd: Long
            | ) extends Table
            |object Table_T extends TableCompanion[Table_T]
          """.stripMargin
        )
      }

      "Array[Byte] field" in {
        assertCompiles(
          """
            |import com.kantefier.ora4s._
            |
            |@DbTable("qwerty")
            |case class Table_T(
            | @Field asd: Array[Byte]
            | ) extends Table
            |object Table_T extends TableCompanion[Table_T]
          """.stripMargin
        )
      }

      //TODO: Enumeration field (Int, String)
      //TODO: Optional fields

      
    }

    "should fail for unsupported types" ignore {

    }
  }
}
