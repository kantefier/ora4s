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

      "Int Enumeration field" in {
        assertCompiles(
          """
            |import com.kantefier.ora4s._
            |import enumeratum.values.{IntEnum, IntEnumEntry}
            |
            |sealed abstract class CommChannel(val value: Int) extends IntEnumEntry
            |object CommChannel extends IntEnum[CommChannel] {
            | case object Sms extends CommChannel(1)
            | case object Email extends CommChannel(2)
            |
            | val values = findValues
            |}
            |
            |@DbTable("qwerty")
            |case class Table_T(
            | @Field asd: Option[Int],
            | @Field qwe: CommChannel
            | ) extends Table
            |object Table_T extends TableCompanion[Table_T]
          """.stripMargin
        )
      }

      "String Enumeration field" in {
        assertCompiles(
          """
            |import com.kantefier.ora4s._
            |import enumeratum.values.{StringEnum, StringEnumEntry}
            |
            |sealed abstract class CommChannelStr(val value: String) extends StringEnumEntry
            |object CommChannelStr extends StringEnum[CommChannelStr] {
            | case object Sms extends CommChannelStr("sms")
            | case object Email extends CommChannelStr("email")
            |
            | val values = findValues
            |}
            |
            |@DbTable("qwerty")
            |case class Table_T(
            | @Field asd: Option[Int],
            | @Field qwe: CommChannelStr
            | ) extends Table
            |object Table_T extends TableCompanion[Table_T]
          """.stripMargin
        )
      }

      "Option[Int] field" in {
        assertCompiles(
          """
            |import com.kantefier.ora4s._
            |
            |@DbTable("qwerty")
            |case class Table_T(
            | @Field asd: Option[Int]
            | ) extends Table
            |object Table_T extends TableCompanion[Table_T]
          """.stripMargin
        )
      }

      "Option[String] field" in {
        assertCompiles(
          """
            |import com.kantefier.ora4s._
            |
            |@DbTable("qwerty")
            |case class Table_T(
            | @Field asd: Option[String]
            | ) extends Table
            |object Table_T extends TableCompanion[Table_T]
          """.stripMargin
        )
      }

      "Option[Long] field" in {
        assertCompiles(
          """
            |import com.kantefier.ora4s._
            |
            |@DbTable("qwerty")
            |case class Table_T(
            | @Field asd: Option[Long]
            | ) extends Table
            |object Table_T extends TableCompanion[Table_T]
          """.stripMargin
        )
      }

      "Option[Array[Byte]] field" in {
        assertCompiles(
          """
            |import com.kantefier.ora4s._
            |
            |@DbTable("qwerty")
            |case class Table_T(
            | @Field asd: Option[Array[Byte]]
            | ) extends Table
            |object Table_T extends TableCompanion[Table_T]
          """.stripMargin
        )
      }

      "Optional Int Enumeration field" in {
        assertCompiles(
          """
            |import com.kantefier.ora4s._
            |import enumeratum.values.{IntEnum, IntEnumEntry}
            |
            |sealed abstract class CommChannel(val value: Int) extends IntEnumEntry
            |object CommChannel extends IntEnum[CommChannel] {
            | case object Sms extends CommChannel(1)
            | case object Email extends CommChannel(2)
            |
            | val values = findValues
            |}
            |
            |@DbTable("qwerty")
            |case class Table_T(
            | @Field asd: Option[Int],
            | @Field qwe: Option[CommChannel]
            | ) extends Table
            |object Table_T extends TableCompanion[Table_T]
          """.stripMargin
        )
      }
    }

    "should fail for unsupported types" - {
      "fails for nested Options" in {
        assertDoesNotCompile(
          """
            |import com.kantefier.ora4s._
            |
            |@DbTable("qwerty")
            |case class Table_T(
            | @Field asd: Option[Option[Array[Byte]]]
            | ) extends Table
            |object Table_T extends TableCompanion[Table_T]
          """.stripMargin
        )
      }
    }
  }
}
