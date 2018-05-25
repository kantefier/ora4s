package com.kantefier

import enumeratum.values.{IntEnum, IntEnumEntry, StringEnum, StringEnumEntry}


package object ora4stest {
  sealed abstract class CommChannel(val value: Int) extends IntEnumEntry
  object CommChannel extends IntEnum[CommChannel] {
    case object Sms extends CommChannel(1)
    case object Email extends CommChannel(2)
    val values = findValues
  }

  sealed abstract class CommChannelStr(val value: String) extends StringEnumEntry
  object CommChannelStr extends StringEnum[CommChannelStr] {
    case object Sms extends CommChannelStr("sms")
    case object Email extends CommChannelStr("email")
    val values = findValues
  }
}
