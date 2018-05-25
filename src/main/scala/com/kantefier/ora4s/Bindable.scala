package com.kantefier.ora4s

import java.io.{ByteArrayInputStream, InputStream}
import java.sql.{PreparedStatement, Timestamp}
import enumeratum.values.IntEnumEntry
import org.joda.time.DateTime

import scala.language.implicitConversions


sealed trait Bindable {
  def binder(idx: Int): PreparedStatement => Unit
  def performBind(idx: Int): PreparedStatement => PreparedStatement = { ps =>
    binder(idx)(ps)
    ps
  }
}

trait BoundValue {
  val perform: PreparedStatement => PreparedStatement
}

/**
  * Conversions-magnets for all those types that can be bound to a query
  */
object Bindable {
  /**
    * Primitives
    */
  implicit def fromString(str: String): Bindable = new Bindable {
    def binder(idx: Int): PreparedStatement => Unit = _.setString(idx, str)
  }
  implicit def fromInt(x: Int): Bindable = new Bindable {
    def binder(idx: Int): PreparedStatement => Unit = _.setInt(idx, x)
  }
  implicit def fromLong(l: Long): Bindable = new Bindable {
    def binder(idx: Int): PreparedStatement => Unit = _.setLong(idx, l)
  }

  /**
    * Blob
    */
  implicit def fromByteArray(byteArr: Array[Byte]): Bindable = new Bindable {
    def binder(idx: Int): PreparedStatement => Unit = _.setBinaryStream(idx, new ByteArrayInputStream(byteArr))
  }
  implicit def fromInputStream(inStream: InputStream): Bindable = new Bindable {
    def binder(idx: Int): PreparedStatement => Unit = _.setBinaryStream(idx, inStream)
  }

  /**
    * Enumeratum Int enum mapping
    */
  implicit def fromIntEnumEntry(enumEntry: IntEnumEntry): Bindable = new Bindable {
    def binder(idx: Int): PreparedStatement => Unit = _.setInt(idx, enumEntry.value)
  }

  /**
    * Joda's DateTime to Timestamp
    */
  implicit def fromDateTime(time: DateTime): Bindable = new Bindable {
    def binder(idx: Int): PreparedStatement => Unit = _.setTimestamp(idx, new Timestamp(time.getMillis))
  }

  /**
    * Nullable values
    */
  implicit def fromOptionValue[T](optVal: Option[T])(implicit ev: T => Bindable): Bindable = new Bindable {
    def binder(idx: Int): PreparedStatement => Unit = { ps =>
      optVal match {
        case Some(valueInside) => ev(valueInside).binder(idx)(ps)
          // why varchar?? can't tell. it just works.
        case None => ps.setNull(idx, java.sql.Types.VARCHAR)
      }
    }
  }
}

object Bind {
  def apply(idx: Int, value: Bindable): BoundValue = new BoundValue {
    val perform: PreparedStatement => PreparedStatement = value.performBind(idx)
  }
}
