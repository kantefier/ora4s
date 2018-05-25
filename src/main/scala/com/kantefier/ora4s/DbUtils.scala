package com.kantefier.ora4s

import java.sql.{Connection, ResultSet}

import scala.annotation.tailrec
import scala.util.Try

object DbUtils {
  /**
    * Query a single object from the first ResultSet row
    */
  def querySingle[T](sqlQuery: String, parser: ResultSetParser[T])(implicit conn: Connection): Try[T] = Try {
    val stmt = conn.createStatement()
    val rs = stmt.executeQuery(sqlQuery)
    rs.next()
    parser(rs)
  }

  def querySingle[T](bindQuery: String, binds: List[BoundValue], parser: ResultSetParser[T])(implicit conn: Connection): Try[T] = Try {
    val stmt = conn.prepareStatement(bindQuery).bindValues(binds)
    val rs = stmt.executeQuery()
    rs.next()
    parser(rs)
  }


  @tailrec
  private def rsParseList[T](rs: ResultSet, parser: ResultSetParser[T], accum: List[T] = Nil): List[T] = {
    if(rs.next()) {
      rsParseList(rs, parser, parser(rs) :: accum)
    } else accum
  }

  /**
    * Query a list of T's (could be empty!)
    */
  def queryList[T](sqlQuery: String, parser: ResultSetParser[T])(implicit conn: Connection): Try[List[T]] = Try {
    val stmt = conn.createStatement()
    rsParseList(stmt.executeQuery(sqlQuery), parser)
  }

  def queryList[T](bindQuery: String, binds: List[BoundValue], parser: ResultSetParser[T])(implicit conn: Connection): Try[List[T]] = Try {
    val ps = conn.prepareStatement(bindQuery).bindValues(binds)
    rsParseList(ps.executeQuery(), parser)
  }


  /**
    * INSERTS
    */

  def insertUnit(sqlQuery: String)(implicit conn: Connection): Try[Unit] = Try {
    val stmt = conn.prepareStatement(sqlQuery)
    stmt.executeUpdate()
    ()
  }

  def insertUnit(bindQuery: String, binds: List[BoundValue])(implicit conn: Connection): Try[Unit] = Try {
    val ps = conn.prepareStatement(bindQuery).bindValues(binds)
    ps.executeUpdate()
    ()
  }

  /**
    * Perform INSERT and retrieve generated key
    */
  def insertReturnGeneratedKey(sqlQuery: String, returnKeyFieldName: String)(implicit conn: Connection): Try[Long] = Try {
    val stmt = conn.prepareStatement(sqlQuery, Array(returnKeyFieldName))
    stmt.executeUpdate()
    val rs = stmt.getGeneratedKeys
    rs.next()
    rs.getLong(1)
  }

  def insertReturnGeneratedKey(bindQuery: String, binds: List[BoundValue], returnKeyFieldName: String)(implicit conn: Connection): Try[Long] = Try {
    val ps = conn.prepareStatement(bindQuery, Array(returnKeyFieldName)).bindValues(binds)
    ps.executeUpdate()
    val rs = ps.getGeneratedKeys
    rs.next()
    rs.getLong(1)
  }

  /**
    * Perform INSERT and retrieve generated key from column "id"
    */
  def insertReturnId(sqlQuery: String)(implicit conn: Connection): Try[Long] = insertReturnGeneratedKey(sqlQuery, "id")(conn)
  def insertReturnId(bindQuery: String, binds: List[BoundValue])(implicit conn: Connection): Try[Long] = insertReturnGeneratedKey(bindQuery, binds, "id")(conn)

}
