/*
Copyright (c) 2012, Percy Wegmann
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the <organization> nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
*/

package org.sqins

import java.sql.Connection
import java.sql.PreparedStatement
import java.sql.ResultSet

/**
 * A generic SQL query that takes a text query and an optional seq of parameters.
 */
case class SQL(query: String, params: Seq[BoundValue[_]] = Seq()) {
  def executeUpdate(implicit conn: Connection) = {
    val ps = conn.prepareStatement(query)
    try {
      buildStatement(ps).executeUpdate
    } catch {
      case e: Exception => throw new SQLError(this, e)
    }
  }

  def executeInsert(implicit conn: Connection) = {
    val ps = conn.prepareStatement(query, java.sql.Statement.RETURN_GENERATED_KEYS)
    try {
      buildStatement(ps).executeUpdate
      ps
    } catch {
      case e: Exception => throw new SQLError(this, e)
    }
  }

  def executeQuery(implicit conn: Connection) = {
    val ps = conn.prepareStatement(query)
    try {
      buildStatement(ps).executeQuery
    } catch {
      case e: Exception => throw new SQLError(this, e)
    }
  }

  private def buildStatement(ps: PreparedStatement) = {
    var position = 1
    params.foreach(param => {
      position += param.bind(ps, position)
    })
    ps
  }
}

/**
 * An error that occurred while processing SQL.
 */
class SQLError(sql: SQL, cause: Exception) extends RuntimeException("Error executing SQL \"%1$s\" with params (%2$s) failed: %3$s".
  format(sql.query, sql.params.map(_.actual).mkString(","), cause.getMessage), cause)

/**
 * An INSERT query that can be executed as a function.
 */
case class InsertValuesQuery[T, +K](into: IntoItem[T, K], values: Seq[BoundValue[_]]) {
  def expression = "INSERT INTO %1$s VALUES(%2$s)".format(into.intoExpression, values.map(_ => "?").mkString(", "))

  def apply(implicit conn: Connection): Option[K] = {
    val ps = SQL(expression, values).executeInsert(conn)
    into.primaryKey.flatMap(extractor => {
      val generatedKeys = ps.getGeneratedKeys
      if (generatedKeys.next) {
        Some(extractor.extract(generatedKeys, 1).value)
      } else {
        None
      }
    })
  }
  
  def go(implicit conn: Connection) = apply(conn)
  
  override def toString = expression
}

/**
 * Syntax support for building InsertQueries using the INSERT keyword.
 */
object INSERT {
  def INTO[T, K](into: IntoItem[T, K]) = into
}

case class IncompleteUpdateQuery[T](table: Table[T, _]) {
  def SET(set: Expression) = UpdateQuery(table, set)
}

case class UpdateQuery[T](table: Table[T, _], set: Expression, where: Option[Condition] = None) {
  def WHERE(where: Condition) = UpdateQuery(table, set, Some(where))

  def baseExpression = "UPDATE %1$s\nSET %2$s".format(table.fromExpression, set.expression)

  def expression = where match {
    case Some(where: Condition) => "%1$s\nWHERE %2$s".format(baseExpression, where.expression)
    case None                   => baseExpression
  }

  def apply(implicit conn: Connection) = {
    var boundValues = where match {
      case Some(where: Condition) => set.boundValues ++ where.boundValues
      case None                   => set.boundValues
    }

    SQL(expression, boundValues).executeUpdate(conn)
  }
  
  def go(implicit conn: Connection) = apply(conn)
  
  override def toString = expression
}

object UPDATE {
  def apply[T](table: Table[T, _]) = IncompleteUpdateQuery(table)
}

case class DeleteQuery[T](table: Table[T, _], where: Option[Condition] = None) {
  def WHERE(where: Condition) = DeleteQuery(table, Some(where))

  def baseExpression = "DELETE FROM %1$s".format(table.fromExpression)

  def expression = where match {
    case Some(where: Condition) => "%1$s\nWHERE %2$s".format(baseExpression, where.expression)
    case None                   => baseExpression
  }

  def apply(implicit conn: Connection) = {
    var boundValues = where match {
      case Some(where: Condition) => where.boundValues
      case None                   => Seq()
    }

    SQL(expression, boundValues).executeUpdate(conn)
  }
  
  def go(implicit conn: Connection) = apply(conn)
  
  override def toString = expression
}

object DELETE {
  def FROM[T](table: Table[T, _]) = DeleteQuery(table)
}

/**
 * A SELECT query missing its FROM clause.
 */
case class IncompleteSelectQuery[T](select: Extractable[T] with Expression, distinct: Boolean) {
  def FROM(from: FromItem) = SelectQuery(select, distinct, from)
}

/**
 * A SELECT query that can be executed as a function.
 */
case class SelectQuery[T](select: Extractable[T] with Expression, distinct: Boolean, from: FromItem, where: Option[Condition] = None, orderBy: Option[Expression] = None, groupBy: Option[Expression] = None, limit: Option[BoundValue[Long]] = None, offset: Option[BoundValue[Long]] = None) {
  /**
   * Build the expression representing this query
   */
  def expression = {
    var selectKeyword = distinct match {
      case true  => "SELECT DISTINCT"
      case false => "SELECT"
    }
    var expression = "%1$s %2$s\nFROM %3$s".format(selectKeyword, select.selectExpression, from.fromExpression)

    where match {
      case Some(where: Condition) => expression = "%1$s\nWHERE %2$s".format(expression, where.expression)
      case None                   => // ignore
    }

    groupBy match {
      case Some(groupBy: Expression) => expression = "%1$s\nGROUP BY %2$s".format(expression, groupBy.expression)
      case None                      => // ignore
    }

    orderBy match {
      case Some(orderBy: Expression) => expression = "%1$s\nORDER BY %2$s".format(expression, orderBy.expression)
      case None                      => // ignore
    }
    
    limit match {
      case Some(limit: BoundValue[Long]) => expression = "%1$s\nLIMIT ?".format(expression)
      case None                      => // ignore
    }

    offset match {
      case Some(offset: BoundValue[Long]) => expression = "%1$s\nOFFSET ?".format(expression)
      case None                      => // ignore
    }
    
    expression
  }

  /**
   * Add a WHERE clause.
   */
  def WHERE(where: Condition) = SelectQuery(select, distinct, from, Some(where), orderBy, groupBy, limit, offset)

  /**
   * Add a GROUP BY clause.
   */
  def GROUP_BY(groupBy: Expression) = SelectQuery(select, distinct, from, where, orderBy, Some(groupBy), limit, offset)

  /**
   * Add an ORDER BY clause.
   */
  def ORDER_BY(orderBy: Expression) = SelectQuery(select, distinct, from, where, Some(orderBy), groupBy, limit, offset)
  
  /**
   * Add a LIMIT clause.
   */
  def LIMIT(limit: BoundValue[Long]) = SelectQuery(select, distinct, from, where, orderBy, groupBy, Some(limit), offset)
  
  /**
   * Add an OFFSET clause.
   */
  def OFFSET(offset: BoundValue[Long]) = SelectQuery(select, distinct, from, where, orderBy, groupBy, limit, Some(offset))

  def apply(implicit conn: Connection) = {
    var boundValues = select.boundValues
    where match {
      case Some(where: Condition) => boundValues ++= where.boundValues
      case None                   => // ignore
    }

    groupBy match {
      case Some(groupBy: Expression) => boundValues ++= groupBy.boundValues
      case None                      => // ignore
    }

    orderBy match {
      case Some(orderBy: Expression) => boundValues ++= orderBy.boundValues
      case None                      => // ignore
    }
    
    limit match {
      case Some(limit: BoundValue[Long]) => boundValues ++= Seq(limit)
      case None                      => // ignore
    }

    offset match {
      case Some(offset: BoundValue[Long]) => boundValues ++= Seq(offset)
      case None                      => // ignore
    }

    new SelectResult(SQL(expression, boundValues).executeQuery, (rs: ResultSet) => select.extract(rs, 1).value)
  }
  
  def go(implicit conn: Connection) = apply(conn)

  override def toString = expression
}

/**
 * Syntax support for building SelectQuerys using the SELECT keyword.
 */
object SELECT {
  def apply[T](expression: Extractable[T] with Expression) = IncompleteSelectQuery(expression, false)

  def DISTINCT[T](expression: Extractable[T] with Expression) = IncompleteSelectQuery(expression, true)
}

/**
 * The result of executing a SELECT query.
 */
class SelectResult[T](rs: ResultSet, rowReader: (ResultSet => T)) extends Iterable[T] {
  def iterator = new SelectResultIterator(rs, rowReader)
}

protected class SelectResultIterator[T](rs: ResultSet, rowReader: (ResultSet => T)) extends Iterator[T] {
  private var needsRead = true

  def hasNext = {
    if (!needsRead) true
    needsRead = false
    rs.next
  }

  def next = rowReader(rs)
}

object on {
  def apply[T](conn: Connection)(fn: (Connection) => T) = fn(conn)
}

object transaction {
  def apply[T](conn: Connection)(fn: (Connection) => T) = {
    val originalAutoCommit = conn.getAutoCommit()
    conn.setAutoCommit(false)
    try {
      fn(conn)
      conn.commit()
    } finally {
      conn.setAutoCommit(originalAutoCommit)
    }
  }
}
