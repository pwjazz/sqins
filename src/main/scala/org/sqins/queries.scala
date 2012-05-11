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
  format(sql.query, sql.params.map(_.actual).mkString(", "), cause.getMessage), cause)

/**
 * A SELECT query missing its FROM clause.
 */
case class IncompleteSelectQuery[T](select: Extractable[T] with Expression, distinct: Boolean) {
  def FROM(from: FromItem) = SelectQuery(select, distinct, from)
}

trait BaseSelectQuery[T] extends ScalarExpression with Extractable[T] {
  def select: Extractable[T] with Expression
  def distinct: Boolean
  def from: FromItem
  def where: Option[Condition]
  def orderBy: Option[Expression]
  def groupBy: Option[Expression]
  def limit: Option[BoundValue[Long]]
  def offset: Option[BoundValue[Long]]

  /**
   * Build the expression representing this query
   */
  def queryExpression = {
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
      case None                          => // ignore
    }

    offset match {
      case Some(offset: BoundValue[Long]) => expression = "%1$s\nOFFSET ?".format(expression)
      case None                           => // ignore
    }

    expression
  }

  def expression = "(%1$s)".format(queryExpression)

  def extract(rs: ResultSet, position: Int) = select.extract(rs, position)

  override def boundValues = {
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
      case None                          => // ignore
    }

    offset match {
      case Some(offset: BoundValue[Long]) => boundValues ++= Seq(offset)
      case None                           => // ignore
    }
    boundValues
  }

  override def toString = queryExpression
}

/**
 * A SELECT query that can be executed as a function.
 */
case class SelectQuery[T](select: Extractable[T] with Expression,
                          distinct: Boolean,
                          from: FromItem,
                          where: Option[Condition] = None,
                          orderBy: Option[Expression] = None,
                          groupBy: Option[Expression] = None,
                          limit: Option[BoundValue[Long]] = None,
                          offset: Option[BoundValue[Long]] = None) extends BaseSelectQuery[T] with ScalarValue[T] {

  /**
   * Add a WHERE clause.
   */
  def WHERE(where: Condition) = copy(where = Some(where))

  /**
   * Add a GROUP BY clause.
   */
  def GROUP_BY(groupBy: Expression) = copy(groupBy = Some(groupBy))

  /**
   * Add an ORDER BY clause.
   */
  def ORDER_BY(orderBy: Expression) = copy(orderBy = Some(orderBy))

  /**
   * Add a LIMIT clause.
   */
  def LIMIT(limit: BoundValue[Long]) = copy(limit = Some(limit))

  /**
   * Add an OFFSET clause.
   */
  def OFFSET(offset: BoundValue[Long]) = copy(offset = Some(offset))

  def go(implicit conn: Connection) = apply(conn)

  def apply(implicit conn: Connection): SelectResult[T] = new SelectResult(SQL(queryExpression, boundValues).executeQuery, (rs: ResultSet) => select.extract(rs, 1).value)
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

/**
 * An INSERT ... VALUES query
 */
case class InsertValuesQuery[T, +K](into: IntoItem[T, K], values: Seq[BoundValue[_]]) {
  def insertExpression = "INSERT INTO %1$s VALUES(%2$s)".format(into.intoExpression, values.map(_ => "?").mkString(", "))

  def apply(implicit conn: Connection): K = {
    val ps = SQL(insertExpression, values).executeInsert(conn)
    val generatedKeys = ps.getGeneratedKeys
    if (generatedKeys.next) {
      into.primaryKey.extract(generatedKeys, 1).value
    } else {
      throw new Error("Inserting value failed for query: %1$s with values %2$s".format(insertExpression, values.mkString(", ")))
    }
  }

  def go(implicit conn: Connection) = apply(conn)

  override def toString = insertExpression
}

/**
 * An INSERT INTO ... SELECT ... FROM query without the FROM clause
 */
case class IncompleteInsertSelectQuery[T, T2](into: IntoItem[_, _], select: Extractable[T] with Expression, distinct: Boolean) {
  def FROM(from: FromItem) = InsertSelectQuery(into, select, distinct, from)
}

/**
 * An INSERT INTO ... SELECT ... FROM query
 */
case class InsertSelectQuery[T](into: IntoItem[_, _],
                                select: Extractable[T] with Expression,
                                distinct: Boolean,
                                from: FromItem,
                                where: Option[Condition] = None,
                                orderBy: Option[Expression] = None,
                                groupBy: Option[Expression] = None,
                                limit: Option[BoundValue[Long]] = None,
                                offset: Option[BoundValue[Long]] = None) extends BaseSelectQuery[T] {
  def insertExpression = "INSERT INTO %1$s\n%2$s".format(into.intoExpression, this.queryExpression)

  /**
   * Add a WHERE clause.
   */
  def WHERE(where: Condition) = copy(where = Some(where))

  /**
   * Add a GROUP BY clause.
   */
  def GROUP_BY(groupBy: Expression) = copy(groupBy = Some(groupBy))

  /**
   * Add an ORDER BY clause.
   */
  def ORDER_BY(orderBy: Expression) = copy(orderBy = Some(orderBy))

  /**
   * Add a LIMIT clause.
   */
  def LIMIT(limit: BoundValue[Long]) = copy(limit = Some(limit))

  /**
   * Add an OFFSET clause.
   */
  def OFFSET(offset: BoundValue[Long]) = copy(offset = Some(offset))

  override def boundValues = select.boundValues ++ super.boundValues

  def apply(implicit conn: Connection): Int = {
    SQL(insertExpression, boundValues).executeUpdate(conn)
  }

  def go(implicit conn: Connection) = apply(conn)

  override def toString = queryExpression
}

/**
 * Syntax support for building InsertQueries using the INSERT keyword.
 */
object INSERT {
  def INTO[T, K](into: IntoItem[T, K]) = into
}

case class IncompleteUpdateQuery[T](table: Table[T, _]) {
  def SET(set: Expression) = UpdateQuery(table, set)

  def SET(row: T) = UpdateQuery(table, table.setExpression(row), Some(table.primaryKey == row))
}

case class UpdateQuery[T](table: Table[T, _], set: Expression, where: Option[Condition] = None) {
  def WHERE(where: Condition) = UpdateQuery(table, set, Some(where))

  def baseExpression = "UPDATE %1$s\nSET %2$s".format(table.fromExpression, set.expression)

  def updateExpression = where match {
    case Some(where: Condition) => "%1$s\nWHERE %2$s".format(baseExpression, where.expression)
    case None                   => baseExpression
  }

  def apply(implicit conn: Connection): Int = {
    var boundValues = where match {
      case Some(where: Condition) => set.boundValues ++ where.boundValues
      case None                   => set.boundValues
    }

    SQL(updateExpression, boundValues).executeUpdate(conn)
  }

  def go(implicit conn: Connection) = apply(conn)

  override def toString = updateExpression
}

object UPDATE {
  def apply[T](table: Table[T, _]) = IncompleteUpdateQuery(table)
}

case class DeleteQuery[T](table: Table[T, _], where: Option[Condition] = None) {
  def WHERE(where: Condition) = DeleteQuery(table, Some(where))

  def baseExpression = "DELETE FROM %1$s".format(table.fromExpression)

  def deleteExpression = where match {
    case Some(where: Condition) => "%1$s\nWHERE %2$s".format(baseExpression, where.expression)
    case None                   => baseExpression
  }

  def apply(implicit conn: Connection) = {
    var boundValues = where match {
      case Some(where: Condition) => where.boundValues
      case None                   => Seq()
    }

    SQL(deleteExpression, boundValues).executeUpdate(conn)
  }

  def go(implicit conn: Connection) = apply(conn)

  override def toString = deleteExpression
}

object DELETE {
  def FROM[T](table: Table[T, _]) = DeleteQuery(table)
}