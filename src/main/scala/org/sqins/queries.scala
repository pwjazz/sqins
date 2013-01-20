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
  def executeQuery(implicit conn: Connection) = {
    val ps = conn.prepareStatement(query)
    try {
      buildStatement(ps).executeQuery
      ps
    } catch {
      case e: Exception => throw new SQLError(this, e)
    }
  }

  def executeUpdate(implicit conn: Connection) = {
    val ps = conn.prepareStatement(query)
    try {
      buildStatement(ps).executeUpdate
      ps
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
case class IncompleteSelectQuery[T](select: Extractable[T], distinct: Boolean) {
  def FROM(from: FromItem) = SelectQuery(select, distinct, from)
}

/**
 * Base class for all variants of SELECT query, including SELECT and INSERT INTO ... SELECT ...
 */
private[sqins] trait BaseSelectQuery[T] extends ScalarExpression with Extractable[T] {
  def select: Extractable[T]
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
    var expression = selectKeyword + " " + select.selectExpression + "\nFROM " + from.fromExpression

    where match {
      case Some(where: Condition) => expression = expression + "\nWHERE " + where.expression
      case None                   => // ignore
    }

    groupBy match {
      case Some(groupBy: Expression) => expression = expression + "\nGROUP BY " + groupBy.expression
      case None                      => // ignore
    }

    orderBy match {
      case Some(orderBy: Expression) => expression = expression + "\nORDER BY " + orderBy.expression
      case None                      => // ignore
    }

    limit match {
      case Some(limit: BoundValue[_]) => expression = expression + "\nLIMIT ?"
      case None                          => // ignore
    }

    offset match {
      case Some(offset: BoundValue[_]) => expression = expression + "\nOFFSET ?"
      case None                           => // ignore
    }

    expression
  }

  def expression = "(" + queryExpression + ")"

  def extract(rs: ResultSet, position: Int) = select.extract(rs, position)

  override def boundValues = {
    var boundValues = select.boundValues ++ from.boundValues
    
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
      case Some(limit: BoundValue[_]) => boundValues ++= Seq(limit)
      case None                          => // ignore
    }

    offset match {
      case Some(offset: BoundValue[_]) => boundValues ++= Seq(offset)
      case None                           => // ignore
    }
    boundValues
  }

  override def toString = queryExpression
}

/**
 * A SELECT query that can be executed as a function.
 */
case class SelectQuery[T](select: Extractable[T],
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
  def apply[T](expression: Extractable[T]) = IncompleteSelectQuery(expression, false)

  def DISTINCT[T](expression: Extractable[T]) = IncompleteSelectQuery(expression, true)
}

/**
 * The result of executing a SELECT query.
 */
class SelectResult[T](ps: PreparedStatement, rowReader: (ResultSet => T)) extends Iterable[T] {
  def iterator = new SelectResultIterator(ps, rowReader)
}

protected class SelectResultIterator[T](ps: PreparedStatement, rowReader: (ResultSet => T)) extends Iterator[T] {
  private val rs = ps.getResultSet()
  private var needsRead = true

  def hasNext = {
    if (!needsRead) true
    needsRead = false
    rs.next
  }

  def next = rowReader(rs)

  override protected def finalize(): Unit = {
    try {
      ps.close()
    } catch {
      case e: Throwable => // ignore
    }
    try {
      rs.close()
    } catch {
      case e: Throwable => // ignore
    }
  }
}

/**
 * An INSERT ... VALUES query
 */
case class InsertValuesQuery[T](into: IntoItem[T], values: Seq[BoundValue[_]]) {
  def insertExpression = "INSERT INTO " + into.intoExpression + " VALUES(" + values.map(_ => "?").mkString(", ") + ")"

  def RETURNING[R](returning: Extractable[R]) = InsertValuesQueryReturning(this, returning)

  def apply(implicit conn: Connection): Int = {
    val ps = SQL(insertExpression, values).executeUpdate(conn)
    ps.getUpdateCount()
  }

  def go(implicit conn: Connection) = apply(conn)

  override def toString = insertExpression
}

case class InsertValuesQueryReturning[T, R](query: InsertValuesQuery[T], returning: Extractable[R]) {
  def insertExpression = query.insertExpression + "\nRETURNING " + returning.unaliasedExpression

  def apply(implicit conn: Connection): R = {
    val ps = SQL(insertExpression, query.values ++ returning.boundValues).executeQuery(conn)
    try {
      val rs = ps.getResultSet()
      try {
        rs.next()
        returning.extract(rs, 1).value
      } finally {
        try {
          rs.close()
        } catch {
          case e: Throwable => // ignore
        }
      }
    } finally {
      try {
        ps.close()
      } catch {
        case e: Throwable => // ignore
      }
    }
  }

  def go(implicit conn: Connection) = apply(conn)

  override def toString = insertExpression
}

/**
 * Start to build the SELECT clause of this INSERT query.
 */
case class InsertSelectQueryBuilder(into: IntoItem[_]) {
  def SELECT[V](select: Extractable[V]) = IncompleteInsertSelectQuery(into, select, false)
}

/**
 * An INSERT INTO ... SELECT ... FROM query without the FROM clause
 */
case class IncompleteInsertSelectQuery[T, T2](into: IntoItem[_], select: Extractable[T], distinct: Boolean) {
  def FROM(from: FromItem) = InsertSelectQuery(into, select, distinct, from)
}

/**
 * An INSERT INTO ... SELECT ... FROM query
 */
case class InsertSelectQuery[T](into: IntoItem[_],
                                select: Extractable[T],
                                distinct: Boolean,
                                from: FromItem,
                                where: Option[Condition] = None,
                                orderBy: Option[Expression] = None,
                                groupBy: Option[Expression] = None,
                                limit: Option[BoundValue[Long]] = None,
                                offset: Option[BoundValue[Long]] = None) extends BaseSelectQuery[T] {
  def insertExpression = "INSERT INTO " + into.intoExpression + "\n" + this.queryExpression

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

  def RETURNING[R](returning: Extractable[R]) = InsertSelectQueryReturning(this, returning)

  override def boundValues = select.boundValues ++ super.boundValues

  def apply(implicit conn: Connection): Int = {
    val ps = SQL(insertExpression, boundValues).executeUpdate(conn)
    ps.getUpdateCount()
  }

  def go(implicit conn: Connection) = apply(conn)

  override def toString = queryExpression
}

trait QueryReturning[R] {
  def returning: Extractable[R]

  protected def originalExpression: String

  protected def originalBoundValues: Seq[BoundValue[_]]

  protected def returningExpression = returning.expression

  protected def fullExpression = originalExpression + "\nRETURNING " + returningExpression
  
  private def boundValues = originalBoundValues ++ returning.boundValues

  def apply(implicit conn: Connection): SelectResult[R] = {
    val ps = SQL(fullExpression, boundValues).executeQuery(conn)
    new SelectResult(ps, (rs: ResultSet) => returning.extract(rs, 1).value)
  }

  def go(implicit conn: Connection) = apply(conn)

  override def toString = fullExpression
}

case class InsertSelectQueryReturning[T, R](query: InsertSelectQuery[T], returning: Extractable[R]) extends QueryReturning[R] {
  protected def originalExpression = query.insertExpression

  protected def originalBoundValues = query.boundValues
  
  override protected def returningExpression = returning.unaliasedExpression
  
  def insertExpression = fullExpression
}

/**
 * Syntax support for building InsertQueries using the INSERT keyword.
 */
object INSERT {
  def INTO[T](into: IntoItem[T]) = into
}

case class IncompleteUpdateQuery[T](table: Table[T]) {
  def SET(set: Expression) = UpdateQuery(table, set)

  def SET(row: T) = UpdateQuery(table, table.setExpression(row))
}

case class UpdateQuery[T](table: Table[T], set: Expression, where: Option[Condition] = None) {
  def WHERE(where: Condition) = UpdateQuery(table, set, Some(where))

  def baseExpression = "UPDATE " + table.fromExpression + "\nSET " + set.expression

  def updateExpression = where match {
    case Some(where: Condition) => baseExpression + "\nWHERE " + where.expression
    case None                   => baseExpression
  }

  def boundValues = where match {
    case Some(where: Condition) => set.boundValues ++ where.boundValues
    case None                   => set.boundValues
  }

  def RETURNING[R](returning: Extractable[R]) = UpdateQueryReturning(this, returning)

  def apply(implicit conn: Connection): Int = {
    val ps = SQL(updateExpression, boundValues).executeUpdate(conn)
    try {
      ps.getUpdateCount()
    } finally {
      try {
        ps.close()
      } catch {
        case e: Throwable => // ignore
      }
    }
  }

  def go(implicit conn: Connection) = apply(conn)

  override def toString = updateExpression
}

case class UpdateQueryReturning[T, R](query: UpdateQuery[T], returning: Extractable[R]) extends QueryReturning[R] {
  protected def originalExpression = query.updateExpression

  protected def originalBoundValues = query.boundValues
  
  def updateExpression = fullExpression
}

object UPDATE {
  def apply[T](table: Table[T]) = IncompleteUpdateQuery(table)
}

case class DeleteQuery[T](table: Table[T], where: Option[Condition] = None) {
  def WHERE(where: Condition) = DeleteQuery(table, Some(where))

  def baseExpression = "DELETE FROM " + table.fromExpression

  def deleteExpression = where match {
    case Some(where: Condition) => baseExpression + "\nWHERE " + where.expression
    case None                   => baseExpression
  }

  def RETURNING[R](returning: Extractable[R]) = DeleteQueryReturning(this, returning)

  def boundValues = where match {
    case Some(where: Condition) => where.boundValues
    case None                   => Seq()
  }

  def apply(implicit conn: Connection): Int = {
    val ps = SQL(deleteExpression, boundValues).executeUpdate(conn)
    try {
      ps.getUpdateCount()
    } finally {
      try {
        ps.close()
      } catch {
        case e: Throwable => // ignore
      }
    }
  }

  def go(implicit conn: Connection) = apply(conn)

  override def toString = deleteExpression
}

case class DeleteQueryReturning[T, R](query: DeleteQuery[T], returning: Extractable[R]) extends QueryReturning[R] {
  protected def originalExpression = query.deleteExpression

  protected def originalBoundValues = query.boundValues
  
  def deleteExpression = fullExpression
}

object DELETE {
  def FROM[T](table: Table[T]) = DeleteQuery(table)
}