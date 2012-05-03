package org.sqins

import java.sql.Connection
import java.sql.PreparedStatement
import java.sql.ResultSet

case class IncompleteSelectStatement[T](select: Extractable[T], distinct: Boolean) {
  def FROM(from: FromItem) = SelectStatement(select, distinct, from)
}

case class SelectStatement[T](select: Extractable[T], distinct: Boolean, from: FromItem, where: Option[Condition] = None, orderBy: Option[Expression] = None, groupBy: Option[Expression] = None) {
  /**
   * Build the expression representing this query
   */
  def expression = {
    var selectKeyword = distinct match {
      case true => "SELECT DISTINCT"
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
    
    expression
  }

  def WHERE(where: Condition) = SelectStatement(select, distinct, from, Some(where), orderBy, groupBy)
  
  def GROUP_BY(groupBy: Expression) = SelectStatement(select, distinct, from, where, orderBy, Some(groupBy))

  def ORDER_BY(orderBy: Expression) = SelectStatement(select, distinct, from, where, Some(orderBy), groupBy)
  
  def apply(implicit conn: Connection) = {
    val ps = conn.prepareStatement(expression)
    var position = 1
    position = select.bind(ps, position)
    // TODO: handle sub-queries in bind
    // position = from.bind(ps, position)
    where match {
      case Some(where: Condition) => position = where.bind(ps, position)
      case None                   => // ignore
    }

    groupBy match {
      case Some(groupBy: Expression) => position = groupBy.bind(ps, position)
      case None                      => // ignore
    }
    
    orderBy match {
      case Some(orderBy: Expression) => position = orderBy.bind(ps, position)
      case None                      => // ignore
    }

    new SelectResult(ps.executeQuery, (rs: ResultSet) => select.extract(rs, 1).value)
  }

  override def toString = expression
}

object SELECT {
  def apply[T](expression: Extractable[T]) = IncompleteSelectStatement(expression, false)
  
  def DISTINCT[T](expression: Extractable[T]) = IncompleteSelectStatement(expression, true)
}

class SelectResult[T](rs: ResultSet, rowReader: (ResultSet => T)) extends Iterable[T] {
  def iterator = new SelectResultIterator(rs, rowReader)
}

class SelectResultIterator[T](rs: ResultSet, rowReader: (ResultSet => T)) extends Iterator[T] {
  private var needsRead = true

  def hasNext = {
    if (!needsRead) true
    needsRead = false
    rs.next
  }

  def next = rowReader(rs)
}