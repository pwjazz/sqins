package org.sqins

import java.sql.Connection
import java.sql.PreparedStatement
import java.sql.ResultSet

case class IncompleteSelectStatement[T](select: Extractable[T]) {
  def FROM(from: FromItem) = SelectStatement(select, from)
}

case class SelectStatement[T](select: Extractable[T], from: FromItem, where: Option[Condition] = None, orderBy: Option[Expression] = None) {
  /**
   * Build the expression representing this query
   */
  def expression = {
    var expression = "SELECT %1$s FROM %2$s".format(select.selectExpression, from.fromExpression)

    where match {
      case Some(where: Condition) => expression = "%1$s WHERE %2$s".format(expression, where.expression)
      case None                   => // ignore
    }

    orderBy match {
      case Some(orderBy: Expression) => expression = "%1$s ORDER BY %2$s".format(expression, orderBy.expression)
      case None                      => // ignore
    }

    expression
  }

  def WHERE(where: Condition) = SelectStatement(select, from, Some(where), orderBy)

  def ORDER_BY(orderBy: Expression) = SelectStatement(select, from, where, Some(orderBy))

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

    orderBy match {
      case Some(orderBy: Expression) => position = orderBy.bind(ps, position)
      case None                      => // ignore
    }

    new SelectResult(ps.executeQuery, (rs: ResultSet) => select.extract(rs))
  }

  override def toString = expression
}

object SELECT {
  def apply[T](expression: Extractable[T]) = IncompleteSelectStatement(expression)
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