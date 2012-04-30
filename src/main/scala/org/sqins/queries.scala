package org.sqins

case class IncompleteSelectStatement(select: Expression) {
  def FROM(from: FromItem) = SelectStatement(select, from)
}

case class SelectStatement(select: Expression, from: FromItem, where: Option[Condition] = None, orderBy: Option[Expression] = None) {
  /**
   * Build the expression representing this query
   */
  def expression = {
    var expression = "%1$s FROM %2$s".format(select.expressionString, from.fromExpression)

    where match {
      case Some(where: Condition) => expression = "%1$s WHERE %2$s".format(expression, where.conditionExpression)
      case None                   => // ignore
    }

    orderBy match {
      case Some(orderBy: Expression) => expression = "%1$s ORDER BY %2$s".format(expression, orderBy.expressionString)
      case None                      => // ignore
    }
    
    expression
  }
  
  def WHERE(where: Condition) = SelectStatement(select, from, Some(where), orderBy)
  
  def ORDER_BY(orderBy: Expression) = SelectStatement(select, from, where, Some(orderBy))
}

object SELECT {
  def apply(expression: Expression) = IncompleteSelectStatement(expression)
}