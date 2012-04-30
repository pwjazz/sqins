package org.sqins

/**
 * An expression that represents (or returns) a value.  Can be turned into a Condition using the operators
 * == <> != > < ISNULL ISNOTNULL
 */
trait Value {
  def valueExpression: String

  def ==(right: Value) = Comparison(this, "=", right)

  def <>(right: Value) = Comparison(this, "<>", right)

  def !=(right: Value) = this.<>(right)

  def >(right: Value) = Comparison(this, ">", right)

  def <(right: Value) = Comparison(this, "<", right)

  def ISNULL = Null(this)

  def ISNOTNULL = NotNull(this)
}

case class BoundValue[+T](actual: T)(implicit typeMapping: TypeMapping[T]) extends Value {
  val valueExpression = "?"
}

object Bind {
  def apply[T](value: T)(implicit typeMapping: TypeMapping[T]) = BoundValue(value)(typeMapping)
}

/**
 * Represents an expression that evaluates to a boolean like A = B, A <> B, A IS NULL, etc.
 */
trait Condition {
  def conditionExpression: String

  def &&(left: Condition) = CompoundConditional(left, "AND", this)

  def ||(left: Condition) = CompoundConditional(left, "OR", this)
}

case class Comparison(left: Value, comparison: String, right: Value) extends Condition {
  val conditionExpression = "%1$s %2$s %3$s".format(left.valueExpression, comparison, right.valueExpression)
}

case class Null(valueExpression: Value) extends Condition {
  val conditionExpression = "%1$s IS NULL".format(valueExpression.valueExpression)
}

case class NotNull(valueExpression: Value) extends Condition {
  val conditionExpression = "%1$s IS NOT NULL".format(valueExpression.valueExpression)
}

case class CompoundConditional(left: Condition, composition: String, right: Condition) extends Condition {
  val conditionExpression = "%1$s %2$s %3$s".format(left.conditionExpression, composition, right.conditionExpression)
}

/**
 * Represents an expression such as a column or list of columns, functions, etc
 */
trait Expression {
  def expressionString: String

  def ::(left: Expression) = CompoundExpression(left, this)

  override def toString = expressionString
}

case class CompoundExpression(left: Expression, right: Expression) extends Expression {
  def expressionString = "%1$s, $2$s".format(left, right)
}

object ListExpression {
  def apply(list: List[Expression]) = list.reduce { (left: Expression, right: Expression) => CompoundExpression(left, right) }
}

case class RelationExpression(relation: Relation) extends Expression {
  def expressionString = relation.columns.mkString(", ")
}

trait SortableExpression { self: Expression =>
  def ASC = SortedExpression(self, "ASC")
  
  def DESC = SortedExpression(self, "DESC")
}

case class SortedExpression(left: Expression, sort: String) extends Expression {
  def expressionString = "%1$s %2$s".format(left.expressionString, sort) 
}

/**
 * Represents an expression from which one can select, such as a table, list of tables, joined tables.
 */
trait FromItem {
  def fromExpression: String

  def ::(left: FromItem) = CompoundFromItem(left, this)

  def INNER_JOIN(right: Relation) = IncompleteJoin(this, "INNER JOIN", right)
}

/**
 * A list of from items separated by comma.
 */
case class CompoundFromItem(left: FromItem, right: FromItem) extends FromItem {
  def fromExpression = "%1$s, $2$s".format(left, right)
}

case class IncompleteJoin(left: FromItem, joinType: String, right: Relation) {
  def ON(on: Condition) = Join(left, joinType, right, on)
}

/**
 * A from item representing a join.
 */
case class Join(left: FromItem, joinType: String, right: Relation, on: Condition) extends FromItem {
  def fromExpression = "%1$s %2$s %3$s ON %4$s".format(left, joinType, right, on)
}

/**
 * A named list of columns (table, alias, etc.)
 */
abstract class Relation(_name: String) extends FromItem {
  val name = _name;

  var alias: Option[String] = None

  var columns = List[Column[Any]]()

  /**
   * Use the alias if set, otherwise the name.
   */
  def aliasedName = alias match {
    case Some(alias: String) => alias
    case _                   => name
  }

  def fromExpression = alias match {
    case Some(alias: String) => "%1$s AS %2$s".format(name, alias)
    case _                   => name
  }

  def addColumn(column: Column[Any]) = columns = (column :: columns).reverse
}