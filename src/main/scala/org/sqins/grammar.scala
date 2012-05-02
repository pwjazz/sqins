package org.sqins

import java.sql.{ PreparedStatement, ResultSet }

/**
 * Represents an expression such as a column, a scalar, a function call, or a list of these things
 */
trait Expression {
  def expression: String

  def selectExpression = expression

  /**
   * Bind this expression at the given position on the given PreparedStatement, returning the position at which
   * the next Expression should be bound.  THe expression needs to know its own state for binding.
   *
   * The default implementation does nothing and returns the same position
   */
  def bind(ps: PreparedStatement, position: Int) = position

  override def toString = expression
}

/**
 * Something that can be extracted from a JDBC ResultSet.
 */
trait Extractable[+T] extends Expression {
  /**
   * Extract the value from the given ResultSet
   */
  def extract(rs: ResultSet): T
}

/**
 * A single-valued expression, such as a column, function call, scalar value, etc.
 */
trait UnaryExpression extends Expression {
  def ==(right: UnaryExpression) = Comparison(this, "=", right)

  def <>(right: UnaryExpression) = Comparison(this, "<>", right)

  def !=(right: UnaryExpression) = this.<>(right)

  def >(right: UnaryExpression) = Comparison(this, ">", right)

  def <(right: UnaryExpression) = Comparison(this, "<", right)

  def ISNULL = Null(this)

  def ISNOTNULL = NotNull(this)

  def ASC = SortedExpression(this, "ASC")

  def DESC = SortedExpression(this, "DESC")
}

/**
 * An expression that represents (or returns) a value.  Can be turned into a Condition using the operators
 * == <> != > < ISNULL ISNOTNULL
 */
trait Value[+T] extends UnaryExpression with Extractable[T]

case class BoundValue[+T](actual: T)(implicit typeMapping: TypeMapping[T]) extends Value[T] {
  val expression = "?"

  def extract(rs: ResultSet) = actual

  override def bind(ps: PreparedStatement, position: Int) = {
    typeMapping.set(ps, position, actual)
    position + 1
  }
}

object Bind {
  def apply[T](value: T)(implicit typeMapping: TypeMapping[T]) = BoundValue(value)(typeMapping)
}

/**
 * A named list of columns (table, alias, etc.)
 */
abstract class Relation(_name: String) extends FromItem {
  val name = _name;

  var alias: Option[String] = None

  var columns = Seq[Column[Any]]()

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
}

/**
 * Definition of a Table, including its name and columns.
 */
abstract class Table[T: Manifest](_name: String) extends Relation(_name) {
  val rowType = manifest[T]

  implicit def relation = this

  /**
   * Alias this table.  Aliasing just creates another table of the same type but with a different name.
   */
  def AS(alias: String): this.type = {
    val constructor = this.getClass().getDeclaredConstructors()(0)
    constructor.setAccessible(true)
    val result = constructor.newInstance().asInstanceOf[this.type]
    result.alias = Some(alias)
    result.columns = columns.map { column => column.aliasedTo(result) }
    result
  }

  def * = Projection(this)
}

/**
 * A column in a relation
 */
case class Column[+T](name: String)(implicit relation: Relation, typeMapping: TypeMapping[T]) extends Value[T] {
  def aliasedTo(alias: Relation) = Column[T](name)(alias, typeMapping)

  def expression = "%1$s.%2$s".format(relation.aliasedName, name)

  override def selectExpression = "%1$s AS %2$s".format(expression, asName)

  def asName = expression.replace(".", "_")

  def extract(rs: ResultSet) = typeMapping.get(rs, asName)
}

/**
 * The projection of a table to a case class representing a complete row from that table.
 */
case class Projection[T](table: Table[T]) extends Value[T] {
  def expression = table.columns.map { column => column.expression }.mkString(", ")

  override def selectExpression = table.columns.map { column => column.selectExpression }.mkString(", ")

  def extract(rs: ResultSet) = {
    val constructor = table.rowType.erasure.getConstructors()(0)
    if (table.columns.length != constructor.getParameterTypes().length) {
      // TODO: see if we can make this a compile-time check
      throw new RuntimeException("The number of columns in table does not match the number of fields in %1$s.  Please check your class definitions".format(table.rowType.erasure.getName))
    }
    // Get column values from ResultSet
    val columnValues = table.columns.map { column => column.extract(rs) }

    // Construct the row
    try {
      // Return the Extraction
      constructor.newInstance(columnValues.map(_.asInstanceOf[Object]): _*).asInstanceOf[T]
    } catch {
      case e: Exception => throw new RuntimeException(
        "Unable to create row of type %1$s with values %2$s using constructor with argument types %3$s".format(
          table.rowType.erasure.getName,
          columnValues,
          constructor.getParameterTypes.map { clazz => clazz.getName }.mkString(", ")),
        e)
    }
  }
}

case class CompoundExpression2[T1 <: Expression, T2 <: Expression](override val _1: T1, override val _2: T2) extends Tuple2(_1, _2) with Expression {
  def expression = "%1$s, %2$s".format(_1.expression, _2.expression)

  override def selectExpression = "%1$s, %2$s".format(_1.selectExpression, _2.selectExpression)
}

case class CompoundExtractableExpression[T1, T2](override val _1: Extractable[T1], override val _2: Extractable[T2]) extends CompoundExpression2(_1, _2) with Extractable[Tuple2[T1, T2]] {
  override def extract(rs: ResultSet) = (_1.extract(rs), _2.extract(rs))
}

/**
 * An expression that evaluates to a boolean like A = B, A <> B, A IS NULL, etc.
 */
trait Condition extends Expression {
  def &&(right: Condition) = CompoundCondition(this, "AND", right)

  def ||(right: Condition) = CompoundCondition(this, "OR", right)
}

/**
 * A condition based on a single expression.
 */
abstract class UnaryCondition(val value: UnaryExpression) extends Condition {
  override def bind(ps: PreparedStatement, position: Int) = value.bind(ps, position)
}

/**
 * A condition based on two expressions and an operator
 */
abstract class BinaryCondition(val left: Expression, val operator: String, val right: Expression) extends Condition {
  val expression = "%1$s %2$s %3$s".format(left.expression, operator, right.expression)

  override def bind(ps: PreparedStatement, position: Int) = {
    var currentPosition = position
    currentPosition = left.bind(ps, currentPosition)
    currentPosition = right.bind(ps, currentPosition)
    currentPosition
  }
}

case class Null(override val value: UnaryExpression) extends UnaryCondition(value) {
  val expression = "%1$s IS NULL".format(value.expression)
}

case class NotNull(override val value: UnaryExpression) extends UnaryCondition(value) {
  val expression = "%1$s IS NOT NULL".format(value.expression)
}

case class Comparison[T](override val left: UnaryExpression, comparison: String, override val right: UnaryExpression) extends BinaryCondition(left, comparison, right)

case class CompoundCondition[T](override val left: Condition, composition: String, override val right: Condition) extends BinaryCondition(left, composition, right)

/**
 * A sorted UnaryExpression.
 */
case class SortedExpression(left: UnaryExpression, sort: String) extends Expression {
  def expression = "%1$s %2$s".format(left.expression, sort)
}

/**
 * An item from which one can select, such as a table, list of tables, joined tables.
 */
trait FromItem {
  def fromExpression: String

  def INNER_JOIN(right: Relation) = IncompleteJoin(this, "INNER JOIN", right)
}

case class IncompleteJoin(left: FromItem, joinType: String, right: Relation) {
  def ON(on: Condition) = Join(left, joinType, right, on)
}

/**
 * A from item representing a join.
 */
case class Join(left: FromItem, joinType: String, right: Relation, on: Condition) extends FromItem {
  def fromExpression = "%1$s %2$s %3$s ON %4$s".format(left.fromExpression, joinType, right.fromExpression, on)
}