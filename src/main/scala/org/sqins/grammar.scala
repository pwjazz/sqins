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
}

/**
 * Something that can be extracted from a JDBC ResultSet.
 */
trait Extractable[+T] extends Expression {
  /**
   * Extract the value from the given ResultSet
   */
  def extract(rs: ResultSet, position: Int): Extraction[T]
}

case class Extraction[+T](value: T, columnsRead: Int)

trait CompoundExtractable {
  var columnsRead = 0

  def doExtract[T](extractable: Extractable[T], rs: ResultSet, position: Int) = {
    val extracted = extractable.extract(rs, position + columnsRead)
    columnsRead += extracted.columnsRead
    extracted.value
  }
}

/**
 * A single-valued expression, such as a column, function call, scalar value, etc.
 */
trait ScalarExpression extends Expression {
  def ==(right: ScalarExpression) = Comparison(this, "=", right)

  def <>(right: ScalarExpression) = Comparison(this, "<>", right)

  def !=(right: ScalarExpression) = this.<>(right)

  def >(right: ScalarExpression) = Comparison(this, ">", right)

  def <(right: ScalarExpression) = Comparison(this, "<", right)

  def IS_NULL = Null(this)

  def IS_NOT_NULL = NotNull(this)

  def ASC = SortedExpression(this, "ASC")

  def DESC = SortedExpression(this, "DESC")
}

/**
 * An expression that represents (or returns) a value.  Can be turned into a Condition using the operators
 * == <> != > < ISNULL ISNOTNULL
 */
trait Value[+T] extends ScalarExpression with Extractable[T] {
  def AS(alias: String) = Alias[T, Value[T]](this, alias)
}

case class Alias[+T, +E <: Value[T]](aliased: E, alias: String) extends Extractable[T] {
  def expression = aliased.expression

  override def selectExpression = "%1$s AS %2$s".format(aliased.expression, alias)

  def extract(rs: ResultSet, position: Int) = aliased.extract(rs, position)
}

case class BoundValue[+T](actual: T)(implicit typeMapping: TypeMapping[T]) extends Value[T] {
  val expression = "?"

  def extract(rs: ResultSet, position: Int) = Extraction(actual, 0)

  override def bind(ps: PreparedStatement, position: Int) = {
    typeMapping.set(ps, position, actual)
    position + 1
  }
}

/**
 * Bind a value
 */
object ? {
  def apply[T](value: T)(implicit typeMapping: TypeMapping[T]) = BoundValue(value)(typeMapping)
}

case class FunctionCall[T](name: String, params: Expression, typeMapping: TypeMapping[T]) extends Value[T] {
  val expression = "%1$s(%2$s)".format(name, params.expression)

  def extract(rs: ResultSet, position: Int) = typeMapping.get(rs, position)
}

case class FN(name: String) {
  def apply[T](params: Expression)(implicit typeMapping: TypeMapping[T]) = new FunctionCall(name, params, typeMapping)

  def apply[T](params: Value[T])(implicit typeMapping: TypeMapping[T]) = new FunctionCall(name, params, typeMapping)
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

  def extract(rs: ResultSet, position: Int) = typeMapping.get(rs, position)
}

/**
 * The projection of a table to a case class representing a complete row from that table.
 */
case class Projection[T](table: Table[T]) extends Value[T] {
  def expression = table.columns.map { column => column.expression }.mkString(", ")

  override def selectExpression = table.columns.map { column => column.selectExpression }.mkString(", ")

  def extract(rs: ResultSet, position: Int) = {
    val constructor = table.rowType.erasure.getConstructors()(0)
    if (table.columns.length != constructor.getParameterTypes().length) {
      // TODO: see if we can make this a compile-time check
      throw new RuntimeException("The number of columns in table does not match the number of fields in %1$s.  Please check your class definitions".format(table.rowType.erasure.getName))
    }
    // Get column values
    var columnsRead = 0
    val columnValues = table.columns.map(column => {
      val extract = column.extract(rs, position + columnsRead)
      columnsRead += extract.columnsRead
      extract.value
    })

    // Construct the row
    try {
      // Return the Extraction
      Extraction(constructor.newInstance(columnValues.map(_.asInstanceOf[Object]): _*).asInstanceOf[T], columnsRead)
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

/**
 * An expression that evaluates to a boolean like A = B, A <> B, A IS NULL, etc.
 */
trait Condition extends ScalarExpression {
  def &&(right: Condition) = CompoundCondition(this, "AND", right)

  def ||(right: Condition) = CompoundCondition(this, "OR", right)
}

/**
 * A condition based on a single expression.
 */
abstract class ScalarCondition(val value: ScalarExpression) extends Condition {
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

case class Null(override val value: ScalarExpression) extends ScalarCondition(value) {
  val expression = "%1$s IS NULL".format(value.expression)
}

case class NotNull(override val value: ScalarExpression) extends ScalarCondition(value) {
  val expression = "%1$s IS NOT NULL".format(value.expression)
}

class NOT(override val value: ScalarExpression) extends ScalarCondition(value) {
  val expression = "NOT %1$s".format(value.expression)
}

object NOT {
  def apply(value: ScalarExpression) = new NOT(value)

  def apply(value: Condition) = new NOT(value)
}

case class Comparison[T](override val left: ScalarExpression, comparison: String, override val right: ScalarExpression) extends BinaryCondition(left, comparison, right)

case class CompoundCondition[T](override val left: Condition, composition: String, override val right: Condition) extends BinaryCondition(left, composition, right)

/**
 * A sorted ScalarExpression.
 */
case class SortedExpression(left: ScalarExpression, sort: String) extends Expression {
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
  def fromExpression = "%1$s %2$s %3$s ON %4$s".format(left.fromExpression, joinType, right.fromExpression, on.expression)
}