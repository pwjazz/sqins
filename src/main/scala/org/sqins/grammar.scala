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

import java.sql.{ PreparedStatement, ResultSet }

/**
 * Represents an expression such as a column, a scalar, a function call, or a list of these things
 */
private[sqins] trait Expression {
  /**
   * The string representing this expression.
   */
  def expression: String

  /**
   * The string representing this expression inside of a select clause.
   */
  def selectExpression = expression

  /**
   * All the bound values associated with this expression.
   */
  def boundValues = Seq[BoundValue[_]]()
}

/**
 * Something that can be extracted from a JDBC ResultSet.
 */
private[sqins] trait Extractable[+T] {
  /**
   * Extract the value from the given ResultSet.
   */
  def extract(rs: ResultSet, position: Int): Extraction[T]
}

/**
 * The value extracted and the number of columns read in order to extract it.
 */
private[sqins] case class Extraction[+T](value: T, columnsRead: Int)

/**
 * A single-valued expression, such as a column, function call, scalar value, etc.
 * Provides methods for construction Conditions and SortedExpressions from the scalar expression.
 */
private[sqins] trait ScalarExpression extends Expression {
  def ==(right: ScalarExpression) = Comparison(this, "=", right)

  def <>(right: ScalarExpression) = Comparison(this, "<>", right)

  def !=(right: ScalarExpression) = this.<>(right)

  def >(right: ScalarExpression) = Comparison(this, ">", right)

  def >=(right: ScalarExpression) = Comparison(this, ">=", right)

  def <=(right: ScalarExpression) = Comparison(this, "<=", right)
  
  def LIKE(right: ScalarExpression) = Comparison(this, "LIKE", right)
  
  def ILIKE(right: ScalarExpression) = Comparison(this, "ILIKE", right)

  def IS_NULL = Null(this)

  def IS_NOT_NULL = NotNull(this)

  def ASC = SortedExpression(this, "ASC")

  def DESC = SortedExpression(this, "DESC")
}

/**
 * A scalar expression that is just a string. Useful for plugging in stuff not natively supported by the syntax.
 */
case class ConstantScalarExpression(expression: String) extends Condition

/**
 * A scalar value that is just a string. Useful for plugging in stuff not natively supported by the syntax.
 */
case class ConstantScalarValue[T](expression: String, typeMapping: TypeMapping[T]) extends ScalarValue[T] {
  def extract(rs: ResultSet, position: Int) = typeMapping.get(rs, position)
}

/**
 * Factory for ConstantScalarExpressions
 */
object EXPR {
  def apply(expression: String) = ConstantScalarExpression(expression)
}

/**
 * Factory for ConstantScalarValues
 */
object VEXPR {
  def apply[T](expression: String)(implicit typeMapping: TypeMapping[T]) = ConstantScalarValue(expression, typeMapping)
}

/**
 * An expression that represents (or returns) a value.
 */
private[sqins] trait ScalarValue[+T] extends ScalarExpression with Extractable[T] {
  def AS(alias: String) = Alias[T, ScalarValue[T]](this, alias)

  def +(other: ScalarExpression) = CalculatedValue(this, "+", other)

  def -(other: ScalarExpression) = CalculatedValue(this, "-", other)

  def *(other: ScalarExpression) = CalculatedValue(this, "*", other)

  def /(other: ScalarExpression) = CalculatedValue(this, "/", other)

  /**
   * String concatenation
   */
  def ||(other: ScalarExpression) = CalculatedValue(this, "||", other)
  
  def IN[U >: T](query: SelectQuery[U]) = InSelectQueryCondition(this, query)
  
  def IN[U >: T](vals: Seq[BoundValue[U]]) = InSequenceCondition(this, vals)
}

/**
 * A ScalarValue + an operator + a ScalarExpression = a CalculatedValue
 */
case class CalculatedValue[+T](scalarValue: ScalarValue[T], operator: String, other: ScalarExpression)
    extends ScalarValue[T] {
  override def expression = "%1$s %2$s %3$s".format(scalarValue.expression, operator, other.expression)

  def extract(rs: ResultSet, position: Int) = scalarValue.extract(rs, position)

  override def boundValues = scalarValue.boundValues ++ other.boundValues
}

/**
 * An alias for Value.
 */
private[sqins] case class Alias[+T, +E <: ScalarValue[T]](aliased: E, alias: String) extends Extractable[T] with Expression {
  def expression = aliased.expression

  override def selectExpression = "%1$s AS %2$s".format(aliased.expression, alias)

  def extract(rs: ResultSet, position: Int) = aliased.extract(rs, position)
}

/**
 * A Scala value that can be bound into a query.
 */
private[sqins] case class BoundValue[+T](actual: T)(implicit typeMapping: TypeMapping[T]) extends ScalarValue[T] {
  val expression = "?"

  def extract(rs: ResultSet, position: Int) = Extraction(actual, 0)

  def bind(ps: PreparedStatement, position: Int) = {
    typeMapping.set(ps, position, actual)
    1
  }

  override def boundValues = Seq(this)
}

/**
 * Bind a value.  Only values of a type with an in-scope implicit TypeMapping can be bound.
 *
 * Alternately, you can explicitly pass the TypeMapping, but the encouraged style is to use implicits
 * to aid readability.
 */
object ? {
  def apply[T](value: T)(implicit typeMapping: TypeMapping[T]) = BoundValue(value)(typeMapping)
  
  def apply[T](values: Seq[T])(implicit typeMapping: TypeMapping[T]) = values.map { value => BoundValue(value)(typeMapping) }
}

/**
 * A call to a database function.  The function can take one or more parameters as captured by the params Expression.
 * Only function calls returning a type of value with an in-scope implicit TypeMapping can be made.
 */
case class FunctionCall[T](name: String, qualifiers: Option[String], params: Expression, typeMapping: TypeMapping[T]) extends ScalarValue[T] {
  val expression = qualifiers match {
    case Some(qualifiers: String) => "%1$s(%2$s %3$s)".format(name, qualifiers, params.expression)
    case None                     => "%1$s(%2$s)".format(name, params.expression)
  }

  def extract(rs: ResultSet, position: Int) = typeMapping.get(rs, position)
}

/**
 * Constructs FunctionCalls from expressions and scalar values.
 */
case class FN(name: String, qualifiers: Option[String] = None) {
  def apply[T](params: Expression)(implicit typeMapping: TypeMapping[T]) = new FunctionCall(name, qualifiers, params, typeMapping)

  def apply[T](params: ScalarValue[T])(implicit typeMapping: TypeMapping[T]) = new FunctionCall(name, qualifiers, params, typeMapping)
}

/**
 * Something that can be used as the INTO clause of an INSERT statement.
 */
trait IntoItem[T, K] {
  def intoExpression: String

  def primaryKey: PrimaryKey[K]

  /**
   * Build the VALUES clause from a row.
   */
  def VALUES(row: T): InsertValuesQuery[T, K]

  /**
   * Build the VALUES clause from a sequence of BoundValues.
   */
  def VALUES(boundValues: Seq[BoundValue[_]]): InsertValuesQuery[T, K]

  /**
   * Build an INSERT query using a predefined SELECT ... FROM ...
   */
  def apply[T](query: SelectQuery[T]) = InsertSelectQuery(this,
    query.select,
    query.distinct,
    query.from,
    query.where,
    query.orderBy,
    query.groupBy,
    query.limit,
    query.offset)

  /**
   * Start to build the SELECT clause of this INSERT query.
   */
  def SELECT[V](select: Extractable[V] with Expression) = IncompleteInsertSelectQuery(this, select, false)
}

/**
 * An item from which one can select, such as a table, list of tables, joined tables, sub-selects.
 */
trait FromItem {
  def fromExpression: String

  def INNER_JOIN(right: Relation) = IncompleteJoin(this, "INNER JOIN", right)
}

/**
 * A Join that is missing the join condition
 */
case class IncompleteJoin(left: FromItem, joinType: String, right: Relation) {
  /**
   * Add the join condition.
   */
  def ON(on: Condition) = Join(left, joinType, right, on)
}

/**
 * A FromItem representing a join.
 */
case class Join(left: FromItem, joinType: String, right: Relation, on: Condition) extends FromItem {
  def fromExpression = "%1$s %2$s %3$s ON %4$s".format(left.fromExpression, joinType, right.fromExpression, on.expression)
}

/**
 * A named list of columns (table, alias, etc.)
 */
abstract class Relation(val name: String) extends FromItem {
  /**
   * An optional alias for this Relation.
   */
  var alias: Option[String] = None

  /**
   * All of the columns in this relation.
   */
  var columns = Seq[ColumnDef[_, this.type]]()

  def columns(newCols: ColumnDef[_, this.type]*): Unit = columns = newCols

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
abstract class Table[T: Manifest, K](override val name: String) extends Relation(name) with IntoItem[T, K] {
  val rowType = manifest[T]
  private var _primaryKey: PrimaryKey[K] = null

  def primaryKey = _primaryKey

  /**
   * Set the primary key by specifying its Column.
   */
  def primaryKey(column: ColumnDef[K, this.type]): Unit = _primaryKey = PrimaryKey(column)

  implicit def relation = this

  /**
   * Define a column on this Table.  Only columns of a type with an in-scope implicit TypeMapping can be created.
   */
  def Column[T](name: String)(implicit typeMapping: TypeMapping[T]) = ColumnDef[T, this.type](name)(this, typeMapping)

  /**
   * Alias this table.  Aliasing just creates another table of the same type but with a different name.
   */
  def AS(alias: String): this.type = {
    val constructor = this.getClass().getDeclaredConstructors()(0)
    if (constructor.getParameterTypes.length > 0) {
      throw new RuntimeException("Please make sure that your table object has a no-argument constructor.  Make sure that it's not defined inside of another scope.")
    }
    val originalAccessible = constructor.isAccessible
    constructor.setAccessible(true)
    try {
      val result = constructor.newInstance().asInstanceOf[this.type]
      result.alias = Some(alias)
      result.columns = columns.map { column => column.aliasedTo(result) }
      result
    } finally {
      constructor.setAccessible(originalAccessible)
    }
  }

  def nonAutoGeneratedColumns = columns.filter(!_.isAutoGenerated)

  def intoExpression = "%1$s (%2$s)".format(name, nonAutoGeneratedColumns.map(_.name).mkString(", "))

  def setExpression(row: T) = SetRowExpression(this, row)

  /**
   * Bind an entire row (typically used for the VALUES() clause of an INSERT statement).
   */
  def VALUES(row: T) = VALUES(rowToBoundValues(row))

  /**
   * Build an INSERT ... VALUES query.
   */
  def VALUES(boundValues: Seq[BoundValue[_]]) = InsertValuesQuery(this, boundValues)

  /**
   * Convert the given row to a sequence of BoundValues for all non-autogenerated columns of this Table.
   */
  def rowToBoundValues(row: T) = nonAutoGeneratedColumns.map(_.boundValueFrom(row))

  /**
   * Start to build an INSERT VALUES query.
   */
  def apply(columns: ColumnDef[_, this.type]*) = IntoWithSpecificColumns(this, columns.toList)

  def * = Projection(this)
}

/**
 * Represents the primary key of a table.  Currently only 1 column - hopefully eventually we'll support multiple.
 */
case class PrimaryKey[K](column: ColumnDef[K, _ <: Relation]) extends Extractable[K] with Expression {
  def expression = column.expression

  def extract(rs: ResultSet, position: Int) = column.extract(rs, position)

  def ==[T](row: T) = column == column.boundValueFrom(row)
}

/**
 * An INSERT INTO clause with a restricted list of columns.
 */
case class IntoWithSpecificColumns[T, K](table: Table[T, K], columns: Seq[ColumnDef[_, _]]) extends IntoItem[T, K] {
  def intoExpression = "%1$s (%2$s)".format(table.name, columns.map(_.name).mkString(", "))

  def primaryKey = table.primaryKey

  def VALUES(row: T) = VALUES(table.rowToBoundValues(row))

  def VALUES(boundValues: Seq[BoundValue[_]]) = InsertValuesQuery(this, boundValues)
}

/**
 * A column in a Relation.
 */
case class ColumnDef[T, R <: Relation](name: String, isAutoGenerated: Boolean = false)(implicit relation: R, typeMapping: TypeMapping[T]) extends ScalarValue[T] {
  /**
   * Alias this column to the given Relation.
   */
  def aliasedTo(alias: R) = ColumnDef[T, R](name, isAutoGenerated)(alias, typeMapping)

  def expression = "%1$s.%2$s".format(relation.aliasedName, name)

  def extract(rs: ResultSet, position: Int) = typeMapping.get(rs, position)

  /**
   * Mark this column as being auto-generated by the database so that full-row insert statements will omit this column.
   */
  def autoGenerated = ColumnDef(name, true)(relation, typeMapping)

  /**
   * Obtain a BoundValue from this column in the given row
   */
  def boundValueFrom(row: Any) = BoundValue(row.getClass.getMethod(name).invoke(row).asInstanceOf[T])(typeMapping)

  def :=(value: ScalarValue[T]) = SetExpression(this, value)
}

/**
 * An expression setting a column equal to a scalar value.
 */
case class SetExpression[T, R <: Relation](column: ColumnDef[T, R], value: ScalarValue[T]) extends Expression {
  def expression = "%1$s = %2$s".format(column.name, value.expression)

  override def boundValues = value.boundValues
}

/**
 * An expression for setting all non-autogenerated fields of a row in an UPDATE query.
 */
case class SetRowExpression[T, K](table: Table[T, K], row: T) extends Expression {
  def expression = table.nonAutoGeneratedColumns.map { column => "%1$s = ?".format(column.name) }.mkString(", ")

  override def boundValues = table.rowToBoundValues(row)
}

/**
 * Represents all columns of a table in a SELECT clause.
 */
case class Projection[T, K](table: Table[T, K]) extends ScalarValue[T] {
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
  override def boundValues = value.boundValues
}

/**
 * A condition based on two expressions and an operator
 */
abstract class BinaryCondition(val left: Expression, val operator: String, val right: Expression) extends Condition {
  val expression = "%1$s %2$s %3$s".format(left.expression, operator, right.expression)

  override def boundValues = left.boundValues ++ right.boundValues
}

case class Null(override val value: ScalarExpression) extends ScalarCondition(value) {
  val expression = "%1$s IS NULL".format(value.expression)
}

case class NotNull(override val value: ScalarExpression) extends ScalarCondition(value) {
  val expression = "%1$s IS NOT NULL".format(value.expression)
}

case class InSelectQueryCondition[T](value: ScalarValue[T], query: SelectQuery[T]) extends Condition {
  val expression = "%1$s IN %2$s".format(value.expression, query.expression)
  
  override def boundValues = value.boundValues ++ query.boundValues
}
  
case class InSequenceCondition[T](value: ScalarValue[T], vals: Seq[BoundValue[T]]) extends Condition {
  val expression = "%1$s IN (%2$s)".format(value.expression, vals.map { _ => "?"}.mkString(", "))
  
  override def boundValues = value.boundValues ++ vals
}

case class EXISTS[T](query: SelectQuery[T]) extends Condition {
  val expression = "EXISTS %1$s".format(query.expression)
  
  override def boundValues = query.boundValues
}

/**
 * Logically negates a given condition.
 */
class NOT(val condition: Condition) extends Condition {
  val expression = "NOT (%1$s)".format(condition.expression)
  
  override def boundValues = condition.boundValues
}

object NOT {
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