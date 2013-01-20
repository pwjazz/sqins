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
import java.lang.reflect.Method

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
   * The expression without aliases
   */
  def unaliasedExpression = expression

  /**
   * All the bound values associated with this expression.
   */
  def boundValues = Seq[BoundValue[_]]()
}

/**
 * Something that can be extracted from a JDBC ResultSet.
 */
private[sqins] trait Extractable[+T] extends Expression {
  /**
   * Extract the value from the given ResultSet.
   */
  def extract(rs: ResultSet, position: Int): Extraction[T] 
}

/**
 * The value extracted and the number of columns read in order to extract it.
 */
case class Extraction[+T](value: T, columnsRead: Int)

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
  
  def ?[U >: T](implicit typeMapping: TypeMapping[Option[U]]) = OptionOfScalarValue(this, typeMapping)
}

case class OptionOfScalarValue[T](scalarValue: ScalarValue[T], typeMapping: TypeMapping[Option[T]]) extends Extractable[Option[T]] {
  def expression = scalarValue.expression
  override def selectExpression = scalarValue.selectExpression
  override def unaliasedExpression = scalarValue.unaliasedExpression
  override def boundValues = scalarValue.boundValues
  
  def extract(rs: ResultSet, position: Int) = typeMapping.get(rs, position)
}

/**
 * A ScalarValue + an operator + a ScalarExpression = a CalculatedValue
 */
case class CalculatedValue[+T](scalarValue: ScalarValue[T], operator: String, other: ScalarExpression)
    extends ScalarValue[T] {
  override def expression = scalarValue.expression + " " + operator + " " + other.expression

  def extract(rs: ResultSet, position: Int) = scalarValue.extract(rs, position)

  override def boundValues = scalarValue.boundValues ++ other.boundValues
}

/**
 * An alias for Value.
 */
case class Alias[+T, +E <: ScalarValue[T]](aliased: E, alias: String) extends Extractable[T] {
  def expression = aliased.expression

  override def selectExpression = aliased.expression + " AS " + alias

  def extract(rs: ResultSet, position: Int) = aliased.extract(rs, position)
}

/**
 * A Scala value that can be bound into a query.
 */
case class BoundValue[+T](actual: T)(implicit typeMapping: TypeMapping[T]) extends ScalarValue[T] {
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
    case Some(qualifiers: String) => name + "(" + qualifiers + " " + params.expression + ")"
    case None                     => name + "(" + params.expression + ")"
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
private[sqins] trait IntoItem[T] {
  def intoExpression: String

  /**
   * Build the VALUES clause from a row.
   */
  def VALUES(row: T): InsertValuesQuery[T]

  /**
   * Build the VALUES clause from a sequence of BoundValues.
   */
  def VALUES(boundValues: Seq[BoundValue[_]]): InsertValuesQuery[T]

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
}

/**
 * An item from which one can select, such as a table, list of tables, joined tables, sub-selects.
 */
private[sqins] trait FromItem {
  def fromExpression: String

  def INNER_JOIN(right: Relation) = IncompleteJoin(this, "INNER JOIN", right)
  
  def LEFT_OUTER_JOIN(right: Relation) = IncompleteJoin(this, "LEFT OUTER JOIN", right)
  
  def RIGHT_OUTER_JOIN(right: Relation) = IncompleteJoin(this, "RIGHT OUTER JOIN", right)
  
  def boundValues = Seq[BoundValue[_]]()
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
  def fromExpression = left.fromExpression + " " + joinType + " " + right.fromExpression + " ON " + on.expression
  
  override def boundValues = left.boundValues ++ right.boundValues ++ on.boundValues
}

/**
 * A named list of columns (table, alias, etc.)
 */
abstract class Relation(val _name: String) extends FromItem {
  /**
   * An optional alias for this Relation.
   */
  var _alias: Option[String] = None

  /**
   * All of the columns in this relation.
   */
  var columns = Seq[ColumnDef[_, this.type]]()

  def columns(newCols: ColumnDef[_, this.type]*): Unit = columns = newCols

  /**
   * Use the alias if set, otherwise the name.
   */
  def aliasedName = _alias match {
    case Some(alias: String) => alias
    case _                   => _name
  }

  def fromExpression = _alias match {
    case Some(alias: String) => _name + " AS " + alias
    case _                   => _name
  }
}

/**
 * Definition of a Table, including its name and columns.
 */
abstract class Table[T: Manifest](override val _name: String) extends Relation(_name) with IntoItem[T] {
  val _rowType = manifest[T]
  implicit def _relation = this

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
      result._alias = Some(alias)
      result.columns = columns.map { column => column.aliasedTo(result) }
      result
    } finally {
      constructor.setAccessible(originalAccessible)
    }
  }

  def nonAutoGeneratedColumns = columns.filter(!_.isAutoGenerated)

  def intoExpression = _name + "(" + nonAutoGeneratedColumns.map(_.name).mkString(", ") + ")"

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
 * An INSERT INTO clause with a restricted list of columns.
 */
case class IntoWithSpecificColumns[T](table: Table[T], columns: Seq[ColumnDef[_, _]]) extends IntoItem[T] {
  def intoExpression = table._name + " (" + columns.map(_.name).mkString(", ") + ")"

  def VALUES(row: T) = VALUES(table.rowToBoundValues(row))

  def VALUES(boundValues: Seq[BoundValue[_]]) = InsertValuesQuery(this, boundValues)
}

/**
 * A column in a Relation.
 */
case class ColumnDef[T, R <: Relation](name: String, isAutoGenerated: Boolean = false)(implicit relation: R, typeMapping: TypeMapping[T]) extends ScalarValue[T] {
  val methodCache = new java.util.concurrent.ConcurrentHashMap[Class[_], Method]
  
  /**
   * Alias this column to the given Relation.
   */
  def aliasedTo(alias: R) = ColumnDef[T, R](name, isAutoGenerated)(alias, typeMapping)

  def expression = relation.aliasedName + "." + name
  
  override def unaliasedExpression = relation._name + "." + name

  def extract(rs: ResultSet, position: Int) = typeMapping.get(rs, position)

  /**
   * Mark this column as being auto-generated by the database so that full-row insert statements will omit this column.
   */
  def autoGenerated = ColumnDef(name, true)(relation, typeMapping)

  /**
   * Obtain a BoundValue from this column in the given row
   */
  def boundValueFrom(row: Any) = BoundValue(getSetterMethod(row).invoke(row).asInstanceOf[T])(typeMapping)
  
  def getSetterMethod(row: Any) = {
    val rowClass = row.getClass
    var method = methodCache.get(rowClass)
    if (method == null) {
      method = rowClass.getMethod(name)
      methodCache.put(rowClass, method)
    }
    method
  }

  def :=(value: ScalarValue[T]) = SetExpression(this, value)
}

/**
 * An expression setting a column equal to a scalar value.
 */
case class SetExpression[T, R <: Relation](column: ColumnDef[T, R], value: ScalarValue[T]) extends Expression {
  def expression = column.name + " = " + value.expression

  override def boundValues = value.boundValues
}

/**
 * An expression for setting all non-autogenerated fields of a row in an UPDATE query.
 */
case class SetRowExpression[T, K](table: Table[T], row: T) extends Expression {
  def expression = table.nonAutoGeneratedColumns.map { column => column.name + " = ?" }.mkString(", ")

  override def boundValues = table.rowToBoundValues(row)
}

/**
 * Represents all columns of a table in a SELECT clause.
 */
case class Projection[T](table: Table[T]) extends ScalarValue[T] {
  def expression = table.columns.map { column => column.expression }.mkString(", ")

  override def selectExpression = table.columns.map { column => column.selectExpression }.mkString(", ")

  def extract(rs: ResultSet, position: Int) = {
    val constructor = table._rowType.erasure.getConstructors()(0)
    if (table.columns.length != constructor.getParameterTypes().length) {
      // TODO: see if we can make this a compile-time check
      throw new RuntimeException("The number of columns in table does not match the number of fields in %1$s.  Please check your class definitions".format(table._rowType.erasure.getName))
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
          table._rowType.erasure.getName,
          columnValues,
          constructor.getParameterTypes.map { clazz => clazz.getName }.mkString(", ")),
        e)
    }
  }
  
  def ? = OptionOfProjection(this)
}

case class OptionOfProjection[T](projection: Projection[T]) extends Extractable[Option[T]] {
  def expression = projection.expression
  override def selectExpression = projection.selectExpression
  override def unaliasedExpression = projection.unaliasedExpression
  override def boundValues = projection.boundValues
  
  def extract(rs: ResultSet, position: Int) = {
    // Check if any of the columns are non-null
    // Get column values
    var columnsRead = 0
    val firstNonNullColumn = projection.table.columns.find { column => rs.getObject(position + columnsRead) != null }
    // TODO: the below assumes 1 database column per value type.  This may change in future.
    firstNonNullColumn match {
      case Some(_) => {
        Extraction(Some(projection.extract(rs, position).value), projection.table.columns.length)
      }
      case None => Extraction(None, projection.table.columns.length)
    }
  }
}

/**
 * An expression that evaluates to a boolean like A = B, A <> B, A IS NULL, etc.
 */
private[sqins] trait Condition extends ScalarExpression {
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
  val expression = left.expression + " " + operator + " " + right.expression

  override def boundValues = left.boundValues ++ right.boundValues
}

case class Null(override val value: ScalarExpression) extends ScalarCondition(value) {
  val expression = value.expression + " IS NULL"
}

case class NotNull(override val value: ScalarExpression) extends ScalarCondition(value) {
  val expression = value.expression + " IS NOT NULL"
}

case class InSelectQueryCondition[T](value: ScalarValue[T], query: SelectQuery[T]) extends Condition {
  val expression = value.expression + " IN " + query.expression
  
  override def boundValues = value.boundValues ++ query.boundValues
}
  
case class InSequenceCondition[T](value: ScalarValue[T], vals: Seq[BoundValue[T]]) extends Condition {
  val expression = value.expression + " IN (" + vals.map { _ => "?"}.mkString(", ") + ")"
  
  override def boundValues = value.boundValues ++ vals
}

case class EXISTS[T](query: SelectQuery[T]) extends Condition {
  val expression = "EXISTS " + query.expression
  
  override def boundValues = query.boundValues
}

/**
 * Logically negates a given condition.
 */
case class NOT(val condition: Condition) extends Condition {
  val expression = "NOT (" + condition.expression + ")"
  
  override def boundValues = condition.boundValues
}

case class Comparison[T](override val left: ScalarExpression, comparison: String, override val right: ScalarExpression) extends BinaryCondition(left, comparison, right)

case class CompoundCondition[T](override val left: Condition, composition: String, override val right: Condition) extends BinaryCondition(left, composition, right)

/**
 * A sorted ScalarExpression.
 */
case class SortedExpression(left: ScalarExpression, sort: String) extends Expression {
  def expression = left.expression + " " + sort
}