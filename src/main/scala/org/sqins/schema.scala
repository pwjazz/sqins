package org.sqins

import java.sql.ResultSet
import java.sql.PreparedStatement

/**
 * Definition of a Table, including its name and columns.
 */
class Table[T](_name: String) extends Relation(_name) {
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

  def * = RelationExpression(this)
}

/**
 * Represents a column in a table (or relation)
 */
case class Column[+T](name: String)(implicit relation: Relation, typeMapping: TypeMapping[T]) extends Value with Expression with SortableExpression {
  // Let the relation know about this Column
  relation.addColumn(this)

  def aliasedTo(alias: Relation) = Column[T](name)(alias, typeMapping)

  def valueExpression = "%1$s.%2$s".format(relation.aliasedName, name)
  
  def expressionString = valueExpression
}

/**
 * A TypeMapping handles retrieving a Field's value from a ResultSet and for setting parameters on a PreparedStatement.
 */
class TypeMapping[T](_get: (ResultSet, Int) => T, _set: (PreparedStatement, Int, T) => Unit) {
  def get(rs: ResultSet, position: Int): T = _get(rs, position)

  def set(ps: PreparedStatement, position: Int, value: T): Unit = _set(ps, position, value)
}

/**
 * OptionTypeMapping is a specialization of TypeMapping that handles Option values, delegating to a regular TypeMapping
 * to perform the actual getting and setting.
 */
class OptionTypeMapping[T](typeMapping: TypeMapping[T]) extends TypeMapping[Option[T]](
  _get = (rs: ResultSet, position: Int) => rs.getObject(position) match {
    case Some(value: T) => Some(value)
    case _              => None
  },
  _set = (ps: PreparedStatement, position: Int, value: Option[T]) => value match {
    case Some(value: T) => typeMapping.set(ps, position, value)
    case _              => ps.setObject(position, null)
  })

