package org.sqins

import java.sql.PreparedStatement
import java.sql.ResultSet

/**
 * A TypeMapping handles retrieving a Field's value from a ResultSet and for setting parameters on a PreparedStatement.
 */
class TypeMapping[T](_get: (ResultSet, Int) => Extraction[T], _set: (PreparedStatement, Int, T) => Unit) {
  def get(rs: ResultSet, position: Int) = _get(rs, position)

  def set(ps: PreparedStatement, position: Int, value: T): Unit = _set(ps, position, value)
}

/**
 * OptionTypeMapping is a specialization of TypeMapping that handles Option values, delegating to a regular TypeMapping
 * to perform the actual getting and setting.
 */
class OptionTypeMapping[T](typeMapping: TypeMapping[T]) extends TypeMapping[Option[T]](
  _get = (rs: ResultSet, position: Int) => rs.getObject(position) match {
    case Some(value: T) => Extraction(Some(value), 1)
    case _              => Extraction(None, 1)
  },
  _set = (ps: PreparedStatement, position: Int, value: Option[T]) => value match {
    case Some(value: T) => typeMapping.set(ps, position, value)
    case _              => ps.setObject(position, null)
  })

