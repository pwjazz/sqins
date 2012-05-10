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

import java.sql.PreparedStatement
import java.sql.ResultSet

/**
 * A TypeMapping handles retrieving a Field's value from a ResultSet and for setting parameters on a PreparedStatement.
 */
trait TypeMapping[T] {
  def get(rs: ResultSet, position: Int) = _get(rs, position)

  def _get(rs: ResultSet, position: Int): Extraction[T]

  def set(ps: PreparedStatement, position: Int, value: T): Unit = _set(ps, position, value)

  def _set(ps: PreparedStatement, position: Int, value: T): Unit
}

/**
 * OptionTypeMapping is a specialization of TypeMapping that handles Option values, delegating to a regular TypeMapping
 * to perform the actual getting and setting.
 */
class OptionTypeMapping[T](typeMapping: TypeMapping[T]) extends TypeMapping[Option[T]] {
  def _get(rs: ResultSet, position: Int) = rs.getObject(position) match {
    case Some(value: T) => Extraction(Some(value), 1)
    case _              => Extraction(None, 1)
  }

  def _set(ps: PreparedStatement, position: Int, value: Option[T]) = value match {
    case Some(value: T) => typeMapping.set(ps, position, value)
    case _              => ps.setObject(position, null)
  }
}

