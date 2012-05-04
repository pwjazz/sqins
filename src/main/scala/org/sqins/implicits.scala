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

import java.sql.ResultSet
import java.sql.PreparedStatement
import java.sql.Date
import java.sql.Timestamp

/**
 * Pre-defined implicit type mappings.  Users will import these as TypeMappings._ in order to define their Tables and
 *  Fields.
 */
object Implicits {
  // Type mappings
  implicit val ByteTypeMapping = new TypeMapping[Byte](
    _get = (rs: ResultSet, position: Int) => Extraction(rs.getByte(position), 1),
    _set = (ps: PreparedStatement, position: Int, value: Byte) => ps.setByte(position, value))
  implicit val OptionByteTypeMapping = new OptionTypeMapping(ByteTypeMapping)

  implicit val ShortTypeMapping = new TypeMapping[Short](
    _get = (rs: ResultSet, position: Int) => Extraction(rs.getShort(position), 1),
    _set = (ps: PreparedStatement, position: Int, value: Short) => ps.setShort(position, value))
  implicit val OptionShortTypeMapping = new OptionTypeMapping(ShortTypeMapping)

  implicit val IntTypeMapping = new TypeMapping[Int](
    _get = (rs: ResultSet, position: Int) => Extraction(rs.getInt(position), 1),
    _set = (ps: PreparedStatement, position: Int, value: Int) => ps.setInt(position, value))
  implicit val OptionIntTypeMapping = new OptionTypeMapping(IntTypeMapping)

  implicit val LongTypeMapping = new TypeMapping[Long](
    _get = (rs: ResultSet, position: Int) => Extraction(rs.getLong(position), 1),
    _set = (ps: PreparedStatement, position: Int, value: Long) => ps.setLong(position, value))
  implicit val OptionLongTypeMapping = new OptionTypeMapping(LongTypeMapping)

  implicit val FloatTypeMapping = new TypeMapping[Float](
    _get = (rs: ResultSet, position: Int) => Extraction(rs.getFloat(position), 1),
    _set = (ps: PreparedStatement, position: Int, value: Float) => ps.setFloat(position, value))
  implicit val OptionFloatTypeMapping = new OptionTypeMapping(FloatTypeMapping)

  implicit val DoubleTypeMapping = new TypeMapping[Double](
    _get = (rs: ResultSet, position: Int) => Extraction(rs.getDouble(position), 1),
    _set = (ps: PreparedStatement, position: Int, value: Double) => ps.setDouble(position, value))
  implicit val OptionDoubleTypeMapping = new OptionTypeMapping(DoubleTypeMapping)

  implicit val BooleanTypeMapping = new TypeMapping[Boolean](
    _get = (rs: ResultSet, position: Int) => Extraction(rs.getBoolean(position), 1),
    _set = (ps: PreparedStatement, position: Int, value: Boolean) => ps.setBoolean(position, value))
  implicit val OptionBooleanTypeMapping = new OptionTypeMapping(BooleanTypeMapping)

  implicit val StringTypeMapping = new TypeMapping[String](
    _get = (rs: ResultSet, position: Int) => Extraction(rs.getString(position), 1),
    _set = (ps: PreparedStatement, position: Int, value: String) => ps.setString(position, value))
  implicit val OptionStringTypeMapping = new OptionTypeMapping(StringTypeMapping)

  implicit val BigDecimalTypeMapping = new TypeMapping[BigDecimal](
    _get = (rs: ResultSet, position: Int) => Extraction(BigDecimal(rs.getBigDecimal(position)), 1),
    _set = (ps: PreparedStatement, position: Int, value: BigDecimal) => ps.setBigDecimal(position, java.math.BigDecimal.valueOf(value.doubleValue())))
  implicit val OptionBigDecimalTypeMapping = new OptionTypeMapping(BigDecimalTypeMapping)

  implicit val DateTypeMapping = new TypeMapping[Date](
    _get = (rs: ResultSet, position: Int) => Extraction(rs.getDate(position), 1),
    _set = (ps: PreparedStatement, position: Int, value: Date) => ps.setDate(position, value))
  implicit val OptionDateTypeMapping = new OptionTypeMapping(DateTypeMapping)

  implicit val TimestampTypeMapping = new TypeMapping[Timestamp](
    _get = (rs: ResultSet, position: Int) => Extraction(rs.getTimestamp(position), 1),
    _set = (ps: PreparedStatement, position: Int, value: Timestamp) => ps.setTimestamp(position, value))
  implicit val OptionTimestampTypeMapping = new OptionTypeMapping(TimestampTypeMapping)

  // Allow Aliases to be treated as whatever was aliased
  implicit def aliasToAliased[T, E <: Value[T]](alias: Alias[T, E]) = alias.aliased

  // Built-in functions (aggregates, etc)
  val AVG = FN("AVG")
  val MAX = FN("MAX")

  // Automatically convert tuples of expressions into single Expressions
  implicit def tuple2ToExpression[T1 <: Expression, T2 <: Expression](tuple: Tuple2[T1, T2]) = CompoundExpression2(tuple._1, tuple._2)
  implicit def tuple3ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression](tuple: Tuple3[T1, T2, T3]) = CompoundExpression3(tuple._1, tuple._2, tuple._3)
  implicit def tuple4ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression](tuple: Tuple4[T1, T2, T3, T4]) = CompoundExpression4(tuple._1, tuple._2, tuple._3, tuple._4)
  implicit def tuple5ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression](tuple: Tuple5[T1, T2, T3, T4, T5]) = CompoundExpression5(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5) 
  implicit def tuple6ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression](tuple: Tuple6[T1, T2, T3, T4, T5, T6]) = CompoundExpression6(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6)
  implicit def tuple7ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression](tuple: Tuple7[T1, T2, T3, T4, T5, T6, T7]) = CompoundExpression7(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7)
  implicit def tuple8ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression](tuple: Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]) = CompoundExpression8(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8)
  implicit def tuple9ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression](tuple: Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]) = CompoundExpression9(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9)
  implicit def tuple10ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression](tuple: Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]) = CompoundExpression10(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10)
  implicit def tuple11ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression](tuple: Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]) = CompoundExpression11(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11)
  implicit def tuple12ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression, T12 <: Expression](tuple: Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]) = CompoundExpression12(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12)
  implicit def tuple13ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression, T12 <: Expression, T13 <: Expression](tuple: Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]) = CompoundExpression13(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13)
  implicit def tuple14ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression, T12 <: Expression, T13 <: Expression, T14 <: Expression](tuple: Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]) = CompoundExpression14(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14)
  implicit def tuple15ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression, T12 <: Expression, T13 <: Expression, T14 <: Expression, T15 <: Expression](tuple: Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]) = CompoundExpression15(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15)
  implicit def tuple16ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression, T12 <: Expression, T13 <: Expression, T14 <: Expression, T15 <: Expression, T16 <: Expression](tuple: Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]) = CompoundExpression16(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16)
  implicit def tuple17ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression, T12 <: Expression, T13 <: Expression, T14 <: Expression, T15 <: Expression, T16 <: Expression, T17 <: Expression](tuple: Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]) = CompoundExpression17(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17)
  implicit def tuple18ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression, T12 <: Expression, T13 <: Expression, T14 <: Expression, T15 <: Expression, T16 <: Expression, T17 <: Expression, T18 <: Expression](tuple: Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]) = CompoundExpression18(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17, tuple._18)
  implicit def tuple19ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression, T12 <: Expression, T13 <: Expression, T14 <: Expression, T15 <: Expression, T16 <: Expression, T17 <: Expression, T18 <: Expression, T19 <: Expression](tuple: Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]) = CompoundExpression19(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17, tuple._18, tuple._19)
  implicit def tuple20ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression, T12 <: Expression, T13 <: Expression, T14 <: Expression, T15 <: Expression, T16 <: Expression, T17 <: Expression, T18 <: Expression, T19 <: Expression, T20 <: Expression](tuple: Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]) = CompoundExpression20(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17, tuple._18, tuple._19, tuple._20)
  implicit def tuple21ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression, T12 <: Expression, T13 <: Expression, T14 <: Expression, T15 <: Expression, T16 <: Expression, T17 <: Expression, T18 <: Expression, T19 <: Expression, T20 <: Expression, T21 <: Expression](tuple: Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]) = CompoundExpression21(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17, tuple._18, tuple._19, tuple._20, tuple._21)
  implicit def tuple22ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression, T12 <: Expression, T13 <: Expression, T14 <: Expression, T15 <: Expression, T16 <: Expression, T17 <: Expression, T18 <: Expression, T19 <: Expression, T20 <: Expression, T21 <: Expression, T22 <: Expression](tuple: Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]) = CompoundExpression22(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17, tuple._18, tuple._19, tuple._20, tuple._21, tuple._22)

  implicit def tuple2ToExtractableExpression[T1, T2](tuple: Tuple2[Extractable[T1], Extractable[T2]]) = CompoundExtractableExpression2(tuple._1, tuple._2)
  implicit def tuple3ToExtractableExpression[T1, T2, T3](tuple: Tuple3[Extractable[T1], Extractable[T2], Extractable[T3]]) = CompoundExtractableExpression3(tuple._1, tuple._2, tuple._3)
  implicit def tuple4ToExtractableExpression[T1, T2, T3, T4](tuple: Tuple4[Extractable[T1], Extractable[T2], Extractable[T3], Extractable[T4]]) = CompoundExtractableExpression4(tuple._1, tuple._2, tuple._3, tuple._4)
  implicit def tuple5ToExtractableExpression[T1, T2, T3, T4, T5](tuple: Tuple5[Extractable[T1], Extractable[T2], Extractable[T3], Extractable[T4], Extractable[T5]]) = CompoundExtractableExpression5(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5)
  implicit def tuple6ToExtractableExpression[T1, T2, T3, T4, T5, T6](tuple: Tuple6[Extractable[T1], Extractable[T2], Extractable[T3], Extractable[T4], Extractable[T5], Extractable[T6]]) = CompoundExtractableExpression6(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6)
  implicit def tuple7ToExtractableExpression[T1, T2, T3, T4, T5, T6, T7](tuple: Tuple7[Extractable[T1], Extractable[T2], Extractable[T3], Extractable[T4], Extractable[T5], Extractable[T6], Extractable[T7]]) = CompoundExtractableExpression7(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7)
  implicit def tuple8ToExtractableExpression[T1, T2, T3, T4, T5, T6, T7, T8](tuple: Tuple8[Extractable[T1], Extractable[T2], Extractable[T3], Extractable[T4], Extractable[T5], Extractable[T6], Extractable[T7], Extractable[T8]]) = CompoundExtractableExpression8(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8)
  implicit def tuple9ToExtractableExpression[T1, T2, T3, T4, T5, T6, T7, T8, T9](tuple: Tuple9[Extractable[T1], Extractable[T2], Extractable[T3], Extractable[T4], Extractable[T5], Extractable[T6], Extractable[T7], Extractable[T8], Extractable[T9]]) = CompoundExtractableExpression9(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9)
  implicit def tuple10ToExtractableExpression[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](tuple: Tuple10[Extractable[T1], Extractable[T2], Extractable[T3], Extractable[T4], Extractable[T5], Extractable[T6], Extractable[T7], Extractable[T8], Extractable[T9], Extractable[T10]]) = CompoundExtractableExpression10(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10)
  implicit def tuple11ToExtractableExpression[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](tuple: Tuple11[Extractable[T1], Extractable[T2], Extractable[T3], Extractable[T4], Extractable[T5], Extractable[T6], Extractable[T7], Extractable[T8], Extractable[T9], Extractable[T10], Extractable[T11]]) = CompoundExtractableExpression11(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11)
  implicit def tuple12ToExtractableExpression[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](tuple: Tuple12[Extractable[T1], Extractable[T2], Extractable[T3], Extractable[T4], Extractable[T5], Extractable[T6], Extractable[T7], Extractable[T8], Extractable[T9], Extractable[T10], Extractable[T11], Extractable[T12]]) = CompoundExtractableExpression12(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12)
  implicit def tuple13ToExtractableExpression[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](tuple: Tuple13[Extractable[T1], Extractable[T2], Extractable[T3], Extractable[T4], Extractable[T5], Extractable[T6], Extractable[T7], Extractable[T8], Extractable[T9], Extractable[T10], Extractable[T11], Extractable[T12], Extractable[T13]]) = CompoundExtractableExpression13(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13)
  implicit def tuple14ToExtractableExpression[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](tuple: Tuple14[Extractable[T1], Extractable[T2], Extractable[T3], Extractable[T4], Extractable[T5], Extractable[T6], Extractable[T7], Extractable[T8], Extractable[T9], Extractable[T10], Extractable[T11], Extractable[T12], Extractable[T13], Extractable[T14]]) = CompoundExtractableExpression14(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14)
  implicit def tuple15ToExtractableExpression[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](tuple: Tuple15[Extractable[T1], Extractable[T2], Extractable[T3], Extractable[T4], Extractable[T5], Extractable[T6], Extractable[T7], Extractable[T8], Extractable[T9], Extractable[T10], Extractable[T11], Extractable[T12], Extractable[T13], Extractable[T14], Extractable[T15]]) = CompoundExtractableExpression15(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15)
  implicit def tuple16ToExtractableExpression[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](tuple: Tuple16[Extractable[T1], Extractable[T2], Extractable[T3], Extractable[T4], Extractable[T5], Extractable[T6], Extractable[T7], Extractable[T8], Extractable[T9], Extractable[T10], Extractable[T11], Extractable[T12], Extractable[T13], Extractable[T14], Extractable[T15], Extractable[T16]]) = CompoundExtractableExpression16(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16)
  implicit def tuple17ToExtractableExpression[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](tuple: Tuple17[Extractable[T1], Extractable[T2], Extractable[T3], Extractable[T4], Extractable[T5], Extractable[T6], Extractable[T7], Extractable[T8], Extractable[T9], Extractable[T10], Extractable[T11], Extractable[T12], Extractable[T13], Extractable[T14], Extractable[T15], Extractable[T16], Extractable[T17]]) = CompoundExtractableExpression17(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17)
  implicit def tuple18ToExtractableExpression[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](tuple: Tuple18[Extractable[T1], Extractable[T2], Extractable[T3], Extractable[T4], Extractable[T5], Extractable[T6], Extractable[T7], Extractable[T8], Extractable[T9], Extractable[T10], Extractable[T11], Extractable[T12], Extractable[T13], Extractable[T14], Extractable[T15], Extractable[T16], Extractable[T17], Extractable[T18]]) = CompoundExtractableExpression18(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17, tuple._18)
  implicit def tuple19ToExtractableExpression[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](tuple: Tuple19[Extractable[T1], Extractable[T2], Extractable[T3], Extractable[T4], Extractable[T5], Extractable[T6], Extractable[T7], Extractable[T8], Extractable[T9], Extractable[T10], Extractable[T11], Extractable[T12], Extractable[T13], Extractable[T14], Extractable[T15], Extractable[T16], Extractable[T17], Extractable[T18], Extractable[T19]]) = CompoundExtractableExpression19(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17, tuple._18, tuple._19)
  implicit def tuple20ToExtractableExpression[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](tuple: Tuple20[Extractable[T1], Extractable[T2], Extractable[T3], Extractable[T4], Extractable[T5], Extractable[T6], Extractable[T7], Extractable[T8], Extractable[T9], Extractable[T10], Extractable[T11], Extractable[T12], Extractable[T13], Extractable[T14], Extractable[T15], Extractable[T16], Extractable[T17], Extractable[T18], Extractable[T19], Extractable[T20]]) = CompoundExtractableExpression20(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17, tuple._18, tuple._19, tuple._20)
  implicit def tuple21ToExtractableExpression[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](tuple: Tuple21[Extractable[T1], Extractable[T2], Extractable[T3], Extractable[T4], Extractable[T5], Extractable[T6], Extractable[T7], Extractable[T8], Extractable[T9], Extractable[T10], Extractable[T11], Extractable[T12], Extractable[T13], Extractable[T14], Extractable[T15], Extractable[T16], Extractable[T17], Extractable[T18], Extractable[T19], Extractable[T20], Extractable[T21]]) = CompoundExtractableExpression21(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17, tuple._18, tuple._19, tuple._20, tuple._21)
  implicit def tuple22ToExtractableExpression[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](tuple: Tuple22[Extractable[T1], Extractable[T2], Extractable[T3], Extractable[T4], Extractable[T5], Extractable[T6], Extractable[T7], Extractable[T8], Extractable[T9], Extractable[T10], Extractable[T11], Extractable[T12], Extractable[T13], Extractable[T14], Extractable[T15], Extractable[T16], Extractable[T17], Extractable[T18], Extractable[T19], Extractable[T20], Extractable[T21], Extractable[T22]]) = CompoundExtractableExpression22(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17, tuple._18, tuple._19, tuple._20, tuple._21, tuple._22)

  // Automatically convert column tuples to column lists
  implicit def columnToColumnList[T <: Table[_]](column: ColumnDef[_, T]) = List(column)
  implicit def columnTuple2ToColumnList[T <: Table[_]](tuple: Tuple2[ColumnDef[_, T], ColumnDef[_, T]]) = List(tuple._1, tuple._2)
  implicit def columnTuple3ToColumnList[T <: Table[_]](tuple: Tuple3[ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T]]) = List(tuple._1, tuple._2, tuple._3)
  implicit def columnTuple4ToColumnList[T <: Table[_]](tuple: Tuple4[ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T]]) = List(tuple._1, tuple._2, tuple._3, tuple._4)
  implicit def columnTuple5ToColumnList[T <: Table[_]](tuple: Tuple5[ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T]]) = List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5)
  implicit def columnTuple6ToColumnList[T <: Table[_]](tuple: Tuple6[ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T]]) = List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6)
  implicit def columnTuple7ToColumnList[T <: Table[_]](tuple: Tuple7[ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T]]) = List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7)
  implicit def columnTuple8ToColumnList[T <: Table[_]](tuple: Tuple8[ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T]]) = List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8)
  implicit def columnTuple9ToColumnList[T <: Table[_]](tuple: Tuple9[ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T]]) = List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9)
  implicit def columnTuple10ToColumnList[T <: Table[_]](tuple: Tuple10[ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T]]) = List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10)
  implicit def columnTuple11ToColumnList[T <: Table[_]](tuple: Tuple11[ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T]]) = List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11)
  implicit def columnTuple12ToColumnList[T <: Table[_]](tuple: Tuple12[ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T]]) = List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12)
  implicit def columnTuple13ToColumnList[T <: Table[_]](tuple: Tuple13[ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T]]) = List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13)
  implicit def columnTuple14ToColumnList[T <: Table[_]](tuple: Tuple14[ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T]]) = List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14)
  implicit def columnTuple15ToColumnList[T <: Table[_]](tuple: Tuple15[ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T]]) = List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15)
  implicit def columnTuple16ToColumnList[T <: Table[_]](tuple: Tuple16[ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T]]) = List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16)
  implicit def columnTuple17ToColumnList[T <: Table[_]](tuple: Tuple17[ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T]]) = List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17)
  implicit def columnTuple18ToColumnList[T <: Table[_]](tuple: Tuple18[ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T]]) = List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17, tuple._18)
  implicit def columnTuple19ToColumnList[T <: Table[_]](tuple: Tuple19[ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T]]) = List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17, tuple._18, tuple._19)
  implicit def columnTuple20ToColumnList[T <: Table[_]](tuple: Tuple20[ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T]]) = List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17, tuple._18, tuple._19, tuple._20)
  implicit def columnTuple21ToColumnList[T <: Table[_]](tuple: Tuple21[ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T]]) = List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17, tuple._18, tuple._19, tuple._20, tuple._21)
  implicit def columnTuple22ToColumnList[T <: Table[_]](tuple: Tuple22[ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T], ColumnDef[_, T]]) = List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17, tuple._18, tuple._19, tuple._20, tuple._21, tuple._22)
  
  // Automatically convert bound values to lists of bound values
  implicit def boundValueToList(bound: BoundValue[_]) = List(bound)
  implicit def boundTuple2ToList(tuple: Tuple2[BoundValue[_], BoundValue[_]]) = List(tuple._1, tuple._2) 
}
