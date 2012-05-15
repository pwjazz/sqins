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
import java.sql.Connection
import java.util.UUID

/**
 * Pre-defined implicit type mappings and other implicit conversions.  sqins requires that these be brought into scope
 * in order to build table definitions and queries.
 */
object Implicits {
  // Allow Aliases to be treated as whatever was aliased
  implicit def aliasToAliased[T, E <: ScalarValue[T]](alias: Alias[T, E]) = alias.aliased
  
  // Treat a SelectQuery as its result
  implicit def selectQueryToResult[T](query: SelectQuery[T])(implicit conn: Connection) = query(conn)
  
  // Treat an InsertValuesQuery as its result
  implicit def insertValuesQueryToResult[T](query: InsertValuesQuery[T])(implicit conn: Connection) = query(conn)
  implicit def insertValuesQueryReturningToResult[T, R](query: InsertValuesQueryReturning[T, R])(implicit conn: Connection) = query(conn)
  
  // Treat an InsertSelectQuery as its result
  implicit def insertSelectQueryToResult[T](query: InsertSelectQuery[T])(implicit conn: Connection) = query(conn)
  implicit def insertSelectQueryReturningToResult[T, R](query: InsertSelectQueryReturning[T, R])(implicit conn: Connection) = query(conn)
  
  // Treat an UpdateQuery as its result
  implicit def updateQueryToResult[T](query: UpdateQuery[T])(implicit conn: Connection) = query(conn)
  implicit def updateQueryReturning[T, R](query: UpdateQueryReturning[T, R])(implicit conn: Connection) = query(conn)
  
  // Treat a DeleteQuery as its result
  implicit def deleteQueryToResult[T](query: DeleteQuery[T])(implicit conn: Connection) = query(conn)
  implicit def deleteQueryReturning[T, R](query: DeleteQueryReturning[T, R])(implicit conn: Connection) = query(conn)
  
  // Built-in functions (aggregates, etc)
  val AVG = FN("AVG")
  val COUNT = FN("COUNT")
  val COUNT_DISTINCT = FN("COUNT", Some("DISTINCT"))
  val COUNT_* = EXPR("COUNT(*)")
  val MIN = FN("MIN")
  val MAX = FN("MAX")
  val SUM = FN("SUM")
  val VAR_POP = FN("VAR_POP")
  val VAR_SAMP = FN("VAR_SAMP")

  // Type mappings
  implicit object ByteTypeMapping extends TypeMapping[Byte] {
    def _get(rs: ResultSet, position: Int) = Extraction(rs.getByte(position), 1)
    def _set(ps: PreparedStatement, position: Int, value: Byte) = ps.setByte(position, value)
  }
  implicit val OptionByteTypeMapping = new OptionTypeMapping(ByteTypeMapping)

  implicit object ShortTypeMapping extends TypeMapping[Short] {
    def _get(rs: ResultSet, position: Int) = Extraction(rs.getShort(position), 1)
    def _set(ps: PreparedStatement, position: Int, value: Short) = ps.setShort(position, value)
  }
  implicit val OptionShortTypeMapping = new OptionTypeMapping(ShortTypeMapping)

  implicit object IntTypeMapping extends TypeMapping[Int] {
    def _get(rs: ResultSet, position: Int) = Extraction(rs.getInt(position), 1)
    def _set(ps: PreparedStatement, position: Int, value: Int) = ps.setInt(position, value)
  }
  implicit val OptionIntTypeMapping = new OptionTypeMapping(IntTypeMapping)

  implicit object LongTypeMapping extends TypeMapping[Long] {
    def _get(rs: ResultSet, position: Int) = Extraction(rs.getLong(position), 1)
    def _set(ps: PreparedStatement, position: Int, value: Long) = ps.setLong(position, value)
  }
  implicit val OptionLongTypeMapping = new OptionTypeMapping(LongTypeMapping)

  implicit object FloatTypeMapping extends TypeMapping[Float] {
    def _get(rs: ResultSet, position: Int) = Extraction(rs.getFloat(position), 1)
    def _set(ps: PreparedStatement, position: Int, value: Float) = ps.setFloat(position, value)
  }
  implicit val OptionFloatTypeMapping = new OptionTypeMapping(FloatTypeMapping)

  implicit object DoubleTypeMapping extends TypeMapping[Double] {
    def _get(rs: ResultSet, position: Int) = Extraction(rs.getDouble(position), 1)
    def _set(ps: PreparedStatement, position: Int, value: Double) = ps.setDouble(position, value)
  }
  implicit val OptionDoubleTypeMapping = new OptionTypeMapping(DoubleTypeMapping)

  implicit object BooleanTypeMapping extends TypeMapping[Boolean] {
    def _get(rs: ResultSet, position: Int) = Extraction(rs.getBoolean(position), 1)
    def _set(ps: PreparedStatement, position: Int, value: Boolean) = ps.setBoolean(position, value)
  }
  implicit val OptionBooleanTypeMapping = new OptionTypeMapping(BooleanTypeMapping)

  implicit object StringTypeMapping extends TypeMapping[String] {
    def _get(rs: ResultSet, position: Int) = Extraction(rs.getString(position), 1)
    def _set(ps: PreparedStatement, position: Int, value: String) = ps.setString(position, value)
  }
  implicit val OptionStringTypeMapping = new OptionTypeMapping(StringTypeMapping)

  implicit object BigDecimalTypeMapping extends TypeMapping[BigDecimal] {
    def _get(rs: ResultSet, position: Int) = Extraction(BigDecimal(rs.getBigDecimal(position)), 1)
    def _set(ps: PreparedStatement, position: Int, value: BigDecimal) = ps.setBigDecimal(position, java.math.BigDecimal.valueOf(value.doubleValue()))
  }
  implicit val OptionBigDecimalTypeMapping = new OptionTypeMapping(BigDecimalTypeMapping)

  implicit object DateTypeMapping extends TypeMapping[Date] {
    def _get(rs: ResultSet, position: Int) = Extraction(rs.getDate(position), 1)
    def _set(ps: PreparedStatement, position: Int, value: Date) = ps.setDate(position, value)
  }
  implicit val OptionDateTypeMapping = new OptionTypeMapping(DateTypeMapping)

  implicit object TimestampTypeMapping extends TypeMapping[Timestamp] {
    def _get(rs: ResultSet, position: Int) = Extraction(rs.getTimestamp(position), 1)
    def _set(ps: PreparedStatement, position: Int, value: Timestamp) = ps.setTimestamp(position, value)
  }
  implicit val OptionTimestampTypeMapping = new OptionTypeMapping(TimestampTypeMapping)
  
  implicit object ByteArrayTypeMapping extends TypeMapping[Array[Byte]] {
    def _get(rs: ResultSet, position: Int) = Extraction(rs.getBytes(position), 1)
    def _set(ps: PreparedStatement, position: Int, value: Array[Byte]) = ps.setBytes(position, value)
  }
  implicit val OptionByteArrayTypeMapping = new OptionTypeMapping(ByteArrayTypeMapping)
  
  implicit object UUIDTypeMapping extends TypeMapping[UUID] {
    def _get(rs: ResultSet, position: Int) = Extraction(UUID.fromString(rs.getString(position)), 1)
    def _set(ps: PreparedStatement, position: Int, value: UUID) = ps.setString(position, value.toString())
  }
  implicit val OptionUUIDTypeMapping = new OptionTypeMapping(UUIDTypeMapping)

  // Automatically convert tuples of expressions into single Expressions
  implicit def tuple2ToExpression[T1 <: Expression, T2 <: Expression](tuple: Tuple2[T1, T2]) = new CompoundExpression2(tuple._1, tuple._2)
  implicit def tuple3ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression](tuple: Tuple3[T1, T2, T3]) = new CompoundExpression3(tuple._1, tuple._2, tuple._3)
  implicit def tuple4ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression](tuple: Tuple4[T1, T2, T3, T4]) = new CompoundExpression4(tuple._1, tuple._2, tuple._3, tuple._4)
  implicit def tuple5ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression](tuple: Tuple5[T1, T2, T3, T4, T5]) = new CompoundExpression5(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5)
  implicit def tuple6ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression](tuple: Tuple6[T1, T2, T3, T4, T5, T6]) = new CompoundExpression6(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6)
  implicit def tuple7ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression](tuple: Tuple7[T1, T2, T3, T4, T5, T6, T7]) = new CompoundExpression7(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7)
  implicit def tuple8ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression](tuple: Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]) = new CompoundExpression8(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8)
  implicit def tuple9ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression](tuple: Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]) = new CompoundExpression9(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9)
  implicit def tuple10ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression](tuple: Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]) = new CompoundExpression10(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10)
  implicit def tuple11ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression](tuple: Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]) = new CompoundExpression11(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11)
  implicit def tuple12ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression, T12 <: Expression](tuple: Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]) = new CompoundExpression12(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12)
  implicit def tuple13ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression, T12 <: Expression, T13 <: Expression](tuple: Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]) = new CompoundExpression13(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13)
  implicit def tuple14ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression, T12 <: Expression, T13 <: Expression, T14 <: Expression](tuple: Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]) = new CompoundExpression14(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14)
  implicit def tuple15ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression, T12 <: Expression, T13 <: Expression, T14 <: Expression, T15 <: Expression](tuple: Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]) = new CompoundExpression15(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15)
  implicit def tuple16ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression, T12 <: Expression, T13 <: Expression, T14 <: Expression, T15 <: Expression, T16 <: Expression](tuple: Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]) = new CompoundExpression16(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16)
  implicit def tuple17ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression, T12 <: Expression, T13 <: Expression, T14 <: Expression, T15 <: Expression, T16 <: Expression, T17 <: Expression](tuple: Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]) = new CompoundExpression17(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17)
  implicit def tuple18ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression, T12 <: Expression, T13 <: Expression, T14 <: Expression, T15 <: Expression, T16 <: Expression, T17 <: Expression, T18 <: Expression](tuple: Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]) = new CompoundExpression18(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17, tuple._18)
  implicit def tuple19ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression, T12 <: Expression, T13 <: Expression, T14 <: Expression, T15 <: Expression, T16 <: Expression, T17 <: Expression, T18 <: Expression, T19 <: Expression](tuple: Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]) = new CompoundExpression19(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17, tuple._18, tuple._19)
  implicit def tuple20ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression, T12 <: Expression, T13 <: Expression, T14 <: Expression, T15 <: Expression, T16 <: Expression, T17 <: Expression, T18 <: Expression, T19 <: Expression, T20 <: Expression](tuple: Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]) = new CompoundExpression20(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17, tuple._18, tuple._19, tuple._20)
  implicit def tuple21ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression, T12 <: Expression, T13 <: Expression, T14 <: Expression, T15 <: Expression, T16 <: Expression, T17 <: Expression, T18 <: Expression, T19 <: Expression, T20 <: Expression, T21 <: Expression](tuple: Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]) = new CompoundExpression21(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17, tuple._18, tuple._19, tuple._20, tuple._21)
  implicit def tuple22ToExpression[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression, T12 <: Expression, T13 <: Expression, T14 <: Expression, T15 <: Expression, T16 <: Expression, T17 <: Expression, T18 <: Expression, T19 <: Expression, T20 <: Expression, T21 <: Expression, T22 <: Expression](tuple: Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]) = new CompoundExpression22(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17, tuple._18, tuple._19, tuple._20, tuple._21, tuple._22)

  implicit def tuple2ToExtractableExpression[T1, T2](tuple: Tuple2[Extractable[T1] with Expression, Extractable[T2] with Expression]) = new CompoundExtractableExpression2(tuple._1, tuple._2)
  implicit def tuple3ToExtractableExpression[T1, T2, T3](tuple: Tuple3[Extractable[T1] with Expression, Extractable[T2] with Expression, Extractable[T3] with Expression]) = new CompoundExtractableExpression3(tuple._1, tuple._2, tuple._3)
  implicit def tuple4ToExtractableExpression[T1, T2, T3, T4](tuple: Tuple4[Extractable[T1] with Expression, Extractable[T2] with Expression, Extractable[T3] with Expression, Extractable[T4] with Expression]) = new CompoundExtractableExpression4(tuple._1, tuple._2, tuple._3, tuple._4)
  implicit def tuple5ToExtractableExpression[T1, T2, T3, T4, T5](tuple: Tuple5[Extractable[T1] with Expression, Extractable[T2] with Expression, Extractable[T3] with Expression, Extractable[T4] with Expression, Extractable[T5] with Expression]) = new CompoundExtractableExpression5(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5)
  implicit def tuple6ToExtractableExpression[T1, T2, T3, T4, T5, T6](tuple: Tuple6[Extractable[T1] with Expression, Extractable[T2] with Expression, Extractable[T3] with Expression, Extractable[T4] with Expression, Extractable[T5] with Expression, Extractable[T6] with Expression]) = new CompoundExtractableExpression6(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6)
  implicit def tuple7ToExtractableExpression[T1, T2, T3, T4, T5, T6, T7](tuple: Tuple7[Extractable[T1] with Expression, Extractable[T2] with Expression, Extractable[T3] with Expression, Extractable[T4] with Expression, Extractable[T5] with Expression, Extractable[T6] with Expression, Extractable[T7] with Expression]) = new CompoundExtractableExpression7(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7)
  implicit def tuple8ToExtractableExpression[T1, T2, T3, T4, T5, T6, T7, T8](tuple: Tuple8[Extractable[T1] with Expression, Extractable[T2] with Expression, Extractable[T3] with Expression, Extractable[T4] with Expression, Extractable[T5] with Expression, Extractable[T6] with Expression, Extractable[T7] with Expression, Extractable[T8] with Expression]) = new CompoundExtractableExpression8(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8)
  implicit def tuple9ToExtractableExpression[T1, T2, T3, T4, T5, T6, T7, T8, T9](tuple: Tuple9[Extractable[T1] with Expression, Extractable[T2] with Expression, Extractable[T3] with Expression, Extractable[T4] with Expression, Extractable[T5] with Expression, Extractable[T6] with Expression, Extractable[T7] with Expression, Extractable[T8] with Expression, Extractable[T9] with Expression]) = new CompoundExtractableExpression9(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9)
  implicit def tuple10ToExtractableExpression[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](tuple: Tuple10[Extractable[T1] with Expression, Extractable[T2] with Expression, Extractable[T3] with Expression, Extractable[T4] with Expression, Extractable[T5] with Expression, Extractable[T6] with Expression, Extractable[T7] with Expression, Extractable[T8] with Expression, Extractable[T9] with Expression, Extractable[T10] with Expression]) = new CompoundExtractableExpression10(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10)
  implicit def tuple11ToExtractableExpression[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](tuple: Tuple11[Extractable[T1] with Expression, Extractable[T2] with Expression, Extractable[T3] with Expression, Extractable[T4] with Expression, Extractable[T5] with Expression, Extractable[T6] with Expression, Extractable[T7] with Expression, Extractable[T8] with Expression, Extractable[T9] with Expression, Extractable[T10] with Expression, Extractable[T11] with Expression]) = new CompoundExtractableExpression11(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11)
  implicit def tuple12ToExtractableExpression[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](tuple: Tuple12[Extractable[T1] with Expression, Extractable[T2] with Expression, Extractable[T3] with Expression, Extractable[T4] with Expression, Extractable[T5] with Expression, Extractable[T6] with Expression, Extractable[T7] with Expression, Extractable[T8] with Expression, Extractable[T9] with Expression, Extractable[T10] with Expression, Extractable[T11] with Expression, Extractable[T12] with Expression]) = new CompoundExtractableExpression12(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12)
  implicit def tuple13ToExtractableExpression[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](tuple: Tuple13[Extractable[T1] with Expression, Extractable[T2] with Expression, Extractable[T3] with Expression, Extractable[T4] with Expression, Extractable[T5] with Expression, Extractable[T6] with Expression, Extractable[T7] with Expression, Extractable[T8] with Expression, Extractable[T9] with Expression, Extractable[T10] with Expression, Extractable[T11] with Expression, Extractable[T12] with Expression, Extractable[T13] with Expression]) = new CompoundExtractableExpression13(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13)
  implicit def tuple14ToExtractableExpression[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](tuple: Tuple14[Extractable[T1] with Expression, Extractable[T2] with Expression, Extractable[T3] with Expression, Extractable[T4] with Expression, Extractable[T5] with Expression, Extractable[T6] with Expression, Extractable[T7] with Expression, Extractable[T8] with Expression, Extractable[T9] with Expression, Extractable[T10] with Expression, Extractable[T11] with Expression, Extractable[T12] with Expression, Extractable[T13] with Expression, Extractable[T14] with Expression]) = new CompoundExtractableExpression14(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14)
  implicit def tuple15ToExtractableExpression[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](tuple: Tuple15[Extractable[T1] with Expression, Extractable[T2] with Expression, Extractable[T3] with Expression, Extractable[T4] with Expression, Extractable[T5] with Expression, Extractable[T6] with Expression, Extractable[T7] with Expression, Extractable[T8] with Expression, Extractable[T9] with Expression, Extractable[T10] with Expression, Extractable[T11] with Expression, Extractable[T12] with Expression, Extractable[T13] with Expression, Extractable[T14] with Expression, Extractable[T15] with Expression]) = new CompoundExtractableExpression15(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15)
  implicit def tuple16ToExtractableExpression[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](tuple: Tuple16[Extractable[T1] with Expression, Extractable[T2] with Expression, Extractable[T3] with Expression, Extractable[T4] with Expression, Extractable[T5] with Expression, Extractable[T6] with Expression, Extractable[T7] with Expression, Extractable[T8] with Expression, Extractable[T9] with Expression, Extractable[T10] with Expression, Extractable[T11] with Expression, Extractable[T12] with Expression, Extractable[T13] with Expression, Extractable[T14] with Expression, Extractable[T15] with Expression, Extractable[T16] with Expression]) = new CompoundExtractableExpression16(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16)
  implicit def tuple17ToExtractableExpression[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](tuple: Tuple17[Extractable[T1] with Expression, Extractable[T2] with Expression, Extractable[T3] with Expression, Extractable[T4] with Expression, Extractable[T5] with Expression, Extractable[T6] with Expression, Extractable[T7] with Expression, Extractable[T8] with Expression, Extractable[T9] with Expression, Extractable[T10] with Expression, Extractable[T11] with Expression, Extractable[T12] with Expression, Extractable[T13] with Expression, Extractable[T14] with Expression, Extractable[T15] with Expression, Extractable[T16] with Expression, Extractable[T17] with Expression]) = new CompoundExtractableExpression17(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17)
  implicit def tuple18ToExtractableExpression[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](tuple: Tuple18[Extractable[T1] with Expression, Extractable[T2] with Expression, Extractable[T3] with Expression, Extractable[T4] with Expression, Extractable[T5] with Expression, Extractable[T6] with Expression, Extractable[T7] with Expression, Extractable[T8] with Expression, Extractable[T9] with Expression, Extractable[T10] with Expression, Extractable[T11] with Expression, Extractable[T12] with Expression, Extractable[T13] with Expression, Extractable[T14] with Expression, Extractable[T15] with Expression, Extractable[T16] with Expression, Extractable[T17] with Expression, Extractable[T18] with Expression]) = new CompoundExtractableExpression18(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17, tuple._18)
  implicit def tuple19ToExtractableExpression[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](tuple: Tuple19[Extractable[T1] with Expression, Extractable[T2] with Expression, Extractable[T3] with Expression, Extractable[T4] with Expression, Extractable[T5] with Expression, Extractable[T6] with Expression, Extractable[T7] with Expression, Extractable[T8] with Expression, Extractable[T9] with Expression, Extractable[T10] with Expression, Extractable[T11] with Expression, Extractable[T12] with Expression, Extractable[T13] with Expression, Extractable[T14] with Expression, Extractable[T15] with Expression, Extractable[T16] with Expression, Extractable[T17] with Expression, Extractable[T18] with Expression, Extractable[T19] with Expression]) = new CompoundExtractableExpression19(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17, tuple._18, tuple._19)
  implicit def tuple20ToExtractableExpression[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](tuple: Tuple20[Extractable[T1] with Expression, Extractable[T2] with Expression, Extractable[T3] with Expression, Extractable[T4] with Expression, Extractable[T5] with Expression, Extractable[T6] with Expression, Extractable[T7] with Expression, Extractable[T8] with Expression, Extractable[T9] with Expression, Extractable[T10] with Expression, Extractable[T11] with Expression, Extractable[T12] with Expression, Extractable[T13] with Expression, Extractable[T14] with Expression, Extractable[T15] with Expression, Extractable[T16] with Expression, Extractable[T17] with Expression, Extractable[T18] with Expression, Extractable[T19] with Expression, Extractable[T20] with Expression]) = new CompoundExtractableExpression20(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17, tuple._18, tuple._19, tuple._20)
  implicit def tuple21ToExtractableExpression[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](tuple: Tuple21[Extractable[T1] with Expression, Extractable[T2] with Expression, Extractable[T3] with Expression, Extractable[T4] with Expression, Extractable[T5] with Expression, Extractable[T6] with Expression, Extractable[T7] with Expression, Extractable[T8] with Expression, Extractable[T9] with Expression, Extractable[T10] with Expression, Extractable[T11] with Expression, Extractable[T12] with Expression, Extractable[T13] with Expression, Extractable[T14] with Expression, Extractable[T15] with Expression, Extractable[T16] with Expression, Extractable[T17] with Expression, Extractable[T18] with Expression, Extractable[T19] with Expression, Extractable[T20] with Expression, Extractable[T21] with Expression]) = new CompoundExtractableExpression21(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17, tuple._18, tuple._19, tuple._20, tuple._21)
  implicit def tuple22ToExtractableExpression[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](tuple: Tuple22[Extractable[T1] with Expression, Extractable[T2] with Expression, Extractable[T3] with Expression, Extractable[T4] with Expression, Extractable[T5] with Expression, Extractable[T6] with Expression, Extractable[T7] with Expression, Extractable[T8] with Expression, Extractable[T9] with Expression, Extractable[T10] with Expression, Extractable[T11] with Expression, Extractable[T12] with Expression, Extractable[T13] with Expression, Extractable[T14] with Expression, Extractable[T15] with Expression, Extractable[T16] with Expression, Extractable[T17] with Expression, Extractable[T18] with Expression, Extractable[T19] with Expression, Extractable[T20] with Expression, Extractable[T21] with Expression, Extractable[T22] with Expression]) = new CompoundExtractableExpression22(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17, tuple._18, tuple._19, tuple._20, tuple._21, tuple._22)

  // Automatically convert bound values to lists of bound values
  implicit def boundValueToList(bound: BoundValue[_]) = List(bound)
  implicit def boundTuple2ToList(tuple: Tuple2[BoundValue[_], BoundValue[_]]) = List(tuple._1, tuple._2)
}
