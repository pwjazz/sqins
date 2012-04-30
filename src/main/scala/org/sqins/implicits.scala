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
    _get = (rs: ResultSet, position: Int) => rs.getByte(position),
    _set = (ps: PreparedStatement, position: Int, value: Byte) => ps.setByte(position, value))
  implicit val OptionByteTypeMapping = new OptionTypeMapping(ByteTypeMapping)

  implicit val ShortTypeMapping = new TypeMapping[Short](
    _get = (rs: ResultSet, position: Int) => rs.getShort(position),
    _set = (ps: PreparedStatement, position: Int, value: Short) => ps.setShort(position, value))
  implicit val OptionShortTypeMapping = new OptionTypeMapping(ShortTypeMapping)

  implicit val IntTypeMapping = new TypeMapping[Int](
    _get = (rs: ResultSet, position: Int) => rs.getInt(position),
    _set = (ps: PreparedStatement, position: Int, value: Int) => ps.setInt(position, value))
  implicit val OptionIntTypeMapping = new OptionTypeMapping(IntTypeMapping)

  implicit val LongTypeMapping = new TypeMapping[Long](
    _get = (rs: ResultSet, position: Int) => rs.getLong(position),
    _set = (ps: PreparedStatement, position: Int, value: Long) => ps.setLong(position, value))
  implicit val OptionLongTypeMapping = new OptionTypeMapping(LongTypeMapping)

  implicit val FloatTypeMapping = new TypeMapping[Float](
    _get = (rs: ResultSet, position: Int) => rs.getFloat(position),
    _set = (ps: PreparedStatement, position: Int, value: Float) => ps.setFloat(position, value))
  implicit val OptionFloatTypeMapping = new OptionTypeMapping(FloatTypeMapping)

  implicit val DoubleTypeMapping = new TypeMapping[Double](
    _get = (rs: ResultSet, position: Int) => rs.getDouble(position),
    _set = (ps: PreparedStatement, position: Int, value: Double) => ps.setDouble(position, value))
  implicit val OptionDoubleTypeMapping = new OptionTypeMapping(DoubleTypeMapping)

  implicit val BooleanTypeMapping = new TypeMapping[Boolean](
    _get = (rs: ResultSet, position: Int) => rs.getBoolean(position),
    _set = (ps: PreparedStatement, position: Int, value: Boolean) => ps.setBoolean(position, value))
  implicit val OptionBooleanTypeMapping = new OptionTypeMapping(BooleanTypeMapping)

  implicit val StringTypeMapping = new TypeMapping[String](
    _get = (rs: ResultSet, position: Int) => rs.getString(position),
    _set = (ps: PreparedStatement, position: Int, value: String) => ps.setString(position, value))
  implicit val OptionStringTypeMapping = new OptionTypeMapping(StringTypeMapping)

  implicit val BigDecimalTypeMapping = new TypeMapping[BigDecimal](
    _get = (rs: ResultSet, position: Int) => BigDecimal(rs.getBigDecimal(position)),
    _set = (ps: PreparedStatement, position: Int, value: BigDecimal) => ps.setBigDecimal(position, java.math.BigDecimal.valueOf(value.doubleValue())))
  implicit val OptionBigDecimalTypeMapping = new OptionTypeMapping(BigDecimalTypeMapping)

  implicit val DateTypeMapping = new TypeMapping[Date](
    _get = (rs: ResultSet, position: Int) => rs.getDate(position),
    _set = (ps: PreparedStatement, position: Int, value: Date) => ps.setDate(position, value))
  implicit val OptionDateTypeMapping = new OptionTypeMapping(DateTypeMapping)

  implicit val TimestampTypeMapping = new TypeMapping[Timestamp](
    _get = (rs: ResultSet, position: Int) => rs.getTimestamp(position),
    _set = (ps: PreparedStatement, position: Int, value: Timestamp) => ps.setTimestamp(position, value))
  implicit val OptionTimestampTypeMapping = new OptionTypeMapping(TimestampTypeMapping)

  // Automatically convert values into bound values
  implicit def value2BoundValue[T](value: T)(implicit typeMapping: TypeMapping[T]) = BoundValue[T](value)(typeMapping)

  // Automatically convert tuples of expressions into single Expressions
  implicit def tuple1ToExpression(tuple: Tuple1[Expression]) = ListExpression(List(tuple._1))
  implicit def tuple2ToExpression(tuple: Tuple2[Expression, Expression]) = ListExpression(List(tuple._1, tuple._2))
  implicit def tuple3ToExpression(tuple: Tuple3[Expression, Expression, Expression]) = ListExpression(List(tuple._1, tuple._2, tuple._3))
  implicit def tuple4ToExpression(tuple: Tuple4[Expression, Expression, Expression, Expression]) = ListExpression(List(tuple._1, tuple._2, tuple._3, tuple._4))
  implicit def tuple5ToExpression(tuple: Tuple5[Expression, Expression, Expression, Expression, Expression]) = ListExpression(List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5))
  implicit def tuple6ToExpression(tuple: Tuple6[Expression, Expression, Expression, Expression, Expression, Expression]) = ListExpression(List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6))
  implicit def tuple7ToExpression(tuple: Tuple7[Expression, Expression, Expression, Expression, Expression, Expression, Expression]) = ListExpression(List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7))
  implicit def tuple8ToExpression(tuple: Tuple8[Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression]) = ListExpression(List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8))
  implicit def tuple9ToExpression(tuple: Tuple9[Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression]) = ListExpression(List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9))
  implicit def tuple10ToExpression(tuple: Tuple10[Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression]) = ListExpression(List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10))
  implicit def tuple11ToExpression(tuple: Tuple11[Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression]) = ListExpression(List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11))
  implicit def tuple12ToExpression(tuple: Tuple12[Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression]) = ListExpression(List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12))
  implicit def tuple13ToExpression(tuple: Tuple13[Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression]) = ListExpression(List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13))
  implicit def tuple14ToExpression(tuple: Tuple14[Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression]) = ListExpression(List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14))
  implicit def tuple15ToExpression(tuple: Tuple15[Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression]) = ListExpression(List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15))
  implicit def tuple16ToExpression(tuple: Tuple16[Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression]) = ListExpression(List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16))
  implicit def tuple17ToExpression(tuple: Tuple17[Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression]) = ListExpression(List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17))
  implicit def tuple18ToExpression(tuple: Tuple18[Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression]) = ListExpression(List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17, tuple._18))
  implicit def tuple19ToExpression(tuple: Tuple19[Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression]) = ListExpression(List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17, tuple._18, tuple._19))
  implicit def tuple20ToExpression(tuple: Tuple20[Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression]) = ListExpression(List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17, tuple._18, tuple._19, tuple._20))
  implicit def tuple21ToExpression(tuple: Tuple21[Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression]) = ListExpression(List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17, tuple._18, tuple._19, tuple._20, tuple._21))
  implicit def tuple22ToExpression(tuple: Tuple22[Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression, Expression]) = ListExpression(List(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10, tuple._11, tuple._12, tuple._13, tuple._14, tuple._15, tuple._16, tuple._17, tuple._18, tuple._19, tuple._20, tuple._21, tuple._22))

}