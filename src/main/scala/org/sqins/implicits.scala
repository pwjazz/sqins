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
  implicit def tuple2ToExpression[T1 <: Expression, T2 <: Expression](tuple: Tuple2[T1, T2]) = CompoundExpression2(tuple._1, tuple._2)
  implicit def tuple2ToExpression[T1, T2](tuple: Tuple2[Extractable[T1], Extractable[T2]]) = CompoundExtractableExpression(tuple._1, tuple._2)
}