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

/*
 * This file defines compound expressions and compound extractable expressions based on tuples.
 * These are auto-generated from the CompoundExpressionGenerator class at the bottom of this file.
 * If we had macros, we wouldn't need to do this, but there you have it.
 */

/**
 * Provides functionality common to all CompoundExtractables.
 */
private[sqins] class CompoundExtractor {
  var columnsRead = 0

  def doExtract[T](extractable: Extractable[T], rs: ResultSet, position: Int) = {
    val extracted = extractable.extract(rs, position + columnsRead)
    columnsRead += extracted.columnsRead
    extracted.value
  }
}

private[sqins] class CompoundExpression2[T1 <: Expression, T2 <: Expression](override val _1: T1, override val _2: T2) extends Tuple2(_1, _2) with Expression {
  override def expression = Array(_1.expression, _2.expression) mkString ", "
  override def selectExpression = Array(_1.selectExpression, _2.selectExpression) mkString ", "
  override def unaliasedExpression = Array(_1.unaliasedExpression, _2.unaliasedExpression) mkString ", "
  override def boundValues = _1.boundValues ++ _2.boundValues
}
private[sqins] class CompoundExpression3[T1 <: Expression, T2 <: Expression, T3 <: Expression](override val _1: T1, override val _2: T2, override val _3: T3) extends Tuple3(_1, _2, _3) with Expression {
  override def expression = Array(_1.expression, _2.expression, _3.expression) mkString ", "
  override def selectExpression = Array(_1.selectExpression, _2.selectExpression, _3.selectExpression) mkString ", "
  override def unaliasedExpression = Array(_1.unaliasedExpression, _2.unaliasedExpression, _3.unaliasedExpression) mkString ", "
  override def boundValues = _1.boundValues ++ _2.boundValues ++ _3.boundValues
}
private[sqins] class CompoundExpression4[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression](override val _1: T1, override val _2: T2, override val _3: T3, override val _4: T4) extends Tuple4(_1, _2, _3, _4) with Expression {
  override def expression = Array(_1.expression, _2.expression, _3.expression, _4.expression) mkString ", "
  override def selectExpression = Array(_1.selectExpression, _2.selectExpression, _3.selectExpression, _4.selectExpression) mkString ", "
  override def unaliasedExpression = Array(_1.unaliasedExpression, _2.unaliasedExpression, _3.unaliasedExpression, _4.unaliasedExpression) mkString ", "
  override def boundValues = _1.boundValues ++ _2.boundValues ++ _3.boundValues ++ _4.boundValues
}
private[sqins] class CompoundExpression5[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression](override val _1: T1, override val _2: T2, override val _3: T3, override val _4: T4, override val _5: T5) extends Tuple5(_1, _2, _3, _4, _5) with Expression {
  override def expression = Array(_1.expression, _2.expression, _3.expression, _4.expression, _5.expression) mkString ", "
  override def selectExpression = Array(_1.selectExpression, _2.selectExpression, _3.selectExpression, _4.selectExpression, _5.selectExpression) mkString ", "
  override def unaliasedExpression = Array(_1.unaliasedExpression, _2.unaliasedExpression, _3.unaliasedExpression, _4.unaliasedExpression, _5.unaliasedExpression) mkString ", "
  override def boundValues = _1.boundValues ++ _2.boundValues ++ _3.boundValues ++ _4.boundValues ++ _5.boundValues
}
private[sqins] class CompoundExpression6[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression](override val _1: T1, override val _2: T2, override val _3: T3, override val _4: T4, override val _5: T5, override val _6: T6) extends Tuple6(_1, _2, _3, _4, _5, _6) with Expression {
  override def expression = Array(_1.expression, _2.expression, _3.expression, _4.expression, _5.expression, _6.expression) mkString ", "
  override def selectExpression = Array(_1.selectExpression, _2.selectExpression, _3.selectExpression, _4.selectExpression, _5.selectExpression, _6.selectExpression) mkString ", "
  override def unaliasedExpression = Array(_1.unaliasedExpression, _2.unaliasedExpression, _3.unaliasedExpression, _4.unaliasedExpression, _5.unaliasedExpression, _6.unaliasedExpression) mkString ", "
  override def boundValues = _1.boundValues ++ _2.boundValues ++ _3.boundValues ++ _4.boundValues ++ _5.boundValues ++ _6.boundValues
}
private[sqins] class CompoundExpression7[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression](override val _1: T1, override val _2: T2, override val _3: T3, override val _4: T4, override val _5: T5, override val _6: T6, override val _7: T7) extends Tuple7(_1, _2, _3, _4, _5, _6, _7) with Expression {
  override def expression = Array(_1.expression, _2.expression, _3.expression, _4.expression, _5.expression, _6.expression, _7.expression) mkString ", "
  override def selectExpression = Array(_1.selectExpression, _2.selectExpression, _3.selectExpression, _4.selectExpression, _5.selectExpression, _6.selectExpression, _7.selectExpression) mkString ", "
  override def unaliasedExpression = Array(_1.unaliasedExpression, _2.unaliasedExpression, _3.unaliasedExpression, _4.unaliasedExpression, _5.unaliasedExpression, _6.unaliasedExpression, _7.unaliasedExpression) mkString ", "
  override def boundValues = _1.boundValues ++ _2.boundValues ++ _3.boundValues ++ _4.boundValues ++ _5.boundValues ++ _6.boundValues ++ _7.boundValues
}
private[sqins] class CompoundExpression8[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression](override val _1: T1, override val _2: T2, override val _3: T3, override val _4: T4, override val _5: T5, override val _6: T6, override val _7: T7, override val _8: T8) extends Tuple8(_1, _2, _3, _4, _5, _6, _7, _8) with Expression {
  override def expression = Array(_1.expression, _2.expression, _3.expression, _4.expression, _5.expression, _6.expression, _7.expression, _8.expression) mkString ", "
  override def selectExpression = Array(_1.selectExpression, _2.selectExpression, _3.selectExpression, _4.selectExpression, _5.selectExpression, _6.selectExpression, _7.selectExpression, _8.selectExpression) mkString ", "
  override def unaliasedExpression = Array(_1.unaliasedExpression, _2.unaliasedExpression, _3.unaliasedExpression, _4.unaliasedExpression, _5.unaliasedExpression, _6.unaliasedExpression, _7.unaliasedExpression, _8.unaliasedExpression) mkString ", "
  override def boundValues = _1.boundValues ++ _2.boundValues ++ _3.boundValues ++ _4.boundValues ++ _5.boundValues ++ _6.boundValues ++ _7.boundValues ++ _8.boundValues
}
private[sqins] class CompoundExpression9[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression](override val _1: T1, override val _2: T2, override val _3: T3, override val _4: T4, override val _5: T5, override val _6: T6, override val _7: T7, override val _8: T8, override val _9: T9) extends Tuple9(_1, _2, _3, _4, _5, _6, _7, _8, _9) with Expression {
  override def expression = Array(_1.expression, _2.expression, _3.expression, _4.expression, _5.expression, _6.expression, _7.expression, _8.expression, _9.expression) mkString ", "
  override def selectExpression = Array(_1.selectExpression, _2.selectExpression, _3.selectExpression, _4.selectExpression, _5.selectExpression, _6.selectExpression, _7.selectExpression, _8.selectExpression, _9.selectExpression) mkString ", "
  override def unaliasedExpression = Array(_1.unaliasedExpression, _2.unaliasedExpression, _3.unaliasedExpression, _4.unaliasedExpression, _5.unaliasedExpression, _6.unaliasedExpression, _7.unaliasedExpression, _8.unaliasedExpression, _9.unaliasedExpression) mkString ", "
  override def boundValues = _1.boundValues ++ _2.boundValues ++ _3.boundValues ++ _4.boundValues ++ _5.boundValues ++ _6.boundValues ++ _7.boundValues ++ _8.boundValues ++ _9.boundValues
}
private[sqins] class CompoundExpression10[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression](override val _1: T1, override val _2: T2, override val _3: T3, override val _4: T4, override val _5: T5, override val _6: T6, override val _7: T7, override val _8: T8, override val _9: T9, override val _10: T10) extends Tuple10(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10) with Expression {
  override def expression = Array(_1.expression, _2.expression, _3.expression, _4.expression, _5.expression, _6.expression, _7.expression, _8.expression, _9.expression, _10.expression) mkString ", "
  override def selectExpression = Array(_1.selectExpression, _2.selectExpression, _3.selectExpression, _4.selectExpression, _5.selectExpression, _6.selectExpression, _7.selectExpression, _8.selectExpression, _9.selectExpression, _10.selectExpression) mkString ", "
  override def unaliasedExpression = Array(_1.unaliasedExpression, _2.unaliasedExpression, _3.unaliasedExpression, _4.unaliasedExpression, _5.unaliasedExpression, _6.unaliasedExpression, _7.unaliasedExpression, _8.unaliasedExpression, _9.unaliasedExpression, _10.unaliasedExpression) mkString ", "
  override def boundValues = _1.boundValues ++ _2.boundValues ++ _3.boundValues ++ _4.boundValues ++ _5.boundValues ++ _6.boundValues ++ _7.boundValues ++ _8.boundValues ++ _9.boundValues ++ _10.boundValues
}
private[sqins] class CompoundExpression11[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression](override val _1: T1, override val _2: T2, override val _3: T3, override val _4: T4, override val _5: T5, override val _6: T6, override val _7: T7, override val _8: T8, override val _9: T9, override val _10: T10, override val _11: T11) extends Tuple11(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11) with Expression {
  override def expression = Array(_1.expression, _2.expression, _3.expression, _4.expression, _5.expression, _6.expression, _7.expression, _8.expression, _9.expression, _10.expression, _11.expression) mkString ", "
  override def selectExpression = Array(_1.selectExpression, _2.selectExpression, _3.selectExpression, _4.selectExpression, _5.selectExpression, _6.selectExpression, _7.selectExpression, _8.selectExpression, _9.selectExpression, _10.selectExpression, _11.selectExpression) mkString ", "
  override def unaliasedExpression = Array(_1.unaliasedExpression, _2.unaliasedExpression, _3.unaliasedExpression, _4.unaliasedExpression, _5.unaliasedExpression, _6.unaliasedExpression, _7.unaliasedExpression, _8.unaliasedExpression, _9.unaliasedExpression, _10.unaliasedExpression, _11.unaliasedExpression) mkString ", "
  override def boundValues = _1.boundValues ++ _2.boundValues ++ _3.boundValues ++ _4.boundValues ++ _5.boundValues ++ _6.boundValues ++ _7.boundValues ++ _8.boundValues ++ _9.boundValues ++ _10.boundValues ++ _11.boundValues
}
private[sqins] class CompoundExpression12[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression, T12 <: Expression](override val _1: T1, override val _2: T2, override val _3: T3, override val _4: T4, override val _5: T5, override val _6: T6, override val _7: T7, override val _8: T8, override val _9: T9, override val _10: T10, override val _11: T11, override val _12: T12) extends Tuple12(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12) with Expression {
  override def expression = Array(_1.expression, _2.expression, _3.expression, _4.expression, _5.expression, _6.expression, _7.expression, _8.expression, _9.expression, _10.expression, _11.expression, _12.expression) mkString ", "
  override def selectExpression = Array(_1.selectExpression, _2.selectExpression, _3.selectExpression, _4.selectExpression, _5.selectExpression, _6.selectExpression, _7.selectExpression, _8.selectExpression, _9.selectExpression, _10.selectExpression, _11.selectExpression, _12.selectExpression) mkString ", "
  override def unaliasedExpression = Array(_1.unaliasedExpression, _2.unaliasedExpression, _3.unaliasedExpression, _4.unaliasedExpression, _5.unaliasedExpression, _6.unaliasedExpression, _7.unaliasedExpression, _8.unaliasedExpression, _9.unaliasedExpression, _10.unaliasedExpression, _11.unaliasedExpression, _12.unaliasedExpression) mkString ", "
  override def boundValues = _1.boundValues ++ _2.boundValues ++ _3.boundValues ++ _4.boundValues ++ _5.boundValues ++ _6.boundValues ++ _7.boundValues ++ _8.boundValues ++ _9.boundValues ++ _10.boundValues ++ _11.boundValues ++ _12.boundValues
}
private[sqins] class CompoundExpression13[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression, T12 <: Expression, T13 <: Expression](override val _1: T1, override val _2: T2, override val _3: T3, override val _4: T4, override val _5: T5, override val _6: T6, override val _7: T7, override val _8: T8, override val _9: T9, override val _10: T10, override val _11: T11, override val _12: T12, override val _13: T13) extends Tuple13(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13) with Expression {
  override def expression = Array(_1.expression, _2.expression, _3.expression, _4.expression, _5.expression, _6.expression, _7.expression, _8.expression, _9.expression, _10.expression, _11.expression, _12.expression, _13.expression) mkString ", "
  override def selectExpression = Array(_1.selectExpression, _2.selectExpression, _3.selectExpression, _4.selectExpression, _5.selectExpression, _6.selectExpression, _7.selectExpression, _8.selectExpression, _9.selectExpression, _10.selectExpression, _11.selectExpression, _12.selectExpression, _13.selectExpression) mkString ", "
  override def unaliasedExpression = Array(_1.unaliasedExpression, _2.unaliasedExpression, _3.unaliasedExpression, _4.unaliasedExpression, _5.unaliasedExpression, _6.unaliasedExpression, _7.unaliasedExpression, _8.unaliasedExpression, _9.unaliasedExpression, _10.unaliasedExpression, _11.unaliasedExpression, _12.unaliasedExpression, _13.unaliasedExpression) mkString ", "
  override def boundValues = _1.boundValues ++ _2.boundValues ++ _3.boundValues ++ _4.boundValues ++ _5.boundValues ++ _6.boundValues ++ _7.boundValues ++ _8.boundValues ++ _9.boundValues ++ _10.boundValues ++ _11.boundValues ++ _12.boundValues ++ _13.boundValues
}
private[sqins] class CompoundExpression14[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression, T12 <: Expression, T13 <: Expression, T14 <: Expression](override val _1: T1, override val _2: T2, override val _3: T3, override val _4: T4, override val _5: T5, override val _6: T6, override val _7: T7, override val _8: T8, override val _9: T9, override val _10: T10, override val _11: T11, override val _12: T12, override val _13: T13, override val _14: T14) extends Tuple14(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14) with Expression {
  override def expression = Array(_1.expression, _2.expression, _3.expression, _4.expression, _5.expression, _6.expression, _7.expression, _8.expression, _9.expression, _10.expression, _11.expression, _12.expression, _13.expression, _14.expression) mkString ", "
  override def selectExpression = Array(_1.selectExpression, _2.selectExpression, _3.selectExpression, _4.selectExpression, _5.selectExpression, _6.selectExpression, _7.selectExpression, _8.selectExpression, _9.selectExpression, _10.selectExpression, _11.selectExpression, _12.selectExpression, _13.selectExpression, _14.selectExpression) mkString ", "
  override def unaliasedExpression = Array(_1.unaliasedExpression, _2.unaliasedExpression, _3.unaliasedExpression, _4.unaliasedExpression, _5.unaliasedExpression, _6.unaliasedExpression, _7.unaliasedExpression, _8.unaliasedExpression, _9.unaliasedExpression, _10.unaliasedExpression, _11.unaliasedExpression, _12.unaliasedExpression, _13.unaliasedExpression, _14.unaliasedExpression) mkString ", "
  override def boundValues = _1.boundValues ++ _2.boundValues ++ _3.boundValues ++ _4.boundValues ++ _5.boundValues ++ _6.boundValues ++ _7.boundValues ++ _8.boundValues ++ _9.boundValues ++ _10.boundValues ++ _11.boundValues ++ _12.boundValues ++ _13.boundValues ++ _14.boundValues
}
private[sqins] class CompoundExpression15[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression, T12 <: Expression, T13 <: Expression, T14 <: Expression, T15 <: Expression](override val _1: T1, override val _2: T2, override val _3: T3, override val _4: T4, override val _5: T5, override val _6: T6, override val _7: T7, override val _8: T8, override val _9: T9, override val _10: T10, override val _11: T11, override val _12: T12, override val _13: T13, override val _14: T14, override val _15: T15) extends Tuple15(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15) with Expression {
  override def expression = Array(_1.expression, _2.expression, _3.expression, _4.expression, _5.expression, _6.expression, _7.expression, _8.expression, _9.expression, _10.expression, _11.expression, _12.expression, _13.expression, _14.expression, _15.expression) mkString ", "
  override def selectExpression = Array(_1.selectExpression, _2.selectExpression, _3.selectExpression, _4.selectExpression, _5.selectExpression, _6.selectExpression, _7.selectExpression, _8.selectExpression, _9.selectExpression, _10.selectExpression, _11.selectExpression, _12.selectExpression, _13.selectExpression, _14.selectExpression, _15.selectExpression) mkString ", "
  override def unaliasedExpression = Array(_1.unaliasedExpression, _2.unaliasedExpression, _3.unaliasedExpression, _4.unaliasedExpression, _5.unaliasedExpression, _6.unaliasedExpression, _7.unaliasedExpression, _8.unaliasedExpression, _9.unaliasedExpression, _10.unaliasedExpression, _11.unaliasedExpression, _12.unaliasedExpression, _13.unaliasedExpression, _14.unaliasedExpression, _15.unaliasedExpression) mkString ", "
  override def boundValues = _1.boundValues ++ _2.boundValues ++ _3.boundValues ++ _4.boundValues ++ _5.boundValues ++ _6.boundValues ++ _7.boundValues ++ _8.boundValues ++ _9.boundValues ++ _10.boundValues ++ _11.boundValues ++ _12.boundValues ++ _13.boundValues ++ _14.boundValues ++ _15.boundValues
}
private[sqins] class CompoundExpression16[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression, T12 <: Expression, T13 <: Expression, T14 <: Expression, T15 <: Expression, T16 <: Expression](override val _1: T1, override val _2: T2, override val _3: T3, override val _4: T4, override val _5: T5, override val _6: T6, override val _7: T7, override val _8: T8, override val _9: T9, override val _10: T10, override val _11: T11, override val _12: T12, override val _13: T13, override val _14: T14, override val _15: T15, override val _16: T16) extends Tuple16(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16) with Expression {
  override def expression = Array(_1.expression, _2.expression, _3.expression, _4.expression, _5.expression, _6.expression, _7.expression, _8.expression, _9.expression, _10.expression, _11.expression, _12.expression, _13.expression, _14.expression, _15.expression, _16.expression) mkString ", "
  override def selectExpression = Array(_1.selectExpression, _2.selectExpression, _3.selectExpression, _4.selectExpression, _5.selectExpression, _6.selectExpression, _7.selectExpression, _8.selectExpression, _9.selectExpression, _10.selectExpression, _11.selectExpression, _12.selectExpression, _13.selectExpression, _14.selectExpression, _15.selectExpression, _16.selectExpression) mkString ", "
  override def unaliasedExpression = Array(_1.unaliasedExpression, _2.unaliasedExpression, _3.unaliasedExpression, _4.unaliasedExpression, _5.unaliasedExpression, _6.unaliasedExpression, _7.unaliasedExpression, _8.unaliasedExpression, _9.unaliasedExpression, _10.unaliasedExpression, _11.unaliasedExpression, _12.unaliasedExpression, _13.unaliasedExpression, _14.unaliasedExpression, _15.unaliasedExpression, _16.unaliasedExpression) mkString ", "
  override def boundValues = _1.boundValues ++ _2.boundValues ++ _3.boundValues ++ _4.boundValues ++ _5.boundValues ++ _6.boundValues ++ _7.boundValues ++ _8.boundValues ++ _9.boundValues ++ _10.boundValues ++ _11.boundValues ++ _12.boundValues ++ _13.boundValues ++ _14.boundValues ++ _15.boundValues ++ _16.boundValues
}
private[sqins] class CompoundExpression17[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression, T12 <: Expression, T13 <: Expression, T14 <: Expression, T15 <: Expression, T16 <: Expression, T17 <: Expression](override val _1: T1, override val _2: T2, override val _3: T3, override val _4: T4, override val _5: T5, override val _6: T6, override val _7: T7, override val _8: T8, override val _9: T9, override val _10: T10, override val _11: T11, override val _12: T12, override val _13: T13, override val _14: T14, override val _15: T15, override val _16: T16, override val _17: T17) extends Tuple17(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17) with Expression {
  override def expression = Array(_1.expression, _2.expression, _3.expression, _4.expression, _5.expression, _6.expression, _7.expression, _8.expression, _9.expression, _10.expression, _11.expression, _12.expression, _13.expression, _14.expression, _15.expression, _16.expression, _17.expression) mkString ", "
  override def selectExpression = Array(_1.selectExpression, _2.selectExpression, _3.selectExpression, _4.selectExpression, _5.selectExpression, _6.selectExpression, _7.selectExpression, _8.selectExpression, _9.selectExpression, _10.selectExpression, _11.selectExpression, _12.selectExpression, _13.selectExpression, _14.selectExpression, _15.selectExpression, _16.selectExpression, _17.selectExpression) mkString ", "
  override def unaliasedExpression = Array(_1.unaliasedExpression, _2.unaliasedExpression, _3.unaliasedExpression, _4.unaliasedExpression, _5.unaliasedExpression, _6.unaliasedExpression, _7.unaliasedExpression, _8.unaliasedExpression, _9.unaliasedExpression, _10.unaliasedExpression, _11.unaliasedExpression, _12.unaliasedExpression, _13.unaliasedExpression, _14.unaliasedExpression, _15.unaliasedExpression, _16.unaliasedExpression, _17.unaliasedExpression) mkString ", "
  override def boundValues = _1.boundValues ++ _2.boundValues ++ _3.boundValues ++ _4.boundValues ++ _5.boundValues ++ _6.boundValues ++ _7.boundValues ++ _8.boundValues ++ _9.boundValues ++ _10.boundValues ++ _11.boundValues ++ _12.boundValues ++ _13.boundValues ++ _14.boundValues ++ _15.boundValues ++ _16.boundValues ++ _17.boundValues
}
private[sqins] class CompoundExpression18[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression, T12 <: Expression, T13 <: Expression, T14 <: Expression, T15 <: Expression, T16 <: Expression, T17 <: Expression, T18 <: Expression](override val _1: T1, override val _2: T2, override val _3: T3, override val _4: T4, override val _5: T5, override val _6: T6, override val _7: T7, override val _8: T8, override val _9: T9, override val _10: T10, override val _11: T11, override val _12: T12, override val _13: T13, override val _14: T14, override val _15: T15, override val _16: T16, override val _17: T17, override val _18: T18) extends Tuple18(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18) with Expression {
  override def expression = Array(_1.expression, _2.expression, _3.expression, _4.expression, _5.expression, _6.expression, _7.expression, _8.expression, _9.expression, _10.expression, _11.expression, _12.expression, _13.expression, _14.expression, _15.expression, _16.expression, _17.expression, _18.expression) mkString ", "
  override def selectExpression = Array(_1.selectExpression, _2.selectExpression, _3.selectExpression, _4.selectExpression, _5.selectExpression, _6.selectExpression, _7.selectExpression, _8.selectExpression, _9.selectExpression, _10.selectExpression, _11.selectExpression, _12.selectExpression, _13.selectExpression, _14.selectExpression, _15.selectExpression, _16.selectExpression, _17.selectExpression, _18.selectExpression) mkString ", "
  override def unaliasedExpression = Array(_1.unaliasedExpression, _2.unaliasedExpression, _3.unaliasedExpression, _4.unaliasedExpression, _5.unaliasedExpression, _6.unaliasedExpression, _7.unaliasedExpression, _8.unaliasedExpression, _9.unaliasedExpression, _10.unaliasedExpression, _11.unaliasedExpression, _12.unaliasedExpression, _13.unaliasedExpression, _14.unaliasedExpression, _15.unaliasedExpression, _16.unaliasedExpression, _17.unaliasedExpression, _18.unaliasedExpression) mkString ", "
  override def boundValues = _1.boundValues ++ _2.boundValues ++ _3.boundValues ++ _4.boundValues ++ _5.boundValues ++ _6.boundValues ++ _7.boundValues ++ _8.boundValues ++ _9.boundValues ++ _10.boundValues ++ _11.boundValues ++ _12.boundValues ++ _13.boundValues ++ _14.boundValues ++ _15.boundValues ++ _16.boundValues ++ _17.boundValues ++ _18.boundValues
}
private[sqins] class CompoundExpression19[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression, T12 <: Expression, T13 <: Expression, T14 <: Expression, T15 <: Expression, T16 <: Expression, T17 <: Expression, T18 <: Expression, T19 <: Expression](override val _1: T1, override val _2: T2, override val _3: T3, override val _4: T4, override val _5: T5, override val _6: T6, override val _7: T7, override val _8: T8, override val _9: T9, override val _10: T10, override val _11: T11, override val _12: T12, override val _13: T13, override val _14: T14, override val _15: T15, override val _16: T16, override val _17: T17, override val _18: T18, override val _19: T19) extends Tuple19(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19) with Expression {
  override def expression = Array(_1.expression, _2.expression, _3.expression, _4.expression, _5.expression, _6.expression, _7.expression, _8.expression, _9.expression, _10.expression, _11.expression, _12.expression, _13.expression, _14.expression, _15.expression, _16.expression, _17.expression, _18.expression, _19.expression) mkString ", "
  override def selectExpression = Array(_1.selectExpression, _2.selectExpression, _3.selectExpression, _4.selectExpression, _5.selectExpression, _6.selectExpression, _7.selectExpression, _8.selectExpression, _9.selectExpression, _10.selectExpression, _11.selectExpression, _12.selectExpression, _13.selectExpression, _14.selectExpression, _15.selectExpression, _16.selectExpression, _17.selectExpression, _18.selectExpression, _19.selectExpression) mkString ", "
  override def unaliasedExpression = Array(_1.unaliasedExpression, _2.unaliasedExpression, _3.unaliasedExpression, _4.unaliasedExpression, _5.unaliasedExpression, _6.unaliasedExpression, _7.unaliasedExpression, _8.unaliasedExpression, _9.unaliasedExpression, _10.unaliasedExpression, _11.unaliasedExpression, _12.unaliasedExpression, _13.unaliasedExpression, _14.unaliasedExpression, _15.unaliasedExpression, _16.unaliasedExpression, _17.unaliasedExpression, _18.unaliasedExpression, _19.unaliasedExpression) mkString ", "
  override def boundValues = _1.boundValues ++ _2.boundValues ++ _3.boundValues ++ _4.boundValues ++ _5.boundValues ++ _6.boundValues ++ _7.boundValues ++ _8.boundValues ++ _9.boundValues ++ _10.boundValues ++ _11.boundValues ++ _12.boundValues ++ _13.boundValues ++ _14.boundValues ++ _15.boundValues ++ _16.boundValues ++ _17.boundValues ++ _18.boundValues ++ _19.boundValues
}
private[sqins] class CompoundExpression20[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression, T12 <: Expression, T13 <: Expression, T14 <: Expression, T15 <: Expression, T16 <: Expression, T17 <: Expression, T18 <: Expression, T19 <: Expression, T20 <: Expression](override val _1: T1, override val _2: T2, override val _3: T3, override val _4: T4, override val _5: T5, override val _6: T6, override val _7: T7, override val _8: T8, override val _9: T9, override val _10: T10, override val _11: T11, override val _12: T12, override val _13: T13, override val _14: T14, override val _15: T15, override val _16: T16, override val _17: T17, override val _18: T18, override val _19: T19, override val _20: T20) extends Tuple20(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20) with Expression {
  override def expression = Array(_1.expression, _2.expression, _3.expression, _4.expression, _5.expression, _6.expression, _7.expression, _8.expression, _9.expression, _10.expression, _11.expression, _12.expression, _13.expression, _14.expression, _15.expression, _16.expression, _17.expression, _18.expression, _19.expression, _20.expression) mkString ", "
  override def selectExpression = Array(_1.selectExpression, _2.selectExpression, _3.selectExpression, _4.selectExpression, _5.selectExpression, _6.selectExpression, _7.selectExpression, _8.selectExpression, _9.selectExpression, _10.selectExpression, _11.selectExpression, _12.selectExpression, _13.selectExpression, _14.selectExpression, _15.selectExpression, _16.selectExpression, _17.selectExpression, _18.selectExpression, _19.selectExpression, _20.selectExpression) mkString ", "
  override def unaliasedExpression = Array(_1.unaliasedExpression, _2.unaliasedExpression, _3.unaliasedExpression, _4.unaliasedExpression, _5.unaliasedExpression, _6.unaliasedExpression, _7.unaliasedExpression, _8.unaliasedExpression, _9.unaliasedExpression, _10.unaliasedExpression, _11.unaliasedExpression, _12.unaliasedExpression, _13.unaliasedExpression, _14.unaliasedExpression, _15.unaliasedExpression, _16.unaliasedExpression, _17.unaliasedExpression, _18.unaliasedExpression, _19.unaliasedExpression, _20.unaliasedExpression) mkString ", "
  override def boundValues = _1.boundValues ++ _2.boundValues ++ _3.boundValues ++ _4.boundValues ++ _5.boundValues ++ _6.boundValues ++ _7.boundValues ++ _8.boundValues ++ _9.boundValues ++ _10.boundValues ++ _11.boundValues ++ _12.boundValues ++ _13.boundValues ++ _14.boundValues ++ _15.boundValues ++ _16.boundValues ++ _17.boundValues ++ _18.boundValues ++ _19.boundValues ++ _20.boundValues
}
private[sqins] class CompoundExpression21[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression, T12 <: Expression, T13 <: Expression, T14 <: Expression, T15 <: Expression, T16 <: Expression, T17 <: Expression, T18 <: Expression, T19 <: Expression, T20 <: Expression, T21 <: Expression](override val _1: T1, override val _2: T2, override val _3: T3, override val _4: T4, override val _5: T5, override val _6: T6, override val _7: T7, override val _8: T8, override val _9: T9, override val _10: T10, override val _11: T11, override val _12: T12, override val _13: T13, override val _14: T14, override val _15: T15, override val _16: T16, override val _17: T17, override val _18: T18, override val _19: T19, override val _20: T20, override val _21: T21) extends Tuple21(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21) with Expression {
  override def expression = Array(_1.expression, _2.expression, _3.expression, _4.expression, _5.expression, _6.expression, _7.expression, _8.expression, _9.expression, _10.expression, _11.expression, _12.expression, _13.expression, _14.expression, _15.expression, _16.expression, _17.expression, _18.expression, _19.expression, _20.expression, _21.expression) mkString ", "
  override def selectExpression = Array(_1.selectExpression, _2.selectExpression, _3.selectExpression, _4.selectExpression, _5.selectExpression, _6.selectExpression, _7.selectExpression, _8.selectExpression, _9.selectExpression, _10.selectExpression, _11.selectExpression, _12.selectExpression, _13.selectExpression, _14.selectExpression, _15.selectExpression, _16.selectExpression, _17.selectExpression, _18.selectExpression, _19.selectExpression, _20.selectExpression, _21.selectExpression) mkString ", "
  override def unaliasedExpression = Array(_1.unaliasedExpression, _2.unaliasedExpression, _3.unaliasedExpression, _4.unaliasedExpression, _5.unaliasedExpression, _6.unaliasedExpression, _7.unaliasedExpression, _8.unaliasedExpression, _9.unaliasedExpression, _10.unaliasedExpression, _11.unaliasedExpression, _12.unaliasedExpression, _13.unaliasedExpression, _14.unaliasedExpression, _15.unaliasedExpression, _16.unaliasedExpression, _17.unaliasedExpression, _18.unaliasedExpression, _19.unaliasedExpression, _20.unaliasedExpression, _21.unaliasedExpression) mkString ", "
  override def boundValues = _1.boundValues ++ _2.boundValues ++ _3.boundValues ++ _4.boundValues ++ _5.boundValues ++ _6.boundValues ++ _7.boundValues ++ _8.boundValues ++ _9.boundValues ++ _10.boundValues ++ _11.boundValues ++ _12.boundValues ++ _13.boundValues ++ _14.boundValues ++ _15.boundValues ++ _16.boundValues ++ _17.boundValues ++ _18.boundValues ++ _19.boundValues ++ _20.boundValues ++ _21.boundValues
}
private[sqins] class CompoundExpression22[T1 <: Expression, T2 <: Expression, T3 <: Expression, T4 <: Expression, T5 <: Expression, T6 <: Expression, T7 <: Expression, T8 <: Expression, T9 <: Expression, T10 <: Expression, T11 <: Expression, T12 <: Expression, T13 <: Expression, T14 <: Expression, T15 <: Expression, T16 <: Expression, T17 <: Expression, T18 <: Expression, T19 <: Expression, T20 <: Expression, T21 <: Expression, T22 <: Expression](override val _1: T1, override val _2: T2, override val _3: T3, override val _4: T4, override val _5: T5, override val _6: T6, override val _7: T7, override val _8: T8, override val _9: T9, override val _10: T10, override val _11: T11, override val _12: T12, override val _13: T13, override val _14: T14, override val _15: T15, override val _16: T16, override val _17: T17, override val _18: T18, override val _19: T19, override val _20: T20, override val _21: T21, override val _22: T22) extends Tuple22(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22) with Expression {
  override def expression = Array(_1.expression, _2.expression, _3.expression, _4.expression, _5.expression, _6.expression, _7.expression, _8.expression, _9.expression, _10.expression, _11.expression, _12.expression, _13.expression, _14.expression, _15.expression, _16.expression, _17.expression, _18.expression, _19.expression, _20.expression, _21.expression, _22.expression) mkString ", "
  override def selectExpression = Array(_1.selectExpression, _2.selectExpression, _3.selectExpression, _4.selectExpression, _5.selectExpression, _6.selectExpression, _7.selectExpression, _8.selectExpression, _9.selectExpression, _10.selectExpression, _11.selectExpression, _12.selectExpression, _13.selectExpression, _14.selectExpression, _15.selectExpression, _16.selectExpression, _17.selectExpression, _18.selectExpression, _19.selectExpression, _20.selectExpression, _21.selectExpression, _22.selectExpression) mkString ", "
  override def unaliasedExpression = Array(_1.unaliasedExpression, _2.unaliasedExpression, _3.unaliasedExpression, _4.unaliasedExpression, _5.unaliasedExpression, _6.unaliasedExpression, _7.unaliasedExpression, _8.unaliasedExpression, _9.unaliasedExpression, _10.unaliasedExpression, _11.unaliasedExpression, _12.unaliasedExpression, _13.unaliasedExpression, _14.unaliasedExpression, _15.unaliasedExpression, _16.unaliasedExpression, _17.unaliasedExpression, _18.unaliasedExpression, _19.unaliasedExpression, _20.unaliasedExpression, _21.unaliasedExpression, _22.unaliasedExpression) mkString ", "
  override def boundValues = _1.boundValues ++ _2.boundValues ++ _3.boundValues ++ _4.boundValues ++ _5.boundValues ++ _6.boundValues ++ _7.boundValues ++ _8.boundValues ++ _9.boundValues ++ _10.boundValues ++ _11.boundValues ++ _12.boundValues ++ _13.boundValues ++ _14.boundValues ++ _15.boundValues ++ _16.boundValues ++ _17.boundValues ++ _18.boundValues ++ _19.boundValues ++ _20.boundValues ++ _21.boundValues ++ _22.boundValues
}

private[sqins] class CompoundExtractableExpression2[T1, T2](override val _1: Extractable[T1] with Expression, override val _2: Extractable[T2] with Expression) extends CompoundExpression2(_1, _2) with Extractable[Tuple2[T1, T2]] {
  override def extract(rs: ResultSet, position: Int) = {
    val e = new CompoundExtractor()
    Extraction((e.doExtract(_1, rs, position), e.doExtract(_2, rs, position)), e.columnsRead)
  }
}
private[sqins] class CompoundExtractableExpression3[T1, T2, T3](override val _1: Extractable[T1] with Expression, override val _2: Extractable[T2] with Expression, override val _3: Extractable[T3] with Expression) extends CompoundExpression3(_1, _2, _3) with Extractable[Tuple3[T1, T2, T3]] {
  override def extract(rs: ResultSet, position: Int) = {
    val e = new CompoundExtractor()
    Extraction((e.doExtract(_1, rs, position), e.doExtract(_2, rs, position), e.doExtract(_3, rs, position)), e.columnsRead)
  }
}
private[sqins] class CompoundExtractableExpression4[T1, T2, T3, T4](override val _1: Extractable[T1] with Expression, override val _2: Extractable[T2] with Expression, override val _3: Extractable[T3] with Expression, override val _4: Extractable[T4] with Expression) extends CompoundExpression4(_1, _2, _3, _4) with Extractable[Tuple4[T1, T2, T3, T4]] {
  override def extract(rs: ResultSet, position: Int) = {
    val e = new CompoundExtractor()
    Extraction((e.doExtract(_1, rs, position), e.doExtract(_2, rs, position), e.doExtract(_3, rs, position), e.doExtract(_4, rs, position)), e.columnsRead)
  }
}
private[sqins] class CompoundExtractableExpression5[T1, T2, T3, T4, T5](override val _1: Extractable[T1] with Expression, override val _2: Extractable[T2] with Expression, override val _3: Extractable[T3] with Expression, override val _4: Extractable[T4] with Expression, override val _5: Extractable[T5] with Expression) extends CompoundExpression5(_1, _2, _3, _4, _5) with Extractable[Tuple5[T1, T2, T3, T4, T5]] {
  override def extract(rs: ResultSet, position: Int) = {
    val e = new CompoundExtractor()
    Extraction((e.doExtract(_1, rs, position), e.doExtract(_2, rs, position), e.doExtract(_3, rs, position), e.doExtract(_4, rs, position), e.doExtract(_5, rs, position)), e.columnsRead)
  }
}
private[sqins] class CompoundExtractableExpression6[T1, T2, T3, T4, T5, T6](override val _1: Extractable[T1] with Expression, override val _2: Extractable[T2] with Expression, override val _3: Extractable[T3] with Expression, override val _4: Extractable[T4] with Expression, override val _5: Extractable[T5] with Expression, override val _6: Extractable[T6] with Expression) extends CompoundExpression6(_1, _2, _3, _4, _5, _6) with Extractable[Tuple6[T1, T2, T3, T4, T5, T6]] {
  override def extract(rs: ResultSet, position: Int) = {
    val e = new CompoundExtractor()
    Extraction((e.doExtract(_1, rs, position), e.doExtract(_2, rs, position), e.doExtract(_3, rs, position), e.doExtract(_4, rs, position), e.doExtract(_5, rs, position), e.doExtract(_6, rs, position)), e.columnsRead)
  }
}
private[sqins] class CompoundExtractableExpression7[T1, T2, T3, T4, T5, T6, T7](override val _1: Extractable[T1] with Expression, override val _2: Extractable[T2] with Expression, override val _3: Extractable[T3] with Expression, override val _4: Extractable[T4] with Expression, override val _5: Extractable[T5] with Expression, override val _6: Extractable[T6] with Expression, override val _7: Extractable[T7] with Expression) extends CompoundExpression7(_1, _2, _3, _4, _5, _6, _7) with Extractable[Tuple7[T1, T2, T3, T4, T5, T6, T7]] {
  override def extract(rs: ResultSet, position: Int) = {
    val e = new CompoundExtractor()
    Extraction((e.doExtract(_1, rs, position), e.doExtract(_2, rs, position), e.doExtract(_3, rs, position), e.doExtract(_4, rs, position), e.doExtract(_5, rs, position), e.doExtract(_6, rs, position), e.doExtract(_7, rs, position)), e.columnsRead)
  }
}
private[sqins] class CompoundExtractableExpression8[T1, T2, T3, T4, T5, T6, T7, T8](override val _1: Extractable[T1] with Expression, override val _2: Extractable[T2] with Expression, override val _3: Extractable[T3] with Expression, override val _4: Extractable[T4] with Expression, override val _5: Extractable[T5] with Expression, override val _6: Extractable[T6] with Expression, override val _7: Extractable[T7] with Expression, override val _8: Extractable[T8] with Expression) extends CompoundExpression8(_1, _2, _3, _4, _5, _6, _7, _8) with Extractable[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]] {
  override def extract(rs: ResultSet, position: Int) = {
    val e = new CompoundExtractor()
    Extraction((e.doExtract(_1, rs, position), e.doExtract(_2, rs, position), e.doExtract(_3, rs, position), e.doExtract(_4, rs, position), e.doExtract(_5, rs, position), e.doExtract(_6, rs, position), e.doExtract(_7, rs, position), e.doExtract(_8, rs, position)), e.columnsRead)
  }
}
private[sqins] class CompoundExtractableExpression9[T1, T2, T3, T4, T5, T6, T7, T8, T9](override val _1: Extractable[T1] with Expression, override val _2: Extractable[T2] with Expression, override val _3: Extractable[T3] with Expression, override val _4: Extractable[T4] with Expression, override val _5: Extractable[T5] with Expression, override val _6: Extractable[T6] with Expression, override val _7: Extractable[T7] with Expression, override val _8: Extractable[T8] with Expression, override val _9: Extractable[T9] with Expression) extends CompoundExpression9(_1, _2, _3, _4, _5, _6, _7, _8, _9) with Extractable[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] {
  override def extract(rs: ResultSet, position: Int) = {
    val e = new CompoundExtractor()
    Extraction((e.doExtract(_1, rs, position), e.doExtract(_2, rs, position), e.doExtract(_3, rs, position), e.doExtract(_4, rs, position), e.doExtract(_5, rs, position), e.doExtract(_6, rs, position), e.doExtract(_7, rs, position), e.doExtract(_8, rs, position), e.doExtract(_9, rs, position)), e.columnsRead)
  }
}
private[sqins] class CompoundExtractableExpression10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](override val _1: Extractable[T1] with Expression, override val _2: Extractable[T2] with Expression, override val _3: Extractable[T3] with Expression, override val _4: Extractable[T4] with Expression, override val _5: Extractable[T5] with Expression, override val _6: Extractable[T6] with Expression, override val _7: Extractable[T7] with Expression, override val _8: Extractable[T8] with Expression, override val _9: Extractable[T9] with Expression, override val _10: Extractable[T10] with Expression) extends CompoundExpression10(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10) with Extractable[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] {
  override def extract(rs: ResultSet, position: Int) = {
    val e = new CompoundExtractor()
    Extraction((e.doExtract(_1, rs, position), e.doExtract(_2, rs, position), e.doExtract(_3, rs, position), e.doExtract(_4, rs, position), e.doExtract(_5, rs, position), e.doExtract(_6, rs, position), e.doExtract(_7, rs, position), e.doExtract(_8, rs, position), e.doExtract(_9, rs, position), e.doExtract(_10, rs, position)), e.columnsRead)
  }
}
private[sqins] class CompoundExtractableExpression11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](override val _1: Extractable[T1] with Expression, override val _2: Extractable[T2] with Expression, override val _3: Extractable[T3] with Expression, override val _4: Extractable[T4] with Expression, override val _5: Extractable[T5] with Expression, override val _6: Extractable[T6] with Expression, override val _7: Extractable[T7] with Expression, override val _8: Extractable[T8] with Expression, override val _9: Extractable[T9] with Expression, override val _10: Extractable[T10] with Expression, override val _11: Extractable[T11] with Expression) extends CompoundExpression11(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11) with Extractable[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] {
  override def extract(rs: ResultSet, position: Int) = {
    val e = new CompoundExtractor()
    Extraction((e.doExtract(_1, rs, position), e.doExtract(_2, rs, position), e.doExtract(_3, rs, position), e.doExtract(_4, rs, position), e.doExtract(_5, rs, position), e.doExtract(_6, rs, position), e.doExtract(_7, rs, position), e.doExtract(_8, rs, position), e.doExtract(_9, rs, position), e.doExtract(_10, rs, position), e.doExtract(_11, rs, position)), e.columnsRead)
  }
}
private[sqins] class CompoundExtractableExpression12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](override val _1: Extractable[T1] with Expression, override val _2: Extractable[T2] with Expression, override val _3: Extractable[T3] with Expression, override val _4: Extractable[T4] with Expression, override val _5: Extractable[T5] with Expression, override val _6: Extractable[T6] with Expression, override val _7: Extractable[T7] with Expression, override val _8: Extractable[T8] with Expression, override val _9: Extractable[T9] with Expression, override val _10: Extractable[T10] with Expression, override val _11: Extractable[T11] with Expression, override val _12: Extractable[T12] with Expression) extends CompoundExpression12(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12) with Extractable[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] {
  override def extract(rs: ResultSet, position: Int) = {
    val e = new CompoundExtractor()
    Extraction((e.doExtract(_1, rs, position), e.doExtract(_2, rs, position), e.doExtract(_3, rs, position), e.doExtract(_4, rs, position), e.doExtract(_5, rs, position), e.doExtract(_6, rs, position), e.doExtract(_7, rs, position), e.doExtract(_8, rs, position), e.doExtract(_9, rs, position), e.doExtract(_10, rs, position), e.doExtract(_11, rs, position), e.doExtract(_12, rs, position)), e.columnsRead)
  }
}
private[sqins] class CompoundExtractableExpression13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](override val _1: Extractable[T1] with Expression, override val _2: Extractable[T2] with Expression, override val _3: Extractable[T3] with Expression, override val _4: Extractable[T4] with Expression, override val _5: Extractable[T5] with Expression, override val _6: Extractable[T6] with Expression, override val _7: Extractable[T7] with Expression, override val _8: Extractable[T8] with Expression, override val _9: Extractable[T9] with Expression, override val _10: Extractable[T10] with Expression, override val _11: Extractable[T11] with Expression, override val _12: Extractable[T12] with Expression, override val _13: Extractable[T13] with Expression) extends CompoundExpression13(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13) with Extractable[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] {
  override def extract(rs: ResultSet, position: Int) = {
    val e = new CompoundExtractor()
    Extraction((e.doExtract(_1, rs, position), e.doExtract(_2, rs, position), e.doExtract(_3, rs, position), e.doExtract(_4, rs, position), e.doExtract(_5, rs, position), e.doExtract(_6, rs, position), e.doExtract(_7, rs, position), e.doExtract(_8, rs, position), e.doExtract(_9, rs, position), e.doExtract(_10, rs, position), e.doExtract(_11, rs, position), e.doExtract(_12, rs, position), e.doExtract(_13, rs, position)), e.columnsRead)
  }
}
private[sqins] class CompoundExtractableExpression14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](override val _1: Extractable[T1] with Expression, override val _2: Extractable[T2] with Expression, override val _3: Extractable[T3] with Expression, override val _4: Extractable[T4] with Expression, override val _5: Extractable[T5] with Expression, override val _6: Extractable[T6] with Expression, override val _7: Extractable[T7] with Expression, override val _8: Extractable[T8] with Expression, override val _9: Extractable[T9] with Expression, override val _10: Extractable[T10] with Expression, override val _11: Extractable[T11] with Expression, override val _12: Extractable[T12] with Expression, override val _13: Extractable[T13] with Expression, override val _14: Extractable[T14] with Expression) extends CompoundExpression14(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14) with Extractable[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] {
  override def extract(rs: ResultSet, position: Int) = {
    val e = new CompoundExtractor()
    Extraction((e.doExtract(_1, rs, position), e.doExtract(_2, rs, position), e.doExtract(_3, rs, position), e.doExtract(_4, rs, position), e.doExtract(_5, rs, position), e.doExtract(_6, rs, position), e.doExtract(_7, rs, position), e.doExtract(_8, rs, position), e.doExtract(_9, rs, position), e.doExtract(_10, rs, position), e.doExtract(_11, rs, position), e.doExtract(_12, rs, position), e.doExtract(_13, rs, position), e.doExtract(_14, rs, position)), e.columnsRead)
  }
}
private[sqins] class CompoundExtractableExpression15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](override val _1: Extractable[T1] with Expression, override val _2: Extractable[T2] with Expression, override val _3: Extractable[T3] with Expression, override val _4: Extractable[T4] with Expression, override val _5: Extractable[T5] with Expression, override val _6: Extractable[T6] with Expression, override val _7: Extractable[T7] with Expression, override val _8: Extractable[T8] with Expression, override val _9: Extractable[T9] with Expression, override val _10: Extractable[T10] with Expression, override val _11: Extractable[T11] with Expression, override val _12: Extractable[T12] with Expression, override val _13: Extractable[T13] with Expression, override val _14: Extractable[T14] with Expression, override val _15: Extractable[T15] with Expression) extends CompoundExpression15(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15) with Extractable[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]] {
  override def extract(rs: ResultSet, position: Int) = {
    val e = new CompoundExtractor()
    Extraction((e.doExtract(_1, rs, position), e.doExtract(_2, rs, position), e.doExtract(_3, rs, position), e.doExtract(_4, rs, position), e.doExtract(_5, rs, position), e.doExtract(_6, rs, position), e.doExtract(_7, rs, position), e.doExtract(_8, rs, position), e.doExtract(_9, rs, position), e.doExtract(_10, rs, position), e.doExtract(_11, rs, position), e.doExtract(_12, rs, position), e.doExtract(_13, rs, position), e.doExtract(_14, rs, position), e.doExtract(_15, rs, position)), e.columnsRead)
  }
}
private[sqins] class CompoundExtractableExpression16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](override val _1: Extractable[T1] with Expression, override val _2: Extractable[T2] with Expression, override val _3: Extractable[T3] with Expression, override val _4: Extractable[T4] with Expression, override val _5: Extractable[T5] with Expression, override val _6: Extractable[T6] with Expression, override val _7: Extractable[T7] with Expression, override val _8: Extractable[T8] with Expression, override val _9: Extractable[T9] with Expression, override val _10: Extractable[T10] with Expression, override val _11: Extractable[T11] with Expression, override val _12: Extractable[T12] with Expression, override val _13: Extractable[T13] with Expression, override val _14: Extractable[T14] with Expression, override val _15: Extractable[T15] with Expression, override val _16: Extractable[T16] with Expression) extends CompoundExpression16(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16) with Extractable[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]] {
  override def extract(rs: ResultSet, position: Int) = {
    val e = new CompoundExtractor()
    Extraction((e.doExtract(_1, rs, position), e.doExtract(_2, rs, position), e.doExtract(_3, rs, position), e.doExtract(_4, rs, position), e.doExtract(_5, rs, position), e.doExtract(_6, rs, position), e.doExtract(_7, rs, position), e.doExtract(_8, rs, position), e.doExtract(_9, rs, position), e.doExtract(_10, rs, position), e.doExtract(_11, rs, position), e.doExtract(_12, rs, position), e.doExtract(_13, rs, position), e.doExtract(_14, rs, position), e.doExtract(_15, rs, position), e.doExtract(_16, rs, position)), e.columnsRead)
  }
}
private[sqins] class CompoundExtractableExpression17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](override val _1: Extractable[T1] with Expression, override val _2: Extractable[T2] with Expression, override val _3: Extractable[T3] with Expression, override val _4: Extractable[T4] with Expression, override val _5: Extractable[T5] with Expression, override val _6: Extractable[T6] with Expression, override val _7: Extractable[T7] with Expression, override val _8: Extractable[T8] with Expression, override val _9: Extractable[T9] with Expression, override val _10: Extractable[T10] with Expression, override val _11: Extractable[T11] with Expression, override val _12: Extractable[T12] with Expression, override val _13: Extractable[T13] with Expression, override val _14: Extractable[T14] with Expression, override val _15: Extractable[T15] with Expression, override val _16: Extractable[T16] with Expression, override val _17: Extractable[T17] with Expression) extends CompoundExpression17(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17) with Extractable[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]] {
  override def extract(rs: ResultSet, position: Int) = {
    val e = new CompoundExtractor()
    Extraction((e.doExtract(_1, rs, position), e.doExtract(_2, rs, position), e.doExtract(_3, rs, position), e.doExtract(_4, rs, position), e.doExtract(_5, rs, position), e.doExtract(_6, rs, position), e.doExtract(_7, rs, position), e.doExtract(_8, rs, position), e.doExtract(_9, rs, position), e.doExtract(_10, rs, position), e.doExtract(_11, rs, position), e.doExtract(_12, rs, position), e.doExtract(_13, rs, position), e.doExtract(_14, rs, position), e.doExtract(_15, rs, position), e.doExtract(_16, rs, position), e.doExtract(_17, rs, position)), e.columnsRead)
  }
}
private[sqins] class CompoundExtractableExpression18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](override val _1: Extractable[T1] with Expression, override val _2: Extractable[T2] with Expression, override val _3: Extractable[T3] with Expression, override val _4: Extractable[T4] with Expression, override val _5: Extractable[T5] with Expression, override val _6: Extractable[T6] with Expression, override val _7: Extractable[T7] with Expression, override val _8: Extractable[T8] with Expression, override val _9: Extractable[T9] with Expression, override val _10: Extractable[T10] with Expression, override val _11: Extractable[T11] with Expression, override val _12: Extractable[T12] with Expression, override val _13: Extractable[T13] with Expression, override val _14: Extractable[T14] with Expression, override val _15: Extractable[T15] with Expression, override val _16: Extractable[T16] with Expression, override val _17: Extractable[T17] with Expression, override val _18: Extractable[T18] with Expression) extends CompoundExpression18(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18) with Extractable[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]] {
  override def extract(rs: ResultSet, position: Int) = {
    val e = new CompoundExtractor()
    Extraction((e.doExtract(_1, rs, position), e.doExtract(_2, rs, position), e.doExtract(_3, rs, position), e.doExtract(_4, rs, position), e.doExtract(_5, rs, position), e.doExtract(_6, rs, position), e.doExtract(_7, rs, position), e.doExtract(_8, rs, position), e.doExtract(_9, rs, position), e.doExtract(_10, rs, position), e.doExtract(_11, rs, position), e.doExtract(_12, rs, position), e.doExtract(_13, rs, position), e.doExtract(_14, rs, position), e.doExtract(_15, rs, position), e.doExtract(_16, rs, position), e.doExtract(_17, rs, position), e.doExtract(_18, rs, position)), e.columnsRead)
  }
}
private[sqins] class CompoundExtractableExpression19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](override val _1: Extractable[T1] with Expression, override val _2: Extractable[T2] with Expression, override val _3: Extractable[T3] with Expression, override val _4: Extractable[T4] with Expression, override val _5: Extractable[T5] with Expression, override val _6: Extractable[T6] with Expression, override val _7: Extractable[T7] with Expression, override val _8: Extractable[T8] with Expression, override val _9: Extractable[T9] with Expression, override val _10: Extractable[T10] with Expression, override val _11: Extractable[T11] with Expression, override val _12: Extractable[T12] with Expression, override val _13: Extractable[T13] with Expression, override val _14: Extractable[T14] with Expression, override val _15: Extractable[T15] with Expression, override val _16: Extractable[T16] with Expression, override val _17: Extractable[T17] with Expression, override val _18: Extractable[T18] with Expression, override val _19: Extractable[T19] with Expression) extends CompoundExpression19(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19) with Extractable[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]] {
  override def extract(rs: ResultSet, position: Int) = {
    val e = new CompoundExtractor()
    Extraction((e.doExtract(_1, rs, position), e.doExtract(_2, rs, position), e.doExtract(_3, rs, position), e.doExtract(_4, rs, position), e.doExtract(_5, rs, position), e.doExtract(_6, rs, position), e.doExtract(_7, rs, position), e.doExtract(_8, rs, position), e.doExtract(_9, rs, position), e.doExtract(_10, rs, position), e.doExtract(_11, rs, position), e.doExtract(_12, rs, position), e.doExtract(_13, rs, position), e.doExtract(_14, rs, position), e.doExtract(_15, rs, position), e.doExtract(_16, rs, position), e.doExtract(_17, rs, position), e.doExtract(_18, rs, position), e.doExtract(_19, rs, position)), e.columnsRead)
  }
}
private[sqins] class CompoundExtractableExpression20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](override val _1: Extractable[T1] with Expression, override val _2: Extractable[T2] with Expression, override val _3: Extractable[T3] with Expression, override val _4: Extractable[T4] with Expression, override val _5: Extractable[T5] with Expression, override val _6: Extractable[T6] with Expression, override val _7: Extractable[T7] with Expression, override val _8: Extractable[T8] with Expression, override val _9: Extractable[T9] with Expression, override val _10: Extractable[T10] with Expression, override val _11: Extractable[T11] with Expression, override val _12: Extractable[T12] with Expression, override val _13: Extractable[T13] with Expression, override val _14: Extractable[T14] with Expression, override val _15: Extractable[T15] with Expression, override val _16: Extractable[T16] with Expression, override val _17: Extractable[T17] with Expression, override val _18: Extractable[T18] with Expression, override val _19: Extractable[T19] with Expression, override val _20: Extractable[T20] with Expression) extends CompoundExpression20(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20) with Extractable[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]] {
  override def extract(rs: ResultSet, position: Int) = {
    val e = new CompoundExtractor()
    Extraction((e.doExtract(_1, rs, position), e.doExtract(_2, rs, position), e.doExtract(_3, rs, position), e.doExtract(_4, rs, position), e.doExtract(_5, rs, position), e.doExtract(_6, rs, position), e.doExtract(_7, rs, position), e.doExtract(_8, rs, position), e.doExtract(_9, rs, position), e.doExtract(_10, rs, position), e.doExtract(_11, rs, position), e.doExtract(_12, rs, position), e.doExtract(_13, rs, position), e.doExtract(_14, rs, position), e.doExtract(_15, rs, position), e.doExtract(_16, rs, position), e.doExtract(_17, rs, position), e.doExtract(_18, rs, position), e.doExtract(_19, rs, position), e.doExtract(_20, rs, position)), e.columnsRead)
  }
}
private[sqins] class CompoundExtractableExpression21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](override val _1: Extractable[T1] with Expression, override val _2: Extractable[T2] with Expression, override val _3: Extractable[T3] with Expression, override val _4: Extractable[T4] with Expression, override val _5: Extractable[T5] with Expression, override val _6: Extractable[T6] with Expression, override val _7: Extractable[T7] with Expression, override val _8: Extractable[T8] with Expression, override val _9: Extractable[T9] with Expression, override val _10: Extractable[T10] with Expression, override val _11: Extractable[T11] with Expression, override val _12: Extractable[T12] with Expression, override val _13: Extractable[T13] with Expression, override val _14: Extractable[T14] with Expression, override val _15: Extractable[T15] with Expression, override val _16: Extractable[T16] with Expression, override val _17: Extractable[T17] with Expression, override val _18: Extractable[T18] with Expression, override val _19: Extractable[T19] with Expression, override val _20: Extractable[T20] with Expression, override val _21: Extractable[T21] with Expression) extends CompoundExpression21(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21) with Extractable[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]] {
  override def extract(rs: ResultSet, position: Int) = {
    val e = new CompoundExtractor()
    Extraction((e.doExtract(_1, rs, position), e.doExtract(_2, rs, position), e.doExtract(_3, rs, position), e.doExtract(_4, rs, position), e.doExtract(_5, rs, position), e.doExtract(_6, rs, position), e.doExtract(_7, rs, position), e.doExtract(_8, rs, position), e.doExtract(_9, rs, position), e.doExtract(_10, rs, position), e.doExtract(_11, rs, position), e.doExtract(_12, rs, position), e.doExtract(_13, rs, position), e.doExtract(_14, rs, position), e.doExtract(_15, rs, position), e.doExtract(_16, rs, position), e.doExtract(_17, rs, position), e.doExtract(_18, rs, position), e.doExtract(_19, rs, position), e.doExtract(_20, rs, position), e.doExtract(_21, rs, position)), e.columnsRead)
  }
}
private[sqins] class CompoundExtractableExpression22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](override val _1: Extractable[T1] with Expression, override val _2: Extractable[T2] with Expression, override val _3: Extractable[T3] with Expression, override val _4: Extractable[T4] with Expression, override val _5: Extractable[T5] with Expression, override val _6: Extractable[T6] with Expression, override val _7: Extractable[T7] with Expression, override val _8: Extractable[T8] with Expression, override val _9: Extractable[T9] with Expression, override val _10: Extractable[T10] with Expression, override val _11: Extractable[T11] with Expression, override val _12: Extractable[T12] with Expression, override val _13: Extractable[T13] with Expression, override val _14: Extractable[T14] with Expression, override val _15: Extractable[T15] with Expression, override val _16: Extractable[T16] with Expression, override val _17: Extractable[T17] with Expression, override val _18: Extractable[T18] with Expression, override val _19: Extractable[T19] with Expression, override val _20: Extractable[T20] with Expression, override val _21: Extractable[T21] with Expression, override val _22: Extractable[T22] with Expression) extends CompoundExpression22(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22) with Extractable[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]] {
  override def extract(rs: ResultSet, position: Int) = {
    val e = new CompoundExtractor()
    Extraction((e.doExtract(_1, rs, position), e.doExtract(_2, rs, position), e.doExtract(_3, rs, position), e.doExtract(_4, rs, position), e.doExtract(_5, rs, position), e.doExtract(_6, rs, position), e.doExtract(_7, rs, position), e.doExtract(_8, rs, position), e.doExtract(_9, rs, position), e.doExtract(_10, rs, position), e.doExtract(_11, rs, position), e.doExtract(_12, rs, position), e.doExtract(_13, rs, position), e.doExtract(_14, rs, position), e.doExtract(_15, rs, position), e.doExtract(_16, rs, position), e.doExtract(_17, rs, position), e.doExtract(_18, rs, position), e.doExtract(_19, rs, position), e.doExtract(_20, rs, position), e.doExtract(_21, rs, position), e.doExtract(_22, rs, position)), e.columnsRead)
  }
}


/**
 * This class generates the CompoundExpressions and CompoundExtractableExpressions used in this file.
 */
object CompoundExpressionGenerator {
  def main(args: Array[String]) {
    System.out.println("");
    System.out.println("");
    System.out.println("");
    System.out.println("");
    for (depth <- 2 to 22) {
      buildCompoundExpression(depth)
    }
    System.out.println("");
    for (depth <- 2 to 22) {
      buildCompoundExtractableExpression(depth)
    }
    System.out.println("");
    System.out.println("");
    System.out.println("");
    System.out.println("");
  }
  
  def buildCompoundExpression(depth: Int) {
    val types = clauseAtDepth("T%1$s <: Expression", ", ", depth)
    val params = clauseAtDepth("override val _%1$s: T%1$s", ", ", depth)
    val tupleParams = clauseAtDepth("_%1$s", ", ", depth)
    val expression = clauseAtDepth("_%1$s.expression", ", ", depth)
    val selectExpression = clauseAtDepth("_%1$s.selectExpression", ", ", depth)
    val unaliasedExpression = clauseAtDepth("_%1$s.unaliasedExpression", ", ", depth)
    val boundValues = clauseAtDepth("_%1$s.boundValues", " ++ ", depth)
    
    val builder = new StringBuilder()
    builder.append("private[sqins] class CompoundExpression%1$s[%2$s](%3$s) extends Tuple%1$s(%4$s) with Expression {\n".format(depth, types, params, tupleParams))
    builder.append("  override def expression = Array(%1$s) mkString \", \"\n".format(expression))
    builder.append("  override def selectExpression = Array(%1$s) mkString \", \"\n".format(selectExpression))
    builder.append("  override def unaliasedExpression = Array(%1$s) mkString \", \"\n".format(unaliasedExpression))
    builder.append("  override def boundValues = %1$s\n".format(boundValues))
    builder.append("}")
    
    System.out.println(builder);
  }
  
  def buildCompoundExtractableExpression(depth: Int) {
    val types = clauseAtDepth("T%1$s", ", ", depth)
    val params = clauseAtDepth("override val _%1$s: Extractable[T%1$s] with Expression", ", ", depth)
    val tupleParams = clauseAtDepth("_%1$s", ", ", depth)
    val doExtract = clauseAtDepth("e.doExtract(_%1$s, rs, position)", ", ", depth)
    
    val builder = new StringBuilder()
    builder.append("private[sqins] class CompoundExtractableExpression%1$s[%2$s](%3$s) extends CompoundExpression%1$s(%4$s) with Extractable[Tuple%1$s[%2$s]] {\n".format(depth, types, params, tupleParams, types))
    builder.append("  override def extract(rs: ResultSet, position: Int) = {\n")
    builder.append("    val e = new CompoundExtractor()\n")
    builder.append("    Extraction((%1$s), e.columnsRead)\n".format(doExtract))
    builder.append("  }\n")
    builder.append("}")
    
    System.out.println(builder);
  }
  
  def clauseAtDepth(clause: String, joinedBy: String, depth: Int) = {
    var result = List[String]()
    for (i <- 1 to depth) {
      result = clause.format(i) :: result
    }
    result.reverse.mkString(joinedBy)
  }
}