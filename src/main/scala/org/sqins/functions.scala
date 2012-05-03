package org.sqins

import Implicits._

object SQL {
  def AVG[T](param: Value[T])(implicit typeMapping: TypeMapping[T]) = "AVG".call(param)(typeMapping)
  
  def MAX[T](param: Value[T])(implicit typeMapping: TypeMapping[T]) = "MAX".call(param)(typeMapping)
}