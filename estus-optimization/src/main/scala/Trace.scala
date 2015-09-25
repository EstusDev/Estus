package com.estus.optimization

import scala.math._



case class Trace () {

  private var trace = List.empty[Double]

  def size: Int = trace.size

  def add (node: Double): Unit = trace = (node :: trace).sorted take 2

  def contains (value: Double): Boolean = trace.contains(value)

  def min: Double = trace.min

  def converged(tolRel: Double): Option[Boolean] = {
    if (trace.size == 2)
      Some((trace.max - trace.min) < (tolRel * (abs(trace.max) + tolRel)))
    else
      None
  }

}
