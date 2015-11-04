package com.estus.distribution

import routine._
import rng._

object unif {

  /** *
    *
    * Uniform Distribution
    *
    * d_ - probability density function
    * p_ - cumulative density function
    * q_ - inverse cumulative density function
    * r_ - random number from that distribution
    *
    */

  def dunif(q: Double, min: Double, max: Double, log_p: Boolean): Double = {
    (q, min, max) match {
      case (x, a, b) if x.isNaN || a.isNaN || b.isNaN => Double.NaN
      case (x, a, b) if b <= a || a.isInfinite || b.isInfinity => throw new IllegalArgumentException
      case (x, a, b) if a <= x && x <= b => if(log_p) -math.log(b - a) else 1/(b - a)
      case _ => D__0(log_p)
    }
  }
  def dunif(q: List[Double], min: Double, max: Double, log_p: Boolean): List[Double] = {
    if(q.isEmpty) throw new IllegalArgumentException
    q.map(tup => dunif(tup, min, max, log_p))
  }



  def punif(q: Double, min: Double, max: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    (q, min, max) match {
      case (x, a, b) if x.isNaN || a.isNaN || b.isNaN => Double.NaN
      case (x, a, b) if b < a || a.isInfinite || b.isInfinity => throw new IllegalArgumentException
      case (x, a, b) if x >= b => DT_1(lower_tail, log_p)
      case (x, a, b) if x <= a => DT_0(lower_tail, log_p)
      case _ => if(lower_tail) D_val((q-min)/(max-min), log_p) else D_val((max-q)/(max-min), log_p)
    }
  }
  def punif(q: List[Double], min: Double, max: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(q.isEmpty) throw new IllegalArgumentException
    q.map(tup => punif(tup, min, max, lower_tail, log_p))
  }



  def qunif(p: Double, min: Double, max: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    Q_P01_check(p, log_p)
    (p, min, max) match {
      case (x, a, b) if x.isNaN || a.isNaN || b.isNaN => Double.NaN
      case (x, a, b) if b < a || a.isInfinite || b.isInfinity => throw new IllegalArgumentException
      case (x, a, b) if b == a => a
      case _ => min + DT_qIv(p, lower_tail, log_p) * (max - min)
    }
  }
  def qunif(p: List[Double], min: Double, max: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(p.isEmpty) throw new IllegalArgumentException
    p.map(tup => qunif(tup, min, max, lower_tail, log_p))
  }



  def runif(n: Int, min: Double, max: Double): List[Double] = {
    if(min.isNaN || max.isNaN) return List(Double.NaN)
    if(n < 1 || max < min || min.isInfinite || max.isInfinite) throw new IllegalArgumentException
    if(min == max) List(min) else List.fill(n)(min + (max - min) * rng.nextDouble)
  }



}
