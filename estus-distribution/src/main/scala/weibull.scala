package com.estus.distribution

import routine._

object weibull {

  /** *
    *
    * Weibull Distribution
    *
    * d_ - probability density function
    * p_ - cumulative density function
    * q_ - inverse cumulative density function
    * r_ - random number from that distribution
    *
    */

  private def dweibull_internal(q: Double, shape: Double, scale: Double, log_p: Boolean): Double = {
    (q, shape, scale) match {
      case (a, b, c) if a.isNaN || b.isNaN || c.isNaN => Double.NaN
      case (a, b, c) if b <= 0.0 || c <= 0.0 => throw new IllegalArgumentException
      case (a, b, c) if a < 0.0 || a.isInfinite => D__0(log_p)
      case (a, b, c) if a == 0.0 && b < 1 => Double.PositiveInfinity
      case _ =>
        val tmp1 = math.pow(q / scale, shape - 1)
        val tmp2 = tmp1 * (q / scale)
        if(log_p) -tmp2 + math.log(shape * tmp1 / scale) else shape * tmp1 * math.exp(-tmp2) / scale
    }
  }
  def dweibull(q: Double, shape: Double, scale: Double, log_p: Boolean): Double = {
    dweibull_internal(q, shape, scale, log_p)
  }
  def dweibull(q: List[Double], shape: Double, scale: Double, log_p: Boolean): List[Double] = {
    if(q.isEmpty || shape <= 0.0 || scale <= 0.0) throw new IllegalArgumentException
    q.map(tup => dweibull_internal(tup, shape, scale, log_p))
  }



  private def pweibull_internal(q: Double, shape: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    (q, shape, scale) match {
      case (a, b, c) if a.isNaN || b.isNaN || c.isNaN => Double.NaN
      case (a, b, c) if b <= 0.0 || c <= 0.0 => throw new IllegalArgumentException
      case (a, b, c) if a <= 0.0 => DT_0(lower_tail, log_p)
      case _ =>
        val x = -math.pow(q / scale, shape)
        if(lower_tail){
          if(log_p) Log1_Exp(x) else -math.expm1(x)
        } else {
          D_exp(x, log_p)
        }
    }
  }
  def pweibull(q: Double, shape: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    pweibull_internal(q, shape, scale, lower_tail, log_p)
  }
  def pweibull(q: List[Double], shape: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(q.isEmpty || shape <= 0.0 || scale <= 0.0) throw new IllegalArgumentException
    q.map(tup => pweibull_internal(tup, shape, scale, lower_tail, log_p))
  }



  private def qweibull_internal(p: Double, shape: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    (p, shape, scale) match {
      case (a, b, c) if a.isNaN || b.isNaN || c.isNaN => Double.NaN
      case (a, b, c) if b <= 0.0 || c <= 0.0 => throw new IllegalArgumentException
      case _ =>
        val check = Q_P01_boundaries(p, 0, Double.PositiveInfinity, lower_tail, log_p)
        if(check.isNaN) Double.NaN
        scale * math.pow(- DT_Clog(p, lower_tail, log_p), 1.0/shape)
    }
  }
  def qweibull(p: Double, shape: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    qweibull_internal(p, shape, scale, lower_tail, log_p)
  }
  def qweibull(p: List[Double], shape: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(p.isEmpty || shape <= 0.0 || scale <= 0.0) throw new IllegalArgumentException
    p.map(tup => qweibull_internal(tup, shape, scale, lower_tail, log_p))
  }



  def rweibull(n: Int, shape: Double, scale: Double): List[Double] = {
    if(n < 1) throw new IllegalArgumentException
    if(shape.isInfinite || scale.isInfinite || shape <= 0.0 || scale <= 0.0) {
      if(scale == 0.0) 0.0 else throw new IllegalArgumentException
    }
    unif.runif(n, 0.0, 1.0).map(tup => scale * math.pow(-math.log(tup), 1.0 / shape))
  }



}
