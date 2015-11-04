package com.estus.distribution

import routine._

object logis {

  /** *
    *
    * Logistic Distribution
    *
    * d_ - probability density function
    * p_ - cumulative density function
    * q_ - inverse cumulative density function
    * r_ - random number from that distribution
    *
    */

  private def dlogis_internal(q: Double, location: Double, scale: Double, log_p: Boolean): Double = {
    (q, location, scale) match {
      case (a, b, c) if a.isNaN || b.isNaN || c.isNaN => Double.NaN
      case (a, b, c) if c <= 0.0 => throw new IllegalArgumentException
      case _ =>
        val x = math.abs((q-location)/scale)
        val e = math.exp(-x)
        val f = 1.0 + e
        if(log_p) -(x + math.log(scale * f * f)) else e / (scale * f * f)
    }
  }
  def dlogis(q: Double, location: Double, scale: Double, log_p: Boolean): Double = {
    dlogis_internal(q, location, scale, log_p)
  }
  def dlogis(q: List[Double], location: Double, scale: Double, log_p: Boolean): List[Double] = {
    if(q.isEmpty || scale <= 0) throw new IllegalArgumentException
    q.map(tup => dlogis_internal(tup, location, scale, log_p))
  }



  private def plogis_internal(q: Double, location: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    (q, location, scale) match {
      case (a, b, c) if a.isNaN || b.isNaN || c.isNaN => Double.NaN
      case (a, b, c) if c <= 0.0 => throw new IllegalArgumentException
      case _ =>
        val x = (q - location)/scale
        if(x.isNaN) throw new IllegalArgumentException
        val check = P_bounds_Inf_01(x, lower_tail, log_p)
        if(check.isNaN) Double.NaN
        if(log_p) {
          -log1pexp(if(lower_tail) -x else x)
        } else {
          1 / (1 + math.exp(if(lower_tail) -x else x))
        }
    }
  }
  def plogis(q: Double, location: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    plogis(q, location, scale, lower_tail, log_p)
  }
  def plogis(q: List[Double], location: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(q.isEmpty) throw new IllegalArgumentException
    q.map(tup => plogis_internal(tup, location, scale, lower_tail, log_p))
  }



  private def qlogis_internal(p: Double, location: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    (p, location, scale) match {
      case (a, b, c) if a.isNaN || b.isNaN || c.isNaN => Double.NaN
      case (a, b, c) if c < 0.0 => throw new IllegalArgumentException
      case (a, b, c) if c == 0.0 => location
      case _ =>
        var p_use = 0.0
        val check = Q_P01_boundaries(p, Double.PositiveInfinity, Double.NegativeInfinity, lower_tail, log_p)
        if(check.isNaN) Double.NaN
        if(log_p) {
          p_use = if(lower_tail) p - Log1_Exp(p) else Log1_Exp(p) - p
        } else {
          p_use = math.log(if(lower_tail) p / (1.0 - p) else (1.0 - p) / p)
        }
        location + scale * p_use
    }
  }
  def qlogis(p: Double, location: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    qlogis_internal(p, location, scale, lower_tail, log_p)
  }
  def qlogis(p: List[Double], location: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(p.isEmpty || scale < 0.0) throw new IllegalArgumentException
    p.map(tup => qlogis_internal(tup, location, scale, lower_tail, log_p))
  }



  def rlogis(n: Int, location: Double, scale: Double): List[Double] = {
    if(n < 1 || location.isNaN || scale.isInfinite || scale < 0.0) throw new IllegalArgumentException
    if(scale == 0.0 || location.isInfinite) {
      List.fill(n)(location)
    } else {
      unif.runif(n, 0.0, 1.0).map(tup => location + scale * math.log(tup / (1.0 - tup)))
    }
  }



}
