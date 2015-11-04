package com.estus.distribution

import routine._

object cauchy {

  /** *
    *
    * Cauchy Distribution
    *
    * d_ - probability density function
    * p_ - cumulative density function
    * q_ - inverse cumulative density function
    * r_ - random number from that distribution
    *
    */

  private def dcauchy_internal(q: Double, location: Double, scale: Double, log_p: Boolean): Double = {
    (q, location, scale) match {
      case (a, b, c) if a.isNaN || b.isNaN || c.isNaN => Double.NaN
      case (a, b, c) if c <= 0 => throw new IllegalArgumentException
      case _ =>
        val y = (q - location)/scale
        if(log_p) -math.log(M_PI * scale * (1.0 + y * y)) else 1.0/(M_PI * scale * (1.0 + y * y))
    }
  }
  def dcauchy(q: Double, location: Double, scale: Double, log_p: Boolean): Double = {
    dcauchy_internal(q, location, scale, log_p)
  }
  def dcauchy(q: List[Double], location: Double, scale: Double, log_p: Boolean): List[Double] = {
    if(q.isEmpty || scale <= 0) throw new IllegalArgumentException
    q.map(tup => dcauchy_internal(tup, location, scale, log_p))
  }



  private def pcauchy_internal(q: Double, location: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    (q, location, scale) match {
      case (a, b, c) if a.isNaN || b.isNaN || c.isNaN => Double.NaN
      case (a, b, c) if c <= 0 => throw new IllegalAccessException
      case  _ =>
        var x = (q-location)/scale
        x match {
          case a if a.isNaN => throw new IllegalAccessException
          case a if a.isInfinite => if(a < 0) DT_0(lower_tail, log_p) else DT_1(lower_tail, log_p)
          case _ =>
            if(!lower_tail) x = -x
            if (math.abs(x) > 1) {
              val y = math.atan(1/x) / M_PI
              if (x > 0) D_Clog(y, log_p) else D_val(-y, log_p)
            } else {
              D_val(0.5 + math.atan(x) / M_PI, log_p)
            }
        }
    }
  }
  def pcauchy(q: Double, location: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    pcauchy_internal(q, location, scale, lower_tail, log_p)
  }
  def pcauchy(q: List[Double], location: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(q.isEmpty || scale <= 0) throw new IllegalArgumentException
    q.map(tup => pcauchy_internal(tup, location, scale, lower_tail, log_p))
  }



  private def qcauchy_internal(p: Double, location: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    if(p.isNaN || location.isNaN || scale.isNaN) Double.NaN
    Q_P01_check(p, log_p)
    if(scale <= 0 || scale.isInfinite)  if(scale == 0) location else throw new IllegalArgumentException
    val my_INF = location + (if(lower_tail) scale else -scale) * Double.PositiveInfinity
    var p_use = p
    var lower_tail_use = lower_tail
    if(log_p) {
      if(p_use > -1) {
        if(p_use == 0.0) {
          my_INF
        } else {
          lower_tail_use = !lower_tail_use
          p_use = -math.expm1(p_use)
        }
      } else {
        p_use = math.exp(p_use)
      }
    } else {
      if(p_use > 0.5) {
        if(p_use == 1.0) {
          my_INF
        } else {
          p_use = 1 - p_use
          lower_tail_use = !lower_tail_use
        }
      }
    }
    if(p_use == 0.5) location
    if(p_use == 0.0) location + (if(lower_tail_use) scale else -scale) * Double.NegativeInfinity
    location + (if(lower_tail_use) -scale else scale) / tanpi(p_use)
  }
  def qcauchy(p: Double, location: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    qcauchy_internal(p, location, scale, lower_tail, log_p)
  }
  def qcauchy(p: List[Double], location: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(p.isEmpty || scale <= 0) throw new IllegalArgumentException
    p.map(tup => qcauchy_internal(tup, location, scale, lower_tail, log_p))
  }



  def rcauchy(n: Int, location: Double, scale: Double): List[Double] = {
    if(n < 1 || scale <= 0.0) throw new IllegalArgumentException
    qcauchy(unif.runif(n, 0.0, 1.0), location, scale, lower_tail = true, log_p = false)
  }



}
