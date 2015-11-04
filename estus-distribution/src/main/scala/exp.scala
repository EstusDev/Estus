package com.estus.distribution

import routine._

object exp {

  /** *
    *
    * Exponential Distribution
    *
    * d_ - probability density function
    * p_ - cumulative density function
    * q_ - inverse cumulative density function
    * r_ - random number from that distribution
    *
    */

  private def dexp_internal(q: Double, scale: Double, log_p: Boolean): Double = {
    (q, scale) match {
      case (a, b) if a.isNaN || b.isNaN => Double.NaN
      case (a, b) if b <= 0.0 => throw new IllegalArgumentException
      case (a, b) if a < 0.0 => D__0(log_p)
      case _ =>
        if(log_p) (-q / scale) - math.log(scale) else math.exp(-q / scale) / scale
    }
  }
  def dexp(q: Double, rate: Double, log_p: Boolean): Double = {
    dexp_internal(q, 1/rate, log_p)
  }
  def dexp(q: List[Double], rate: Double, log_p: Boolean): List[Double] = {
    if(q.isEmpty) throw new IllegalArgumentException
    q.map(tup => dexp_internal(tup, 1/rate, log_p))
  }



  private def pexp_internal(q: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    (q, scale) match {
      case (a, b) if a.isNaN || b.isNaN => Double.NaN
      case (a, b) if b <= 0.0 => throw new IllegalArgumentException
      case (a, b) if a <= 0.0 => DT_0(lower_tail, log_p)
      case _ =>
        val x = -(q/scale)
        if(lower_tail){
          if(log_p) {
            if(x > -M_LN2) math.log(-math.expm1(x)) else math.log1p(-math.exp(x))
          } else {
            -math.expm1(x)
          }
        } else {
          D_exp(x, log_p)
        }
    }
  }
  def pexp(q: Double, rate: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    pexp_internal(q, 1/rate, lower_tail, log_p)
  }
  def pexp(q: List[Double], rate: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(q.isEmpty || 1/rate <= 0.0) throw new IllegalArgumentException
    q.map(tup => pexp_internal(tup, 1/rate, lower_tail, log_p))
  }



  private def qexp_internal(p: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    (p, scale) match {
      case (a, b) if a.isNaN || b.isNaN => Double.NaN
      case (a, b) if b < 0.0 => throw new IllegalArgumentException
      case _ =>
        Q_P01_check(p, log_p)
        if(p == DT_0(lower_tail, log_p)) 0.0 else - scale * DT_Clog(p, lower_tail, log_p)
    }
  }
  def qexp(p: Double, rate: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    qexp_internal(p, 1/rate, lower_tail, log_p)
  }
  def qexp(p: List[Double], rate: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(p.isEmpty || 1/rate < 0.0) throw new IllegalArgumentException
    p.map(tup => qexp_internal(tup, 1/rate, lower_tail, log_p))
  }



  def rexp(n: Int, rate: Double): List[Double] = {
    if(n < 1) throw new IllegalArgumentException
    if((1/rate).isInfinite || 1/rate <= 0.0) {
      if(1/rate == 0.0) List.fill(n)(0.0) else throw new IllegalArgumentException
    }
    qexp(unif.runif(n, 0, 1), rate, lower_tail = true, log_p = false)
  }



}
