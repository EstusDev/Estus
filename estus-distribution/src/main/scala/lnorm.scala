package com.estus.distribution

import routine._

object lnorm {

  /** *
    *
    * Log Normal Distribution
    *
    * d_ - probability density function
    * p_ - cumulative density function
    * q_ - inverse cumulative density function
    * r_ - random number from that distribution
    *
    */

  def dlnorm(q: Double, meanlog: Double, sdlog: Double, log_p: Boolean): Double = {
    (q, meanlog, sdlog) match {
      case (a, b, c) if a.isNaN || b.isNaN || c.isNaN => Double.NaN
      case (a, b, c) if c <= 0 =>
        if(c < 0) {
          throw new IllegalArgumentException
        } else {
          if(math.log(q) == meanlog) Double.PositiveInfinity else D__0(log_p)
        }
      case (a, b, c) if a <= 0 => D__0(log_p)
      case _ =>
        val y = (math.log(q) - meanlog) / sdlog
        if(log_p) {
          -(M_LN_SQRT_2PI + 0.5 * y * y + math.log(q * sdlog))
        } else {
          M_1_SQRT_2PI * math.exp(-0.5 * y * y) / (q * sdlog)
        }
    }
  }
  def dlnorm(q: List[Double], meanlog: Double, sdlog: Double, log_p: Boolean): List[Double] = {
    if(q.isEmpty) throw new IllegalArgumentException
    q.map(tup => dlnorm(tup, meanlog, sdlog, log_p))
  }



  def plnorm(q: Double, meanlog: Double, sdlog: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    (q, meanlog, sdlog) match {
      case (a, b, c) if a.isNaN || b.isNaN || c.isNaN => Double.NaN
      case (a, b, c) if c < 0 => throw new IllegalArgumentException
      case (a, b, c) if a <= 0 => DT_0(lower_tail, log_p)
      case _ => norm.pnorm(math.log(q), meanlog, sdlog, lower_tail, log_p)
    }
  }
  def plnorm(q: List[Double], meanlog: Double, sdlog: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(q.isEmpty) throw new IllegalArgumentException
    q.map(tup => plnorm(tup, meanlog, sdlog, lower_tail, log_p))
  }



  def qlnorm(p: Double, meanlog: Double, sdlog: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    (p, meanlog, sdlog) match {
      case (a, b, c) if a.isNaN || b.isNaN || c.isNaN => Double.NaN
      case _ =>
        val Q_P01 = Q_P01_boundaries(p, 0, Double.PositiveInfinity, lower_tail, log_p)
        if(Q_P01.isNaN) Q_P01
        math.exp(norm.qnorm(p, meanlog, sdlog, lower_tail, log_p))
    }
  }
  def qlnorm(p: List[Double], meanlog: Double, sdlog: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(p.isEmpty) throw new IllegalArgumentException
    p.map(tup => qlnorm(tup, meanlog, sdlog, lower_tail, log_p))
  }



  def rlnorm(n: Int, meanlog: Double, sdlog: Double): List[Double] = {
    if(n < 1 || meanlog.isNaN || sdlog.isInfinite || sdlog < 0.0) throw new IllegalArgumentException
    qlnorm(unif.runif(n, 0.0, 1.0), meanlog, sdlog, lower_tail = true, log_p = false)
  }



}
