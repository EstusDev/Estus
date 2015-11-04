package com.estus.distribution

import routine._

object chisq {

  /** *
    *
    * Chi-Squared Distribution
    *
    * d_ - probability density function
    * p_ - cumulative density function
    * q_ - inverse cumulative density function
    * r_ - random number from that distribution
    *
    */

  private def dchisq_internal(q: Double, df: Double, log_p: Boolean): Double = {
    gamma.dgamma(q, df/2.0, 2.0, log_p)
  }
  def dchisq(q: Double, df: Double, log_p: Boolean): Double = {
    dchisq_internal(q, df, log_p)
  }
  def dchisq(q: List[Double], df: Double, log_p: Boolean): List[Double] = {
    if(q.isEmpty || df <= 0) throw new IllegalArgumentException
    q.map(tup => dchisq_internal(tup, df, log_p))
  }



  private def pchisq_internal(q: Double, df: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    gamma.pgamma(q, df/2.0, 2.0, lower_tail, log_p)
  }
  def pchisq(q: Double, df: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    pchisq_internal(q, df, lower_tail, log_p)
  }
  def pchisq(q: List[Double], df: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(q.isEmpty || df <= 0) throw new IllegalArgumentException
    q.map(tup => pchisq_internal(tup, df, lower_tail, log_p))
  }



  private def qchisq_internal(p: Double, df: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    gamma.qgamma(p, 0.5 * df, 2.0, lower_tail, log_p)
  }
  def qchisq(p: Double, df: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    qchisq_internal(p, df, lower_tail, log_p)
  }
  def qchisq(p: List[Double], df: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(p.isEmpty || df <= 0) throw new IllegalArgumentException
    p.map(tup => qchisq_internal(tup, df, lower_tail, log_p))
  }



  def rchisq(n: Int, df: Double): List[Double] = {
    if(n < 1 || df.isInfinite || df < 0.0) throw new IllegalArgumentException
    gamma.rgamma(n, df/2.0, 2.0)
  }



}
