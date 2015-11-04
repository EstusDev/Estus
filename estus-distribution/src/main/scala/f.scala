package com.estus.distribution

import routine._

import org.apache.commons.math3.distribution.FDistribution

object f {

  /** *
    *
    * F Distribution
    *
    * d_ - probability density function
    * p_ - cumulative density function
    * q_ - inverse cumulative density function
    * r_ - random number from that distribution
    *
    */

  private def df_internal(q: Double, log_p: Boolean, dist: FDistribution): Double = {
    (dist.getNumeratorDegreesOfFreedom, dist.getDenominatorDegreesOfFreedom) match {
      case (a, b) if a.isNaN || b.isNaN || q.isNaN => Double.NaN
      case _ =>
        if(log_p) dist.logDensity(q) else dist.density(q)
    }
  }
  def df(q: Double, df1: Double, df2: Double, log_p: Boolean): Double = {
    val dist = new FDistribution(df1, df2)
    df_internal(q, log_p, dist)
  }
  def df(q: List[Double], df1: Double, df2: Double, log_p: Boolean): List[Double] = {
    if(q.isEmpty) throw new IllegalArgumentException
    val dist = new FDistribution(df1, df2)
    q.map(tup => df_internal(tup, log_p, dist))
  }



  private def pf_internal(q: Double, lower_tail: Boolean, log_p: Boolean, dist: FDistribution): Double = {
    val a = dist.getNumeratorDegreesOfFreedom
    val b = dist.getDenominatorDegreesOfFreedom
    if(q.isNaN || a.isNaN || b.isNaN) throw new IllegalArgumentException
    val cumprob = dist.cumulativeProbability(q)
    (lower_tail, log_p) match {
      case (true, false) => cumprob
      case (true, true) => math.log(cumprob)
      case (false, false) => 1 - cumprob
      case (false, true) => math.log(1 - cumprob)
    }
  }
  def pf(q: Double, df1: Double, df2: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    val dist = new FDistribution(df1, df2)
    pf_internal(q, lower_tail, log_p, dist)
  }
  def pf(q: List[Double], df1: Double, df2: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(q.isEmpty) throw new IllegalArgumentException
    val dist = new FDistribution(df1, df2)
    q.map(tup => pf_internal(tup, lower_tail, log_p, dist))
  }



  private def qf_internal(p: Double, lower_tail: Boolean, log_p: Boolean, dist: FDistribution): Double = {
    val p_tmp = (lower_tail, log_p) match {
      case (true, false) => p
      case (true, true) => math.exp(p)
      case (false, false) => 1 - p
      case (false, true) => 1 - math.exp(p)
    }
    dist.inverseCumulativeProbability(p_tmp)
  }
  def qf(p: Double, df1: Double, df2: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    val dist = new FDistribution(df1, df2)
    qf_internal(p, lower_tail, log_p, dist)
  }
  def qf(p: List[Double], df1: Double, df2: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(p.isEmpty) throw new IllegalArgumentException
    val dist = new FDistribution(df1, df2)
    p.map(tup => qf_internal(tup, lower_tail, log_p, dist))
  }



  def rf(n: Int, df1: Double, df2: Double): List[Double] = {
    if(n < 1 || (df1 + df2).isNaN) throw new IllegalArgumentException
    val dist = new FDistribution(df1, df2)
    List.fill(n)(dist.sample)
  }



}
