package com.estus.distribution

import routine._
import org.apache.commons.math3.distribution.BetaDistribution

object beta {

  /** *
    *
    * Beta Distribution
    *
    * d_ - probability density function
    * p_ - cumulative density function
    * q_ - inverse cumulative density function
    * r_ - random number from that distribution
    *
    */

  private def dbeta_internal(q: Double, log_p: Boolean, dist: BetaDistribution): Double = {
    (dist.getAlpha, dist.getBeta) match {
      case (a, b) if a.isNaN || b.isNaN || q.isNaN => Double.NaN
      case (a, b) if a < 0 || b < 0 => throw new IllegalArgumentException
      case (a, b) if q < 0 || q > 1 => D__0(log_p)
      case (a, b) if a == 0 || b == 0 || a.isInfinite || b.isInfinite =>
        if(a == 0 && b == 0) {if (q == 0 || q == 1) return Double.PositiveInfinity else D__0(log_p)}
        if(a == 0 || a/b == 0) {if (q == 0) return Double.PositiveInfinity else D__0(log_p)}
        if(b == 0 || b/a == 0) {if (q == 1) return Double.PositiveInfinity else D__0(log_p)}
        if(q == 0.5) Double.PositiveInfinity else D__0(log_p)
      case _ =>
        val (a, b) = (dist.getAlpha, dist.getBeta)
        if(q == 0) {
          if(a > 1) D__0(log_p)
          if(a < 1) return Double.PositiveInfinity
          D_val(b, log_p)
        }
        if(q == 1) {
          if(b > 1) D__0(log_p)
          if(b < 1) return Double.PositiveInfinity
          D_val(a, log_p)
        }
        if(log_p) dist.logDensity(q) else dist.density(q)
    }
  }
  def dbeta(q: Double, a: Double, b: Double, log_p: Boolean): Double = {
    val dist = new BetaDistribution(a, b)
    dbeta_internal(q, log_p, dist)
  }
  def dbeta(q: List[Double], a: Double, b: Double, log_p: Boolean): List[Double] = {
    if(q.isEmpty) throw new IllegalArgumentException
    val dist = new BetaDistribution(a, b)
    q.map(tup => dbeta_internal(tup, log_p, dist))
  }



  private def pbeta_internal(q: Double, lower_tail: Boolean, log_p: Boolean, dist: BetaDistribution): Double = {
    val a = dist.getAlpha
    val b = dist.getBeta
    if(q.isNaN || a.isNaN || b.isNaN) throw new IllegalArgumentException
    if(q <= 0) DT_0(lower_tail, log_p)
    if(q >= 1) DT_1(lower_tail, log_p)
    val cumprob = dist.cumulativeProbability(q)
    (lower_tail, log_p) match {
      case (true, false) => cumprob
      case (true, true) => math.log(cumprob)
      case (false, false) => 1 - cumprob
      case (false, true) => math.log(1 - cumprob)
    }
  }
  def pbeta(q: Double, a: Double, b: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    val dist = new BetaDistribution(a, b)
    pbeta_internal(q, lower_tail, log_p, dist)
  }
  def pbeta(q: List[Double], a: Double, b: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(q.isEmpty) throw new IllegalArgumentException
    val dist = new BetaDistribution(a, b)
    q.map(tup => pbeta_internal(tup, lower_tail, log_p, dist))
  }


  private def qbeta_internal(p: Double, lower_tail: Boolean, log_p: Boolean, dist: BetaDistribution): Double = {
    val p_tmp = (lower_tail, log_p) match {
      case (true, false) => p
      case (true, true) => math.exp(p)
      case (false, false) => 1 - p
      case (false, true) => 1 - math.exp(p)
    }
    dist.inverseCumulativeProbability(p_tmp)
  }
  def qbeta(p: Double, a: Double, b: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    val dist = new BetaDistribution(a, b)
    qbeta_internal(p, lower_tail, log_p, dist)
  }
  def qbeta(p: List[Double], a: Double, b: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(p.isEmpty) throw new IllegalArgumentException
    val dist = new BetaDistribution(a, b)
    p.map(tup => qbeta_internal(tup, lower_tail, log_p, dist))
  }



  def rbeta(n: Int, a: Double, b: Double): List[Double] = {
    if(n < 1 || (a+b).isNaN || a < 0 || b < 0) throw new IllegalArgumentException
    val dist = new BetaDistribution(a, b)
    List.fill(n)(dist.sample)
  }



}
