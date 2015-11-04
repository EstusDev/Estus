package com.estus.distribution

import routine._
import org.apache.commons.math3.distribution.GammaDistribution

object gamma {

  /** *
    *
    * Gamma Distribution
    *
    * d_ - probability density function
    * p_ - cumulative density function
    * q_ - inverse cumulative density function
    * r_ - random number from that distribution
    *
    */

  private def dgamma_internal(q: Double, log_p: Boolean, dist: GammaDistribution): Double = {
    (q, dist.getShape, dist.getScale) match {
      case (a, b, c) if a.isNaN || b.isNaN || c.isNaN => Double.NaN
      case (a, b, c) if b < 0.0 || c <= 0.0 => throw new IllegalArgumentException
      case (a, b, c) if a < 0.0 => D__0(log_p)
      case (a, b, c) if b == 0.0 => if(q == 0) Double.PositiveInfinity else D__0(log_p)
      case (a, b, c) if a == 0.0 =>
        if(b < 1) {
          Double.PositiveInfinity
        } else if (b > 1.0) {
          D__0(log_p)
        } else {
          if(log_p) -math.log(c) else 1.0/c
        }
      case _ =>
        if(log_p) dist.logDensity(q) else dist.density(q)
    }
  }
  def dgamma(q: Double, shape: Double, scale: Double, log_p: Boolean): Double = {
    val dist = new GammaDistribution(shape, scale)
    dgamma_internal(q, log_p, dist)
  }
  def dgamma(q: List[Double], shape: Double, scale: Double, log_p: Boolean): List[Double] = {
    if(q.isEmpty) throw new IllegalArgumentException
    val dist = new GammaDistribution(shape, scale)
    q.map(tup => dgamma_internal(tup, log_p, dist))
  }



  private def pgamma_internal(q: Double, lower_tail: Boolean, log_p: Boolean, dist: GammaDistribution): Double = {
    val shape = dist.getShape
    val scale = dist.getScale
    if(q.isNaN || shape.isNaN || scale.isNaN) throw new IllegalArgumentException
    if(shape < 0.0 || scale <= 0.0) throw new IllegalArgumentException
    if(shape == 0.0) if(q/scale <= 0.0) DT_0(lower_tail, log_p) else DT_1(lower_tail, log_p)
    val cumprob = dist.cumulativeProbability(q)
    (lower_tail, log_p) match {
      case (true, false) => cumprob
      case (true, true) => math.log(cumprob)
      case (false, false) => 1 - cumprob
      case (false, true) => math.log(1 - cumprob)
    }
  }
  def pgamma(q: Double, shape: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    val dist = new GammaDistribution(shape, scale)
    pgamma_internal(q, lower_tail, log_p, dist)
  }
  def pgamma(q: List[Double], shape: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(q.isEmpty) throw new IllegalArgumentException
    val dist = new GammaDistribution(shape, scale)
    q.map(tup => pgamma_internal(tup, lower_tail, log_p, dist))
  }



  private def qgamma_internal(p: Double, lower_tail: Boolean, log_p: Boolean, dist: GammaDistribution): Double = {
    val p_tmp = (lower_tail, log_p) match {
      case (true, false) => p
      case (true, true) => math.exp(p)
      case (false, false) => 1 - p
      case (false, true) => 1 - math.exp(p)
    }
    dist.inverseCumulativeProbability(p_tmp)
  }
  def qgamma(p: Double, shape: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    val dist = new GammaDistribution(shape, scale)
    qgamma_internal(p, lower_tail, log_p, dist)
  }
  def qgamma(p: List[Double], shape: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(p.isEmpty) throw new IllegalArgumentException
    val dist = new GammaDistribution(shape, scale)
    p.map(tup => qgamma_internal(tup, lower_tail, log_p, dist))
  }



  def rgamma(n: Int, shape: Double, scale: Double): List[Double] = {
    if(n < 1 || (shape+scale).isNaN || shape < 0.0 || scale <= 0.0) throw new IllegalArgumentException
    val dist = new GammaDistribution(shape, scale)
    List.fill(n)(dist.sample)
  }



}
