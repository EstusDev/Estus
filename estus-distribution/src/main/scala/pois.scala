package com.estus.distribution

import routine._

import org.apache.commons.math3.distribution.PoissonDistribution

object pois {

  /** *
    *
    * Poisson Distribution
    *
    * d_ - probability density function
    * p_ - cumulative density function
    * q_ - inverse cumulative density function
    * r_ - random number from that distribution
    *
    */

  private def dpois_internal(x: Int, log_p: Boolean, dist: PoissonDistribution): Double = {
    if(log_p) dist.logProbability(x) else dist.probability(x)
  }
  def dpois(x: Int, log_p: Boolean, lambda: Double): Double = {
    val dist = new PoissonDistribution(lambda)
    dpois_internal(x, log_p, dist)
  }
  def dpois(x: List[Int], lambda: Double, log_p: Boolean): List[Double] = {
    val dist = new PoissonDistribution(lambda)
    x.map(tup => dpois_internal(tup, log_p, dist))
  }



  private def ppois_internal(x: Int, lower_tail: Boolean, log_p: Boolean, dist: PoissonDistribution): Double = {
    val lambda = dist.getMean()
    if(lambda < 0.0) throw new IllegalArgumentException
    if(x < 0.0) DT_0(lower_tail, log_p)
    if(lambda == 0.0) DT_1(lower_tail, log_p)
    if(x.toDouble.isInfinite) DT_1(lower_tail, log_p)
    val cumprob = dist.cumulativeProbability(x)
    (lower_tail, log_p) match {
      case (true, false) => cumprob
      case (true, true) => math.log(cumprob)
      case (false, false) => 1 - cumprob
      case (false, true) => math.log(1 - cumprob)
    }
  }
  def ppois(x: Int, lambda: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    val dist = new PoissonDistribution(lambda)
    ppois_internal(x, lower_tail, log_p, dist)
  }
  def ppois(x: List[Int], lambda: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    val dist = new PoissonDistribution(lambda)
    x.map(tup => ppois_internal(tup, lower_tail, log_p, dist))
  }



  private def qpois_internal(p: Double, lower_tail: Boolean, log_p: Boolean, dist: PoissonDistribution): Int = {
    val p_tmp = (lower_tail, log_p) match {
      case (true, false) => p
      case (true, true) => math.exp(p)
      case (false, false) => 1 - p
      case (false, true) => 1 - math.exp(p)
    }
    dist.inverseCumulativeProbability(p_tmp)
  }
  def qpois(p: Double, lambda: Double, lower_tail: Boolean, log_p: Boolean): Int = {
    val dist = new PoissonDistribution(lambda)
    qpois_internal(p, lower_tail, log_p, dist)
  }
  def qpois(p: List[Double], lambda: Double, lower_tail: Boolean, log_p: Boolean): List[Int] = {
    val dist = new PoissonDistribution(lambda)
    p.map(tup => qpois_internal(tup, lower_tail, log_p, dist))
  }



  def rpois(n: Int, lambda: Double): List[Int] = {
    if(n < 1 || lambda.isNaN || lambda.isInfinite || lambda < 0.0) throw new IllegalArgumentException
    val dist = new PoissonDistribution(lambda)
    List.fill(n)(dist.sample)
  }



}
