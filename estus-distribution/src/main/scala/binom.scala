package com.estus.distribution

import routine._
import org.apache.commons.math3.distribution.BinomialDistribution

object binom {

  /** *
    *
    * Binomial Distribution
    *
    * d_ - probability density function
    * p_ - cumulative density function
    * q_ - inverse cumulative density function
    * r_ - random number from that distribution
    *
    */

  private def dbinom_internal(x: Int, log_p: Boolean, dist: BinomialDistribution): Double = {
    (dist.getNumberOfTrials, dist.getProbabilityOfSuccess) match {
      case (a, b) if a < 0 || b < 0.0 || b > 1.0 || b.isNaN => Double.NaN
      case _ =>
        if(log_p) dist.logProbability(x) else dist.probability(x)
    }
  }
  def dbinom(x: Int, size: Int, prob: Double, log_p: Boolean): Double = {
    val dist = new BinomialDistribution(size, prob)
    dbinom_internal(x, log_p, dist)
  }
  def dbinom(x: List[Int], size: Int, prob: Double, log_p: Boolean): List[Double] = {
    if(x.isEmpty) throw new IllegalArgumentException
    val dist = new BinomialDistribution(size, prob)
    x.map(tup => dbinom_internal(tup, log_p, dist))
  }



  private def pbinom_internal(x: Int, lower_tail: Boolean, log_p: Boolean, dist: BinomialDistribution): Double = {
    val a = dist.getNumberOfTrials
    val b = dist.getProbabilityOfSuccess
    if(a < 0 || b < 0.0 || b > 1.0 || b.isNaN) throw new IllegalArgumentException
    val cumprob = dist.cumulativeProbability(x)
    (lower_tail, log_p) match {
      case (true, false) => cumprob
      case (true, true) => math.log(cumprob)
      case (false, false) => 1 - cumprob
      case (false, true) => math.log(1 - cumprob)
    }
  }
  def pbinom(x: Int, size: Int, prob: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    val dist = new BinomialDistribution(size, prob)
    pbinom_internal(x, lower_tail, log_p, dist)
  }
  def pbinom(x: List[Int], size: Int, prob: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(x.isEmpty) throw new IllegalArgumentException
    val dist = new BinomialDistribution(size, prob)
    x.map(tup => pbinom_internal(tup, lower_tail, log_p, dist))
  }



  private def qbinom_internal(p: Double, lower_tail: Boolean, log_p: Boolean, dist: BinomialDistribution): Int = {
    val p_tmp = (lower_tail, log_p) match {
      case (true, false) => p
      case (true, true) => math.exp(p)
      case (false, false) => 1 - p
      case (false, true) => 1 - math.exp(p)
    }
    dist.inverseCumulativeProbability(p_tmp)
  }
  def qbinom(p: Double, size: Int, prob: Double, lower_tail: Boolean, log_p: Boolean): Int = {
    val dist = new BinomialDistribution(size, prob)
    qbinom_internal(p, lower_tail, log_p, dist)
  }
  def qbinom(p: List[Double], size: Int, prob: Double, lower_tail: Boolean, log_p: Boolean): List[Int] = {
    if(p.isEmpty) throw new IllegalArgumentException
    val dist = new BinomialDistribution(size, prob)
    p.map(tup => qbinom_internal(tup, lower_tail, log_p, dist))
  }



  def rbinom(n: Int, size: Int, prob: Double): List[Int] = {
    if(n < 1 || prob < 0.0 || prob > 1.0 || (size + prob).isNaN) throw new IllegalArgumentException
    val dist = new BinomialDistribution(size, prob)
    List.fill(n)(dist.sample)
  }



}
