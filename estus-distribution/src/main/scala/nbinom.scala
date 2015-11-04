package com.estus.distribution

import routine._
import org.apache.commons.math3.distribution.PascalDistribution

object nbinom {

  /** *
    *
    * Negative Binomial Distribution
    *
    * d_ - probability density function
    * p_ - cumulative density function
    * q_ - inverse cumulative density function
    * r_ - random number from that distribution
    *
    */

  private def dnbinom_internal(x: Int, log_p: Boolean, dist: PascalDistribution): Double = {
    (dist.getNumberOfSuccesses, dist.getProbabilityOfSuccess) match {
      case (a, b) if a < 0 || b < 0.0 || b > 1.0 || b.isNaN => Double.NaN
      case _ =>
        if(log_p) dist.logProbability(x) else dist.probability(x)
    }
  }
  def dnbinom(x: Int, size: Int, prob: Double, log_p: Boolean): Double = {
    val dist = new PascalDistribution(size, prob)
    dnbinom_internal(x, log_p, dist)
  }
  def dnbinom(x: List[Int], size: Int, prob: Double, log_p: Boolean): List[Double] = {
    if(x.isEmpty) throw new IllegalArgumentException
    val dist = new PascalDistribution(size, prob)
    x.map(tup => dnbinom_internal(tup, log_p, dist))
  }



  private def pnbinom_internal(x: Int, lower_tail: Boolean, log_p: Boolean, dist: PascalDistribution): Double = {
    val a = dist.getNumberOfSuccesses
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
  def pnbinom(x: Int, size: Int, prob: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    val dist = new PascalDistribution(size, prob)
    pnbinom_internal(x, lower_tail, log_p, dist)
  }
  def pnbinom(x: List[Int], size: Int, prob: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(x.isEmpty) throw new IllegalArgumentException
    val dist = new PascalDistribution(size, prob)
    x.map(tup => pnbinom_internal(tup, lower_tail, log_p, dist))
  }



  private def qnbinom_internal(p: Double, lower_tail: Boolean, log_p: Boolean, dist: PascalDistribution): Int = {
    val p_tmp = (lower_tail, log_p) match {
      case (true, false) => p
      case (true, true) => math.exp(p)
      case (false, false) => 1 - p
      case (false, true) => 1 - math.exp(p)
    }
    dist.inverseCumulativeProbability(p_tmp)
  }
  def qnbinom(p: Double, size: Int, prob: Double, lower_tail: Boolean, log_p: Boolean): Int = {
    val dist = new PascalDistribution(size, prob)
    qnbinom_internal(p, lower_tail, log_p, dist)
  }
  def qnbinom(p: List[Double], size: Int, prob: Double, lower_tail: Boolean, log_p: Boolean): List[Int] = {
    if(p.isEmpty) throw new IllegalArgumentException
    val dist = new PascalDistribution(size, prob)
    p.map(tup => qnbinom_internal(tup, lower_tail, log_p, dist))
  }



  def rnbinom(n: Int, size: Int, prob: Double): List[Int] = {
    if(n < 1 || prob < 0.0 || prob > 1.0 || (size + prob).isNaN) throw new IllegalArgumentException
    val dist = new PascalDistribution(size, prob)
    List.fill(n)(dist.sample)
  }



}
