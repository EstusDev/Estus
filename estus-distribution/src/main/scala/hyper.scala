package com.estus.distribution

import routine._
import org.apache.commons.math3.distribution.HypergeometricDistribution

object hyper {

  /** *
    *
    * Hypergeometric Distribution
    *
    * d_ - probability density function
    * p_ - cumulative density function
    * q_ - inverse cumulative density function
    * r_ - random number from that distribution
    *
    */

  /*
    R Parameters:
    m: the number of white balls in the urn
    n: the number of black balls in the urn
    k: the number of balls drawn from the urn

    Java Parameters:
    int populationSize = m + n
    int numberOfSuccesses = m
    int sampleSize = k
  */

  private def dhyper_internal(x: Int, log_p: Boolean, dist: HypergeometricDistribution): Double = {
    (dist.getPopulationSize, dist.getNumberOfSuccesses, dist.getSampleSize) match {
      case (m, n, k) if m < 0 || n < 0 || k < 0 => Double.NaN
      case _ =>
        if(log_p) dist.logProbability(x) else dist.probability(x)
    }
  }
  def dhyper(x: Int, m: Int, n: Int, k: Int, log_p: Boolean): Double = {
    val dist = new HypergeometricDistribution(m + n, m, k)
    dhyper_internal(x, log_p, dist)
  }
  def dhyper(x: List[Int], m: Int, n: Int, k: Int, log_p: Boolean): List[Double] = {
    if(x.isEmpty) throw new IllegalArgumentException
    val dist = new HypergeometricDistribution(m + n, m, k)
    x.map(tup => dhyper_internal(tup, log_p, dist))
  }



  private def phyper_internal(x: Int, lower_tail: Boolean, log_p: Boolean, dist: HypergeometricDistribution): Double = {
    val (m, n, k) = (dist.getPopulationSize, dist.getNumberOfSuccesses, dist.getSampleSize)
    if(m < 0 || n < 0 || k < 0) throw new IllegalArgumentException
    val cumprob = dist.cumulativeProbability(x)
    (lower_tail, log_p) match {
      case (true, false) => cumprob
      case (true, true) => math.log(cumprob)
      case (false, false) => 1 - cumprob
      case (false, true) => math.log(1 - cumprob)
    }
  }
  def phyper(x: Int, m: Int, n: Int, k: Int, lower_tail: Boolean, log_p: Boolean): Double = {
    val dist = new HypergeometricDistribution(m + n, m, k)
    phyper_internal(x, lower_tail, log_p, dist)
  }
  def phyper(x: List[Int], m: Int, n: Int, k: Int, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(x.isEmpty) throw new IllegalArgumentException
    val dist = new HypergeometricDistribution(m + n, m, k)
    x.map(tup => phyper_internal(tup, lower_tail, log_p, dist))
  }



  private def qhyper_internal(p: Double, lower_tail: Boolean, log_p: Boolean, dist: HypergeometricDistribution): Int = {
    val p_tmp = (lower_tail, log_p) match {
      case (true, false) => p
      case (true, true) => math.exp(p)
      case (false, false) => 1 - p
      case (false, true) => 1 - math.exp(p)
    }
    dist.inverseCumulativeProbability(p_tmp)
  }
  def qhyper(p: Double, m: Int, n: Int, k: Int, lower_tail: Boolean, log_p: Boolean): Int = {
    val dist = new HypergeometricDistribution(m + n, m, k)
    qhyper_internal(p, lower_tail, log_p, dist)
  }
  def qhyper(p: List[Double], m: Int, n: Int, k: Int, lower_tail: Boolean, log_p: Boolean): List[Int] = {
    if(p.isEmpty) throw new IllegalArgumentException
    val dist = new HypergeometricDistribution(m + n, m, k)
    p.map(tup => qhyper_internal(tup, lower_tail, log_p, dist))
  }



  def rhyper(nn: Int, m: Int, n: Int, k: Int): List[Int] = {
    if(nn < 1 || m < 0 || n < 0 || k < 0) throw new IllegalArgumentException
    val dist = new HypergeometricDistribution(m + n, m, k)
    List.fill(nn)(dist.sample)
  }



}
