package com.estus.distribution

import routine._

import org.apache.commons.math3.distribution.ZipfDistribution

object zipf {

  /** *
    *
    * Zipf Distribution
    *
    * d_ - probability density function
    * p_ - cumulative density function
    * q_ - inverse cumulative density function
    * r_ - random number from that distribution
    *
    */

  private def dzipf_internal(x: Int, log_p: Boolean, dist: ZipfDistribution): Double = {
    (dist.getNumberOfElements, dist.getExponent) match {
      case (a, b) if a < 0 || b.isNaN => Double.NaN
      case _ =>
        if(log_p) dist.logProbability(x) else dist.probability(x)
    }
  }
  def dzipf(x: Int, size: Int, exponent: Double, log_p: Boolean): Double = {
    val dist = new ZipfDistribution(size, exponent)
    dzipf_internal(x, log_p, dist)
  }
  def dzipf(x: List[Int], size: Int, exponent: Double, log_p: Boolean): List[Double] = {
    if(x.isEmpty) throw new IllegalArgumentException
    val dist = new ZipfDistribution(size, exponent)
    x.map(tup => dzipf_internal(tup, log_p, dist))
  }



  private def pzipf_internal(x: Int, lower_tail: Boolean, log_p: Boolean, dist: ZipfDistribution): Double = {
    val a = dist.getNumberOfElements
    val b = dist.getExponent
    if(a < 0 || b.isNaN) throw new IllegalArgumentException
    val cumprob = dist.cumulativeProbability(x)
    (lower_tail, log_p) match {
      case (true, false) => cumprob
      case (true, true) => math.log(cumprob)
      case (false, false) => 1 - cumprob
      case (false, true) => math.log(1 - cumprob)
    }
  }
  def pzipf(x: Int, size: Int, exponent: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    val dist = new ZipfDistribution(size, exponent)
    pzipf_internal(x, lower_tail, log_p, dist)
  }
  def pzipf(x: List[Int], size: Int, exponent: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(x.isEmpty) throw new IllegalArgumentException
    val dist = new ZipfDistribution(size, exponent)
    x.map(tup => pzipf_internal(tup, lower_tail, log_p, dist))
  }



  private def qzipf_internal(p: Double, lower_tail: Boolean, log_p: Boolean, dist: ZipfDistribution): Int = {
    val p_tmp = (lower_tail, log_p) match {
      case (true, false) => p
      case (true, true) => math.exp(p)
      case (false, false) => 1 - p
      case (false, true) => 1 - math.exp(p)
    }
    dist.inverseCumulativeProbability(p_tmp)
  }
  def qzipf(p: Double, size: Int, exponent: Double, lower_tail: Boolean, log_p: Boolean): Int = {
    val dist = new ZipfDistribution(size, exponent)
    qzipf_internal(p, lower_tail, log_p, dist)
  }
  def qzipf(p: List[Double], size: Int, exponent: Double, lower_tail: Boolean, log_p: Boolean): List[Int] = {
    if(p.isEmpty) throw new IllegalArgumentException
    val dist = new ZipfDistribution(size, exponent)
    p.map(tup => qzipf_internal(tup, lower_tail, log_p, dist))
  }



  def rzipf(n: Int, size: Int, exponent: Double): List[Int] = {
    if(n < 1 || (size + exponent).isNaN) throw new IllegalArgumentException
    val dist = new ZipfDistribution(size, exponent)
    List.fill(n)(dist.sample)
  }



}
