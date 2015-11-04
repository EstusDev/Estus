package com.estus.distribution

import routine._
import org.apache.commons.math3.distribution.GumbelDistribution

object gumbel {

  /** *
    *
    * Gumbel Distribution
    *
    * d_ - probability density function
    * p_ - cumulative density function
    * q_ - inverse cumulative density function
    * r_ - random number from that distribution
    *
    */

  private def dgumbel_internal(q: Double, log_p: Boolean, dist: GumbelDistribution): Double = {
    (dist.getLocation, dist.getScale) match {
      case (a, b) if a.isNaN || b.isNaN || q.isNaN => Double.NaN
      case _ =>
        if(log_p) dist.logDensity(q) else dist.density(q)
    }
  }
  def dgumbel(q: Double, location: Double, scale: Double, log_p: Boolean): Double = {
    val dist = new GumbelDistribution(location, scale)
    dgumbel_internal(q, log_p, dist)
  }
  def dgumbel(q: List[Double], location: Double, scale: Double, log_p: Boolean): List[Double] = {
    if(q.isEmpty) throw new IllegalArgumentException
    val dist = new GumbelDistribution(location, scale)
    q.map(tup => dgumbel_internal(tup, log_p, dist))
  }



  private def pgumbel_internal(q: Double, lower_tail: Boolean, log_p: Boolean, dist: GumbelDistribution): Double = {
    val a = dist.getLocation
    val b = dist.getScale
    if(q.isNaN || a.isNaN || b.isNaN) throw new IllegalArgumentException
    val cumprob = dist.cumulativeProbability(q)
    (lower_tail, log_p) match {
      case (true, false) => cumprob
      case (true, true) => math.log(cumprob)
      case (false, false) => 1 - cumprob
      case (false, true) => math.log(1 - cumprob)
    }
  }
  def pgumbel(q: Double, location: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    val dist = new GumbelDistribution(location, scale)
    pgumbel_internal(q, lower_tail, log_p, dist)
  }
  def pgumbel(q: List[Double], location: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(q.isEmpty) throw new IllegalArgumentException
    val dist = new GumbelDistribution(location, scale)
    q.map(tup => pgumbel_internal(tup, lower_tail, log_p, dist))
  }



  private def qgumbel_internal(p: Double, lower_tail: Boolean, log_p: Boolean, dist: GumbelDistribution): Double = {
    val p_tmp = (lower_tail, log_p) match {
      case (true, false) => p
      case (true, true) => math.exp(p)
      case (false, false) => 1 - p
      case (false, true) => 1 - math.exp(p)
    }
    dist.inverseCumulativeProbability(p_tmp)
  }
  def qgumbel(p: Double, location: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    val dist = new GumbelDistribution(location, scale)
    qgumbel_internal(p, lower_tail, log_p, dist)
  }
  def qgumbel(p: List[Double], location: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(p.isEmpty) throw new IllegalArgumentException
    val dist = new GumbelDistribution(location, scale)
    p.map(tup => qgumbel_internal(tup, lower_tail, log_p, dist))
  }



  def rgumbel(n: Int, location: Double, scale: Double): List[Double] = {
    if(n < 1 || (location + scale).isNaN) throw new IllegalArgumentException
    val dist = new GumbelDistribution(location, scale)
    List.fill(n)(dist.sample)
  }



}
