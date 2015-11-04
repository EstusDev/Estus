package com.estus.distribution

import routine._
import org.apache.commons.math3.distribution.LevyDistribution

object levy {

  /** *
    *
    * Levy Distribution
    *
    * d_ - probability density function
    * p_ - cumulative density function
    * q_ - inverse cumulative density function
    * r_ - random number from that distribution
    *
    */

  private def dlevy_internal(q: Double, log_p: Boolean, dist: LevyDistribution): Double = {
    (dist.getLocation, dist.getScale) match {
      case (a, b) if a.isNaN || b.isNaN || q.isNaN => Double.NaN
      case _ =>
        if(log_p) dist.logDensity(q) else dist.density(q)
    }
  }
  def dlevy(q: Double, location: Double, scale: Double, log_p: Boolean): Double = {
    val dist = new LevyDistribution(location, scale)
    dlevy_internal(q, log_p, dist)
  }
  def dlevy(q: List[Double], location: Double, scale: Double, log_p: Boolean): List[Double] = {
    if(q.isEmpty) throw new IllegalArgumentException
    val dist = new LevyDistribution(location, scale)
    q.map(tup => dlevy_internal(tup, log_p, dist))
  }



  private def plevy_internal(q: Double, lower_tail: Boolean, log_p: Boolean, dist: LevyDistribution): Double = {
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
  def plevy(q: Double, location: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    val dist = new LevyDistribution(location, scale)
    plevy_internal(q, lower_tail, log_p, dist)
  }
  def plevy(q: List[Double], location: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(q.isEmpty) throw new IllegalArgumentException
    val dist = new LevyDistribution(location, scale)
    q.map(tup => plevy_internal(tup, lower_tail, log_p, dist))
  }



  private def qlevy_internal(p: Double, lower_tail: Boolean, log_p: Boolean, dist: LevyDistribution): Double = {
    val p_tmp = (lower_tail, log_p) match {
      case (true, false) => p
      case (true, true) => math.exp(p)
      case (false, false) => 1 - p
      case (false, true) => 1 - math.exp(p)
    }
    dist.inverseCumulativeProbability(p_tmp)
  }
  def qlevy(p: Double, location: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    val dist = new LevyDistribution(location, scale)
    qlevy_internal(p, lower_tail, log_p, dist)
  }
  def qlevy(p: List[Double], location: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(p.isEmpty) throw new IllegalArgumentException
    val dist = new LevyDistribution(location, scale)
    p.map(tup => qlevy_internal(tup, lower_tail, log_p, dist))
  }



  def rlevy(n: Int, location: Double, scale: Double): List[Double] = {
    if(n < 1 || (location + scale).isNaN) throw new IllegalArgumentException
    val dist = new LevyDistribution(location, scale)
    List.fill(n)(dist.sample)
  }



}
