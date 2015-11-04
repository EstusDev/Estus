package com.estus.distribution

import routine._

import org.apache.commons.math3.distribution.TriangularDistribution

object triangular {

  /** *
    *
    * Triangular Distribution
    *
    * d_ - probability density function
    * p_ - cumulative density function
    * q_ - inverse cumulative density function
    * r_ - random number from that distribution
    *
    */

  private def dtriangular_internal(q: Double, log_p: Boolean, dist: TriangularDistribution): Double = {
    (dist.getNumericalMean, dist.getNumericalVariance, dist.getMode) match {
      case (a, b, c) if a.isNaN || b.isNaN || c.isNaN || q.isNaN => Double.NaN
      case _ =>
        if(log_p) dist.logDensity(q) else dist.density(q)
    }
  }
  def dtriangular(q: Double, location: Double, scale: Double, mode: Double, log_p: Boolean): Double = {
    val dist = new TriangularDistribution(location, scale, mode)
    dtriangular_internal(q, log_p, dist)
  }
  def dtriangular(q: List[Double], location: Double, scale: Double, mode: Double, log_p: Boolean): List[Double] = {
    if(q.isEmpty) throw new IllegalArgumentException
    val dist = new TriangularDistribution(location, scale, mode)
    q.map(tup => dtriangular_internal(tup, log_p, dist))
  }



  private def ptriangular_internal(q: Double, lower_tail: Boolean, log_p: Boolean, dist: TriangularDistribution): Double = {
    val a = dist.getNumericalMean
    val b = dist.getNumericalVariance
    val c = dist.getMode
    if(q.isNaN || a.isNaN || b.isNaN || c.isNaN) throw new IllegalArgumentException
    val cumprob = dist.cumulativeProbability(q)
    (lower_tail, log_p) match {
      case (true, false) => cumprob
      case (true, true) => math.log(cumprob)
      case (false, false) => 1 - cumprob
      case (false, true) => math.log(1 - cumprob)
    }
  }
  def ptriangular(q: Double, location: Double, scale: Double, mode: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    val dist = new TriangularDistribution(location, scale, mode)
    ptriangular_internal(q, lower_tail, log_p, dist)
  }
  def ptriangular(q: List[Double], location: Double, scale: Double, mode: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(q.isEmpty) throw new IllegalArgumentException
    val dist = new TriangularDistribution(location, scale, mode)
    q.map(tup => ptriangular_internal(tup, lower_tail, log_p, dist))
  }



  private def qtriangular_internal(p: Double, lower_tail: Boolean, log_p: Boolean, dist: TriangularDistribution): Double = {
    val p_tmp = (lower_tail, log_p) match {
      case (true, false) => p
      case (true, true) => math.exp(p)
      case (false, false) => 1 - p
      case (false, true) => 1 - math.exp(p)
    }
    dist.inverseCumulativeProbability(p_tmp)
  }
  def qtriangular(p: Double, location: Double, scale: Double, mode: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    val dist = new TriangularDistribution(location, scale, mode)
    qtriangular_internal(p, lower_tail, log_p, dist)
  }
  def qtriangular(p: List[Double], location: Double, scale: Double, mode: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(p.isEmpty) throw new IllegalArgumentException
    val dist = new TriangularDistribution(location, scale, mode)
    p.map(tup => qtriangular_internal(tup, lower_tail, log_p, dist))
  }



  def rtriangular(n: Int, location: Double, scale: Double, mode: Double): List[Double] = {
    if(n < 1 || (location + scale).isNaN) throw new IllegalArgumentException
    val dist = new TriangularDistribution(location, scale, mode)
    List.fill(n)(dist.sample)
  }



}
