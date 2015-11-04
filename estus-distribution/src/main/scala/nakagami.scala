package com.estus.distribution

import routine._
import org.apache.commons.math3.distribution.NakagamiDistribution

object nakagami {

  /** *
    *
    * Nakagami Distribution
    *
    * d_ - probability density function
    * p_ - cumulative density function
    * q_ - inverse cumulative density function
    * r_ - random number from that distribution
    *
    */

  private def dnakagami_internal(q: Double, log_p: Boolean, dist: NakagamiDistribution): Double = {
    (dist.getShape, dist.getScale) match {
      case (a, b) if a.isNaN || b.isNaN || q.isNaN => Double.NaN
      case _ =>
        if(log_p) dist.logDensity(q) else dist.density(q)
    }
  }
  def dnakagami(q: Double, location: Double, scale: Double, log_p: Boolean): Double = {
    val dist = new NakagamiDistribution(location, scale)
    dnakagami_internal(q, log_p, dist)
  }
  def dnakagami(q: List[Double], location: Double, scale: Double, log_p: Boolean): List[Double] = {
    if(q.isEmpty) throw new IllegalArgumentException
    val dist = new NakagamiDistribution(location, scale)
    q.map(tup => dnakagami_internal(tup, log_p, dist))
  }



  private def pnakagami_internal(q: Double, lower_tail: Boolean, log_p: Boolean, dist: NakagamiDistribution): Double = {
    val a = dist.getShape
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
  def pnakagami(q: Double, location: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    val dist = new NakagamiDistribution(location, scale)
    pnakagami_internal(q, lower_tail, log_p, dist)
  }
  def pnakagami(q: List[Double], location: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(q.isEmpty) throw new IllegalArgumentException
    val dist = new NakagamiDistribution(location, scale)
    q.map(tup => pnakagami_internal(tup, lower_tail, log_p, dist))
  }



  private def qnakagami_internal(p: Double, lower_tail: Boolean, log_p: Boolean, dist: NakagamiDistribution): Double = {
    val p_tmp = (lower_tail, log_p) match {
      case (true, false) => p
      case (true, true) => math.exp(p)
      case (false, false) => 1 - p
      case (false, true) => 1 - math.exp(p)
    }
    dist.inverseCumulativeProbability(p_tmp)
  }
  def qnakagami(p: Double, location: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    val dist = new NakagamiDistribution(location, scale)
    qnakagami_internal(p, lower_tail, log_p, dist)
  }
  def qnakagami(p: List[Double], location: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(p.isEmpty) throw new IllegalArgumentException
    val dist = new NakagamiDistribution(location, scale)
    p.map(tup => qnakagami_internal(tup, lower_tail, log_p, dist))
  }



  def rnakagami(n: Int, location: Double, scale: Double): List[Double] = {
    if(n < 1 || (location + scale).isNaN) throw new IllegalArgumentException
    val dist = new NakagamiDistribution(location, scale)
    List.fill(n)(dist.sample)
  }



}
