package com.estus.distribution

import routine._

import org.apache.commons.math3.distribution.ParetoDistribution

object pareto {

  /** *
    *
    * Pareto Distribution
    *
    * d_ - probability density function
    * p_ - cumulative density function
    * q_ - inverse cumulative density function
    * r_ - random number from that distribution
    *
    */

  private def dpareto_internal(q: Double, log_p: Boolean, dist: ParetoDistribution): Double = {
    (dist.getShape, dist.getScale) match {
      case (a, b) if a.isNaN || b.isNaN || q.isNaN => Double.NaN
      case _ =>
        if(log_p) dist.logDensity(q) else dist.density(q)
    }
  }
  def dpareto(q: Double, location: Double, scale: Double, log_p: Boolean): Double = {
    val dist = new ParetoDistribution(scale, location)
    dpareto_internal(q, log_p, dist)
  }
  def dpareto(q: List[Double], location: Double, scale: Double, log_p: Boolean): List[Double] = {
    if(q.isEmpty) throw new IllegalArgumentException
    val dist = new ParetoDistribution(scale, location)
    q.map(tup => dpareto_internal(tup, log_p, dist))
  }



  private def ppareto_internal(q: Double, lower_tail: Boolean, log_p: Boolean, dist: ParetoDistribution): Double = {
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
  def ppareto(q: Double, location: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    val dist = new ParetoDistribution(scale, location)
    ppareto_internal(q, lower_tail, log_p, dist)
  }
  def ppareto(q: List[Double], location: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(q.isEmpty) throw new IllegalArgumentException
    val dist = new ParetoDistribution(scale, location)
    q.map(tup => ppareto_internal(tup, lower_tail, log_p, dist))
  }



  private def qpareto_internal(p: Double, lower_tail: Boolean, log_p: Boolean, dist: ParetoDistribution): Double = {
    val p_tmp = (lower_tail, log_p) match {
      case (true, false) => p
      case (true, true) => math.exp(p)
      case (false, false) => 1 - p
      case (false, true) => 1 - math.exp(p)
    }
    dist.inverseCumulativeProbability(p_tmp)
  }
  def qpareto(p: Double, location: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    val dist = new ParetoDistribution(scale, location)
    qpareto_internal(p, lower_tail, log_p, dist)
  }
  def qpareto(p: List[Double], location: Double, scale: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(p.isEmpty) throw new IllegalArgumentException
    val dist = new ParetoDistribution(scale, location)
    p.map(tup => qpareto_internal(tup, lower_tail, log_p, dist))
  }



  def rpareto(n: Int, location: Double, scale: Double): List[Double] = {
    if(n < 1 || (location + scale).isNaN) throw new IllegalArgumentException
    val dist = new ParetoDistribution(scale, location)
    List.fill(n)(dist.sample)
  }



}
