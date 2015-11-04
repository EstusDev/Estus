package com.estus.distribution

import routine._
import org.apache.commons.math3.distribution.GeometricDistribution

object geom {

  /** *
    *
    * Geometric Distribution
    *
    * d_ - probability density function
    * p_ - cumulative density function
    * q_ - inverse cumulative density function
    * r_ - random number from that distribution
    *
    */

  private def dgeom_internal(x: Int, log_p: Boolean, dist: GeometricDistribution): Double = {
    (dist.getProbabilityOfSuccess) match {
      case a if a < 0.0 || a > 1.0 || a.isNaN => Double.NaN
      case _ =>
        if(log_p) dist.logProbability(x) else dist.probability(x)
    }
  }
  def dgeom(x: Int, prob: Double, log_p: Boolean): Double = {
    val dist = new GeometricDistribution(prob)
    dgeom_internal(x, log_p, dist)
  }
  def dgeom(x: List[Int], prob: Double, log_p: Boolean): List[Double] = {
    if(x.isEmpty) throw new IllegalArgumentException
    val dist = new GeometricDistribution(prob)
    x.map(tup => dgeom_internal(tup, log_p, dist))
  }



  private def pgeom_internal(x: Int, lower_tail: Boolean, log_p: Boolean, dist: GeometricDistribution): Double = {
    val a = dist.getProbabilityOfSuccess
    if(a < 0.0 || a > 1.0 || a.isNaN) throw new IllegalArgumentException
    val cumprob = dist.cumulativeProbability(x)
    (lower_tail, log_p) match {
      case (true, false) => cumprob
      case (true, true) => math.log(cumprob)
      case (false, false) => 1 - cumprob
      case (false, true) => math.log(1 - cumprob)
    }
  }
  def pgeom(x: Int, prob: Double, lower_tail: Boolean, log_p: Boolean): Double = {
    val dist = new GeometricDistribution(prob)
    pgeom_internal(x, lower_tail, log_p, dist)
  }
  def pgeom(x: List[Int], prob: Double, lower_tail: Boolean, log_p: Boolean): List[Double] = {
    if(x.isEmpty) throw new IllegalArgumentException
    val dist = new GeometricDistribution(prob)
    x.map(tup => pgeom_internal(tup, lower_tail, log_p, dist))
  }



  private def qgeom_internal(p: Double, lower_tail: Boolean, log_p: Boolean, dist: GeometricDistribution): Int = {
    val p_tmp = (lower_tail, log_p) match {
      case (true, false) => p
      case (true, true) => math.exp(p)
      case (false, false) => 1 - p
      case (false, true) => 1 - math.exp(p)
    }
    dist.inverseCumulativeProbability(p_tmp)
  }
  def qgeom(p: Double, prob: Double, lower_tail: Boolean, log_p: Boolean): Int = {
    val dist = new GeometricDistribution(prob)
    qgeom_internal(p, lower_tail, log_p, dist)
  }
  def qgeom(p: List[Double], prob: Double, lower_tail: Boolean, log_p: Boolean): List[Int] = {
    if(p.isEmpty) throw new IllegalArgumentException
    val dist = new GeometricDistribution(prob)
    p.map(tup => qgeom_internal(tup, lower_tail, log_p, dist))
  }



  def rgeom(n: Int, prob: Double): List[Int] = {
    if(n < 1 || prob < 0.0 || prob > 1.0 || prob.isNaN) throw new IllegalArgumentException
    val dist = new GeometricDistribution(prob)
    List.fill(n)(dist.sample)
  }



}
