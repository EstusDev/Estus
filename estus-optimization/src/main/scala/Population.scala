package com.estus.optimization

import scala.math.{abs, max}



case class PopulationNode(param: List[Double], request: Request) {

  var objFnVal: Option[Double] = None

  val constVec: List[Double] = List(checkEqB, checkIneqLB, checkIneqUB).flatten

  val constVal: Double = constVec.filter(_ > 0.0).sum

  var F: Option[Double] = None

  var Cr: Option[Double] = None

  var rho: Option[Double] = None

  var DSR: Option[(Int, Double)] = None

  private def checkEqB: List[Double] = (request.eqB, request.eqErr, request.eqFunc) match {
    case (Some(b), Some(e), Some(f)) => f(param).zip(b).map(x => abs(x._1 - x._2) - e)
    case _ => List(0.0)
  }

  private def checkIneqLB: List[Double] = (request.ineqLB, request.ineqFunc) match {
    case (Some(lb), Some(f)) => f(param).zip(lb).map(x => {x._2 - x._1})
    case _ => List(0.0)
  }

  private def checkIneqUB: List[Double] = (request.ineqUB, request.ineqFunc) match {
    case (Some(ub), Some(f)) => f(param).zip(ub).map(x => x._1 - x._2)
    case _ => List(0.0)
  }

}



case class Population (NP: Int) {

  val p = collection.mutable.Map.empty[String, PopulationNode]

  def size: Int = p.size

  def get (i: Int): Option[PopulationNode] = {
    if (i < p.size)
      Some(p(p.keys.toList(i)))
    else
      None
  }

  def add (node: PopulationNode): Unit = {
    if (p.size < NP)
      p(java.util.UUID.randomUUID.toString) = node
  }

  def update (i: Int, node: PopulationNode): Unit = {
    if (i < p.size)
      p(p.keys.toList(i)) = node
  }

  def replaceWorst (node: PopulationNode, constStrategy: String): Unit = {
    if (p.nonEmpty) {
      val wfv = worstFeasibleVal.getOrElse(0.0)
      val keyWfv = p.maxBy(n => n._2.objFnVal match {
        case Some (v) => v
        case _ => n._2.constVal + wfv
      })._1
      val nodeSelected = selectBetterNode(p.getOrElse(keyWfv, node), node, constStrategy)
      p(keyWfv) = nodeSelected
    }
  }

  def empty (): Unit = p.clear()

  def merge (pNew: Population): Population = {
    val popUnion = new Population(NP + pNew.NP)
    p.foreach(n => popUnion.p(n._1) = n._2)
    pNew.p.foreach(n => popUnion.p(n._1) = n._2)
    popUnion
  }

  def worstFeasibleVal: Option[Double] = {
    if (p.nonEmpty)
      p.values.maxBy(_.objFnVal).objFnVal
    else
      None
  }

  def selectBetterNode(
    node1: PopulationNode,
    node2: PopulationNode,
    constStrategy: String): PopulationNode = (node1.objFnVal, node2.objFnVal) match {
    case (Some(a), Some(b)) if a < b =>
      node1
    case (Some(_), None) =>
      node1
    case (None, None) =>
      if (constStrategy == "rank") { // The lowest sum of constraint violation
        if (node1.constVal <= node2.constVal)
          node1
        else
          node2
      } else { // Pareto-dominates
        if (node1.constVec.map(max(_, 0.0)).
          zip(node2.constVec.map(max(_, 0.0))).
          exists(x => x._2 > x._1))
          node1
        else
          node2
      }
    case _ => node2
  }

}
