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

  var p = collection.immutable.Map.empty[String, PopulationNode]

  var keys = List.empty[String]

  def size: Int = p.size

  def get (i: Int): Option[PopulationNode] = p.get(keys(i))

  def add (node: PopulationNode): Unit = {
    if (p.size < NP) {
      val uuid = java.util.UUID.randomUUID.toString
      p = p + (uuid -> node)
      keys = uuid :: keys
    }
  }

  def update (key: Int, node: PopulationNode): Unit = {
    if (key < p.size)
      p = (p - keys(key)) + (keys(key) -> node)
  }

  def replaceWorst (node: PopulationNode, constStrategy: String): Unit = {
    val wfv = worstFeasibleVal.getOrElse(0.0)
    val keyWfv = p.maxBy(node => node._2.objFnVal match {
      case Some (v) => v
      case _ => node._2.constVal + wfv
    })._1
    val nodeSelected = selectBetterNode(p.get(keyWfv).get, node, constStrategy)
    p = (p - keyWfv) + (keyWfv -> nodeSelected)
  }

  def empty: Unit = {
    p = collection.immutable.Map.empty[String, PopulationNode]
    keys = List.empty[String]
  }

  def merge (pNew: Population): Population = {
    val popUnion = new Population(NP + pNew.NP)
    popUnion.p = p ++ pNew.p
    popUnion.keys = keys ++ pNew.keys
    popUnion
  }

  def worstFeasibleVal: Option[Double] = p.values.maxBy(_.objFnVal).objFnVal

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
