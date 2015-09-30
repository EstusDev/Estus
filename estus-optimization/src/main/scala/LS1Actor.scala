package com.estus.optimization

import com.estus.optimization.MessageProtocol._
import akka.actor.{ActorRef, ActorLogging, Actor}
import scala.concurrent.duration.Duration
import scala.util.Failure



case class LS1Actor() extends Actor with ActorLogging {

  class EvalType()

  private object FirstEval extends EvalType

  private object SecondEval extends EvalType

  type Key = (String, EvalType, Int)

  case class Value (
    node: PopulationNode,
    best: PopulationNode,
    master: ActorRef,
    slave: ActorRef,
    d: Int,
    request: Request,
    to: Duration)

  var EvalMap = scala.collection.immutable.Map.empty[Key, Value]

  def selectBetterNode = Population(0).selectBetterNode(
    _: PopulationNode,
    _: PopulationNode,
    _: String)

  def getParam(
    base: List[Double],
    d: Int,
    factor: Double,
    request: Request): List[Double] = {
    val p = base(d) + factor * (request.UB(d) - request.LB(d)) match {
      case v if v < request.LB(d) =>
        base(d) - 0.5 * (base(d) - request.LB(d))
      case v if v > request.UB(d) =>
        base(d) + 0.5 * (request.UB(d) - base(d))
      case v =>
        v
    }
    base.patch(d, Seq(p), 1)
  }

  def getNode(
    base: PopulationNode,
    param: List[Double],
    request: Request): PopulationNode = {
    val node = PopulationNode(param, request)
    node.F = base.F
    node.Cr = base.Cr
    node.rho = base.rho
    node.improve = base.improve
    node.SR = base.SR
    node
  }

  def receive = {

    case WorkAvailable(master) =>
      master ! GimmeWorkLS

    case LocalSearch(master, slave, best, d, request, to) =>
      val uuid = java.util.UUID.randomUUID.toString
      val objFn = request.objFn(_: List[Double], request.additionalParam)
      val mut = request.solverConfig.asInstanceOf[DiffEvoConfig].mutationStrategy
      val node1 = getNode(best, getParam(best.param, d, -best.SR.get, request), request)
      if (node1.constVal <= 0) {
        val key1 = (uuid, FirstEval, 1)
        val value1 = Value(node1, best, master, slave, d, request, to)
        EvalMap = EvalMap + (key1 -> value1)
        slave ! Work(self, key1, node1.param, objFn, to)
      } else {
        if (node1 == selectBetterNode(node1, best, mut)) {
          master ! UpdateBestNode(node1)
          master ! GimmeWorkLS
        } else {
          val node2 = getNode(best, getParam(best.param, d, 0.5 * best.SR.get, request), request)
          if (node2.constVal <= 0) {
            val key2 = (uuid, SecondEval, 1)
            val value2 = Value(node2, best, master, slave, d, request, to)
            EvalMap = EvalMap + (key2 -> value2)
            slave ! Work(self, key2, node2.param, objFn, to)
          } else {
            if (node2 == selectBetterNode(node2, best, mut)) {
              master ! UpdateBestNode(node2)
              master ! GimmeWorkLS
            } else {
              master ! GimmeWorkLS
            }
          }
        }
      }

    case Result((uuid, FirstEval, numEval), v) =>
      val key = (uuid, FirstEval, numEval).asInstanceOf[Key]
      EvalMap.get(key) match {
        case Some(value) =>
          val node1 = value.node
          node1.objFnVal = Some(v)
          val best = value.best
          val master = value.master
          val slave = value.slave
          val d = value.d
          val to = value.to
          val request = value.request
          val objFn = request.objFn(_: List[Double], request.additionalParam)
          val mut = request.solverConfig.asInstanceOf[DiffEvoConfig].mutationStrategy
          if (node1 == selectBetterNode(node1, best, mut)) {
            master ! UpdateBestNode(node1)
            master ! AddNumEval(key._3)
            master ! GimmeWorkLS
          } else {
            val node2 = getNode(best, getParam(best.param, d, 0.5 * best.SR.get, request), request)
            if (node2.constVal <= 0) {
              val key2 = (key._1, SecondEval, key._3 + 1)
              val value2 = Value(node2, best, master, slave, d, request, to)
              EvalMap = EvalMap + (key2 -> value2)
              slave ! Work(self, key2, node2.param, objFn, to)
            } else {
              /* IMPOSSIBLE CONDITION !!!
              if (node2 == selectBetterNode(node2, best, mut)) {
                master ! UpdateBestNode(node2)
                master ! AddNumEval(key._3)
                master ! GimmeWorkLS
              } else {
              */
              master ! AddNumEval(key._3)
              master ! GimmeWorkLS
              /*}*/
            }
          }
        case _ =>
      }
      EvalMap = EvalMap - key

    case Result((uuid, SecondEval, numEval), v) =>
      val key = (uuid, SecondEval, numEval).asInstanceOf[Key]
      EvalMap.get(key) match {
        case Some(value) =>
          val node = value.node
          node.objFnVal = Some(v)
          val best = value.best
          val master = value.master
          val request = value.request
          val mut = request.solverConfig.asInstanceOf[DiffEvoConfig].mutationStrategy
          if (node == selectBetterNode(node, best, mut)) {
            master ! UpdateBestNode(node)
            master ! AddNumEval(key._3)
            master ! GimmeWorkLS
          } else {
            master ! AddNumEval(key._3)
            master ! GimmeWorkLS
          }
        case _ =>
      }
      EvalMap = EvalMap - key

    case Failure(cause) =>
      log.info(cause.toString)

  }

}
