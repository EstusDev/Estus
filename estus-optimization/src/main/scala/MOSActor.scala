package com.estus.optimization

import com.estus.optimization.MessageProtocol._
import akka.actor.{ActorRef, ActorLogging, Actor}
import scala.concurrent.duration.Duration
import scala.util.Failure



case class MOSActor() extends Actor with ActorLogging {

  /* Differential Evolution Variables */

  case class ValueDE (master: ActorRef, id: Any, node: PopulationNode)

  var EvalMapDE = scala.collection.immutable.Map.empty[String, ValueDE]

  /* Local Search 1 Variables */

  trait EvalType

  private object FirstEval extends EvalType

  private object SecondEval extends EvalType

  type KeyLS1 = (String, EvalType, Int)

  case class ValueLS1 (
    node: PopulationNode,
    best: PopulationNode,
    master: ActorRef,
    slave: ActorRef,
    request: Request,
    to: Duration)

  var EvalMapLS1 = scala.collection.immutable.Map.empty[KeyLS1, ValueLS1]

  def selectBetterNode = Population(0).selectBetterNode(
    _: PopulationNode,
    _: PopulationNode,
    _: String)

  def getParam(
    base: List[Double],
    d: Int,
    sr: Double,
    request: Request): List[Double] = {
    // val p = base(d) + factor * (request.UB(d) - request.LB(d)) match {
    val p = base(d) + sr match {
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
    node.improve = Some(false)
    node.DSR = node.DSR
    node
  }

  def receive = {

    /* Common Messages */

    case WorkAvailable(master) =>
      master ! GimmeWork

    case Failure(cause) =>
      log.info(cause.toString)

    /* Differential Evolution Messages */

    case WorkDE(master, slave, id, node, request, to) =>
      val key = java.util.UUID.randomUUID.toString
      val value = ValueDE(master, id, node)
      val objFn = request.objFn(_: List[Double], request.additionalParam)
      EvalMapDE = EvalMapDE + (key -> value)
      slave ! Work(self, key, node.param, objFn, to)

    case Result(k, v) if k.isInstanceOf[String] =>
      val key = k.asInstanceOf[String]
      val objFnVal = v.toDouble
      EvalMapDE.get(key) match {
        case Some(value) =>
          val node = value.node
          node.objFnVal = Some(objFnVal)
          value.master ! ResultMOS(value.id, node)
          value.master ! AddNumEval(1)
          value.master ! GimmeWork
        case _ =>
      }
      EvalMapDE = EvalMapDE - key

    /* Local Search 1 Messages*/

    case WorkLS(master, slave, best, request, to) =>
      val uuid = java.util.UUID.randomUUID.toString
      val objFn = request.objFn(_: List[Double], request.additionalParam)
      val strategy = request.solverConfig.asInstanceOf[MOSConfig].constStrategy
      val d = best.DSR.get._1
      val sr = best.DSR.get._2
      val node1 = getNode(best, getParam(best.param, d, -sr, request), request)
      if (node1.constVal <= 0) {
        val key1 = (uuid, FirstEval, 1)
        val value1 = ValueLS1(node1, best, master, slave, request, to)
        EvalMapLS1 = EvalMapLS1 + (key1 -> value1)
        slave ! Work(self, key1, node1.param, objFn, to)
      } else {
        if (node1 == selectBetterNode(node1, best, strategy)) {
          node1.improve = Some(true)
          master ! ResultMOS(None, node1)
          master ! GimmeWork
        } else {
          val node2 = getNode(best, getParam(best.param, d, 0.5 * sr, request), request)
          if (node2.constVal <= 0) {
            val key2 = (uuid, SecondEval, 1)
            val value2 = ValueLS1(node2, best, master, slave, request, to)
            EvalMapLS1 = EvalMapLS1 + (key2 -> value2)
            slave ! Work(self, key2, node2.param, objFn, to)
          } else {
            if (node2 == selectBetterNode(node2, best, strategy)) {
              node2.improve = Some(true)
              master ! ResultMOS(None, node2)
              master ! GimmeWork
            } else {
              master ! GimmeWork
            }
          }
        }
      }

    case Result((uuid, FirstEval, numEval), v) =>
      val key = (uuid, FirstEval, numEval).asInstanceOf[KeyLS1]
      EvalMapLS1.get(key) match {
        case Some(value) =>
          val node1 = value.node
          node1.objFnVal = Some(v)
          val best = value.best
          val master = value.master
          val slave = value.slave
          val d = best.DSR.get._1
          val sr = best.DSR.get._2
          val to = value.to
          val request = value.request
          val objFn = request.objFn(_: List[Double], request.additionalParam)
          val strategy = request.solverConfig.asInstanceOf[MOSConfig].constStrategy
          if (node1 == selectBetterNode(node1, best, strategy)) {
            node1.improve = Some(true)
            master ! ResultMOS(None, node1)
            master ! AddNumEval(key._3)
            master ! GimmeWork
          } else {
            val node2 = getNode(best, getParam(best.param, d, 0.5 * sr, request), request)
            if (node2.constVal <= 0) {
              val key2 = (key._1, SecondEval, key._3 + 1)
              val value2 = ValueLS1(node2, best, master, slave, request, to)
              EvalMapLS1 = EvalMapLS1 + (key2 -> value2)
              slave ! Work(self, key2, node2.param, objFn, to)
            } else {
              /* IMPOSSIBLE CONDITION !!!
              if (node2 == selectBetterNode(node2, best, strategy)) {
                node2.improve = Some(d)
                master ! ResultMOS(None, node2)
                master ! AddNumEval(key._3)
                master ! GimmeWork
              } else {
              */
              master ! AddNumEval(key._3)
              master ! GimmeWork
              /*}*/
            }
          }
        case _ =>
      }
      EvalMapLS1 = EvalMapLS1 - key

    case Result((uuid, SecondEval, numEval), v) =>
      val key = (uuid, SecondEval, numEval).asInstanceOf[KeyLS1]
      EvalMapLS1.get(key) match {
        case Some(value) =>
          val node = value.node
          node.objFnVal = Some(v)
          val best = value.best
          val master = value.master
          val request = value.request
          val strategy = request.solverConfig.asInstanceOf[MOSConfig].constStrategy
          if (node == selectBetterNode(node, best, strategy)) {
            node.improve = Some(true)
            master ! ResultMOS(None, node)
            master ! AddNumEval(key._3)
            master ! GimmeWork
          } else {
            master ! AddNumEval(key._3)
            master ! GimmeWork
          }
        case _ =>
      }
      EvalMapLS1 = EvalMapLS1 - key

  }

}
