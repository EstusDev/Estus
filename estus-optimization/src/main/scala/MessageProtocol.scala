package com.estus.optimization

import akka.actor.ActorRef

import scala.concurrent.duration.Duration



object MessageProtocol {

  sealed trait Message

  /* Common Actor Messages */

  case class WorkAvailable (master: ActorRef) extends Message

  case class GimmeWork () extends Message

  /* ObjFnActor Messages */

  case class Work (
      master: ActorRef,
      key: Any,
      param: List[Double],
      fn: (List[Double]) => Double,
      timeout: Duration)
    extends Message

  case class Result (key: Any, objVal: Double) extends Message

  /* MOSActor Messages */

  case class StartDE (budget: Int) extends Message

  case class WorkDE (
      master: ActorRef,
      slave: ActorRef,
      key: Any,
      node: PopulationNode,
      request: Request,
      timeout: Duration)
    extends Message

  case class StartLS1 (budget: Int) extends Message

  case class WorkLS (
      master: ActorRef,
      slave: ActorRef,
      best: PopulationNode,
      request: Request,
      timeout: Duration)
    extends Message

  case class ResultMOS (key: Any, node: PopulationNode) extends Message

  case class AddNumEval (num: Int) extends Message

  case class NextStep () extends Message

  /* Solver Messages */

  case class Start () extends Message

  case class Select (targetId: Any, node: PopulationNode) extends Message

  case class Converged () extends Message

  case class NotConverged () extends Message

  case class Timeout () extends Message

}
