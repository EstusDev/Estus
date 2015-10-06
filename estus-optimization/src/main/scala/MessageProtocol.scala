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

  case class DENelderMead (
      master: ActorRef,
      slave: ActorRef,
      key: Any,
      node: PopulationNode,
      request: Request,
      to: Duration
    ) extends Message

  case class LocalSearch (
      master: ActorRef,
      slave: ActorRef,
      best: PopulationNode,
      d: Int,
      request: Request,
      timeout: Duration)
    extends Message

  case class UpdatePopulation (key: Any, node: PopulationNode) extends Message

  case class UpdateBestNode (best: PopulationNode) extends Message

  case class AddNumEval (num: Int) extends Message

  /* FSM Messages */

  case class Start () extends Message

  case class Initiate () extends Message

  case class Evolve () extends Message

  case class Select (targetId: Any, node: PopulationNode) extends Message

  case class Converged () extends Message

  case class NotConverged () extends Message

  case class Timeout () extends Message

}
