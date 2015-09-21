package com.estus.optimization

import akka.actor.ActorRef



object MessageProtocol {

  sealed trait Message

  case class WorkAvailable (master: ActorRef) extends Message

  case class GimmeWork () extends Message

  case class Work (
      master: ActorRef,
      key: Any,
      param: List[Double],
      fn: (List[Double]) => Double)
    extends Message

  case class Result (key: Any, objVal: Double) extends Message

  case class Start () extends Message

  case class Initiate () extends Message

  case class Evolve () extends Message

  case class Select (targetId: Any, node: PopulationNode) extends Message

  case class Converged () extends Message

  case class NotConverged () extends Message

  case class Timeout () extends Message

}
