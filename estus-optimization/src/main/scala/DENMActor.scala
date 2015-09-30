package com.estus.optimization

import com.estus.optimization.MessageProtocol._
import akka.actor.{ActorRef, ActorLogging, Actor}

import scala.util.Failure



case class DENMActor () extends Actor with ActorLogging {

  case class Value (master: ActorRef, id: Any)

  var EvalMap = scala.collection.immutable.Map.empty[String, Value]

  def receive = {

    case WorkAvailable(master) =>
      master ! GimmeWorkDENM

    case DENelderMead(master, slave, id, node, request, to) =>
      val key = java.util.UUID.randomUUID.toString
      val value = Value(master, id)
      val objFn = request.objFn(_: List[Double], request.additionalParam)
      EvalMap = EvalMap + (key -> value)
      slave ! Work(self, key, node.param, objFn, to)

    case Result(k, v) =>
      val key = k.asInstanceOf[String]
      val objFnVal = v.toDouble
      EvalMap.get(key) match {
        case Some(value) =>
          value.master ! UpdatePopulation(value.id, objFnVal)
          value.master ! AddNumEval(1)
          value.master ! GimmeWorkDENM
        case _ =>
      }
      EvalMap = EvalMap - key

    case Failure(cause) =>
      log.info(cause.toString)

  }

}
