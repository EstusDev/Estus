package com.estus.optimization

import com.estus.optimization.MessageProtocol._
import akka.actor.{ActorLogging, Actor}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}



case class ObjFnActor() extends Actor with ActorLogging {

  def receive = {

    case WorkAvailable(master) =>
      master ! GimmeWork

    case Work(master, id, p, fn) =>
      lazy val f = Future { fn(p) }
      f onComplete {
        case Success(v) =>
          master ! Result(id, v)
          master ! GimmeWork
        case Failure(cause) =>
          master ! Failure(cause)
          master ! GimmeWork
      }

  }

}
