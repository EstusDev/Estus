package com.estus.optimization

import com.estus.optimization.MessageProtocol._
import akka.actor.{ActorLogging, Actor}

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}



case class ObjFnActor() extends Actor with ActorLogging {

  def receive = {

    case WorkAvailable(master) =>
      master ! GimmeWork

    case Work(master, id, p, fn, to) =>
      lazy val f = Future { fn(p) }
      Try(Await.result(f, to)) match {
        case Success(v) =>
          master ! Result(id, v)
          master ! GimmeWork
        case Failure(e) =>
          master ! Failure(e)
          master ! GimmeWork
      }

  }

}
