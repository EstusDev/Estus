package com.estus.optimization

import com.estus.optimization.MessageProtocol._
import akka.actor.{ActorLogging, Actor}
import akka.pattern.after

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{TimeoutException, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}



case class ObjFnActor() extends Actor with ActorLogging {

  def receive = {

    case WorkAvailable(master) =>
      master ! GimmeWork

    case Work(master, id, p, fn, to) =>
      lazy val f = if (to.isFinite) {
        val fdur = FiniteDuration(
          to.toMillis,
          java.util.concurrent.TimeUnit.MILLISECONDS)
        lazy val t = after(duration = fdur, using = context.system.scheduler)(
          Future.failed(
            new TimeoutException(s"${Work(master, id, p, fn, to)} timed out!")))
        Future firstCompletedOf Seq(Future { fn(p) }, t)
      } else {
        Future { fn(p) }
      }
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
