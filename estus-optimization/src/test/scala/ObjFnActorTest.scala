package com.estus.optimization

import java.util.concurrent.TimeoutException

import org.scalatest.{Matchers, FlatSpec}



class ObjFnActorTest extends FlatSpec with Matchers {

  import akka.testkit.TestProbe
  import scala.util.Failure
  import scala.concurrent.duration._
  import akka.actor.{ActorSystem, Props}
  import com.estus.optimization.MessageProtocol.{WorkAvailable, Work, GimmeWork, Result}

  val system = ActorSystem()
  val probe = new TestProbe(system)
  val actor = system.actorOf(Props[ObjFnActor])
  val fdur = Duration.Inf

  "A ObjFnActor" should
    "send back 'GimmeWork' after ! 'WorkAvailable'" in {
    actor ! WorkAvailable(probe.ref)
    probe.expectMsg(1000 millis, GimmeWork) should be (GimmeWork)
  }

  "A ObjFnActor" should
    "send back 'Result' and 'GimmeWork' after ! 'Work'" in {
    def fn(param: List[Double]): Double = param.sum
    actor ! Work(probe.ref, "id", List(1.0, 2.0), fn, fdur)
    probe.expectMsg(1000 millis, Result("id", 3.0))
    probe.expectMsg(1000 millis, GimmeWork)
  }

  "A ObjFnActor" should
    "send back 'Failure(Throwable)' and 'GimmeWork' " +
      "after ! 'Work', if ObjFn fails" in {
    def fn(param: List[Double]): Double = 1/0
    actor ! Work(probe.ref, "id", List(1.0, 2.0), fn, fdur)
    probe.expectMsgClass(1000 millis, Failure(new Throwable()).getClass)
    probe.expectMsg(1000 millis, GimmeWork)
  }

  "A ObjFnActor" should
    "send back nothing after ! 'Work', if ObjFn has not finished" in {
    def fn(param: List[Double]): Double = {
      Thread.sleep(10000)
      param.sum
    }
    val fdurShort = Duration(2000, java.util.concurrent.TimeUnit.MILLISECONDS)
    actor ! Work(probe.ref, "id", List(1.0, 2.0), fn, fdurShort)
    probe.expectNoMsg(1000 millis)
  }

  "A ObjFnActor" should
    "send back 'Failure(TimeoutException)' and 'GimmeWork' " +
      "after ! 'Work', if ObjFn is timed out" in {
    def fn(param: List[Double]): Double = {
      Thread.sleep(10000)
      param.sum
    }
    val fdurShort = Duration(200, java.util.concurrent.TimeUnit.MILLISECONDS)
    actor ! Work(probe.ref, "id", List(1.0, 2.0), fn, fdurShort)
    probe.expectMsgClass(1000 millis, Failure(new TimeoutException()).getClass)
    probe.expectMsg(1000 millis, GimmeWork)
  }

}
