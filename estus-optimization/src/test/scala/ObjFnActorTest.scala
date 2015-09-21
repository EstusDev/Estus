package com.estus.optimization

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

  "A ObjFnActor" should
    "send back 'GimmeWork' after ! 'WorkAvailable'" in {
    actor ! WorkAvailable(probe.ref)
    probe.expectMsg(500 millis, GimmeWork) should be (GimmeWork)
  }

  "A ObjFnActor" should
    "send back 'Result' and 'GimmeWork' after ! 'Work'" in {
    def fn(param: List[Double]): Double = param.sum
    actor ! Work(probe.ref, "id", List(1.0, 2.0), fn)
    probe.expectMsg(500 millis, Result("id", 3.0))
    probe.expectMsg(500 millis, GimmeWork)
  }

  "A ObjFnActor" should
    "send back nothing after ! 'Work', if ObjFn has not finished" in {
    def fn(param: List[Double]): Double = {
      Thread.sleep(5000)
      param.sum
    }
    actor ! Work(probe.ref, "id", List(1.0, 2.0), fn)
    probe.expectNoMsg(500 millis)
  }

  "A ObjFnActor" should
    "send back 'Failure' and 'GimmeWork' after ! 'Work', if ObjFn fails" in {
    def fn(param: List[Double]): Double = 1/0
    actor ! Work(probe.ref, "id", List(1.0, 2.0), fn)
    probe.expectMsgClass(500 millis, Failure(new Throwable()).getClass)
    probe.expectMsg(500 millis, GimmeWork)
  }

}
