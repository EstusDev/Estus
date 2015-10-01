package com.estus.optimization

import java.util.concurrent.TimeoutException

import org.scalatest.{Matchers, FlatSpec}



class ObjFnActorTest extends FlatSpec with Matchers {

  import akka.testkit.TestProbe
  import scala.util.Failure
  import scala.concurrent.duration._
  import akka.actor.{ActorSystem, Props}
  import com.estus.optimization.MessageProtocol.{WorkAvailable, Work, GimmeWork, Result}



  "An ObjFnActor" should
    "send back GimmeWork after ! WorkAvailable" in {
    val system = ActorSystem()
    val probe = new TestProbe(system)
    val actor = system.actorOf(Props[ObjFnActor])
    actor ! WorkAvailable(probe.ref)
    probe.expectMsg(30 seconds, GimmeWork) should be (GimmeWork)
    system.terminate()
  }

  it should
    "send back Result and GimmeWork after ! Work" in {
    val system = ActorSystem()
    val probe = new TestProbe(system)
    val actor = system.actorOf(Props[ObjFnActor])
    def fn(param: List[Double]): Double = param.sum
    actor ! Work(probe.ref, "id", List(1.0, 2.0), fn, Duration.Inf)
    probe.expectMsg(30 seconds, Result("id", 3.0))
    probe.expectMsg(30 seconds, GimmeWork)
    system.terminate()
  }

  it should
    "send back Failure(Throwable) and GimmeWork " +
      "after ! Work, if ObjFn fails" in {
    val system = ActorSystem()
    val probe = new TestProbe(system)
    val actor = system.actorOf(Props[ObjFnActor])
    def fn(param: List[Double]): Double = 1/0
    actor ! Work(probe.ref, "id", List(1.0, 2.0), fn, Duration.Inf)
    probe.expectMsgClass(30 seconds, Failure(new Throwable()).getClass)
    probe.expectMsg(30 seconds, GimmeWork)
    system.terminate()
  }

  it should
    "send back nothing after ! Work, if ObjFn has not finished" in {
    val system = ActorSystem()
    val probe = new TestProbe(system)
    val actor = system.actorOf(Props[ObjFnActor])
    def fn(param: List[Double]): Double = {
      Thread.sleep(10000)
      param.sum
    }
    actor ! Work(probe.ref, "id", List(1.0, 2.0), fn, Duration.Inf)
    probe.expectNoMsg(1 seconds)
    system.terminate()
  }

  it should
    "send back Failure(TimeoutException) and GimmeWork " +
      "after ! Work, if ObjFn is timed out" in {
    val system = ActorSystem()
    val probe = new TestProbe(system)
    val actor = system.actorOf(Props[ObjFnActor])
    def fn(param: List[Double]): Double = {
      Thread.sleep(10000)
      param.sum
    }
    val fdur = Duration(200, java.util.concurrent.TimeUnit.MILLISECONDS)
    actor ! Work(probe.ref, "id", List(1.0, 2.0), fn, fdur)
    probe.expectMsgClass(30 seconds, Failure(new TimeoutException()).getClass)
    probe.expectMsg(30 seconds, GimmeWork)
    system.terminate()
  }

}
