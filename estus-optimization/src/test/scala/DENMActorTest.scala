package com.estus.optimization

import org.scalatest.{Matchers, FlatSpec}



class DENMActorTest extends FlatSpec with Matchers {

  import akka.actor.{Props, ActorSystem}
  import akka.testkit.TestProbe
  import scala.concurrent.duration._
  import com.estus.optimization.MessageProtocol._

  val system = ActorSystem()
  val probe = new TestProbe(system)
  val actor = system.actorOf(Props[DENMActor])
  val slave = system.actorOf(Props[ObjFnActor])



  "A DENMActor" should
    "send back 'GimmeWorkDENM' after ! 'WorkAvailable'" in {
    actor ! WorkAvailable(probe.ref)
    probe.expectMsg(500 millis, GimmeWorkDENM)
  }

  "A DENMActor" should
    "send back nothing " +
      "when every ObjFn Evaluation is timed out" in {
    def fn(p: List[Double], other: Option[Seq[Any]] = None): Double = {
      Thread.sleep(10000)
      -p.sum
    }
    val request = Request(
      objFn = fn,
      D = 2,
      LB = List.fill(2)(-1.0),
      UB = List.fill(2)(1.0),
      solverConfig = DEConfig(NP = 10))
    val node = PopulationNode(List(-0.7, -0.7), request)
    val fdur = Duration(200, java.util.concurrent.TimeUnit.MILLISECONDS)
    actor ! DENelderMead(probe.ref, slave, "id", node, request, fdur)
    probe.expectNoMsg(500 millis)
  }

  "A DENMActor" should
    "send back 'UpdatePopulation (key, objVal)', 'AddNumEval(1)' and 'GimmeWorkDENM' " +
      "after ! DENelderMead" in {
    def fn(p: List[Double], other: Option[Seq[Any]] = None): Double = -p.sum
    val request = Request(
      objFn = fn,
      D = 2,
      LB = List.fill(2)(-1.0),
      UB = List.fill(2)(1.0),
      solverConfig = DEConfig(NP = 10))
    val node = PopulationNode(List(-0.7, -0.7), request)
    val fdur = Duration(200, java.util.concurrent.TimeUnit.MILLISECONDS)
    actor ! DENelderMead(probe.ref, slave, "id", node, request, fdur)
    probe.expectMsg(500 millis, UpdatePopulation("id", 1.4))
    probe.expectMsg(500 millis, AddNumEval(1))
    probe.expectMsg(500 millis, GimmeWorkDENM)
  }

}
