package com.estus.optimization

import org.scalatest.{Matchers, FlatSpec}



class MOSActorTest extends FlatSpec with Matchers {

  import akka.actor.{Props, ActorSystem}
  import akka.testkit.TestProbe
  import scala.concurrent.duration._
  import com.estus.optimization.MessageProtocol._



  "A MOSActor" should
    "[DE] send back GimmeWork after ! WorkAvailable" in {
    val system = ActorSystem()
    val probe = new TestProbe(system)
    val actor = system.actorOf(Props[MOSActor])
    val slave = system.actorOf(Props[ObjFnActor])
    actor ! WorkAvailable(probe.ref)
    probe.expectMsg(30 seconds, GimmeWork)
    system.terminate()
  }

  it should
    "[DE] send back nothing " +
      "when every WorkDE request is timed out" in {
    val system = ActorSystem()
    val probe = new TestProbe(system)
    val actor = system.actorOf(Props[MOSActor])
    val slave = system.actorOf(Props[ObjFnActor])
    def fn(p: List[Double], other: Option[Seq[Any]] = None): Double = {
      Thread.sleep(10000)
      -p.sum
    }
    val request = Request(
      objFn = fn,
      D = 2,
      LB = List.fill(2)(-1.0),
      UB = List.fill(2)(1.0),
      solverConfig = MOSConfig(NP = 10, step = 10, maxNumEval = 1000))
    val node = PopulationNode(List(-0.7, -0.7), request)
    val fdur = Duration(200, java.util.concurrent.TimeUnit.MILLISECONDS)
    actor ! WorkDE(probe.ref, slave, "id", node, request, fdur)
    probe.expectNoMsg(30 seconds)
    system.terminate()
  }

  it should
    "[DE] send back ResultMOS(key, objVal), AddNumEval(1) and GimmeWork " +
      "after ! WorkDE" in {
    val system = ActorSystem()
    val probe = new TestProbe(system)
    val actor = system.actorOf(Props[MOSActor])
    val slave = system.actorOf(Props[ObjFnActor])
    def fn(p: List[Double], other: Option[Seq[Any]] = None): Double = -p.sum
    val request = Request(
      objFn = fn,
      D = 2,
      LB = List.fill(2)(-1.0),
      UB = List.fill(2)(1.0),
      solverConfig = MOSConfig(NP = 10, step = 10, maxNumEval = 1000))
    val node = PopulationNode(List(-0.7, -0.7), request)
    actor ! WorkDE(probe.ref, slave, "id", node, request, Duration.Inf)
    probe.expectMsg(30 seconds, ResultMOS("id", node))
    probe.expectMsg(30 seconds, AddNumEval(1))
    probe.expectMsg(30 seconds, GimmeWork)
    system.terminate()
  }

  it should
    "[LS1] send back nothing " +
      "when every WorkLS request is timed out" in {
    val system = ActorSystem()
    val probe = new TestProbe(system)
    val actor = system.actorOf(Props[MOSActor])
    val slave = system.actorOf(Props[ObjFnActor])
    def fn(p: List[Double], other: Option[Seq[Any]] = None): Double = {
      Thread.sleep(10000)
      -p.sum
    }
    val request = Request(
      objFn = fn,
      D = 2,
      LB = List.fill(2)(-1.0),
      UB = List.fill(2)(1.0),
      solverConfig = MOSConfig(NP = 10, step = 10, maxNumEval = 1000))
    val best = PopulationNode(List(-0.7, -0.7), request)
    best.DSR = Some((0, 0.5 * (1.0 - (-1.0))))
    val fdur = Duration(200, java.util.concurrent.TimeUnit.MILLISECONDS)
    actor ! WorkLS(probe.ref, slave, best, request, fdur)
    probe.expectNoMsg(30 seconds)
    system.terminate()
  }

  // Path - [1/9]
  it should
    "[LS1] send back ResultMOS(None, node), AddNumEval(1) and GimmeWork " +
      "when in Path [1/9]" in {
    val system = ActorSystem()
    val probe = new TestProbe(system)
    val actor = system.actorOf(Props[MOSActor])
    val slave = system.actorOf(Props[ObjFnActor])
    def fn(p: List[Double], other: Option[Seq[Any]] = None): Double = p.sum
    val request = Request(
      objFn = fn,
      D = 2,
      LB = List.fill(2)(-1.0),
      UB = List.fill(2)(1.0),
      solverConfig = MOSConfig(NP = 10, step = 10, maxNumEval = 1000))
    val best = PopulationNode(List(-0.7, -0.7), request)
    best.objFnVal = Some(request.objFn(best.param, request.additionalParam))
    best.DSR = Some((0, 0.5 * (1.0 - (-1.0))))
    val node = PopulationNode(List(-0.85, -0.7), request)
    node.objFnVal = Some(request.objFn(node.param, request.additionalParam))
    node.DSR = Some((0, 0.5 * (1.0 - (-1.0))))
    actor ! WorkLS(probe.ref, slave, best, request, Duration.Inf)
    probe.expectMsg(30 seconds, ResultMOS(None, node))
    probe.expectMsg(30 seconds, AddNumEval(1))
    probe.expectMsg(30 seconds, GimmeWork)
    system.terminate()
  }

  // Path - [2/9]
  it should
    "[LS1] send back ResultMOS(None, node), AddNumEval(2) and GimmeWork " +
      "when in Path [2/9]" in {
    val system = ActorSystem()
    val probe = new TestProbe(system)
    val actor = system.actorOf(Props[MOSActor])
    val slave = system.actorOf(Props[ObjFnActor])
    def fn(p: List[Double], other: Option[Seq[Any]] = None): Double = -p.sum
    val request = Request(
      objFn = fn,
      D = 2,
      LB = List.fill(2)(-1.0),
      UB = List.fill(2)(1.0),
      solverConfig = MOSConfig(NP = 10, step = 10, maxNumEval = 1000))
    val best = PopulationNode(List(-0.7, -0.7), request)
    best.objFnVal = Some(request.objFn(best.param, request.additionalParam))
    best.DSR = Some((0, 0.5 * (1.0 - (-1.0))))
    val node = PopulationNode(List(-0.19999999999999996, -0.7), request)
    node.objFnVal = Some(request.objFn(node.param, request.additionalParam))
    node.DSR = Some((0, 0.5 * (1.0 - (-1.0))))
    actor ! WorkLS(probe.ref, slave, best, request, Duration.Inf)
    probe.expectMsg(30 seconds, ResultMOS(None, node))
    probe.expectMsg(30 seconds, AddNumEval(2))
    probe.expectMsg(30 seconds, GimmeWork)
    system.terminate()
  }

  // Path - [3/9]
  it should
    "[LS1] send back AddNumEval(2) and GimmeWork " +
      "when in Path [3/9]" in {
    val system = ActorSystem()
    val probe = new TestProbe(system)
    val actor = system.actorOf(Props[MOSActor])
    val slave = system.actorOf(Props[ObjFnActor])
    def fn(p: List[Double], other: Option[Seq[Any]] = None): Double = -p.sum
    val request = Request(
      objFn = fn,
      D = 2,
      LB = List.fill(2)(-1.0),
      UB = List.fill(2)(1.0),
      solverConfig = MOSConfig(NP = 10, step = 10, maxNumEval = 1000))
    val best = PopulationNode(List(-0.7, -0.7), request)
    best.objFnVal = Some(-9999.9)
    best.DSR = Some((0, 0.5 * (1.0 - (-1.0))))
    actor ! WorkLS(probe.ref, slave, best, request, Duration.Inf)
    probe.expectMsg(30 seconds, AddNumEval(2))
    probe.expectMsg(30 seconds, GimmeWork)
    system.terminate()
  }

  // Path - [4/9]
  it should
    "[LS1] send back AddNumEval(1) and GimmeWork " +
      "when in Path [4/9]" in {
    val system = ActorSystem()
    val probe = new TestProbe(system)
    val actor = system.actorOf(Props[MOSActor])
    val slave = system.actorOf(Props[ObjFnActor])
    def fn(p: List[Double], other: Option[Seq[Any]] = None): Double = -p.sum
    def ineqFunc(param: List[Double]): List[Double] = param
    val request = Request(
      objFn = fn,
      D = 2,
      LB = List.fill(2)(-1.0),
      UB = List.fill(2)(1.0),
      ineqFunc = Some(ineqFunc),
      ineqLB = Some(List(-0.9, -1.0)),
      ineqUB = Some(List(-0.7, 1.0)),
      solverConfig = MOSConfig(NP = 10, step = 10, maxNumEval = 1000))
    val best = PopulationNode(List(-0.7, -0.7), request)
    best.objFnVal = Some(1.50)
    best.DSR = Some((0, 0.5 * (1.0 - (-1.0))))
    val node = PopulationNode(List(-0.19999999999999996, -0.7), request)
    node.objFnVal = Some(request.objFn(node.param, request.additionalParam))
    node.DSR = Some((0, 0.5 * (1.0 - (-1.0))))
    actor ! WorkLS(probe.ref, slave, best, request, Duration.Inf)
    probe.expectMsg(30 seconds, AddNumEval(1))
    probe.expectMsg(30 seconds, GimmeWork)
    system.terminate()
  }

  // Path - [5/9]
  it should
    "[LS1] send back ResultMOS(None, node) and GimmeWork " +
      "when in Path [5/9]" in {
    val system = ActorSystem()
    val probe = new TestProbe(system)
    val actor = system.actorOf(Props[MOSActor])
    val slave = system.actorOf(Props[ObjFnActor])
    def fn(p: List[Double], other: Option[Seq[Any]] = None): Double = -p.sum
    def ineqFunc(param: List[Double]): List[Double] = {
      if (param == List(-0.85, -0.7))
        List(2.0)
      else
        List(3.0)
    }
    val request = Request(
      objFn = fn,
      D = 2,
      LB = List.fill(2)(-1.0),
      UB = List.fill(2)(1.0),
      ineqFunc = Some(ineqFunc),
      ineqLB = Some(List(0.0)),
      ineqUB = Some(List(0.0)),
      solverConfig = MOSConfig(NP = 10, step = 10, maxNumEval = 1000))
    val best = PopulationNode(List(-0.7, -0.7), request)
    best.objFnVal = None
    best.DSR = Some((0, 0.5 * (1.0 - (-1.0))))
    val node = PopulationNode(List(-0.85, -0.7), request)
    node.objFnVal = Some(request.objFn(node.param, request.additionalParam))
    node.DSR = Some((0, 0.5 * (1.0 - (-1.0))))
    actor ! WorkLS(probe.ref, slave, best, request, Duration.Inf)
    probe.expectMsg(30 seconds, ResultMOS(None, node))
    probe.expectMsg(30 seconds, GimmeWork)
    system.terminate()
  }

  // Path - [6/9]
  it should
    "[LS1] send back ResultMOS(None, node), AddNumEval(1) and GimmeWork " +
      "when in Path [6/9]" in {
    val system = ActorSystem()
    val probe = new TestProbe(system)
    val actor = system.actorOf(Props[MOSActor])
    val slave = system.actorOf(Props[ObjFnActor])
    def fn(p: List[Double], other: Option[Seq[Any]] = None): Double = -p.sum
    def ineqFunc(param: List[Double]): List[Double] = param
    val request = Request(
      objFn = fn,
      D = 2,
      LB = List.fill(2)(-1.0),
      UB = List.fill(2)(1.0),
      ineqFunc = Some(ineqFunc),
      ineqLB = Some(List(-0.3, -1.0)),
      solverConfig = MOSConfig(NP = 10, step = 10, maxNumEval = 1000))
    val best = PopulationNode(List(-0.7, -0.7), request)
    best.objFnVal = Some(1.50)
    best.DSR = Some((0, 0.5 * (1.0 - (-1.0))))
    val node = PopulationNode(List(-0.19999999999999996, -0.7), request)
    node.objFnVal = Some(request.objFn(node.param, request.additionalParam))
    node.DSR = Some((0, 0.5 * (1.0 - (-1.0))))
    actor ! WorkLS(probe.ref, slave, best, request, Duration.Inf)
    probe.expectMsg(30 seconds, ResultMOS(None, node))
    probe.expectMsg(30 seconds, AddNumEval(1))
    probe.expectMsg(30 seconds, GimmeWork)
    system.terminate()
  }

  // Path - [7/9]
  it should
    "[LS1] send back AddNumEval(1) and GimmeWork " +
      "when in Path [7/9]" in {
    val system = ActorSystem()
    val probe = new TestProbe(system)
    val actor = system.actorOf(Props[MOSActor])
    val slave = system.actorOf(Props[ObjFnActor])
    def fn(p: List[Double], other: Option[Seq[Any]] = None): Double = {
      if (p(0) == -0.19999999999999996)
        9999.9
      else
        -p.sum
    }
    def ineqFunc(param: List[Double]): List[Double] = param
    val request = Request(
      objFn = fn,
      D = 2,
      LB = List.fill(2)(-1.0),
      UB = List.fill(2)(1.0),
      ineqFunc = Some(ineqFunc),
      ineqLB = Some(List(-0.3, -1.0)),
      solverConfig = MOSConfig(NP = 10, step = 10, maxNumEval = 1000))
    val best = PopulationNode(List(-0.7, -0.7), request)
    best.objFnVal = Some(1.50)
    best.DSR = Some((0, 0.5 * (1.0 - (-1.0))))
    actor ! WorkLS(probe.ref, slave, best, request, Duration.Inf)
    probe.expectMsg(30 seconds, AddNumEval(1))
    probe.expectMsg(30 seconds, GimmeWork)
    system.terminate()
  }

  // Path - [8/9]
  it should
    "[LS1] send back ResultMOS(None, node) and GimmeWork " +
      "when in Path [8/9]" in {
    val system = ActorSystem()
    val probe = new TestProbe(system)
    val actor = system.actorOf(Props[MOSActor])
    val slave = system.actorOf(Props[ObjFnActor])
    def fn(p: List[Double], other: Option[Seq[Any]] = None): Double = -p.sum
    def ineqFunc(param: List[Double]): List[Double] = {
      if (param == List(-0.85, -0.7))
        List(3.0)
      else if (param == List(-0.19999999999999996, -0.7))
        List(1.0)
      else
        List(2.0)
    }
    val request = Request(
      objFn = fn,
      D = 2,
      LB = List.fill(2)(-1.0),
      UB = List.fill(2)(1.0),
      ineqFunc = Some(ineqFunc),
      ineqUB = Some(List(0.0)),
      solverConfig = MOSConfig(NP = 10, step = 10, maxNumEval = 1000))
    val best = PopulationNode(List(-0.7, -0.7), request)
    best.objFnVal = None
    best.DSR = Some((0, 0.5 * (1.0 - (-1.0))))
    val node = PopulationNode(List(-0.19999999999999996, -0.7), request)
    node.objFnVal = Some(request.objFn(node.param, request.additionalParam))
    node.DSR = Some((0, 0.5 * (1.0 - (-1.0))))
    actor ! WorkLS(probe.ref, slave, best, request, Duration.Inf)
    probe.expectMsg(30 seconds, ResultMOS(None, node))
    probe.expectMsg(30 seconds, GimmeWork)
    system.terminate()
  }

  // Path - [9/9]
  it should
    "[LS1] send back GimmeWork " +
      "when in Path [9/9]" in {
    val system = ActorSystem()
    val probe = new TestProbe(system)
    val actor = system.actorOf(Props[MOSActor])
    val slave = system.actorOf(Props[ObjFnActor])
    def fn(p: List[Double], other: Option[Seq[Any]] = None): Double = -p.sum
    def ineqFunc(param: List[Double]): List[Double] = {
      param.map(_ + 0.7)
    }
    val request = Request(
      objFn = fn,
      D = 2,
      LB = List.fill(2)(-1.0),
      UB = List.fill(2)(1.0),
      ineqFunc = Some(ineqFunc),
      ineqLB = Some(List(0.0, 0.0)),
      ineqUB = Some(List(0.0, 0.0)),
      solverConfig = MOSConfig(NP = 10, step = 10, maxNumEval = 1000))
    val best = PopulationNode(List(-0.7, -0.7), request)
    best.objFnVal = Some(1.5)
    best.DSR = Some((0, 0.5 * (1.0 - (-1.0))))
    val node = PopulationNode(List(-0.19999999999999996, -0.7), request)
    node.objFnVal = Some(request.objFn(node.param, request.additionalParam))
    node.DSR = Some((0, 0.5 * (1.0 - (-1.0))))
    actor ! WorkLS(probe.ref, slave, best, request, Duration.Inf)
    probe.expectMsg(30 seconds, GimmeWork)
    system.terminate()
  }

}
