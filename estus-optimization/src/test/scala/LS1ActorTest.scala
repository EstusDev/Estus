package com.estus.optimization

import org.scalatest.{Matchers, FlatSpec}



class LS1ActorTest extends FlatSpec with Matchers {

  import akka.testkit.TestProbe
  import scala.concurrent.duration._
  import akka.actor.{ActorSystem, Props}
  import com.estus.optimization.MessageProtocol._

  val system = ActorSystem()
  val probe = new TestProbe(system)
  val actor = system.actorOf(Props[LS1Actor])
  val slave = system.actorOf(Props[ObjFnActor])



  "A LS1Actor" should
    "send back 'GimmeWorkLS' after ! 'WorkAvailable'" in {
    actor ! WorkAvailable(probe.ref)
    probe.expectMsg(1000 millis, GimmeWorkLS)
  }

  "A LS1Actor" should
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
    val best = PopulationNode(List(-0.7, -0.7), request)
    val fdur = Duration(200, java.util.concurrent.TimeUnit.MILLISECONDS)
    actor ! LocalSearch(probe.ref, slave, best, 0, request, fdur)
    probe.expectNoMsg(1000 millis)
  }

  // Path - [1/9]
  "A LS1Actor" should
    "send back 'UpdateBestNode(node)', 'AddNumEval(1)' and 'GimmeWorkLS' " +
      "when in Path [1/9]" in {
    def fn(p: List[Double], other: Option[Seq[Any]] = None): Double = p.sum
    val request = Request(
      objFn = fn,
      D = 2,
      LB = List.fill(2)(-1.0),
      UB = List.fill(2)(1.0),
      solverConfig = DEConfig(NP = 10))
    val best = PopulationNode(List(-0.7, -0.7), request)
    best.objFnVal = Some(request.objFn(best.param, request.additionalParam))
    val node = PopulationNode(List(-0.85, -0.7), request)
    node.objFnVal = Some(request.objFn(node.param, request.additionalParam))
    val fdur = Duration(200, java.util.concurrent.TimeUnit.MILLISECONDS)
    actor ! LocalSearch(probe.ref, slave, best, 0, request, fdur)
    probe.expectMsg(1000 millis, UpdateBestNode(node))
    probe.expectMsg(1000 millis, AddNumEval(1))
    probe.expectMsg(1000 millis, GimmeWorkLS)
  }

  // Path - [2/9]
  "A LS1Actor" should
    "send back 'UpdateBestNode(node)', 'AddNumEval(2)' and 'GimmeWorkLS' " +
      "when in Path [2/9]" in {
    def fn(p: List[Double], other: Option[Seq[Any]] = None): Double = -p.sum
    val request = Request(
      objFn = fn,
      D = 2,
      LB = List.fill(2)(-1.0),
      UB = List.fill(2)(1.0),
      solverConfig = DEConfig(NP = 10))
    val best = PopulationNode(List(-0.7, -0.7), request)
    best.objFnVal = Some(request.objFn(best.param, request.additionalParam))
    val node = PopulationNode(List(-0.19999999999999996, -0.7), request)
    node.objFnVal = Some(request.objFn(node.param, request.additionalParam))
    val fdur = Duration(200, java.util.concurrent.TimeUnit.MILLISECONDS)
    actor ! LocalSearch(probe.ref, slave, best, 0, request, fdur)
    probe.expectMsg(1000 millis, UpdateBestNode(node))
    probe.expectMsg(1000 millis, AddNumEval(2))
    probe.expectMsg(1000 millis, GimmeWorkLS)
  }

  // Path - [3/9]
  "A LS1Actor" should
    "send back 'AddNumEval(2)' and 'GimmeWorkLS' " +
      "when in Path [3/9]" in {
    def fn(p: List[Double], other: Option[Seq[Any]] = None): Double = -p.sum
    val request = Request(
      objFn = fn,
      D = 2,
      LB = List.fill(2)(-1.0),
      UB = List.fill(2)(1.0),
      solverConfig = DEConfig(NP = 10))
    val best = PopulationNode(List(-0.7, -0.7), request)
    best.objFnVal = Some(-9999.9)
    val fdur = Duration(200, java.util.concurrent.TimeUnit.MILLISECONDS)
    actor ! LocalSearch(probe.ref, slave, best, 0, request, fdur)
    probe.expectMsg(1000 millis, AddNumEval(2))
    probe.expectMsg(1000 millis, GimmeWorkLS)
  }

  // Path - [4/9]
  "A LS1Actor" should
    "send back 'AddNumEval(1)' and 'GimmeWorkLS' " +
      "when in Path [4/9]" in {
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
      solverConfig = DEConfig(NP = 10))
    val best = PopulationNode(List(-0.7, -0.7), request)
    best.objFnVal = Some(1.50)
    val node = PopulationNode(List(-0.19999999999999996, -0.7), request)
    node.objFnVal = Some(request.objFn(node.param, request.additionalParam))
    val fdur = Duration(200, java.util.concurrent.TimeUnit.MILLISECONDS)
    actor ! LocalSearch(probe.ref, slave, best, 0, request, fdur)
    probe.expectMsg(1000 millis, AddNumEval(1))
    probe.expectMsg(1000 millis, GimmeWorkLS)
  }

  // Path - [5/9]
  "A LS1Actor" should
    "send back 'UpdateBestNode(node)' and 'GimmeWorkLS' " +
      "when in Path [5/9]" in {
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
      solverConfig = DEConfig(NP = 10))
    val best = PopulationNode(List(-0.7, -0.7), request)
    best.objFnVal = None
    val node = PopulationNode(List(-0.85, -0.7), request)
    node.objFnVal = Some(request.objFn(node.param, request.additionalParam))
    val fdur = Duration(200, java.util.concurrent.TimeUnit.MILLISECONDS)
    actor ! LocalSearch(probe.ref, slave, best, 0, request, fdur)
    probe.expectMsg(1000 millis, UpdateBestNode(node))
    probe.expectMsg(1000 millis, GimmeWorkLS)
  }

  // Path - [6/9]
  "A LS1Actor" should
    "send back 'UpdateBestNode(node)', 'AddNumEval(1)' and 'GimmeWorkLS' " +
      "when in Path [6/9]" in {
    def fn(p: List[Double], other: Option[Seq[Any]] = None): Double = -p.sum
    def ineqFunc(param: List[Double]): List[Double] = param
    val request = Request(
      objFn = fn,
      D = 2,
      LB = List.fill(2)(-1.0),
      UB = List.fill(2)(1.0),
      ineqFunc = Some(ineqFunc),
      ineqLB = Some(List(-0.3, -1.0)),
      solverConfig = DEConfig(NP = 10))
    val best = PopulationNode(List(-0.7, -0.7), request)
    best.objFnVal = Some(1.50)
    val node = PopulationNode(List(-0.19999999999999996, -0.7), request)
    node.objFnVal = Some(request.objFn(node.param, request.additionalParam))
    val fdur = Duration(200, java.util.concurrent.TimeUnit.MILLISECONDS)
    actor ! LocalSearch(probe.ref, slave, best, 0, request, fdur)
    probe.expectMsg(1000 millis, UpdateBestNode(node))
    probe.expectMsg(1000 millis, AddNumEval(1))
    probe.expectMsg(1000 millis, GimmeWorkLS)
  }

  // Path - [7/9]
  "A LS1Actor" should
    "send back 'AddNumEval(1)' and 'GimmeWorkLS' " +
      "when in Path [7/9]" in {
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
      solverConfig = DEConfig(NP = 10))
    val best = PopulationNode(List(-0.7, -0.7), request)
    best.objFnVal = Some(1.50)
    val fdur = Duration(200, java.util.concurrent.TimeUnit.MILLISECONDS)
    actor ! LocalSearch(probe.ref, slave, best, 0, request, fdur)
    probe.expectMsg(1000 millis, AddNumEval(1))
    probe.expectMsg(1000 millis, GimmeWorkLS)
  }

  // Path - [8/9]
  "A LS1Actor" should
    "send back 'UpdateBestNode(node)' and 'GimmeWorkLS' " +
      "when in Path [8/9]" in {
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
      solverConfig = DEConfig(NP = 10))
    val best = PopulationNode(List(-0.7, -0.7), request)
    best.objFnVal = None
    val node = PopulationNode(List(-0.19999999999999996, -0.7), request)
    node.objFnVal = Some(request.objFn(node.param, request.additionalParam))
    val fdur = Duration(200, java.util.concurrent.TimeUnit.MILLISECONDS)
    actor ! LocalSearch(probe.ref, slave, best, 0, request, fdur)
    probe.expectMsg(1000 millis, UpdateBestNode(node))
    probe.expectMsg(1000 millis, GimmeWorkLS)
  }

  // Path - [9/9]
  "A LS1Actor" should
    "send back 'GimmeWorkLS' " +
      "when in Path [9/9]" in {
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
      solverConfig = DEConfig(NP = 10))
    val best = PopulationNode(List(-0.7, -0.7), request)
    best.objFnVal = Some(1.5)
    val node = PopulationNode(List(-0.19999999999999996, -0.7), request)
    node.objFnVal = Some(request.objFn(node.param, request.additionalParam))
    val fdur = Duration(200, java.util.concurrent.TimeUnit.MILLISECONDS)
    actor ! LocalSearch(probe.ref, slave, best, 0, request, fdur)
    probe.expectMsg(1000 millis, GimmeWorkLS)
  }

}
