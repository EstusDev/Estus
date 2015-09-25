package com.estus.optimization

import org.scalatest.{FlatSpec, Matchers}



class SolverDETest extends FlatSpec with Matchers {

  import akka.actor.{ActorSystem, Props}
  import akka.routing.RoundRobinPool
  import com.estus.optimization.MessageProtocol.Start
  import scala.concurrent.duration.FiniteDuration

  val ds: List[Int] = List(2, 10)
  val system = ActorSystem()
  val workerRouter = system.actorOf(RoundRobinPool(ds.size).props(Props[ObjFnActor]))
  val journal = Journal()
  val config = DiffEvoConfig(
    NP = 50,
    F = None,
    Cr = None,
    Er = 0.3,
    Ar = 0.1,
    mutationStrategy = "current-to-best",
    constStrategy = "rank",
    tolRel = 1e-8,
    tolStep = 1000,
    maxNumEval = 3000000,
    logTrace = false)



  "A Ackley's Function" should
    "produce global minimum 0.0 +- 1e-3 at [0.0 +- 1e-3]^D in 3D seconds" in {
    val keys: List[String] = ds.map(D => {
      val configNew = config.copy(tolRel = 1e-5)
      val request = BenchmarkFunctions(D, configNew).ackleyRequest
      val key = journal.registerRow(request)
      system.actorOf(
        Props(new SolverDE(
          key,
          request,
          workerRouter,
          journal,
          Some(new FiniteDuration(3*D, java.util.concurrent.TimeUnit.SECONDS))
        )),
        name = s"ackley-d$D") ! Start
      key
    })
    val solutions = keys.map(k => {
      while (journal.retrieveRow(k).get.solution.isEmpty) {
        Thread.sleep(1000)
      }
      journal.retrieveRow(k).get.solution.get
    })
    val optimums = solutions.flatten(_.objValue)
    val params = solutions.flatten(_.param)
    optimums should have size ds.size
    optimums.foreach(x => x should be (0.0 +- 1e-3))
    params.foreach(x => x should be (0.0 +- 1e-3))
  }



  "A Rastrigin's Function" should
    "produce global minimum 0.0 +- 1e-3 at [0.0 +- 1e-3]^D in 3D seconds" in {
    val keys: List[String] = ds.map(D => {
      val configNew = config.copy(tolRel = 1e-5)
      val request = BenchmarkFunctions(D, configNew).rastriginRequest
      val key = journal.registerRow(request)
      system.actorOf(
        Props(new SolverDE(
          key,
          request,
          workerRouter,
          journal,
          Some(new FiniteDuration(3*D, java.util.concurrent.TimeUnit.SECONDS))
        )),
        name = s"rastrigin-d$D") ! Start
      key
    })
    val solutions = keys.map(k => {
      while (journal.retrieveRow(k).get.solution.isEmpty) {
        Thread.sleep(1000)
      }
      journal.retrieveRow(k).get.solution.get
    })
    val optimums = solutions.flatten(_.objValue)
    val params = solutions.flatten(_.param)
    optimums should have size ds.size
    optimums.foreach(x => x should be (0.0 +- 1e-3))
    params.foreach(x => x should be (0.0 +- 1e-3))
  }



  "A Schwefel's Function" should
    "produce global minimum 0.0 +- 1e-3 at [420.9687 +- 1e-3]^D in 3D seconds" in {
    val keys: List[String] = ds.map(D => {
      val request = BenchmarkFunctions(D, config).schwefelRequest
      val key = journal.registerRow(request)
      system.actorOf(
        Props(new SolverDE(
          key,
          request,
          workerRouter,
          journal,
          Some(new FiniteDuration(3*D, java.util.concurrent.TimeUnit.SECONDS))
        )),
        name = s"schwefel-d$D") ! Start
      key
    })
    val solutions = keys.map(k => {
      while (journal.retrieveRow(k).get.solution.isEmpty) {
        Thread.sleep(1000)
      }
      journal.retrieveRow(k).get.solution.get
    })
    val optimums = solutions.flatten(_.objValue)
    val params = solutions.flatten(_.param)
    optimums should have size ds.size
    optimums.foreach(x => x should be (0.0 +- 1e-3))
    params.foreach(x => x should be (420.9687 +- 1e-3))
  }



  "A Sphere Function" should
    "produce global minimum 0.0 +- 1e-3 at [0.0 +- 1e-3]^D in 3D seconds" in {
    val keys: List[String] = ds.map(D => {
      val request = BenchmarkFunctions(D, config).sphereRequest
      val key = journal.registerRow(request)
      system.actorOf(
        Props(new SolverDE(
          key,
          request,
          workerRouter,
          journal,
          Some(new FiniteDuration(3*D, java.util.concurrent.TimeUnit.SECONDS))
        )),
        name = s"sphere-d$D") ! Start
      key
    })
    val solutions = keys.map(k => {
      while (journal.retrieveRow(k).get.solution.isEmpty) {
        Thread.sleep(1000)
      }
      journal.retrieveRow(k).get.solution.get
    })
    val optimums = solutions.flatten(_.objValue)
    val params = solutions.flatten(_.param)
    optimums should have size ds.size
    optimums.foreach(x => x should be (0.0 +- 1e-3))
    params.foreach(x => x should be (0.0 +- 1e-3))
  }



  "A Rotated Hyper-Ellipsoid Function" should
    "produce global minimum 0.0 +- 1e-3 at [0.0 +- 1e-3]^D in 3D seconds" in {
    val keys: List[String] = ds.map(D => {
      val request = BenchmarkFunctions(D, config).ellipsoidRequest
      val key = journal.registerRow(request)
      system.actorOf(
        Props(new SolverDE(
          key,
          request,
          workerRouter,
          journal,
          Some(new FiniteDuration(3*D, java.util.concurrent.TimeUnit.SECONDS))
        )),
        name = s"ellipsoid-d$D") ! Start
      key
    })
    val solutions = keys.map(k => {
      while (journal.retrieveRow(k).get.solution.isEmpty) {
        Thread.sleep(1000)
      }
      journal.retrieveRow(k).get.solution.get
    })
    val optimums = solutions.flatten(_.objValue)
    val params = solutions.flatten(_.param)
    optimums should have size ds.size
    optimums.foreach(x => x should be (0.0 +- 1e-3))
    params.foreach(x => x should be (0.0 +- 1e-3))
  }



  "A Zakharov Function" should
    "produce global minimum 0.0 +- 1e-3 at [0.0 +- 1e-3]^D in 3D seconds" in {
    val keys: List[String] = ds.map(D => {
      val request = BenchmarkFunctions(D, config).zakharovRequest
      val key = journal.registerRow(request)
      system.actorOf(
        Props(new SolverDE(
          key,
          request,
          workerRouter,
          journal,
          Some(new FiniteDuration(3*D, java.util.concurrent.TimeUnit.SECONDS))
        )),
        name = s"zakharov-d$D") ! Start
      key
    })
    val solutions = keys.map(k => {
      while (journal.retrieveRow(k).get.solution.isEmpty) {
        Thread.sleep(1000)
      }
      journal.retrieveRow(k).get.solution.get
    })
    val optimums = solutions.flatten(_.objValue)
    val params = solutions.flatten(_.param)
    optimums should have size ds.size
    optimums.foreach(x => x should be (0.0 +- 1e-3))
    params.foreach(x => x should be (0.0 +- 1e-3))
  }



  "A Rosenbrock Function" should
    "have global minimum 0.0 +- 1e-3 at [1.0 +- 1e-3]^D in 3D seconds" in {
    val keys: List[String] = ds.map(D => {
      val request = BenchmarkFunctions(D, config).rosenbrockRequest
      val key = journal.registerRow(request)
      system.actorOf(
        Props(new SolverDE(
          key,
          request,
          workerRouter,
          journal,
          Some(new FiniteDuration(3*D, java.util.concurrent.TimeUnit.SECONDS))
        )),
        name = s"rosenbrock-d$D") ! Start
      key
    })
    val solutions = keys.map(k => {
      while (journal.retrieveRow(k).get.solution.isEmpty) {
        Thread.sleep(1000)
      }
      journal.retrieveRow(k).get.solution.get
    })
    val optimums = solutions.flatten(_.objValue)
    val params = solutions.flatten(_.param)
    optimums should have size ds.size
    optimums.foreach(x => x should be (0.0 +- 1e-3))
    params.foreach(x => x should be (1.0 +- 1e-3))
  }



  "A Deb Function" should
    "produce global minimum 13.59085 +- 1e-3 at (2.246826 +- 1e-3, 2.381865 +- 1e-3) in 2 seconds" in {
    val request = BenchmarkFunctions(2, config).debRequest
    val key = journal.registerRow(request)
    system.actorOf(
      Props(new SolverDE(
        key,
        request,
        workerRouter,
        journal,
        Some(new FiniteDuration(2, java.util.concurrent.TimeUnit.SECONDS))
      )),
      name = s"deb-d2") ! Start
    while (journal.retrieveRow(key).get.solution.isEmpty) {
      Thread.sleep(1000)
    }
    val optimum = journal.retrieveRow(key).get.solution.get.objValue.get
    val param = journal.retrieveRow(key).get.solution.get.param
    optimum should be (13.59085 +- 1e-3)
    param.head should be (2.246826 +- 1e-3)
    param.last should be (2.381865 +- 1e-3)
    request.ineqFunc.get(param).zip(request.ineqLB.get).foreach(tup => {
      tup._1 should be > tup._2
    })
  }

}
