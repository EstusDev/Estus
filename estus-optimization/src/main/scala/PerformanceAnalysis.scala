package com.estus.optimization

import akka.actor.{ActorSystem, ActorRef, Props}
import akka.routing.RoundRobinPool
import com.estus.optimization.MessageProtocol.Start

import scala.concurrent.duration.Duration
import scala.math._



case class BenchmarkFunctions (D: Int, solverConfig: SolverConfig, desc: Option[String]) {

  /***
    GROUP 1: Many Local Minima
    ***/

  /*
    The Ackley’s Function
    Space: [-32.768, 32.768]^D
    Global Optimum: 0
    Solution: [0]^D
    http://www.sfu.ca/~ssurjano/ackley.html
  */
  def ackleyFunc(param: List[Double], other: Option[Seq[Any]] = None): Double = {
    val sum1 = param.map(x => x * x).sum
    val sum2 = param.map(x => cos(2 * Pi * x)).sum
    -20 * exp(-0.2 * sqrt(sum1 / param.size)) - exp(sum2 / param.size) + 20 + exp(1.0)
  }

  val ackleyRequest = Request(
    objFn = ackleyFunc,
    D = D,
    LB = List.fill(D)(-32.768),
    UB = List.fill(D)(32.768),
    //initialParam = Some(List.fill(D)(0.0 + Random.nextDouble * (0.1))),
    solverConfig = solverConfig, description = desc)

  /*
    The Rastrigin’s Function
    Space: [−5.12, 5.12]^D
    Global Optimum: 0
    Solution: [0]^D
    http://www.sfu.ca/~ssurjano/rastr.html
  */
  def rastriginFunc(param: List[Double], other: Option[Seq[Any]] = None): Double = {
    param.map(x => x * x - 10 * cos(2 * Pi * x)).sum + 10 * param.size
  }

  val rastriginRequest = Request(
    objFn = rastriginFunc,
    D = D,
    LB = List.fill(D)(-5.12),
    UB = List.fill(D)(5.12),
    //initialParam = Some(List.fill(D)(0.0 + Random.nextDouble * (0.1))),
    solverConfig = solverConfig, description = desc)

  /*
    The Schwefel’s Function
    Space: [−500.0, 500.0]^D
    Global Optimum: 0
    Solution: [420.9687]^D
    http://www.sfu.ca/~ssurjano/schwef.html
  */
  def schwefelFunc(param: List[Double], other: Option[Seq[Any]] = None): Double = {
    418.9829 * param.size - param.map(x => x * sin(sqrt(abs(x)))).sum
  }

  val schwefelRequest = Request(
    objFn = schwefelFunc,
    D = D,
    LB = List.fill(D)(-500.0),
    UB = List.fill(D)(500.0),
    //initialParam = Some(List.fill(D)(420.5 + Random.nextDouble * (420.0 - 419.0))),
    solverConfig = solverConfig, description = desc)



  /***
    GROUP 2: Bowl-Shaped
    ***/

  /*
    The Sphere Function
    Space: [−5.12, 5.12]^D
    Global Optimum: 0
    Solution: [0]^D
    http://www.sfu.ca/~ssurjano/spheref.html
  */
  def sphereFunc(param: List[Double], other: Option[Seq[Any]] = None): Double = {
    param.map(x => x * x).sum
  }

  val sphereRequest = Request(
    objFn = sphereFunc,
    D = D,
    LB = List.fill(D)(-5.12),
    UB = List.fill(D)(5.12),
    //initialParam = Some(List.fill(D)(0.0 + Random.nextDouble * (0.1))),
    solverConfig = solverConfig, description = desc)

  /*
    The Rotated Hyper-Ellipsoid Function
    Space: [-65.536, 65.536]^D
    Global Optimum: 0
    Solution: [0]^D
    http://www.sfu.ca/~ssurjano/rothyp.html
  */
  def ellipsoidFunc(param: List[Double], other: Option[Seq[Any]] = None): Double = {
    param.zipWithIndex.map(tup => {
      val x = tup._1
      val i = tup._2 + 1
      (param.map(x => x * x) take i).sum
    }).sum
  }

  val ellipsoidRequest = Request(
    objFn = ellipsoidFunc,
    D = D,
    LB = List.fill(D)(-65.536),
    UB = List.fill(D)(65.536),
    //initialParam = Some(List.fill(D)(0.0 + Random.nextDouble * (0.1))),
    solverConfig = solverConfig, description = desc)



  /***
    GROUP 3: Plate-Shaped
    ***/

  /*
    The Zakharov Function
    Space: [-5, 10]^D
    Global Optimum: 0
    Solution: [0]^D
    http://www.sfu.ca/~ssurjano/zakharov.html
  */
  def zakharovFunc(param: List[Double], other: Option[Seq[Any]] = None): Double = {
    val sum1 = param.map(x => x * x).sum
    val sum2 = param.zipWithIndex.map(tup => {
      val x = tup._1
      val i = tup._2.toDouble
      0.5 * (i + 1.0) * x
    }).sum
    sum1 + sum2 * sum2 + sum2 * sum2 * sum2 * sum2
  }

  val zakharovRequest = Request(
    objFn = zakharovFunc,
    D = D,
    LB = List.fill(D)(-5.0),
    UB = List.fill(D)(10.0),
    //initialParam = Some(List.fill(D)(0.0 + Random.nextDouble * (0.1))),
    solverConfig = solverConfig, description = desc)



  /***
    GROUP 4: Valley-Shaped
    ***/

  /*
    The Rosenbrock’s Function
    Space: [−5.0, 10.0]^D
    Global Optimum: 0
    Solution: [1.0]^D
    http://www.sfu.ca/~ssurjano/rosen.html
  */
  def rosenbrockFunc(param: List[Double], other: Option[Seq[Any]] = None): Double = {
    if (param.size <= 1) {
      throw new IllegalArgumentException(
        s"D must be >= 2 (D = ${param.size}).")
    }
    var v = 0.0
    for (i <- 0 until param.length-1)
      v += pow((1 - param(i)), 2.0) +
        100 * pow((param(i+1) - math.pow(param(i), 2.0)), 2.0)
    v
  }

  val rosenbrockRequest = Request(
    objFn = rosenbrockFunc,
    D = D,
    LB = List.fill(D)(-5.0),
    UB = List.fill(D)(10.0),
    //initialParam = Some(List.fill(D)(1.0 + Random.nextDouble * (0.1))),
    solverConfig = solverConfig, description = desc)

}



case class PerformanceTest (
  func: String, D: Int, N: Int,
  system: ActorSystem, workerRouter: ActorRef, nRouter: Int,
  journal: Journal) {

  val id = s"$func-$D-$N"

  val NP = D match {
    case 50 => 10
    case 100 => 20
    case 200 => 50
    case 500 => 100
    case 1000 => 200
  }

  val config = MOSConfig(
    NP = NP,
    stepSize = D * 5000/84,
    maxNumEval = D * 5000,
    F = None,
    Cr = None,
    Er = 0.3,
    Ar = 0.1,
    xi = 0.05,
    minDE = 0.05,
    minSR = 1e-5,
    mutationStrategy = "current-to-best",
    constStrategy = "rank",
    tolRel = 1e-8,
    tolStep = Int.MaxValue,
    logTrace = true)

  val request = func match {
    case "Ackley" => BenchmarkFunctions(D, config, Some(id)).ackleyRequest
    case "Rastrigin" => BenchmarkFunctions(D, config, Some(id)).rastriginRequest
    case "Schwefel" => BenchmarkFunctions(D, config, Some(id)).schwefelRequest
    case "Sphere" => BenchmarkFunctions(D, config, Some(id)).sphereRequest
    case "Ellipsoid" => BenchmarkFunctions(D, config, Some(id)).ellipsoidRequest
    case "Zakharov" => BenchmarkFunctions(D, config, Some(id)).zakharovRequest
    case "Rosenbrock" => BenchmarkFunctions(D, config, Some(id)).rosenbrockRequest
  }

  val key = journal.registerRow(request)
  
  val solver = system.actorOf(
    Props(new SolverMOS(
      key,
      request,
      workerRouter,
      nRouter,
      journal,
      Duration.Inf,
      Duration.Inf)),
      name = id
    )

}

object Main extends App {

  val funcList = List("Ackley", "Rastrigin", "Schwefel", "Sphere", "Ellipsoid", "Zakharov", "Rosenbrock")
  val dimList = List(50, 100, 200, 500, 1000)
  val testList = (1 to 25).toList
  val nRouter = 16
  val system = ActorSystem()
  val workerRouter = system.actorOf(RoundRobinPool(nRouter).props(Props[ObjFnActor]))
  val journal = Journal()

  for(func <- funcList; d <- dimList; n <- testList){
    val test = PerformanceTest(func, d, n, system, workerRouter, nRouter, journal)
    test.solver ! Start
  }

}
