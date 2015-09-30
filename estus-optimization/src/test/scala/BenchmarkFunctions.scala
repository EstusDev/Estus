package com.estus.optimization

import scala.math._
import scala.util.Random



case class BenchmarkFunctions (D: Int, solverConfig: SolverConfig) {

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
    initialParam = Some(List.fill(D)(0.0 + Random.nextDouble * (0.1))),
    solverConfig = solverConfig)

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
    initialParam = Some(List.fill(D)(0.0 + Random.nextDouble * (0.1))),
    solverConfig = solverConfig)

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
    initialParam = Some(List.fill(D)(420.5 + Random.nextDouble * (420.0 - 419.0))),
    solverConfig = solverConfig)



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
    initialParam = Some(List.fill(D)(0.0 + Random.nextDouble * (0.1))),
    solverConfig = solverConfig)

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
    initialParam = Some(List.fill(D)(0.0 + Random.nextDouble * (0.1))),
    solverConfig = solverConfig)



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
    initialParam = Some(List.fill(D)(0.0 + Random.nextDouble * (0.1))),
    solverConfig = solverConfig)



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
    initialParam = Some(List.fill(D)(1.0 + Random.nextDouble * (0.1))),
    solverConfig = solverConfig)



  /***
    GROUP 5: Constrainted Functions
  ***/

  /*
    The Deb Function
    Space: [0.0, 6.0]^2
    Global Optimum: 13.59085
    Solution: (2.246826, 2.381865)
    http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.477.822&rep=rep1&type=pdf
  */
  def debFunc(param: List[Double], other: Option[Seq[Any]] = None): Double = {
    pow(pow(param.head, 2) + param.last - 11, 2) + pow(pow(param.last, 2) + param.head - 7, 2)
  }

  def debIneqFunc(param: List[Double]): List[Double] = {
    val val1 = 4.84 - pow(param.head - 0.05, 2) - pow(param.last - 2.5, 2)
    val val2 = pow(param.head, 2) + pow(param.last - 2.5, 2) - 4.84
    List(val1, val2)
  }

  val debRequest = Request(
    objFn = debFunc,
    D = 2,
    LB = List.fill(2)(0.0),
    UB = List.fill(2)(6.0),
    ineqLB = Some(List(0.0, 0.0)),
    ineqFunc = Some(debIneqFunc),
    initialParam = Some(List.fill(D)(2.0 + Random.nextDouble * (2.5 - 2.0))),
    solverConfig = solverConfig)

}
