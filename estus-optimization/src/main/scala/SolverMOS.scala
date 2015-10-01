package com.estus.optimization

import akka.actor.{Stash, ActorLogging, FSM, ActorRef}

import scala.concurrent.duration.Duration
import scala.util.Random


class SolverMOS (
    key: String,
    request: Request,
    workerRouter: ActorRef,
    journal: Journal,
    timeout: Duration = Duration.Inf,
    timeoutObjFn: Duration = Duration.Inf)
  extends FSM[State, Data]
    with ActorLogging
    with Stash {

  private val startTime = System.currentTimeMillis()
  private val config = request.solverConfig.asInstanceOf[MOSConfig]

  private val pop = Population(config.NP)
  private val trace = Trace()

  private var stepSeq = List(Step(
    id = 0,
    denmPi = 0.5,
    numEval = config.maxNumEval/config.step,
    numEvalLeft = config.maxNumEval - config.maxNumEval/config.step,
    D = request.D,
    NP = config.NP))
  private var bestNode: PopulationNode = _
  private var (crMu, rhoMu) = (0.5, 0.0)
  private var (numEval, numIter, convergeStep) = (0, 0, 0)
  private var converged = false

}



trait EvalTask

case class DENMTask (r0: Int) extends EvalTask

case class LS1Task (d: Int) extends EvalTask

case class Step (
    id: Int,
    denmPi: Double,
    numEval: Int,
    numEvalLeft: Int,
    D: Int,
    NP: Int)
  extends StackKey[(Int, String)] {

  // Input Sanity Check
  if (id < 0)
    throw new IllegalArgumentException(s"id must be >= 0 (id = $id).")
  if (denmPi < 0.0 || denmPi > 1.0)
    throw new IllegalArgumentException(s"denmPi must be in [0, 1] (denmPi = $denmPi).")
  if (numEval <= 0)
    throw new IllegalArgumentException(s"numEval must be > 0 (numEval = $numEval).")
  if (numEvalLeft < 0)
    throw new IllegalArgumentException(s"numEvalLeft must be >= 0 (numEvalLeft = $numEvalLeft).")

  // Current Participation of DENM and LS1
  object Pi {
    private val denm = (numEval*denmPi).toInt
    private val ls1 = numEval - denm
    def apply (): List[Int] = List(denm, ls1)
  }

  // Average Fitness Increments of DENM and LS1
  object Sigma {
    private var denm = List.empty[Double]
    private var ls1 = List.empty[Double]
    def denmAdd (s: Double): Unit = denm = s :: denm
    def ls1Add (s: Double): Unit = ls1 = s :: ls1
    // Assume increments are positive
    def apply (): List[Double] = {
      (denm.nonEmpty, ls1.nonEmpty) match {
        case (true, true) =>
          List(denm.sum/denm.size, ls1.sum/ls1.size).map(_.abs)
        case (true, false) =>
          List(denm.sum/denm.size, 0.0).map(_.abs)
        case (false, true) =>
          List(0.0, ls1.sum/ls1.size).map(_.abs)
        case _ =>
          List(0.0, 0.0)
      }
    }
  }

  // Number of Fitness Increments of DENM and LS1
  object Gamma {
    private var denm = 0
    private var ls1 = 0
    def denmAdd (): Unit = denm += 1
    def ls1Add (): Unit = ls1 += 1
    def apply (): List[Int] = List(denm, ls1)
  }

  // Quality of DENM and LS1
  object Quality {
    def apply (): List[Double] = {
      val sigma = Sigma()
      val gamma = Gamma().map(_.toDouble)
      val sigmaSign = sigma.reduce((x, y) => x - y).signum
      val gammaSign = gamma.reduce((x, y) => x - y).signum
      if (sigmaSign == gammaSign)
        sigma
      else
        gamma
    }
  }

  // EvalStack
  val evalStack = generateEvalStack()

  def generateEvalStack (): EvalStack[KeyType, EvalTask] = {
    val evalStack = EvalStack[KeyType, EvalTask]()
    val p = Pi()
    (0 until p.last).foreach(_ => {
      val key = (id, java.util.UUID.randomUUID.toString)
      evalStack.push((key, LS1Task(Random.nextInt(D))))
    })
    (0 until p.head).foreach(_ => {
      val key = (id, java.util.UUID.randomUUID.toString)
      evalStack.push((key, DENMTask(Random.nextInt(NP))))
    })
    evalStack
  }

  // Dynamic Participation Function
  def generateNextStep (xi: Double = 0.05): Step = {
    val Q = Quality()
    val P = Pi()
    val (iBest, iRest) = if (Q.head >= Q.last)
      (0, 1)
    else
      (1, 0)
    // Participation Decrement of the Weak Technique
    val eta = (xi * (Q(iBest) - Q(iRest)) / Q(iBest) * P(iRest)).toInt
    val newP = P.zipWithIndex.map(x => {
      val v = if (x._2 == iBest)
        x._1 + eta
      else
        x._1 - eta
      v.toDouble
    })
    val idNext = id + 1
    val denmPiNext = newP.head / newP.sum
    val (numEvalNext, numEvalLeftNext) = if (numEvalLeft > numEval)
      (numEval, numEvalLeft - numEval)
    else
      (numEvalLeft, 0)
    Step (
      id = idNext,
      denmPi = denmPiNext,
      numEval = numEvalNext,
      numEvalLeft = numEvalLeftNext,
      D = D,
      NP = NP)
  }

}
