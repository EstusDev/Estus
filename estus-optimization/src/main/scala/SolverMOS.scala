package com.estus.optimization

import akka.actor.{ActorLogging, FSM, ActorRef}

import scala.concurrent.duration.Duration
import scala.util.Random


class SolverMOS(
    key: String,
    request: Request,
    workerRouter: ActorRef,
    journal: Journal,
    timeout: Duration = Duration.Inf,
    timeoutObjFn: Duration = Duration.Inf)
  extends FSM[State, Data]
    with ActorLogging {

  private val startTime = System.currentTimeMillis()
  private val config = request.solverConfig.asInstanceOf[MOSConfig]

  private val pop = Population(config.NP)
  private val trace = Trace()

  private var stepSeq = List.empty[Step]
  private var bestNode: PopulationNode = _
  private var (fMu, crMu) = (0.5, 0.5)
  private var (numEval, convergeStep) = (0, 0)
  private var converged = false



}



trait EvalTask

case class DETask (r0: Int) extends EvalTask

case class LS1Task (d: Int) extends EvalTask

case class Step (
    id: Int,
    dePi: Int,
    numEval: Int,
    D: Int,
    NP: Int)
  extends StackKey[(Int, String)] {

  // Current Participation of DE and LS1
  object Pi {
    private val de = dePi
    private val ls1 = numEval - de
    def apply (): List[Int] = List(de, ls1)
  }

  // Average Fitness Increments of DE and LS1
  object Sigma {
    private var de = List.empty[Double]
    private var ls1 = List.empty[Double]
    def deAdd (s: Double): Unit = de = s :: de
    def ls1Add (s: Double): Unit = ls1 = s :: ls1
    // Assume increments are positive
    def apply (): List[Double] = {
      (de.nonEmpty, ls1.nonEmpty) match {
        case (true, true) =>
          List(de.sum/de.size, ls1.sum/ls1.size).map(_.abs)
        case (true, false) =>
          List(de.sum/de.size, 0.0).map(_.abs)
        case (false, true) =>
          List(0.0, ls1.sum/ls1.size).map(_.abs)
        case _ =>
          List(0.0, 0.0)
      }
    }
  }

  // Number of Fitness Increments of DE and LS1
  object Gamma {
    private var de = 0
    private var ls1 = 0
    def deAdd (): Unit = de += 1
    def ls1Add (): Unit = ls1 += 1
    def apply (): List[Int] = List(de, ls1)
  }

  // Quality of DE and LS1
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
    var randInd = List.empty[Int]
    (0 until p.last).foreach(i => {
      if (i % D == 0)
        randInd = Random.shuffle(0 until D).toList
      val key = (id, java.util.UUID.randomUUID.toString)
      evalStack.push((key, LS1Task(randInd(i % D))))
    })
    (0 until p.head).foreach(i => {
      if (i % NP == 0)
        randInd = Random.shuffle(0 until NP).toList
      val key = (id, java.util.UUID.randomUUID.toString)
      evalStack.push((key, DETask(randInd(i % NP))))
    })
    evalStack
  }

  // Dynamic Participation Function
  def generateNextStep (numEvalLeft: Int, xi: Double = 0.05): Step = {
    val Q = Quality()
    val P = Pi()
    val (iBest, iRest) = if (Q.head >= Q.last)
      (0, 1)
    else
      (1, 0)
    // Participation Decrement of the Weak Technique
    val eta = (xi * (Q(iBest) - Q(iRest)) / Q(iBest) * P(iRest)).toInt
    Step (
      id = id + 1,
      dePi = P.head + eta,
      numEval = if (numEvalLeft > numEval) numEval else numEvalLeft,
      D = D,
      NP = NP)
  }

}
