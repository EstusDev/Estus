package com.estus.optimization

import akka.actor._
import com.estus.optimization.MessageProtocol._

import scala.concurrent.duration.{FiniteDuration, Duration}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Failure


class SolverMOS (
    key: String,
    request: Request,
    workerRouter: ActorRef,
    nRouter: Int,
    journal: Journal,
    timeout: Duration = Duration.Inf,
    timeoutObjFn: Duration = Duration.Inf)
  extends Actor
    with ActorLogging {

  private val startTime = System.currentTimeMillis()
  private val config = request.solverConfig.asInstanceOf[MOSConfig]
  private val pop = Population(config.NP)
  private val popExplore = Population(config.NP)
  private val trace = Trace()
  private val stepSeq = StepSeq(Step(0, config.stepSize/2, config.stepSize, request))
  private var numEval = 0
  private var converged = false
  private var status = ""

  // Initiation Actor, DE Actor and LS1 Actor
  private lazy val ls1Actor = context.system.actorOf(Props(new LS1Actor(
    pop, stepSeq, trace,
    self, workerRouter, nRouter,
    request, timeoutObjFn)))

  private lazy val deActor = context.system.actorOf(Props(new DEActor(
    pop, popExplore, stepSeq, trace,
    self, ls1Actor, workerRouter, nRouter,
    request, timeoutObjFn)))
  ls1Actor ! deActor

  private lazy val initActor = context.system.actorOf(Props(new INITActor(
    pop, popExplore, stepSeq, trace,
    self, deActor, workerRouter, nRouter,
    request, timeoutObjFn)))



  def receive = {

    case Start =>
      if (config.logTrace)
        log.info(s"SolverMOS Started.")
      if (timeout.isFinite()) {
        val fdur = FiniteDuration(
          timeout.toMillis,
          java.util.concurrent.TimeUnit.MILLISECONDS)
        context.system.scheduler.scheduleOnce(fdur, self, Timeout)
      }
      initActor ! Start

    case AddNumEval(num) =>
      numEval += num

    case NextStep if numEval < config.maxNumEval =>
      val senderActor = sender()
      if (config.logTrace) {
        val Q = stepSeq.seq.last.Quality()
        val P = stepSeq.seq.last.Pi().map(_.toDouble)
        val Pr = if (P.sum == 0.0)
          List(0.0, 0.0)
        else
          List(P.head/P.sum, P.last/P.sum)
        log.info(s"Step ${stepSeq.seq.last.id}: " +
          s"Num of Evals: $numEval, " +
          s"Best Value: ${stepSeq.seq.last.bestNode.objFnVal}, " +
          s"Quality: $Q, " +
          s"Participation: $P, " +
          s"Participation Rate: $Pr, " +
          s"Time Elapsed: ${System.currentTimeMillis() - startTime}")
      }
      val numEvalLeft = if (config.stepSize <= config.maxNumEval - numEval) {
        config.stepSize
      } else {
        config.maxNumEval - numEval
      }
      stepSeq.seq = stepSeq.seq :+
        stepSeq.seq.last.generateNextStep(numEvalLeft, config.xi, config.minDE)
      senderActor ! NextStep

    case Converged => // Converged
      converged = true
      status = Converged.toString()
      context stop self
      context become shuttingDown

    case NextStep => // Not Converged
      converged = false
      status = NotConverged.toString()
      context stop self
      context become shuttingDown

    case Timeout => // Timed out
      converged = false
      status = Timeout.toString()
      context stop self
      context become shuttingDown

    case Failure(cause) => // Failed
      converged = false
      status = cause.toString()
      context stop self
      context become shuttingDown

    case mssg: Any =>
      if (config.logTrace)
        log.info(s"SolverMOS received an unknown message: $mssg")

  }

  def shuttingDown: Receive = {
    case mssg: Any =>
      if (config.logTrace)
        log.info(s"SolverMOS is shutting down. $mssg will not be processed")
  }

  override def postStop(): Unit = {
    context stop initActor
    context stop deActor
    context stop ls1Actor
    val solution = Solution(
      objValue = stepSeq.seq.last.bestNode.objFnVal,
      param = stepSeq.seq.last.bestNode.param,
      isFeasible = stepSeq.seq.last.bestNode.constVal <= 0.0,
      isConverged = converged,
      numEval = numEval,
      status = status,
      timeElapsed = System.currentTimeMillis() - startTime)
    if (config.logTrace)
      log.info(solution.toString)
    journal.updateRow(key, solution)



    import java.io.FileWriter
    import java.io.File
    import java.io.BufferedWriter

    val file = new File(s"PerfTest/${request.description.get}.csv")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(s"${request.description.get}, ${solution.objValue.get}, ${solution.numEval}, ${solution.timeElapsed}")
    bw.close()



  }

}
