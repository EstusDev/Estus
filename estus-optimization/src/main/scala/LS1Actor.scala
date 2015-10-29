package com.estus.optimization

import com.estus.optimization.MessageProtocol._

import akka.actor.{Props, ActorRef, ActorLogging, Actor}
import akka.routing.RoundRobinPool

import scala.concurrent.duration.Duration
import scala.util.Random



class LS1Actor (
    pop: Population,
    stepSeq: StepSeq,
    trace: Trace,
    master: ActorRef,
    workerRouter: ActorRef,
    nRouter: Int,
    request: Request,
    timeoutObjFn: Duration)
  extends Actor
    with ActorLogging
    with StackKey[String]{

  private val config = request.solverConfig.asInstanceOf[MOSConfig]
  private val mosRouter = context.system.actorOf(
    RoundRobinPool(nRouter).props(Props[MOSActor]))
  private var deActor: ActorRef = _

  private val evalStack = EvalStack[KeyType, (Int, Double)]()
  private var (numEval, numEvalBudget, r0, nCorpse) = (0, 0, 0, nRouter)
  private var active = false
  private var sr = 0.5



  def receive = {

    case actor: ActorRef =>
      deActor = actor

    case StartLS1(budget) =>
      r0 = pop.p.filter(_._2 == stepSeq.seq.last.bestNode).keys.headOption match {
        case Some(k) =>
          pop.p.keys.zipWithIndex.filter(_._1 == k).head._2
        case _ =>
          Random.nextInt(pop.p.size)
      }
      numEvalBudget = budget
      active = true
      (0 until nCorpse).foreach(_ => mosRouter ! WorkAvailable(self))
      nCorpse = 0

    case ResultMOS(None, node) =>
      val bestNodeTmp = stepSeq.seq.last.bestNode
      stepSeq.seq.last.bestNode = pop.selectBetterNode(
        node, stepSeq.seq.last.bestNode, config.constStrategy)
      // Update Sigma, Gamma and bestNode
      if (node == stepSeq.seq.last.bestNode) {
        pop.update(r0, node)
        (node.objFnVal, bestNodeTmp.objFnVal) match {
          case (Some(v1), Some(v2)) =>
            stepSeq.seq.last.Sigma.ls1Add(v1 - v2)
          case _ =>
        }
        stepSeq.seq.last.Gamma.ls1Add()
      }
      // Compare the better node with the current best node, see if it converges
      if (stepSeq.seq.last.bestNode.objFnVal.nonEmpty &&
        !trace.contains(stepSeq.seq.last.bestNode.objFnVal.get))
        trace.add(stepSeq.seq.last.bestNode.objFnVal.get)
      trace.convergeStep = trace.converged(config.tolRel) match {
        case Some(cond) =>
          if (cond)
            trace.convergeStep + 1
          else
            0
        case _ =>
          trace.convergeStep
      }
      if (trace.convergeStep >= config.tolStep)
        trace.converged = true

    case NextStep =>
      deActor ! StartDE(stepSeq.seq.last.Pi().head)

    case AddNumEval(num) =>
      numEval += num
      master ! AddNumEval(num)



    /* <<<==== Active Divide ====>>> */



    case GimmeWork if !active =>
      nCorpse += 1

    case GimmeWork if trace.converged =>
      nCorpse += 1
      active = false
      numEval = 0
      stepSeq.seq.last.numEvalLS1 = numEval
      master ! Converged

    case GimmeWork if !trace.converged && numEval >= numEvalBudget =>
      nCorpse += 1
      active = false
      numEval = 0
      stepSeq.seq.last.numEvalLS1 = numEval
      master ! NextStep

    case GimmeWork =>
      val senderActor = sender()
      evalStack.pop() match {
        case Some(n) =>
          // Send work
          val best = stepSeq.seq.last.bestNode
          best.DSR = Some((n._2._1, n._2._2))
          senderActor ! WorkLS(self, workerRouter, best, request, timeoutObjFn)
        case _ =>
          scala.util.Random.shuffle((0 until request.D).toList).foreach(i =>
            evalStack.push((java.util.UUID.randomUUID.toString, (i, sr))))
          sr *= 0.5
          if (sr < config.minSR)
            sr = 0.4
          senderActor ! WorkAvailable(self)
      }

  }

}
