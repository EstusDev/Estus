package com.estus.optimization

import com.estus.optimization.MessageProtocol._

import org.apache.commons.math3.distribution.CauchyDistribution

import akka.actor.{Props, ActorLogging, Actor, ActorRef}
import akka.routing.RoundRobinPool

import scala.concurrent.duration.Duration
import scala.util.Random



class DEActor (
    pop: Population,
    popExplore: Population,
    stepSeq: StepSeq,
    trace: Trace,
    master: ActorRef,
    ls1Actor: ActorRef,
    workerRouter: ActorRef,
    nRouter: Int,
    request: Request,
    timeoutObjFn: Duration)
  extends Actor
    with ActorLogging {

  private val config = request.solverConfig.asInstanceOf[MOSConfig]
  private val mosRouter = context.system.actorOf(
    RoundRobinPool(nRouter).props(Props[MOSActor]))
  private var (numEval, numEvalBudget, numIter, nExplore, nCorpse) = (0, 0, 0, 0, nRouter)
  private var (flagExplore, active) = (false, false)
  private var (fMu, crMu, rhoMu) = (0.5, 0.5, 0.0)



  def receive = {

    case StartDE(budget) =>
      numEvalBudget = budget
      active = true
      (0 until nCorpse).foreach(_ => mosRouter ! WorkAvailable(self))
      nCorpse = 0

    case ResultMOS(k, node) =>
      val key = k.asInstanceOf[(Boolean, Int)]
      if (key._1) { // pop
        self ! Select(key._2, node)
      } else { // popExplore
        if (nExplore < config.NP)
          popExplore.update(nExplore, node)
        else
          popExplore.replaceWorst(node, config.constStrategy)
        nExplore += 1
      }

    case Select(i, node) =>
      numIter += 1
      // Compare ith node from pop with a new node, select the better node
      val r0 = i.asInstanceOf[Int]
      val nodeSelected = pop.selectBetterNode(
        node,
        pop.get(r0).get,
        config.constStrategy)
      pop.update(r0, nodeSelected)
      val bestNodeTmp = stepSeq.seq.last.bestNode
      stepSeq.seq.last.bestNode = pop.selectBetterNode(
        nodeSelected,
        stepSeq.seq.last.bestNode,
        config.constStrategy)
      // Update Sigma and Gamma for the Current Step
      if (node == stepSeq.seq.last.bestNode) {
        (node.objFnVal, bestNodeTmp.objFnVal) match {
          case (Some(v1), Some(v2)) =>
            stepSeq.seq.last.Sigma.deAdd(v1 - v2)
          case _ =>
        }
        stepSeq.seq.last.Gamma.deAdd()
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
      // For every generation, 1. update F and Cr 2. explore population
      if (numIter % config.NP == 0 && active) {
        config.F match {
          case None =>
            val fVec = pop.p.values.flatten(_.F)
            if (fVec.nonEmpty)
              fMu = (1 - config.Ar) * fMu +
                config.Ar * (fVec.map(x => x * x).sum / fVec.sum)
          case _ =>
        }
        config.Cr match {
          case None =>
            val crVec = pop.p.values.flatten(_.Cr)
            if (crVec.nonEmpty)
              crMu =  (1 - config.Ar) * crMu +
                config.Ar * (crVec.sum / crVec.size)
          case _ =>
        }
        if (Random.nextDouble < config.Er && !trace.converged) {
          nExplore = 0
          flagExplore = true
        }
      }

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
      stepSeq.seq.last.numEvalDE = numEval
      master ! Converged

    case GimmeWork if !trace.converged && numEval >= numEvalBudget =>
      nCorpse += 1
      active = false
      numEval = 0
      stepSeq.seq.last.numEvalDE = numEval
      ls1Actor ! StartLS1(stepSeq.seq.last.Pi().last)

    case GimmeWork if flagExplore && nExplore < config.NP  =>
      val senderActor = sender()
      // Use Nelder-Mead method to explore population space
      // Construct a mid-point from a simplex - bestNode, goodNode and worstNode
      var (r0, r1) = (Random.nextInt(config.NP), 0)
      do {r1 = Random.nextInt(config.NP)} while (r1 == r0)
      val goodNode = pop.selectBetterNode(
        pop.get(r0).get,
        pop.get(r1).get,
        config.constStrategy)
      val worstNode = if (goodNode == pop.get(r0).get)
        pop.get(r1).get
      else
        pop.get(r0).get
      val midNode = PopulationNode(
        stepSeq.seq.last.bestNode.param.zip(goodNode.param).map(x => (x._1 + x._2)/2),
        request)
      // Explore (contrast or expand) from this mid-point according to rho
      val rho = 2.0 * Random.nextGaussian + rhoMu
      var paramExplore = midNode.param.zip(worstNode.param).
        map(x => x._1 + rho * (x._1 - x._2))
      paramExplore = (request.LB, request.UB, paramExplore).zipped.toList.map(x => {
        if (x._1 > x._3 || x._2 < x._3)
          x._1 + Random.nextDouble * (x._2 - x._1)
        else
          x._3
      })
      val exploreNode = PopulationNode(paramExplore, request)
      exploreNode.rho = Some(rho)
      // Evaluate this exploreNode, add to the popExplore for future comparison
      if (exploreNode.constVal > 0) {
        popExplore.update(nExplore, exploreNode)
        nExplore += 1
        senderActor ! WorkAvailable(self)
      } else {
        val key = (false, -1)
        senderActor ! WorkDE(self, workerRouter, key, exploreNode, request, timeoutObjFn)
      }

    case GimmeWork if flagExplore =>
      val senderActor = sender()
      // Explore finishes, now sort the popUnion and take the bests
      val popUnion = pop.merge(popExplore)
      val wfv = popUnion.worstFeasibleVal.getOrElse(0.0)
      (popUnion.p.toSeq.sortBy(n => n._2.objFnVal match {
        case Some(v) =>
          v
        case _ =>
          n._2.constVal + wfv
      }).map(_._2) take config.NP).zipWithIndex.foreach(x => {
        if (x._2 == 0)
          stepSeq.seq.last.bestNode = x._1
        pop.update(x._2, x._1)
      })
      // Update rho using success trails from the explore phrase
      val rhoVec = pop.p.values.flatten(_.rho)
      if (rhoVec.nonEmpty)
        rhoMu = (1 - config.Ar) * rhoMu +
          config.Ar * (rhoVec.sum / rhoVec.size)
      flagExplore = false
      senderActor ! WorkAvailable(self)

    case GimmeWork =>
      val senderActor = sender()
      // Chose target nodes: r0, r1, r2
      var (r1, r2) = (0, 0)
      val r0 = Random.nextInt(config.NP)
      do {r1 = Random.nextInt(config.NP)} while (r1 == r0)
      do {r2 = Random.nextInt(config.NP)} while (r2 == r1 || r2 == r0)
      val nodeR0 = pop.get(r0).get
      val nodeR1 = pop.get(r1).get
      val nodeR2 = pop.get(r2).get
      // Mutate target node into a trail node
      val F = config.F match {
        case Some(f) =>
          f
        case _ =>
          var f = 0.0
          val dist = new CauchyDistribution(fMu, 0.1)
          do {f = dist.sample} while (f <= 0)
          if (f > 1.0)
            f = 1.0
          f
      }
      val Cr = config.Cr match {
        case Some(cr) =>
          cr
        case _ =>
          var cr = 0.1 * Random.nextGaussian + crMu
          if (cr < 0)
            cr = 0
          if (cr > 1)
            cr = 1
          cr
      }
      var paramTrail = nodeR0.param
      paramTrail = paramTrail.indices.map(i =>
        if (Random.nextDouble <= Cr) {
          config.mutationStrategy match {
            case "current-to-best" =>
              nodeR0.param(i) +
                F * (stepSeq.seq.last.bestNode.param(i) - nodeR0.param(i)) +
                F * (nodeR1.param(i) - nodeR2.param(i))
            case "classic" =>
              nodeR0.param(i) +
                F * (nodeR1.param(i) - nodeR2.param(i))
          }
        } else {
          nodeR0.param(i)
        }
      ).toList
      paramTrail = (request.LB, request.UB, paramTrail).zipped.toList.map(x => {
        if (x._1 > x._3 || x._2 < x._3)
          x._1 + Random.nextDouble * (x._2 - x._1)
        else
          x._3
      })
      val nodeTrail = PopulationNode(paramTrail, request)
      nodeTrail.F = Some(F)
      nodeTrail.Cr = Some(Cr)
      // Evaluate trail node and compare with node r0
      if (nodeTrail.constVal > 0) {
        self ! Select(r0, nodeTrail)
        senderActor ! WorkAvailable(self)
      } else {
        val key = (true, r0)
        senderActor ! WorkDE(self, workerRouter, key, nodeTrail, request, timeoutObjFn)
      }

  }

}
