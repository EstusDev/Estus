package com.estus.optimization

import com.estus.optimization.MessageProtocol._

import akka.actor.{Props, ActorLogging, Actor, ActorRef}
import akka.routing.RoundRobinPool

import scala.concurrent.duration.Duration
import scala.math._
import scala.util.Random



class INITActor (
    pop: Population,
    popExplore: Population,
    stepSeq: StepSeq,
    trace: Trace,
    master: ActorRef,
    deActor: ActorRef,
    workerRouter: ActorRef,
    nRouter: Int,
    request: Request,
    timeoutObjFn: Duration)
  extends Actor
    with ActorLogging {

  private val config = request.solverConfig.asInstanceOf[MOSConfig]
  private val mosRouter = context.system.actorOf(
    RoundRobinPool(nRouter).props(Props[MOSActor]))
  private var active = false



  def receive = {

    case Start =>
      active = true
      (0 until nRouter).foreach(_ => mosRouter ! WorkAvailable(self))

    case ResultMOS(k, node) =>
      if (k.asInstanceOf[Boolean]) { // pop
        if (pop.size < config.NP)
          pop.add(node)
        else
          pop.replaceWorst(node, config.constStrategy)
      } else { // popExplore
        if (popExplore.size < config.NP)
          popExplore.add(node)
        else
          popExplore.replaceWorst(node, config.constStrategy)
      }

    case AddNumEval(num) =>
      master ! AddNumEval(num)



    /* <<<==== Active Divide ====>>> */



    case _ if !active =>

    case GimmeWork if pop.size < config.NP =>
      val senderActor = sender()
      // Generate initial random nodes
      val param = request.initialParam match {
        case Some(p) => // If initial param is provided
          if (pop.size == 0) {
            // First node is from the initial param
            p
          } else {
            // The rests are from a random triangular distribution
            // centered to the initial node
            (request.LB, request.UB, p).zipped.toList.map(x => {
              val (a, b, c) = (x._1, x._2, x._3)
              val fc = (c - a) / (b - a)
              val u = Random.nextDouble()
              if (0 < u && u < fc)
                a + sqrt(u * (b - a) * (c - a))
              else
                b - sqrt((1 - u) * (b - a) * (b - c))
            })
          }
        case _ => // If initial param is NOT provided
          // Generate nodes from an uniform distribution
          request.LB.zip(request.UB).
            map(x => x._1 + Random.nextDouble * (x._2 - x._1))
      }
      val node = PopulationNode(param, request)
      // Evaluate this node, add to the pop for future comparison
      if (node.constVal > 0) {
        pop.add(node)
        senderActor ! WorkAvailable(self)
      } else {
        senderActor ! WorkDE(self, workerRouter, true, node, request, timeoutObjFn)
      }

    case GimmeWork if popExplore.size < config.NP =>
      val senderActor = sender()
      // Make an opposite node of the current node
      val param = pop.get(Random.nextInt(config.NP)).get.param
      val paramExplore = (param, request.LB, request.UB).zipped.toList.
        map(x => x._2 + x._3 - x._1)
      val nodeExplore = PopulationNode(paramExplore, request)
      // Evaluate this node, add to the popExplore for future comparison
      if (nodeExplore.constVal > 0) {
        popExplore.add(nodeExplore)
        senderActor ! WorkAvailable(self)
      } else {
        senderActor ! WorkDE(self, workerRouter, false, nodeExplore, request, timeoutObjFn)
      }

    case GimmeWork =>
      active = false
      // Opposite-based exploration finishes, now sort and take the bests
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
      if (stepSeq.seq.last.bestNode.objFnVal.nonEmpty)
        trace.add(stepSeq.seq.last.bestNode.objFnVal.get)
      deActor ! StartDE(stepSeq.seq.last.Pi().head)

  }

}
