package com.estus.optimization

import akka.actor.{Stash, ActorLogging, FSM, ActorRef}
import scala.concurrent.duration.{FiniteDuration, Duration}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.math._
import scala.util.{Failure, Random}
import com.estus.optimization.MessageProtocol._
import org.apache.commons.math3.distribution.CauchyDistribution



class SolverDE (
    key: String,
    request: Request,
    workerRouter: ActorRef,
    journal: Journal,
    timeout: Duration = Duration.Inf,
    timeoutObjFn: Duration = Duration.Inf)
  extends FSM[State, Data]
    with ActorLogging
    with Stash
    with StackKey[(Boolean, Option[Int], String)] {

  private val startTime = System.currentTimeMillis()
  private val config = request.solverConfig.asInstanceOf[DiffEvoConfig]

  private val pop = Population(config.NP)
  private val popExplore = Population(config.NP)
  private val evalStack = EvalStack[KeyType, PopulationNode]()
  private var evalMap = collection.immutable.Map.empty[KeyType, PopulationNode]
  private val trace = Trace()

  private var bestNode: PopulationNode = _
  private var (fMu, crMu, rhoMu) = (0.5, 0.5, 0.0)
  private var (numEval, numIter, convergeStep) = (0, 0, 0)
  private var (converged, flagExplore) = (false, false)

  startWith(InitiationState, Data())

  when(InitiationState) {

    case Event(Start, _) =>
/*      timeout match {
        case Some(t) => // Set timeout
          context.system.scheduler.scheduleOnce(t, self, Timeout)
        case _ =>
      }*/
      if (timeout.isFinite) {
        val fdur = FiniteDuration(
          timeout.toMillis,
          java.util.concurrent.TimeUnit.MILLISECONDS)
        context.system.scheduler.scheduleOnce(fdur, self, Timeout)
      }
      self ! Initiate
      stay ()

    case Event(Initiate, _) if pop.size < config.NP =>
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
      } else {
        val size = evalStack.size
        val key = (true, None, java.util.UUID.randomUUID.toString)
        evalStack.push((key, node), Some(config.NP))
        if (size == 0)
          workerRouter ! WorkAvailable(self)
      }
      self ! Initiate
      stay()

    case Event(Initiate, _) if popExplore.size < config.NP =>
      // Make an opposite node of the current node
      val param = pop.get(Random.nextInt(config.NP)).get.param
      val paramExplore = (param, request.LB, request.UB).zipped.toList.
        map(x => x._2 + x._3 - x._1)
      val nodeExplore = PopulationNode(paramExplore, request)
      // Evaluate this node, add to the popExplore for future comparison
      if (nodeExplore.constVal > 0) {
        popExplore.add(nodeExplore)
      } else {
        val size = evalStack.size
        val key = (false, None, java.util.UUID.randomUUID.toString)
        evalStack.push((key, nodeExplore), Some(config.NP))
        if (size == 0)
          workerRouter ! WorkAvailable(self)
      }
      self ! Initiate
      stay()

    case Event(Initiate, _) =>
      // Opposite-based exploration finishes, now sort and take the bests
      val popUnion = pop.merge(popExplore)
      pop.empty
      val wfv = popUnion.worstFeasibleVal.getOrElse(0.0)
      (popUnion.p.values.toList.sortBy(node => node.objFnVal match {
        case Some (v) =>
          v
        case _ =>
          node.constVal + wfv
      }) take config.NP).zipWithIndex.foreach(x => {
        if (x._2 == 0)
          bestNode = x._1
        pop.add(x._1)
      })
      goto(EvolutionDEState)

  }

  when (EvolutionDEState) {

    case Event(Evolve, _) if popExplore.size < config.NP && flagExplore =>
      // Use Nelder-Mead method to explore population space
      // Construct a mid-point from a simplex - bestNode, goodNode and worstNode
      var (r0, r1) = (0, 0)
      do {r0 = Random.nextInt(config.NP)} while (pop.get(r0).get == bestNode)
      do {r1 = Random.nextInt(config.NP)} while (r1 == r0 || pop.get(r1).get == bestNode)
      val goodNode = pop.selectBetterNode(
        pop.get(r0).get,
        pop.get(r1).get,
        config.constStrategy)
      val worstNode = if (goodNode == pop.get(r0).get)
        pop.get(r1).get
      else
        pop.get(r0).get
      val midNode = PopulationNode(
        bestNode.param.zip(goodNode.param).map(x => (x._1 + x._2)/2),
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
        popExplore.add(exploreNode)
      } else {
        val size = evalStack.size
        val key = (false, None, java.util.UUID.randomUUID.toString)
        evalStack.push((key, exploreNode), Some(config.NP))
        if (size == 0)
          workerRouter ! WorkAvailable(self)
      }
      self ! Evolve
      stay()

    case Event(Evolve, _) if popExplore.size == config.NP && flagExplore =>
      // Explore finishes, now sort the popUnion and take the bests
      val popUnion = pop.merge(popExplore)
      pop.empty
      val wfv = popUnion.worstFeasibleVal.getOrElse(0.0)
      (popUnion.p.toSeq.sortBy(n => n._2.objFnVal match {
        case Some(v) =>
          v
        case _ =>
          n._2.constVal + wfv
      }).map(_._2) take config.NP).zipWithIndex.foreach(x => {
        if (x._2 == 0)
          bestNode = x._1
        pop.add(x._1)
      })
      // Update rho using success trails from the explore phrase
      val rhoVec = pop.p.values.flatten(_.rho)
      if (rhoVec.nonEmpty)
        rhoMu = (1 - config.Ar) * rhoMu +
          config.Ar * (rhoVec.sum / rhoVec.size)
      flagExplore = false
      self ! Evolve
      stay()

    case Event(Evolve, _) if !converged && numEval < config.maxNumEval =>
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
                F * (bestNode.param(i) - nodeR0.param(i)) +
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
      } else {
        val size = evalStack.size
        val key = (true, Some(r0), java.util.UUID.randomUUID.toString)
        evalStack.push((key, nodeTrail), Some(config.NP))
        if (size == 0)
          workerRouter ! WorkAvailable(self)
      }
      self ! Evolve
      stay()

    case Event(Evolve, _) if converged =>
      self ! Converged
      stay()

    case Event(Evolve, _) if numEval >= config.maxNumEval =>
      self ! NotConverged
      stay()

    case Event(Select(i, node), _) =>
      numIter += 1
      // Compare ith node from pop with a new node, select the better node
      val r0 = i.asInstanceOf[Int]
      val nodeSelected = pop.selectBetterNode(
        node,
        pop.get(r0).get,
        config.constStrategy)
      pop.update(r0, nodeSelected)
      // Compare the better node with the current best node, see if it converges
      bestNode = pop.selectBetterNode(
        nodeSelected,
        bestNode,
        config.constStrategy)
      if (bestNode.objFnVal.nonEmpty &&
        !trace.contains(bestNode.objFnVal.get))
        trace.add(bestNode.objFnVal.get)
      convergeStep = trace.converged(config.tolRel) match {
        case Some(cond) =>
          if (cond)
            convergeStep + 1
          else
            0
        case _ =>
          convergeStep
      }
      if (convergeStep >= config.tolStep)
        converged = true
      // For every generation, 1. update F and Cr 2. explore population
      if (numIter % config.NP == 0) {
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
        if (Random.nextDouble < config.Er && !converged) {
          popExplore.empty
          flagExplore = true
        }
        if (config.logTrace)
          log.info(s"Num of Evals: $numEval, " +
            s"Best Value: ${bestNode.objFnVal}, " +
            s"Time Elapsed: ${System.currentTimeMillis() - startTime}")
      }
      stay()

  }

  whenUnhandled {

    case Event(GimmeWork, _) =>
      evalStack.pop() match {
        case Some(n) =>
          // Send work
          evalMap += (n._1 -> n._2)
          val objFn = n._2.request.objFn(
            _: List[Double],
            n._2.request.additionalParam)
          sender ! Work(self, key = n._1, param = n._2.param, fn = objFn, timeoutObjFn)
        case _ => // Do nothing
      }
      stay()

    case Event(Result(k, objVal), _) =>
      numEval += 1
      val keyStack = k.asInstanceOf[KeyType]
      val node = evalMap.get(keyStack).get
      node.objFnVal = Some(objVal)
      evalMap -= keyStack
      stateName match {
        case InitiationState =>
          if (keyStack._1) { // pop
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
        case EvolutionDEState =>
          if (keyStack._1) { // pop: evolving phrase
            keyStack._2 match {
              case Some(i) =>
                self ! Select(i, node)
              case _ =>
                pop.replaceWorst(node, config.constStrategy)
            }
          } else { // popExplore: exploring phrase
            if (popExplore.size < config.NP)
              popExplore.add(node)
            else
              popExplore.replaceWorst(node, config.constStrategy)
          }
      }
      stay()

    case Event(Failure(cause), _) =>
      if (config.logTrace)
        log.info(cause.toString)
      stay()

    case Event(Converged, _) =>
      stop(FSM.Normal)

    case Event(NotConverged, _) =>
      stop(FSM.Failure(NotConverged))

    case Event(Timeout, _) =>
      stop(FSM.Failure(Timeout))

  }

  onTransition {
    case InitiationState -> EvolutionDEState =>
      self ! Evolve
  }

  onTermination {
    case StopEvent(e, _, _) =>
      val status = e match {
        case FSM.Normal =>
          "Converged"
        case FSM.Failure(cause) =>
          cause.toString
        case _ =>
          e.toString
      }
      val solution = Solution(
        objValue = bestNode.objFnVal,
        param = bestNode.param,
        isFeasible = bestNode.constVal <= 0.0,
        isConverged = converged,
        numEval = numEval,
        status = status,
        timeElapsed = System.currentTimeMillis() - startTime)
      if (config.logTrace)
        log.info(solution.toString)
      journal.updateRow(key, solution)
  }

}
