package com.estus.optimization



case class Step (
    id: Int,
    dePi: Int,
    numEval: Int,
    request: Request) {

  val config = request.solverConfig.asInstanceOf[MOSConfig]
  val D = request.D
  val NP = config.NP
  var bestNode: PopulationNode = _
  var numEvalDE = 0
  var numEvalLS1 = 0

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

  // Dynamic Participation Function
  def generateNextStep (
    numEvalLeft: Int,
    xi: Double = 0.05,
    minDE: Double = 0.05): Step = {
    val Q = Quality()
    val P = Pi()
    val (iBest, iRest) = if (Q.head >= Q.last)
      (0, 1)
    else
      (1, 0)
    // Participation Decrement of the Weak Technique
    val eta = xi * (Q(iBest) - Q(iRest)) / Q(iBest)
    val numEvalNext: Int = {if (numEvalLeft > numEval) numEval else numEvalLeft}
    var dePi = P.head + ({if (iBest == 0) eta else -eta} * numEvalNext).toInt
    if (dePi < (minDE * numEvalNext).toInt)
      dePi = (minDE * numEvalNext).toInt
    if (dePi > numEvalNext)
      dePi = numEvalNext
    val step = Step (
      id = id + 1,
      dePi = dePi,
      numEval = numEvalNext,
      request = request)
    step.bestNode = bestNode
    step
  }

}

case class StepSeq (Step: Step) {

  var seq = List(Step)

}
