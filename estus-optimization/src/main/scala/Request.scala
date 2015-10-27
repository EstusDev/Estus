package com.estus.optimization

trait SolverConfig



case class Request (
    // Objective Function
    objFn: (List[Double], Option[Seq[Any]]) => Double,
    additionalParam: Option[Seq[Any]] = None,
    D: Int,
    // Equality and Inequality Constraints
    LB: List[Double], UB: List[Double],
    eqB: Option[List[Double]] = None, eqErr: Option[Double] = None,
    eqFunc: Option[(List[Double]) => List[Double]] = None,
    ineqLB: Option[List[Double]] = None, ineqUB: Option[List[Double]] = None,
    ineqFunc: Option[(List[Double]) => List[Double]] = None,
    // Initial Parameters
    initialParam: Option[List[Double]] = None,
    // Description of This Optimization Request
    description: Option[String] = None,
    // Config for Solver
    solverConfig: SolverConfig) {

  if (D <= 0) {
    throw new IllegalArgumentException(s"D must be > 0 (D = $D).")
  }

  if (LB.size != D) {
    throw new IllegalArgumentException(
      s"LB must be length of $D (LB.size = ${LB.size}).")
  }

  if (UB.size != D) {
    throw new IllegalArgumentException(
      s"UB must be length of $D (UB.size = ${UB.size}).")
  }

}



case class DEConfig (
    NP: Int,
    F: Option[Double] = None,
    Cr: Option[Double] = None,
    Ar: Double = 0.1, // Adaptive rate for F, Cr and Rho
    Er: Double = 0.3, // Explore rate
    mutationStrategy: String = "classic",
    constStrategy: String = "rank",
    tolRel: Double = 1e-8,
    tolStep: Int = 500,
    maxNumEval: Int = 3000000,
    logTrace: Boolean = false)
  extends SolverConfig {

  if (NP <= 3)
    throw new IllegalArgumentException(s"NP must be > 3 (NP = $NP).")

  F match {
    case Some(f) if f <= 0 || f > 1 =>
      throw new IllegalArgumentException(s"F must be in (0, 1] (F = $F).")
    case _ =>
  }

  Cr match {
    case Some(cr) if cr < 0 || cr > 1 =>
      throw new IllegalArgumentException(s"Cr must be in [0, 1] (Cr = $Cr).")
    case _ =>
  }

  if (Ar <= 0 || Ar > 1)
    throw new IllegalArgumentException(s"Ar must be in (0, 1] (Ar = $Ar).")

  if (Er < 0 || Er > 1)
    throw new IllegalArgumentException(s"Er must be in [0, 1] (Er = $Er).")

  if (tolRel < 0) {
    throw new IllegalArgumentException(
      s"tolRel must be >= 0 (tolRel = $tolRel).")
  }

  if (tolStep <= 0) {
    throw new IllegalArgumentException(
      s"tolStep must be > 0 (tolStep = $tolRel).")
  }

  if (maxNumEval <= 0) {
    throw new IllegalArgumentException(
      s"maxNumEval must be > 0 (maxNumEval = $maxNumEval).")
  }

  mutationStrategy match {
    case "classic" =>
    case "current-to-best" =>
    case _ =>
      throw new IllegalArgumentException(
        "mutationStrategy must be one of (classic, current-to-best)")
  }

  constStrategy match {
    case "rank" =>
    case "dominance" =>
    case _ =>
      throw new IllegalArgumentException(
        "constStrategy must be one of (rank, dominance)")
  }

}



case class MOSConfig (
    NP: Int,
    stepSize: Int,
    maxNumEval: Int,
    F: Option[Double] = None,
    Cr: Option[Double] = None,
    Ar: Double = 0.1, // Adaptive rate for Cr and Rho
    Er: Double = 0.3, // Explore rate
    xi: Double = 0.05, // Adaptive rate for DE participation
    minDE: Double = 0.05, // Min participation rate for DE
    minSR: Double = 1e-5, // Min Search Range for LS1
    mutationStrategy: String = "classic",
    constStrategy: String = "rank",
    tolRel: Double = 1e-8,
    tolStep: Int = 500,
    logTrace: Boolean = false)
  extends SolverConfig {

  if (NP <= 3)
    throw new IllegalArgumentException(s"NP must be > 3 (NP = $NP).")

  if (stepSize < 2)
    throw new IllegalArgumentException(s"stepSize must be >= 2 (stepSize = $stepSize).")

  if (stepSize >= maxNumEval)
    throw new IllegalArgumentException(s"stepSize must be < maxNumEval (stepSize = $stepSize maxNumEval = $maxNumEval).")

  if (maxNumEval <= 0) {
    throw new IllegalArgumentException(
      s"maxNumEval must be > 0 (maxNumEval = $maxNumEval).")
  }

  F match {
    case Some(f) if f <= 0 || f > 1 =>
      throw new IllegalArgumentException(s"F must be in (0, 1] (F = $F).")
    case _ =>
  }

  Cr match {
    case Some(cr) if cr < 0 || cr > 1 =>
      throw new IllegalArgumentException(s"Cr must be in [0, 1] (Cr = $Cr).")
    case _ =>
  }

  if (Ar <= 0 || Ar > 1)
    throw new IllegalArgumentException(s"Ar must be in (0, 1] (Ar = $Ar).")

  if (Er < 0 || Er > 1)
    throw new IllegalArgumentException(s"Er must be in [0, 1] (Er = $Er).")

  if (xi <= 0.0 || xi >= 1.0)
    throw new IllegalArgumentException(s"xi must be in (0, 1) (xi = $xi).")

  if (minDE <= 0.0 || minDE >= 1.0)
    throw new IllegalArgumentException(s"minDE must be in (0, 1) (minDE = $minDE).")

  if (minSR <= 0.0 || minSR >= 0.5)
    throw new IllegalArgumentException(s"minSR must be in (0, 0.5) (minSR = $minSR).")

  if (tolRel < 0) {
    throw new IllegalArgumentException(
      s"tolRel must be >= 0 (tolRel = $tolRel).")
  }

  if (tolStep <= 0) {
    throw new IllegalArgumentException(
      s"tolStep must be > 0 (tolStep = $tolRel).")
  }

  mutationStrategy match {
    case "classic" =>
    case "current-to-best" =>
    case _ =>
      throw new IllegalArgumentException(
        "mutationStrategy must be one of (classic, current-to-best)")
  }

  constStrategy match {
    case "rank" =>
    case "dominance" =>
    case _ =>
      throw new IllegalArgumentException(
        "constStrategy must be one of (rank, dominance)")
  }

}
