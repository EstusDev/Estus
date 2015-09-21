package com.estus.optimization



case class Solution (
  objValue: Option[Double] = None,
  param: List[Double],
  isFeasible: Boolean,
  isConverged: Boolean,
  numEval: Int,
  status: String,
  timeElapsed: Long) {}
