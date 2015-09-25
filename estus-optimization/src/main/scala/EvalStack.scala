package com.estus.optimization



trait StackKey[T] {
  type KeyType = T
}



case class EvalStack [T] () {

  private var stack = List.empty[(T, PopulationNode)]

  def size: Int = stack.size

  def push (
             node: (T, PopulationNode),
             maxSize: Option[Int] = None): Unit = {
    stack = node :: stack
    maxSize match {
      case Some(s) =>
        stack = stack take s
      case _ =>
    }
  }

  def pop (): Option[(T, PopulationNode)] = {
    if (stack.nonEmpty) {
      val v = stack.head
      stack = stack.tail
      Some(v)
    } else {
      None
    }
  }

  def empty: Unit = stack = List.empty[(T, PopulationNode)]

}
