package com.estus.optimization



trait StackKey[T] {
  type KeyType = T
}



case class EvalStack [T, A] () {

  private var stack = List.empty[(T, A)]

  def size: Int = stack.size

  def push (
    node: (T, A),
    maxSize: Option[Int] = None): Unit = {
    stack = node :: stack
    maxSize match {
      case Some(s) =>
        stack = stack take s
      case _ =>
    }
  }

  def pop (): Option[(T, A)] = {
    if (stack.nonEmpty) {
      val v = stack.head
      stack = stack.tail
      Some(v)
    } else {
      None
    }
  }

  def empty: Unit = stack = List.empty[(T, A)]

}
