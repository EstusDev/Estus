package com.estus.optimization

import org.scalatest.{Matchers, FlatSpec}



class EvalStackTest extends FlatSpec with Matchers {

  "An EvalStack" should
    "be able to peek the stack" in {
    val evalStack = EvalStack[String, Int]()
    evalStack.push(("id", 1))
    evalStack.size should be (1)
    evalStack.peek() should be (Some(("id", 1)))
  }

  it should
    "be able to push a node" in {
    val evalStack = EvalStack[String, Int]()
    evalStack.push(("id", 1))
    evalStack.size should be (1)
    evalStack.peek() should be (Some(("id", 1)))
  }

  it should
    "be able to push a node when there is size restriction" in {
    val evalStack = EvalStack[String, Int]()
    evalStack.push(("id1", 1), Some(2))
    evalStack.push(("id2", 2), Some(2))
    evalStack.push(("id3", 3), Some(2))
    evalStack.size should be (2)
    evalStack.peek() should be (Some(("id3", 3)))
  }

  it should
    "be able to pop a node" in {
    val evalStack = EvalStack[String, Int]()
    evalStack.push(("id", 1))
    evalStack.size should be (1)
    evalStack.pop() should be (Some(("id", 1)))
    evalStack.size should be (0)
  }

}
