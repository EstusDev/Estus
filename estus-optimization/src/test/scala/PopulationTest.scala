package com.estus.optimization

import org.scalatest.{Matchers, FlatSpec}



class PopulationTest extends FlatSpec with Matchers {

  def fn(p: List[Double], other: Option[Seq[Any]] = None): Double = p.sum
  def eqFunc(p: List[Double]): List[Double] = List(p.sum)
  def ineqFunc(p: List[Double]): List[Double] = List(p.sum)
  val request = Request(
    objFn = fn,
    D = 2,
    LB = List.fill(2)(-10.0),
    UB = List.fill(2)(10.0),
    eqFunc = Some(eqFunc),
    eqB = Some(List(1.0)),
    eqErr = Some(0.01),
    ineqFunc = Some(ineqFunc),
    ineqLB = Some(List(0.0)),
    ineqUB = Some(List(2.0)),
    solverConfig = DEConfig(NP = 10))



  "A PopulationNode" should
    "be have a correct constVec and constVal" in {
    val node = PopulationNode(List(-1.0, -1.0), request)
    node.constVec should be (List(2.99, 2.0, -4.0))
    node.constVal should be (4.99)
  }

  "A Population" should
    "be able to add a node" in {
    val node = PopulationNode(List(-1.0, -1.0), request)
    val pop = Population(10)
    pop.add(node)
    pop.size should be (1)
    pop.get(0) should be (Some(node))
  }

  it should
    "be able to get a node in the correct order" in {
    val node1 = PopulationNode(List(-1.0, -1.0), request)
    val node2 = PopulationNode(List(1.0, 1.0), request)
    val pop = Population(10)
    pop.add(node2)
    pop.add(node1)
    pop.size should be (2)
    pop.get(1) should be (Some(node2))
    pop.get(0) should be (Some(node1))
  }

  it should
    "be able to update a node" in {
    val node1 = PopulationNode(List(-1.0, -1.0), request)
    val node2 = PopulationNode(List(1.0, 1.0), request)
    val pop = Population(10)
    pop.add(node2)
    pop.add(node1)
    pop.size should be (2)
    pop.get(1) should be (Some(node2))
    pop.update(1, node1)
    pop.get(1) should be (Some(node1))
  }

  it should
    "be able to replace the worst node" in {
    val node1 = PopulationNode(List(-1.0, -1.0), request)
    val node2 = PopulationNode(List(-1.0, -1.0), request)
    node2.objFnVal = Some(0.0)
    val node3 = PopulationNode(List(-1.0, -1.0), request)
    node2.objFnVal = Some(9999.9)
    val pop = Population(10)
    pop.add(node1)
    pop.add(node2)
    pop.size should be (2)
    pop.get(0) should be (Some(node1))
    pop.replaceWorst(node3, "rank")
    pop.get(0) should be (Some(node3))
  }

  it should
    "be able to identify the worst feasible value" in {
    val node1 = PopulationNode(List(-1.0, -1.0), request)
    val node2 = PopulationNode(List(0.5, 0.5), request)
    node2.objFnVal = Some(fn(node2.param))
    val pop = Population(10)
    pop.add(node1)
    pop.add(node2)
    pop.worstFeasibleVal should be (Some(1.0))
  }

  it should
    "be able to select the better node from two nodes" in {
    val node1 = PopulationNode(List(-1.0, -1.0), request)
    val node2 = PopulationNode(List(-1.0, -1.0), request)
    node2.objFnVal = Some(0.0)
    val pop = Population(10)
    pop.selectBetterNode(node1, node2, "rank") should be (node2)
  }

  it should
    "be able to merge with another Population" in {
    val node1 = PopulationNode(List(1.0, 1.0), request)
    val node2 = PopulationNode(List(-1.0, -1.0), request)
    val pop1 = Population(10)
    pop1.add(node1)
    pop1.add(node2)
    val pop2 = Population(10)
    pop2.add(node1)
    pop2.add(node2)
    val popU = pop1.merge(pop2)
    popU.size should be (4)
    popU.keys should be (pop1.keys ++ pop2.keys)
    popU.get(3).get.param should be (List(1.0, 1.0))
  }

}
