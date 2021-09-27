package mf.aerr.sat.hamiltongraph

import mf.aerr.sat.hamiltongraph.graph.{NGraph, Node}

object HamiltonGraph {

  /** Grafo hamiltoniano */
  val n1: Node = Node(1, List(2, 5))
  val n2: Node = Node(2, List(1, 3))
  val n3: Node = Node(3, List(2, 4, 5))
  val n4: Node = Node(4, List(3, 5))
  val n5: Node = Node(5, List(1, 3, 4))

  val hamiltonGraph: NGraph = NGraph(n1, n2, n3, n4, n5)

  /** Grafo no hamiltoniano */
  val m1: Node = Node(1, List(2))
  val m2: Node = Node(2, List(3))
  val m3: Node = Node(3, List(4, 5))
  val m4: Node = Node(4, List(3))
  val m5: Node = Node(5, List(3))

  val noHamiltonGraph: NGraph = NGraph(m1, m2, m3, m4, m5)

}
