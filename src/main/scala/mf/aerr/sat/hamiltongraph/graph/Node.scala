package mf.aerr.sat.hamiltongraph.graph

/** ADT para gestionar los grafos y los nodos de un grafo */

sealed trait Graph

case class NGraph(nd: Node*) extends Graph {
  def node(i: Int): Node = nd.find(node => node.i == i).get

  def neighbors(i: Int): Seq[Int] = node(i).neighbors

  def strangers(i: Int): Seq[Int] = index.diff(i +: neighbors(i))

  def length: Int = nd.length

  def index: Seq[Int] = nd.map(node => node.i)
}

case class Node(i: Int, neighbors: Seq[Int])
