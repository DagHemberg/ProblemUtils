package problemutils.classes.graphs

import scala.collection.mutable

case class FiniteGraph[V] private (private val elems: Set[Edge[V]]) extends Graph[V]:
  override def toString = s"Graph(${elems.mkString(", ")})"
  val edges = elems
  lazy val edgesFrom = edges.groupBy(_.from) withDefaultValue Set.empty
  lazy val edgesTo = edges.groupBy(_.to) withDefaultValue Set.empty
  lazy val vertices = edges.flatMap(e => Set(e.u, e.v))

  protected final given adjacentTo: (V => Set[Edge[V]]) = edgesFrom.apply

  def filterEdges(p: Edge[V] => Boolean) = FiniteGraph(elems.filter(p))
  def filterVertices(p: V => Boolean) = FiniteGraph(elems.filter(e => p(e.u) && p(e.v)))

  def incl(e: Edge[V]) = FiniteGraph(elems incl e)
  def excl(e: Edge[V]) = FiniteGraph(elems excl e)

  def union(other: Graph[V]) = other match
    case g: FiniteGraph[V] => FiniteGraph(elems union g.elems)
    case g: DynamicGraph[V] => g union this

  def intersect(other: Graph[V]) = other match
    case g: FiniteGraph[V] => FiniteGraph(elems intersect g.elems)
    case g: DynamicGraph[V] => g intersect this

  def diff(other: Graph[V]) = other match
    case g: FiniteGraph[V] => FiniteGraph(elems diff g.elems)
    case g: DynamicGraph[V] => g diff this

  /** Reverses all the edges in the graph. */
  def transpose = Graph.from(edges.map(_.reverse))
  def totalCost = edges.toSeq.map(_.weight).sum

  def minimumSpanningGraph = 
    val djs = DisjointSets.from(vertices)
    val connectingEdges = mutable.Set.empty[Edge[V]]
    val pq = mutable.PriorityQueue.empty[Edge[V]](Ordering.by(_.weight)).reverse
    pq ++= edges

    while pq.nonEmpty do
      val edge @ Edge(u, v, _) = pq.dequeue
      if djs.findSet(u) != djs.findSet(v) then
        connectingEdges += edge
        djs.makeUnion(u, v)

    Graph.from(connectingEdges.toSet)

object FiniteGraph:
  def apply[V](elems: Edge[V]*): FiniteGraph[V] = FiniteGraph(elems.toSet)
  def from[V](elems: IterableOnce[Edge[V]]): FiniteGraph[V] = FiniteGraph(elems.iterator.toSet)
  def empty[V]: FiniteGraph[V] = FiniteGraph(Set.empty[Edge[V]])
