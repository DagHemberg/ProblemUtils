package problemutils.classes.graphs

import problemutils.extensions.*

case class ProceduralGraph[V] private (
    override val adjacencyFunction: V => Set[Edge[V]], 
    private val any: Any = None
  ) extends GeneratingGraph[V](adjacencyFunction):

  override def toString = "ProceduralGraph(<not computed>)"

  protected given adjacentTo: (V => Set[Edge[V]]) = adjacencyFunction
  val edges = Set.empty
  val edgesFrom = Map.empty
  val edgesTo = Map.empty
  val vertices = Set.empty

object ProceduralGraph:
  def apply[V](adjacencyFunction: V => IterableOnce[V]): ProceduralGraph[V] =
    def adj(from: V) = adjacencyFunction(from).iterator.map(to => Edge(from, to)).toSet
    ProceduralGraph(adj)

  def from[V](adjacencyFunction: V => Set[Edge[V]]) = ProceduralGraph(adjacencyFunction)

  def fromCost[V](adjacencyFunction: V => IterableOnce[(V, Double)]) =
    def adj(from: V) = adjacencyFunction(from).iterator.map((to, cost) => Edge(from, to, cost)).toSet
    ProceduralGraph(adj)
