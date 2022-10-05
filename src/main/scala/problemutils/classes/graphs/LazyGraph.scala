package problemutils.classes.graphs

import scala.collection.mutable

case class LazyGraph[V] private (
  override val adjacencyFunction: V => Set[Edge[V]], 
  private val any: Any = None
) extends GeneratingGraph[V](adjacencyFunction):
  
  override def toString = if edges.isEmpty 
    then "LazyGraph(<not computed>)"
    else s"LazyGraph(${edges.mkString("", ", ", ", ")}<not computed>)"

  private val mVertices = mutable.Set.empty[V]
  private val mEdges = mutable.Set.empty[Edge[V]]
  private val mEdgesFrom = mutable.Map.empty[V, Set[Edge[V]]] withDefaultValue Set.empty
  private val mEdgesTo = mutable.Map.empty[V, Set[Edge[V]]] withDefaultValue Set.empty

  def edges = mEdges.toSet
  def edgesFrom = mEdgesFrom.toMap 
  def edgesTo = mEdgesTo.toMap
  def vertices = mVertices.toSet

  protected final given adjacentTo: (V => Set[Edge[V]]) = v => 
    if mEdgesFrom isDefinedAt v then mEdgesFrom(v)
    else
      val edgs = adjacencyFunction(v)
      mEdges ++= edgs
      mEdgesFrom += v -> edgs
      mEdgesTo ++= edgs.groupBy(_.to)
      mVertices ++= edgs.flatMap(e => Set(e.u, e.v))
      edgs

object LazyGraph:
  def apply[V](adjacencyFunction: V => IterableOnce[V]): LazyGraph[V] = 
    def adj(from: V) = adjacencyFunction(from).iterator.map(to => Edge(from, to)).toSet
    LazyGraph(adj)
  
  def from[V](adjacencyFunction: V => Set[Edge[V]]) = LazyGraph(adjacencyFunction)

  def fromCost[V](adjacencyFunction: V => IterableOnce[(V, Double)]) = 
    def adj(from: V) = adjacencyFunction(from).iterator.map((to, cost) => Edge(from, to, cost)).toSet
    LazyGraph(adj)