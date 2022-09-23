package problemutils.classes.graphs

import scala.collection.mutable

// TODO
// - components: Set[Graph[V]]
// - stronglyConnectedComponents: Set[Graph[V]]

abstract class Graph[V]:
  import Graph.*
  protected given adjacentTo: (V => Set[Edge[V]])

  def edges: Set[Edge[V]]
  def edgesFrom: Map[V, Set[Edge[V]]]
  def edgesTo: Map[V, Set[Edge[V]]]
  def vertices: Set[V]

  def filterEdges(p: Edge[V] => Boolean): Graph[V]
  def filterVertices(p: V => Boolean): Graph[V]

  def incl(e: Edge[V]): Graph[V]
  def excl(e: Edge[V]): Graph[V]

  def union(g: Graph[V]): Graph[V]
  def intersect(g: Graph[V]): Graph[V]
  def diff(g: Graph[V]): Graph[V]

  def +(e: Edge[V]) = incl(e)
  def -(e: Edge[V]) = excl(e)
  
  def &(g: Graph[V]) = intersect(g)
  def |(g: Graph[V]) = union(g)
  def \(g: Graph[V]) = diff(g)
  
  def apply(v: V) = adjacentTo(v).map(_.to)
  def apply(a: V, b: V) = adjacentTo(a).find(_.to == b).map(_.weight)

  def pathsFrom(start: V) = dijkstra(start)
  def pathBetween(start: V, end: V, heuristic: V => Double = _ => 0) = aStar(start, end, heuristic)

  def reachableFrom(start: V) = pathsFrom(start).map(_.last)

object Graph:
  def apply[V](elems: (V, V)*) = FiniteGraph.from(elems.map(Edge(_, _)).toSet)
  def from[V](edges: IterableOnce[Edge[V]]) = FiniteGraph.from(edges.iterator.toSet)
  
  def generate[V](adjacencyFunction: V => IterableOnce[V]) = 
    ProceduralGraph(adjacencyFunction)
  def generateWith[V](adjacencyFunction: V => IterableOnce[Edge[V]]) = 
    ProceduralGraph.from(adjacencyFunction andThen (_.iterator.toSet))

  def generateLazily[V](adjacencyFunction: V => IterableOnce[V]) = 
    LazyGraph(adjacencyFunction)
  def generateLazilyWith[V](adjacencyFunction: V => IterableOnce[Edge[V]]) = 
    LazyGraph.from(adjacencyFunction andThen (_.iterator.toSet))
    
  def empty[V] = FiniteGraph.empty[V]

  private def backtrack[V](current: V)(using prev: Map[V, V], start: V): Vector[V] =
    if prev(current) == start then Vector(start, current)
    else backtrack(prev(current)) :+ current

  private def fastBfs[V]
    (start: V, stoppingPredicate: V => Boolean, heuristic: V => Double)
    (using adj: V => Set[Edge[V]]) = 

    val pq = mutable.PriorityQueue(start -> 0d)(Ordering.by(_._2)).reverse
    val prev = mutable.Map.empty[V, V]
    val dist = mutable.Map(start -> 0d) withDefaultValue Double.PositiveInfinity

    var found = false
    while !found && pq.nonEmpty do
      val (min, _) = pq.dequeue

      if stoppingPredicate(min) then found = true
      else for edge <- adj(min) do
        val alt = dist(min) + edge.weight
        val dest = edge.to
        if alt < dist(dest) then
          pq.enqueue((dest, alt + heuristic(dest)))
          dist(dest) = alt
          prev(dest) = min

    (dist.toMap, prev.toMap, found)

  def dijkstra[V](start: V)(using V => Set[Edge[V]]): Set[Path[V]] = 
    val (dist, prev, _) = fastBfs(start, _ => false, _ => 0)
    
    // todo clean up
    (dist.keySet - start).map(v => Path(backtrack(v)(using prev, start), dist(v))) + Path(Seq(start), 0)

  def aStar[V](start: V, end: V, heuristic: V => Double)(using V => Set[Edge[V]]) = 
    val (dist, prev, found) = fastBfs(start, _ == end, heuristic)
    
    if found then Some(Path(backtrack(end)(using prev, start), dist(end)))
    else None
