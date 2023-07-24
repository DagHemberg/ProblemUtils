package problemutils.classes.graphs

import scala.collection.mutable
import problemutils.extensions.*

private [graphs] object Pathfinder:
  def backtrack[V](current: V)(using prev: Map[V, V], start: V): Vector[V] =
    if prev(current) == start then Vector(start, current)
    else backtrack(prev(current)) :+ current

  def fastBfs[V]
    (start: V, stoppingPredicate: V => Boolean, heuristic: V => Double)
    (using adj: V => Set[Edge[V]]) = 

    val pq = mutable.PriorityQueue(start -> 0d)(Ordering by snd).reverse
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
          dist(dest) = alt
          prev(dest) = min
          pq.enqueue((dest, alt + heuristic(dest)))

    (dist.toMap, prev.toMap, found)

  def dijkstra[V](start: V)(using V => Set[Edge[V]]): Set[Path[V]] = 
    val (dist, prev, _) = fastBfs(start, _ => false, _ => 0)
    
    // todo clean up
    (dist.keySet - start).map(v => Path(backtrack(v)(using prev, start), dist(v))) + Path(Seq(start), 0)

  def aStar[V](start: V, endPred: V => Boolean, heuristic: V => Double)(using V => Set[Edge[V]]) = 
    val (dist, prev1, found) = Pathfinder.fastBfs(start, endPred, heuristic)
    if found then 
      val prev = if prev1.isEmpty then Map(start -> start) else prev1
      val end = prev.find((a, b) => endPred(a)).get._1
      val path = Pathfinder.backtrack(end)(using prev, start)
      Some(Path(path, dist(end)))
    else None