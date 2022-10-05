package problemutils.classes.graphs

import collection.mutable

/** A simple recursive tree class. 
 * @param value The value of this node.
 * @param children The children of this node.
 */
case class Tree[V](value: V, children: Set[Tree[V]] = Set.empty[Tree[V]]):
  override def toString = 
    if children.isEmpty then value.toString
    else s"{ $value -> { ${children.mkString(", ")} } }"

  /** Returns true if this tree is a subtree of another tree, false otherwise. */
  def isSubtreeOf(other: Tree[V]): Boolean =
    this == other || other.children.exists(isSubtreeOf)

  lazy val vertices: Set[V] = children.flatMap(_.vertices) + value

/** A simple generic edge class. Can represent either a directed or an undirected edge, and so has both `from` / `to` vertex parameters, as well as `u` and `v` value members for easy disambiguation.  
  * @param from the source vertex
  * @param to the destination vertex
  * @param u alias for `from`
  * @param v alias for `to`
  * @param weight the cost of traveling along this edge, defaults to 1 if not provided
  */
case class Edge[V](from: V, to: V, weight: Double = 1):
  val (u, v) = (from, to)
  def @@(w: Double) = copy(weight = w)
  lazy val reverse = Edge(to, from, weight)
  override def toString = s"Edge($from -> $to, $weight)"

/**
  * @param vertices the ordered sequence of vertices making up the path
  * @param cost the total cost of the path
  */
case class Path[V](vertices: Seq[V], cost: Double):
  lazy val reverse = Path(vertices.reverse, cost)
  def apply = vertices.apply
  def head = vertices.head
  def last = vertices.last
  override def toString = s"Path(${vertices.mkString(" -> ")}, $cost)"

/** A generic mutable implementation of the [disjoint sets data structure](https://en.wikipedia.org/wiki/Disjoint-set_data_structure). 
  * @param nodes all the vertices currently tracked by this data structure
  */
case class DisjointSets[V](var nodes: V*):
  override def toString = parents.groupBy(_._2).map(_._2).toSet.mkString("[", ", ", "]")
  val parents = mutable.Map.empty[V, V]
  val rank = mutable.Map.empty[V, Int]

  for node <- nodes do
    parents(node) = node
    rank(node) = 0

  def findSet(v: V): V =
    if parents(v) != v then parents(v) = findSet(parents(v))
    parents(v)

  def makeUnion(a: V, b: V) =
    val u = findSet(a)
    val v = findSet(b)
    if u != v then rank(u) compare rank(v) match
      case 1  => parents(v) = u
      case -1 => parents(u) = v
      case 0  => 
        parents(u) = v
        rank(v) += 1

  def add(v: V) =
    nodes = nodes :+ v
    parents(v) = v
    rank(v) = 0

object DisjointSets:
  def from[V](nodes: Iterable[V]): DisjointSets[V] = DisjointSets(nodes.toSeq*)