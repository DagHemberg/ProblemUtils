package problemutils.classes.graphs

trait DynamicGraph[V] (using val adjacencyFunction: V => Set[Edge[V]]) extends Graph[V]:

  // känns som jag bryter hittills okända lagar i 12 landskap genom att göra så här
  given DynamicGraph[V] = this

  def filterEdges(p: Edge[V] => Boolean) = 
    DynamicGraph(v => adjacencyFunction(v).filter(p))

  def filterVertices(p: V => Boolean) =
    DynamicGraph(v => adjacencyFunction(v).filter(e => p(e.from) && p(e.to)))

  def incl(e: Edge[V]) = 
    if adjacencyFunction(e.from).map(_.to) contains e.to 
    then this
    else DynamicGraph(v => if v == e.from then adjacencyFunction(e.from) + e else adjacencyFunction(v))

  def excl(e: Edge[V]) = 
    if adjacencyFunction(e.from).map(_.to) contains e.to 
    then DynamicGraph(v => adjacencyFunction(v) - e)
    else this

  def union(other: Graph[V]) = other match
    case g: FiniteGraph[V] => DynamicGraph(v => adjacencyFunction(v) union g.edgesFrom(v))
    case g: DynamicGraph[V] => DynamicGraph(v => adjacencyFunction(v) union g.adjacencyFunction(v))

  def intersect(other: Graph[V]) = other match
    case g: FiniteGraph[V] => DynamicGraph(v => adjacencyFunction(v) intersect g.edgesFrom(v))
    case g: DynamicGraph[V] => DynamicGraph(v => adjacencyFunction(v) intersect g.adjacencyFunction(v))

  def diff(other: Graph[V]) = other match
    case g: FiniteGraph[V] => DynamicGraph(v => adjacencyFunction(v) diff g.edgesFrom(v))
    case g: DynamicGraph[V] => DynamicGraph(v => adjacencyFunction(v) diff g.adjacencyFunction(v))

private object DynamicGraph:
  def apply[V](adjacencyFunction: V => Set[Edge[V]])(using subClass: DynamicGraph[V]) = 
    subClass match
      case g: LazyGraph[V] => LazyGraph.from(adjacencyFunction)
      case g: ProceduralGraph[V] => ProceduralGraph.from(adjacencyFunction)