package problemutils.classes.graphs

trait GeneratingGraph[V] (val adjacencyFunction: V => Set[Edge[V]]) extends Graph[V]:

  // känns som jag bryter hittils okända lagar i 12 landskap genom att göra så här
  given GeneratingGraph[V] = this

  def filterEdges(p: Edge[V] => Boolean) = 
    GeneratingGraph(v => adjacencyFunction(v).filter(p))

  def filterVertices(p: V => Boolean) =
    GeneratingGraph(v => adjacencyFunction(v).filter(e => p(e.from) && p(e.to)))

  def incl(e: Edge[V]) = 
    if adjacencyFunction(e.from).map(_.to) contains e.to 
    then this
    else GeneratingGraph(v => if v == e.from then adjacencyFunction(e.from) + e else adjacencyFunction(v))

  def excl(e: Edge[V]) = 
    if adjacencyFunction(e.from).map(_.to) contains e.to 
    then GeneratingGraph(v => adjacencyFunction(v) - e)
    else this

  def union(other: Graph[V]) = other match
    case g: FiniteGraph[V] => GeneratingGraph(v => adjacencyFunction(v) union g.edgesFrom(v))
    case g: GeneratingGraph[V] => GeneratingGraph(v => adjacencyFunction(v) union g.adjacencyFunction(v))

  def intersect(other: Graph[V]) = other match
    case g: FiniteGraph[V] => GeneratingGraph(v => adjacencyFunction(v) intersect g.edgesFrom(v))
    case g: GeneratingGraph[V] => GeneratingGraph(v => adjacencyFunction(v) intersect g.adjacencyFunction(v))

  def diff(other: Graph[V]) = other match
    case g: FiniteGraph[V] => GeneratingGraph(v => adjacencyFunction(v) diff g.edgesFrom(v))
    case g: GeneratingGraph[V] => GeneratingGraph(v => adjacencyFunction(v) diff g.adjacencyFunction(v))

private object GeneratingGraph:
  def apply[V](adjacencyFunction: V => Set[Edge[V]])(using subClass: GeneratingGraph[V]) = 
    subClass match
      case g: LazyGraph[V] => LazyGraph.from(adjacencyFunction)
      case g: ProceduralGraph[V] => ProceduralGraph.from(adjacencyFunction)