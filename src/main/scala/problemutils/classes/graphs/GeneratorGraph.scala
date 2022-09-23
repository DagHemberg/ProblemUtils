package problemutils.classes.graphs

trait GeneratorGraph[V] (val adjacencyFunction: V => Set[Edge[V]]) extends Graph[V]:

  // känns som jag bryter typ 12 lagar genom att göra så här
  given GeneratorGraph[V] = this

  def filterEdges(p: Edge[V] => Boolean) = 
    GeneratorGraph(v => adjacencyFunction(v).filter(p))

  def filterVertices(p: V => Boolean) =
    GeneratorGraph(v => adjacencyFunction(v).filter(e => p(e.from) && p(e.to)))

  def incl(e: Edge[V]) = 
    if adjacencyFunction(e.from).map(_.to) contains e.to 
    then this
    else GeneratorGraph(v => if v == e.from then adjacencyFunction(e.from) + e else adjacencyFunction(v))

  def excl(e: Edge[V]) = 
    if adjacencyFunction(e.from).map(_.to) contains e.to 
    then GeneratorGraph(v => adjacencyFunction(v) - e)
    else this

  def union(other: Graph[V]) = other match
    case g: FiniteGraph[V] => GeneratorGraph(v => adjacencyFunction(v) union g.edgesFrom(v))
    case g: GeneratorGraph[V] => GeneratorGraph(v => adjacencyFunction(v) union g.adjacencyFunction(v))

  def intersect(other: Graph[V]) = other match
    case g: FiniteGraph[V] => GeneratorGraph(v => adjacencyFunction(v) intersect g.edgesFrom(v))
    case g: GeneratorGraph[V] => GeneratorGraph(v => adjacencyFunction(v) intersect g.adjacencyFunction(v))

  def diff(other: Graph[V]) = other match
    case g: FiniteGraph[V] => GeneratorGraph(v => adjacencyFunction(v) diff g.edgesFrom(v))
    case g: GeneratorGraph[V] => GeneratorGraph(v => adjacencyFunction(v) diff g.adjacencyFunction(v))

private object GeneratorGraph:
  def apply[V](adjacencyFunction: V => Set[Edge[V]])(using subClass: GeneratorGraph[V]) = 
    subClass match
      case g: LazyGraph[V] => LazyGraph.from(adjacencyFunction)
      case g: ProceduralGraph[V] => ProceduralGraph.from(adjacencyFunction)