package problemutils

type Vec2 = (Int, Int)
type Vec3 = (Int, Int, Int)
type Pos2D = Vec2
type Pos3D = Vec3

type Cardinal = classes.Cardinal
type Hex = classes.Hex
val Cardinal = classes.Cardinal
val Hex = classes.Hex

type Matrix[A] = classes.Matrix[A]
val Matrix = classes.Matrix
val Axis = classes.Matrix.Axis

type Graph[V] = classes.graphs.Graph[V]
type FiniteGraph[V] = classes.graphs.FiniteGraph[V]
type LazyGraph[V] = classes.graphs.LazyGraph[V]
type ProceduralGraph[V] = classes.graphs.ProceduralGraph[V]
type Edge[V] = classes.graphs.Edge[V]
type Path[V] = classes.graphs.Path[V]
type DisjointSets[V] = classes.graphs.DisjointSets[V]

val Graph = classes.graphs.Graph
val FiniteGraph = classes.graphs.FiniteGraph
val LazyGraph = classes.graphs.LazyGraph
val ProceduralGraph = classes.graphs.ProceduralGraph
val Edge = classes.graphs.Edge
val Path = classes.graphs.Path
val DisjointSets = classes.graphs.DisjointSets
