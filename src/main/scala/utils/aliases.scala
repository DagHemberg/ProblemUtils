package utils

type Vec2 = (Int, Int)
type Vec3 = (Int, Int, Int)
type Pos2D = Vec2
type Pos3D = Vec3

type Problem[A] = classes.Problem[A]
type Primary[A] = classes.Primary[A]
type Secondary[A] = classes.Secondary[A]
val Primary = classes.Primary
val Secondary = classes.Secondary
val Skip = classes.Skip

type Cardinal = classes.Cardinal
type Hex = classes.Hex
type TimedEval[A] = classes.TimedEval[A]
val TimedEval = classes.TimedEval
val Cardinal = classes.Cardinal
val Hex = classes.Hex

type Matrix[A] = classes.Matrix[A]
val Matrix = classes.Matrix
val X = classes.Matrix.Axis.X
val Y = classes.Matrix.Axis.Y
val Z = classes.Matrix.Axis.Z

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

