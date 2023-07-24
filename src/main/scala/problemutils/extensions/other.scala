package problemutils.extensions

import problemutils.*

import math.Numeric.Implicits.infixNumericOps
import math.Integral.Implicits.infixIntegralOps
import math.Fractional.Implicits.infixFractionalOps

extension [V] (a: V)
  def ->>(b: V): Edge[V] = Edge(a, b)

extension (p: Boolean)
  def toInt = if p then 1 else 0
  
extension (p: Int)
  def toBoolean = p != 0

extension [A](mat: Matrix[Matrix[A]])
  def flatten = mat.rows
    .map(_.reduce(_ appendedRight _))
    .reduce(_ appendedBottom _)  

extension (tup: Pos2D)
  def transpose: Pos2D = tup.swap

  def row = tup._1
  def col = tup._2

  def x = tup._2
  def y = tup._1

  def +(other: Pos2D) = (tup._1 + other._1, tup._2 + other._2)
  def -(other: Pos2D) = (tup._1 - other._1, tup._2 - other._2)
  def %(other: Pos2D) = (math.floorMod(tup._1, other._1), math.floorMod(tup._2, other._2))

  def up: Pos2D = (row - 1, col)
  def down: Pos2D = (row + 1, col)
  def left: Pos2D = (row, col - 1)
  def right: Pos2D = (row, col + 1)
  def ul: Pos2D = (row - 1, col - 1)
  def ur: Pos2D = (row - 1, col + 1)
  def dl: Pos2D = (row + 1, col - 1)
  def dr: Pos2D = (row + 1, col + 1)

  def neighbours: List[Pos2D] = 
    List(up, down, left, right, ul, ur, dl, dr)
  def neighboursOrth: List[Pos2D] = 
    List(up, left, right, down)
  def neighboursDiag: List[Pos2D] = 
    List(ul, ur, dl, dr)

  private def outsideFilter[A](list: List[Pos2D])(using mat: Matrix[A]) = 
    list filterNot mat.indexOutsideBounds map mat.apply

  def neighboursIn[A](using mat: Matrix[A]): List[A] = outsideFilter(neighbours)
  def neighboursOrthIn[A](using mat: Matrix[A]): List[A] = outsideFilter(neighboursOrth)
  def neighboursDiagIn[A](using mat: Matrix[A]): List[A] = outsideFilter(neighboursDiag)

  def toVector = Vector(tup._1, tup._2)

  def distance(other: Pos2D) = 
    (tup - other).toVector.magnitude
  def manhattan(other: Pos2D) = 
    (tup - other).toVector.sumBy(math.abs)

  def move(dir: Cardinal): Pos2D = tup + dir.toPos2D

extension (v: Pos3D)
  def x = v._1
  def y = v._2
  def z = v._3

  def q = v._1
  def r = v._2
  def s = v._3

  def up: Pos3D    = (x, y - 1, z)
  def down: Pos3D  = (x, y + 1, z)
  def left: Pos3D  = (x - 1, y, z)
  def right: Pos3D = (x + 1, y, z)
  def in: Pos3D    = (x, y, z - 1)
  def out: Pos3D   = (x, y, z + 1)

  def toVector = Vector(v.x, v.y, v.z)
  def +(other: Pos3D) = (v.x + other.x, v.y + other.y, v.z + other.z)
  def -(other: Pos3D) = (v.x - other.x, v.y - other.y, v.z - other.z)

  def distance(other: Pos3D) = (v - other).toVector.magnitude
  def manhattan(other: Pos3D) = (v - other).toVector.sumBy(math.abs)

  def move(dir: Hex) = v + dir.toVec3
