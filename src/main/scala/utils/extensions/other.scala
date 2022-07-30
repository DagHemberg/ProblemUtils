package utils.extensions

import utils.*
import math.Numeric.Implicits.infixNumericOps
import math.Integral.Implicits.infixIntegralOps
import math.Fractional.Implicits.infixFractionalOps

extension (b: Boolean)
  def toInt = if b then 1 else 0

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
    (tup - other).toVector.map(math.abs).sum

  def move(dir: Cardinal): Pos2D = tup + dir.toPos2D

  // infix def dot(other: Vec2): Double = toVector dot other.toVector
  // infix def cross(other: Vec2): Double = x * other.y - other.x * y
  // def magnitude: Double = toVector.magnitude

extension (v: Pos3D)
  def x = v._1
  def y = v._2
  def z = v._3

  def q = v._1
  def r = v._2
  def s = v._3

  def toVector = Vector(v.x, v.y, v.z)
  def +(other: Pos3D) = (v.x + other.x, v.y + other.y, v.z + other.z)
  def -(other: Pos3D) = (v.x - other.x, v.y - other.y, v.z - other.z)

  def distance(other: Pos3D) = (v - other).toVector.magnitude
  def manhattan(other: Pos3D) = (v - other).toVector.map(math.abs).sum

  def move(dir: Hex) = v + dir.toVec3

  // infix def dot(other: Vec3): Double = v.toVector dot other.toVector
  // infix def cross(other: Vec3): Vec3 = (v.toVector cross other.toVector).toVec3
  // def magnitude: Double = v.toVector.magnitude
