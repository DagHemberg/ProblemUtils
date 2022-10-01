package problemutils.classes

import problemutils.*, extensions.*
import math.*, Numeric.Implicits.infixNumericOps

/** A generic Matrix class. Useful for working with 2D structures.
 * @tparam A The type of elements in the matrix. When `A` is a [[scala.Numeric]] type, a number of extension methods are made available which allow for basic mathematical matrix operations.
 * @param rows The number of rows in the matrix.
 * @param cols The number of columns in the matrix.
 * @param size A tuple of the height and width of the matrix.
*/
case class Matrix[A] private (private val input: Vector[Vector[A]]):
  extension [A] (input: Vector[Vector[A]]) private def tm = Matrix(input)

  val height = input.size
  val width = input.head.size
  val dimensions = (height, width)
  val size = height * width

  // doesn't work super well (read: at all) with other multi-line toStrings 
  override def toString = 
    if input.size == 1 then input.head.mkString("( ", " ", " )")
    else  
      def pad(vec: Vector[A]) = vec
        .zip(input.transpose.map(_.map(_.toString.size).max + 1))
        .map((a, b) => a.toString.padLeftTo(b, ' '))
        .mkString
      
      s"\n⎛${pad(input.head)} ⎞${
        if input.size == 2 then "" 
        else s"\n${input.init.tail.map(row => s"⎜${pad(row)} ⎟").mkString("\n")}"
      }\n⎝${pad(input.last)} ⎠"

  def apply(row: Int, col: Int): A = input(row)(col)
  def apply(index: Pos2D): A = input(index.row)(index.col)

  def isSquare = height == width
  def isSymmetric = this == transpose

  def row(row: Int) = input(row)
  def col(col: Int) = input.map(_(col))
  
  def toVector = input.flatten
  def rows = input
  def cols = input.transpose

  def range = Matrix.range(height, width)
  def indices = 
    (0 until height)
      .toVector
      .map(row => (0 until width).toVector.map(col => (row, col)))
      .tm

  def indexOutsideBounds(row: Int, col: Int): Boolean =
    0 > row || row >= height || 0 > col || col >= width
  def indexOutsideBounds(index: Pos2D): Boolean = 
    indexOutsideBounds(index.row, index.col)

  def map[B](f: A => B) = input.map(_.map(f)).tm
  def foreach[U](f: A => U) = input.foreach(_.foreach(f))
  def count(f: A => Boolean) = input.map(_.count(f)).sum
  def forall(f: A => Boolean) = input.forall(_.forall(f))
  def exists(f: A => Boolean) = input.exists(_.exists(f))

  def slice(row: Int, col: Int)(height: Int, width: Int): Matrix[A] = 
    input.slice(row, row + width).map(_.slice(col, col + height)).tm
  def slice(index: Pos2D)(height: Int, width: Int): Matrix[A] = 
    slice(index.row, index.col)(height, width)

  def reshape(height: Int, width: Int): Matrix[A] = 
    toVector.reshape(height, width)

  def filterRow(f: Vector[A] => Boolean) = input.filter(f).tm
  def filterCol(f: Vector[A] => Boolean) = input.transpose.filter(f).transpose.tm

  def transpose = input.transpose.tm
  def flipCols = input.map(_.reverse).tm
  def flipRows = input.reverse.tm
  def rotateRight = transpose.flipCols
  def rotateLeft = flipCols.transpose

  def swapRows(a: Int, b: Int) = 
    Matrix(input.updated(a, input(b)).updated(b, input(a)))
  
  def swapCols(a: Int, b: Int) =
    Matrix(input.map(_.updated(a, input(a)(b)).updated(b, input(a)(a))))
    // transpose.swapRows(a, b).transpose

  private def checkAppendHorizontal(other: Matrix[A]) = 
    require(
      other.height == height, 
      s"Can't append matrix of height ${other.height} to matrix of height ${height}"
    )

  private def checkAppendVertical(other: Matrix[A]) = 
    require(
      other.width == width, 
      s"Can't append matrix of width ${other.width} to matrix of width ${width}"
    )

  private def checkSquare(function: String) = 
    require(isSquare, s"Can't compute the $function of a non-square matrix")

  def appendedLeft(other: Matrix[A]) = 
    checkAppendHorizontal(other)
    Matrix(input.zip(other.input).map((row, otherRow) => otherRow ++ row))

  def appendedRight(other: Matrix[A]) =
    checkAppendHorizontal(other)
    Matrix(input.zip(other.input).map((row, otherRow) => row ++ otherRow))

  def appendedTop(other: Matrix[A]) =
    checkAppendVertical(other)
    Matrix(other.input ++ input)

  def appendedBottom(other: Matrix[A]) =
    checkAppendVertical(other)
    Matrix(input ++ other.input)

  def dropRow(row: Int) = (input.take(row) ++ input.drop(row + 1)).tm
  def dropCol(col: Int) = input.map(row => row.take(col) ++ row.drop(col + 1)).tm

  def zip[B](other: Matrix[B]): Matrix[(A, B)] =
    require(dimensions == other.dimensions, "Can't zip matrices of different dimensions")
    Matrix(input.zip(other.input).map((row, otherRow) => row.zip(otherRow)))

  def zipWithIndex: Matrix[(A, Pos2D)] = zip(indices)
  def zipWithRange: Matrix[(A, Int)] = zip(Matrix.range(height, width))

  def updated(row: Int, col: Int)(value: A): Matrix[A] = 
    input.updated(row, input(row).updated(col, value)).tm

  def sum(using Numeric[A]) = toVector.sum
  def product(using Numeric[A]) = toVector.product

  def +(other: Matrix[A])(using Numeric[A]): Matrix[A] = zip(other).map(_ + _)
  def -(other: Matrix[A])(using Numeric[A]): Matrix[A] = zip(other).map(_ - _)
  def *(other: Matrix[A])(using Numeric[A]): Matrix[A] = 
    require(width == other.height, "Incompatible matrix dimensions for multiplication")
    val rs = rows
    val cs = other.cols
    Matrix(height, other.width)(rs(_) dot cs(_))

  /** Computes the [determinant](https://en.wikipedia.org/wiki/Determinant) of the matrix.*/
  def determinant(using Numeric[A]): A = 
    checkSquare("determinant")
    width match
      case 1 => apply(0, 0)
      case 2 => (apply(0, 0) * apply(1, 1) - apply(0, 1) * apply(1, 0))
      case n => (0 until n)
        .map(i => (if i % 2 == 0 then apply(0, i) else -apply(0, i)) * (dropCol(i).dropRow(0)).determinant)
        .sum

  def trace(using Numeric[A]): A = 
    checkSquare("trace")
    (0 until width).map(i => apply(i, i)).sum

  def determinantOption(using Numeric[A]): Option[A] = if isSquare then Some(determinant) else None
  def traceOption(using Numeric[A]): Option[A] = if isSquare then Some(trace) else None
  

object Matrix:
  extension [A] (input: Vector[Vector[A]]) private def tm = Matrix(input)

  /** Typesafe helper enum for the rotation functions in the [[problemutils.classes.Matrix]] object. */
  enum Axis:
    case X, Y, Z

  def apply[A](height: Int, width: Int)(f: Pos2D => A): Matrix[A] = 
    Vector.tabulate(height, width)((a, b) => f(a, b)).tm

  def from[A](input: Seq[Seq[A]]) = 
    require(input.size > 0, "Matrix must have at least one row")
    require(input.head.size > 0, "Matrix must have at least one column")
    require(input.forall(_.size == input.head.size), "All rows must have the same length")
    Matrix(input.map(_.toVector).toVector)

  /** Creates an [identity matrix](https://en.wikipedia.org/wiki/Identity_matrix) of the given dimension. */
  def identity(size: Int) = 
    Matrix(size, size)((row, col) => (row == col).toInt)

  /** Produces a matrix of the given dimensions filled with the result of some computation. */
  def fill[A](height: Int, width: Int)(elem: => A) = 
    Vector.fill(height, width)(elem).tm

  def range(height: Int, width: Int) = 
    Matrix(height, width)(_ * width + _)

  /** Creates a 2x2 [rotation matrix](https://en.wikipedia.org/wiki/Rotation_matrix) for the given angle. */
  def rotation(rad: Double) = 
    Vector(
      Vector(cos(rad), -sin(rad)), 
      Vector(sin(rad), cos(rad))
    ).tm

  /** Creates a 3x3 [rotation matrix](https://en.wikipedia.org/wiki/Rotation_matrix) for the given angle and axis. */
  def rotation(rad: Double, dir: Axis) = 
    import Axis.*
    dir match
      case X => 
        Vector(
          Vector(1, 0, 0), 
          Vector(0, cos(rad), -sin(rad)), 
          Vector(0, sin(rad), cos(rad))
        ).tm
      case Y =>
        Vector(
          Vector(cos(rad), 0, sin(rad)), 
          Vector(0, 1, 0), 
          Vector(-sin(rad), 0, cos(rad))
        ).tm
      case Z =>
        Vector(
          Vector(cos(rad), -sin(rad), 0), 
          Vector(sin(rad), cos(rad), 0), 
          Vector(0, 0, 1)
        ).tm