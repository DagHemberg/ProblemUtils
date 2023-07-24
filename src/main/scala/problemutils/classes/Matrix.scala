package problemutils.classes

import problemutils.*, extensions.*
import math.*, Numeric.Implicits.infixNumericOps

/** A generic Matrix class. Useful for working with 2D structures.
 * @tparam A The type of elements in the matrix. When `A` is a [[scala.Numeric]] type, a number of extension methods are made available which allow for basic mathematical matrix operations.
*/
case class Matrix[A] private (private val input: Vector[Vector[A]]) 
  extends IterableOnce[A]:
  
  extension [A] (input: Vector[Vector[A]]) private def tm = new Matrix(input)

  val height = input.size
  val width = input.head.size

  /** The height and width of this matrix. */
  val dimensions = (height, width)
  /** The "area", or `height` x `width`, of this matrix. */
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

  override def iterator: Iterator[A] = input.iterator.flatMap(_.iterator)

  def mkString = input.map(_.mkString).mkString

  def mkString(sep: String) = 
    input.map(_.mkString(sep)).mkString(sep)

  def mkString(sep: String, rowSep: String) = 
    input.map(_.mkString(sep)).mkString(rowSep)

  def mkString(start: String, sep: String, end: String) = 
    input.map(_.mkString(start, sep, end)).mkString(start, sep, end)

  /** Returns the element at the given position `(row, col)` */
  def apply(index: (Int, Int)): A = input(index.row)(index.col)

  def isSquare = height == width

  /** Returns `true` if this matrix is equal to its own transpose. */
  def isSymmetric = this == transpose

  /** Returns the row at the given index */
  def row(row: Int) = input(row)

  /** Returns the column at the given index */
  def col(col: Int) = input.map(_(col))
  
  def toVector = input.flatten
  def rows = input
  def cols = input.transpose

  def range = Matrix.range(height, width)
  def indices = 
    (for row <- 0 until height; col <- 0 until width yield (row, col)).reshape(height, width)

  def indexOutsideBounds(index: Pos2D): Boolean = 
    0 > index.row || index.row >= height || 0 > index.col || index.col >= width

  def map[B](f: A => B) = input.map(_.map(f)).tm
  def foreach[U](f: A => U) = input.foreach(_.foreach(f))
  def count(f: A => Boolean) = input.map(_.count(f)).sum
  def forall(f: A => Boolean) = input.forall(_.forall(f))
  def exists(f: A => Boolean) = input.exists(_.exists(f))

  def slice(index: (Int, Int))(height: Int, width: Int): Matrix[A] = 
    input.slice(index.row, index.row + width).map(_.slice(index.col, index.col + height)).tm

  def reshape(height: Int, width: Int): Matrix[A] = 
    toVector.reshape(height, width)

  def filterRow(f: Vector[A] => Boolean) = input.filter(f).tm
  def filterCol(f: Vector[A] => Boolean) = input.transpose.filter(f).transpose.tm

  def find(pred: A => Boolean) = input.flatten.find(pred)
  def indexWhere(pred: A => Boolean) = zipWithIndex.find((a, b) => pred(a)).map(_._2)

  /** Returns the [transpose](https://en.wikipedia.org/wiki/Transpose) of this matrix. */
  def transpose = input.transpose.tm
  def reverseCols = input.map(_.reverse).tm
  def reverseRows = input.reverse.tm
  def rotateRight = transpose.reverseCols
  def rotateLeft = reverseCols.transpose

  def swapRows(a: Int, b: Int) = 
    new Matrix(input.updated(a, input(b)).updated(b, input(a)))
  
  def swapCols(a: Int, b: Int) =
    new Matrix(input.map(_.updated(a, input(a)(b)).updated(b, input(a)(a))))

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
    new Matrix(input.zip(other.input).map((row, otherRow) => otherRow ++ row))

  def appendedRight(other: Matrix[A]) =
    checkAppendHorizontal(other)
    new Matrix(input.zip(other.input).map((row, otherRow) => row ++ otherRow))

  def appendedTop(other: Matrix[A]) =
    checkAppendVertical(other)
    new Matrix(other.input ++ input)

  def appendedBottom(other: Matrix[A]) =
    checkAppendVertical(other)
    new Matrix(input ++ other.input)

  def dropRow(row: Int) = (input.take(row) ++ input.drop(row + 1)).tm
  def dropCol(col: Int) = input.map(row => row.take(col) ++ row.drop(col + 1)).tm

  def zip[B](other: Matrix[B]): Matrix[(A, B)] =
    // require(dimensions == other.dimensions, "Can't zip matrices of different dimensions")
    new Matrix(input.zip(other.input).map((row, otherRow) => row.zip(otherRow)))

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

  /** Computes the [determinant](https://en.wikipedia.org/wiki/Determinant) of this matrix.*/
  // about ~2% wrong answer rate lol
  // no idea why
  def determinant(using Numeric[A]): Double = 
    checkSquare("determinant")
    var p = 1
    var willBeZero = false
    var gauss = map(_.toDouble)

    // println(gauss)
    if gauss.rows.exists(_.forall(_ == 0)) then 0
    else if gauss.cols.exists(_.forall(_ == 0)) then 0
    else for row <- 0 until height - 1 if !willBeZero do
      val absCol = gauss.col(row).map(_.abs)
      if absCol.drop(row).max == 0 then willBeZero = true
      val pivotIndex = absCol.indexWhere(n => n == absCol.drop(row).max, row)
      // println(s"absCol: $absCol\npivotIndex: $pivotIndex")
      if row < pivotIndex then 
        // println(s"swapping rows $row and $pivotIndex")
        gauss = gauss.swapRows(row, pivotIndex)
        p *= -1

      for rowDown <- row + 1 until height if !willBeZero do
        val factor = gauss(rowDown, row) / gauss(row, row)        
        gauss = gauss.updated(rowDown, row)(0)
        for col <- row + 1 until height if factor != 0 do
          gauss = gauss.updated(rowDown, col)(gauss(rowDown, col) - factor * gauss(row, col))
          // println(s"row: $row\nrowDown: $rowDown\ncol: $col\nfactor: $factor$gauss\n")
      
    if willBeZero then 0 else gauss.diagonalProduct * p

    // for row <- 0 until height if !willBeZero do      
    //   val absCol = gauss.col(row).map(_.abs)
    //   if absCol.drop(row).max == 0 then willBeZero = true
    //   else
    //     val pivotIndex = absCol.indexOf(absCol.drop(row).max)
    //     if pivotIndex != row then
    //       gauss = gauss.swapRows(row, pivotIndex)
    //       p *= -1

    //   row.lg
    //   gauss.lg
    //   willBeZero.logIf(identity)
    //   for rowDown <- row + 1 until height do
    //     val factor = gauss(rowDown, row) / gauss(row, row)
    //     for col <- row until height do
    //       gauss = gauss.updated(rowDown, col)(gauss(rowDown, col) - factor * gauss(row, col))

    // if willBeZero then 0 else gauss.diagonalProduct * p

  def diagonal = 
    (for i <- 0 until (width min height) yield apply(i, i)).toVector

  def oppositeDiagonal = 
    (for i <- 0 until (width min height) yield apply(i, width - i - 1)).toVector

  /** Computes the [trace](https://en.wikipedia.org/wiki/Trace_(linear_algebra)) of this matrix.*/
  def trace(using Numeric[A]): A = 
    checkSquare("trace")
    diagonal.sum

  def diagonalProduct(using Numeric[A]): A = 
    checkSquare("diagonal product")
    diagonal.product

  def determinantOption(using Numeric[A]): Option[Double] = if isSquare then Some(determinant) else None
  def traceOption(using Numeric[A]): Option[A] = if isSquare then Some(trace) else None
  
object Matrix:
  extension [A] (input: Vector[Vector[A]]) private def tm = new Matrix(input)

  /** Typesafe helper enum for the rotation functions in the [[problemutils.classes.Matrix]] object. */
  private [problemutils] enum Axis:
    case X, Y, Z

  def apply[A](height: Int, width: Int)(f: Pos2D => A): Matrix[A] = 
    Vector.tabulate(height, width)((r, c) => f(r, c)).tm

  def from[A](input: Seq[Seq[A]]) = 
    require(input.size > 0, "Matrix must have at least one row")
    require(input.head.size > 0, "Matrix must have at least one column")
    require(input.forall(_.size == input.head.size), "All rows must have the same length")
    new Matrix(input.map(_.toVector).toVector)

  /** Creates an [identity matrix](https://en.wikipedia.org/wiki/Identity_matrix) of the given dimension. */
  def identity(size: Int) = 
    Matrix(size, size)((row, col) => (row == col).toInt)

  /** Produces a matrix of the given dimensions filled with the result of some computation. */
  def fill[A](height: Int, width: Int)(elem: => A) = 
    Vector.fill(height, width)(elem).tm

  /** Produces a matrix of the given dimensions, with the given starting value at the top-left (defaulting to 0), and counting up by 1 for each column and row (going right-down). */
  def range(height: Int, width: Int, start: Int = 0) = 
    Matrix(height, width)(_ * width + _ + start)

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
          Vector(1d, 0d, 0d), 
          Vector(0d, cos(rad), -sin(rad)), 
          Vector(0d, sin(rad), cos(rad))
        ).tm
      case Y =>
        Vector(
          Vector(cos(rad), 0d, sin(rad)), 
          Vector(0d, 1d, 0d), 
          Vector(-sin(rad), 0d, cos(rad))
        ).tm
      case Z =>
        Vector(
          Vector(cos(rad), -sin(rad), 0d), 
          Vector(sin(rad), cos(rad), 0d), 
          Vector(0d, 0d, 1d)
        ).tm
