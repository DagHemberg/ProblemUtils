package problemutils.extensions

import problemutils.*
import math.Numeric.Implicits.infixNumericOps
import math.Integral.Implicits.infixIntegralOps
import math.Fractional.Implicits.infixFractionalOps

extension [A](xs: IterableOnce[A])
  def average(using Numeric[A]) = xs.iterator.sum.toDouble / xs.iterator.size

  def median(using Numeric[A], Ordering[A]) = 
    val sorted = xs.iterator.toSeq.sorted
    val half = xs.iterator.size / 2
    if xs.iterator.size % 2 == 0 then (sorted(half) + sorted(half - 1)).toDouble / 2
    else sorted(half).toDouble

  /** Computes the [root mean square](https://en.wikipedia.org/wiki/Root_mean_square) of this sequence. */
  def rms(using Numeric[A]) = math.sqrt(xs.iterator.map(x => x * x).sum.toDouble / xs.iterator.size)

  def sumBy[B](f: A => B)(using num: Numeric[B]) = 
    xs.iterator.foldLeft(num.zero)((sum, x) => sum + f(x))

  def productBy[B](f: A => B)(using num: Numeric[B]) = 
    xs.iterator.foldLeft(num.one)((prod, x) => prod * f(x))

extension [A](xs: Seq[A])
  /** Reshapes this sequence into a [[problemutils.classes.Matrix]] of the given width and height. */
  def reshape(height: Int, width: Int): Matrix[A] = 
    require(
      height * width == xs.size, 
      s"Cannot reshape ${xs.size} elements into Matrix of dimensions ($height, $width)"
    )
    xs.iterator.toSeq.grouped(width).toVector.toMatrix

  def split(elem: A): List[Seq[A]] = 
    def rec(remaining: Seq[A], acc: List[Seq[A]]): List[Seq[A]] = 
      remaining.indexOf(elem) match
        case -1 => remaining :: acc
        case i => rec(remaining.drop(i + 1), remaining.take(i) :: acc)
    rec(xs, Nil).reverse

extension [A](vss: Seq[Seq[A]])
  def toMatrix = Matrix.from(vss)


extension [A: Numeric](xs: Vector[A])
  def toVec3 = 
    require(xs.size == 3)
    (xs(0).toInt, xs(1).toInt, xs(2).toInt)

  /** Computes the [dot product](https://en.wikipedia.org/wiki/Dot_product) of 2 vectors of the same length. */
  infix def dot (ys: Vector[A]) = 
    require(xs.size == ys.size, "Vectors must be the same size")
    xs.zip(ys).map(_ * _).sum

  /** Computes the [cross product](https://en.wikipedia.org/wiki/Cross_product) of 2 vectors of length 3. */
  infix def cross (ys: Vector[A]) = 
    require(xs.size == 3 && ys.size == 3, "Cross product only defined for 3D vectors")
    Vector(
      xs(1) * ys(2) - xs(2) * ys(1), 
      xs(2) * ys(0) - xs(0) * ys(2), 
      xs(0) * ys(1) - xs(1) * ys(0)
    )

  /** Returns the [magnitude](https://en.wikipedia.org/wiki/Magnitude_(mathematics)#Vector_spaces) of this vector. */
  def magnitude = math.sqrt((xs dot xs).toDouble)

  /** Returns a [normalized](https://en.wikipedia.org/wiki/Unit_vector) version of this vector. */
  def normalized = xs.map(_.toDouble * (1 / xs.magnitude))

extension (str: String)
  def words = str.split("\\s+").toList
  def rows = str.split("\n").toList
  def padLeftTo(n: Int, char: Char) = str.reverse.padTo(n, char).reverse
  def findWith(regex: String) = regex.r.findFirstIn(str)
  def findAllWith(regex: String) = regex.r.findAllIn(str).toList
  def findMatchWith(regex: String) = regex.r.findFirstMatchIn(str)
  def findAllMatchWith(regex: String) = regex.r.findAllMatchIn(str).toList

extension [A, B] (map: Map[A, B])
  def invert = map
  .toSeq
  .map(_.swap)
  .foldLeft(Map.empty[B, Set[A]]): 
    case (acc, (key, value)) => 
      if acc isDefinedAt key then acc + (key -> (acc(key) + value))
      else acc + (key -> Set(value))

def fst[A, B](tup: (A, B)) = tup._1
def snd[A, B](tup: (A, B)) = tup._2