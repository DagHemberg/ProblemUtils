package utils.extensions

import utils.*
import math.Numeric.Implicits.infixNumericOps
import math.Integral.Implicits.infixIntegralOps
import math.Fractional.Implicits.infixFractionalOps

extension [A: Numeric](xs: IndexedSeq[A])
  def average = xs.sum.toDouble / xs.size
  def median(using Ordering[A]) = 
    val sorted = xs.sorted
    val half = xs.size / 2
    if xs.size % 2 == 0 then (sorted(half) + sorted(half - 1)).toDouble / 2
    else sorted(half).toDouble
  def rms = math.sqrt(xs.map(x => x * x).sum.toDouble / xs.size)

extension [A: Numeric](xs: Vector[A])
  def toVec3 = 
    require(xs.size == 3)
    (xs(0).toInt, xs(1).toInt, xs(2).toInt)

  /** Computes the [dot product](https://en.wikipedia.org/wiki/Dot_product) of 2 vectors of the same length. */
  infix def dot (ys: Vector[A]) = 
    require(xs.size == ys.size, "Vectors must be the same size")
    (xs zip ys map (_ * _)).sum

  /** Computes the [cross product](https://en.wikipedia.org/wiki/Cross_product) of 2 vectors of length 3. */
  infix def cross (ys: Vector[A]) = 
    require(xs.size == 3 && ys.size == 3, "Cross product only defined for 3D vectors")
    Vector(
      xs(1) * ys(2) - xs(2) * ys(1), 
      xs(2) * ys(0) - xs(0) * ys(2), 
      xs(0) * ys(1) - xs(1) * ys(0)
    )

  /** Returns the [magnitude](https://en.wikipedia.org/wiki/Magnitude_(mathematics)#Vector_spaces) of the vector. */
  def magnitude = math.sqrt((xs dot xs).toDouble)

  /** Returns a [normalized](https://en.wikipedia.org/wiki/Unit_vector) version of the vector. */
  def normalized = xs.map(_.toDouble * (1.0 / xs.magnitude))

extension [A](vss: Vector[Vector[A]])
  def toMatrix = Matrix.from(vss)

extension (str: String)
  def words = str.split("\\s+").toList
  def lines = str.split("\n").toList
  def padLeftTo(n: Int, char: Char) = str.reverse.padTo(n, char).reverse
  def findAllWith(regex: String) = regex.r.findAllIn(str).toList
  def findAllMatchWith(regex: String) = regex.r.findAllMatchIn(str).toList
  def findWith(regex: String) = regex.r.findFirstIn(str)
  def findMatchWith(regex: String) = regex.r.findFirstMatchIn(str)