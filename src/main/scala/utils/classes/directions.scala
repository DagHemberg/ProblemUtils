package utils.classes

enum Cardinal(y: Int, x: Int):
  import Cardinal.*
  case North     extends Cardinal(-1, 0)
  case NorthEast extends Cardinal(-1, 1)
  case East      extends Cardinal(0, 1)
  case SouthEast extends Cardinal(1, 1)
  case South     extends Cardinal(1, 0)
  case SouthWest extends Cardinal(1, -1)
  case West      extends Cardinal(0, -1)
  case NorthWest extends Cardinal(-1, -1)
  def hardClockwise = fromOrdinal((this.ordinal + 2) % 8)
  def hardCounterClockwise = fromOrdinal((this.ordinal + 6) % 8)
  def reverse = fromOrdinal((this.ordinal + 4) % 8)
  def clockwise = fromOrdinal((this.ordinal + 1) % 8)
  def counterClockwise = fromOrdinal((this.ordinal + 7) % 8)
  def toPos2D = (y, x)
  def toVec2 = (y, x)

enum Hex(q: Int, r: Int, s: Int):
  import Hex.*
  case North     extends Hex(0, -1, 1)
  case NorthEast extends Hex(1, -1, 0)
  case SouthEast extends Hex(1, 0, -1)
  case South     extends Hex(0, 1, -1)
  case SouthWest extends Hex(-1, 1, 0)
  case NorthWest extends Hex(-1, 0, 1)
  def reverse = fromOrdinal((this.ordinal + 3) % 6)
  def clockwise = fromOrdinal((this.ordinal + 1) % 6)
  def counterClockwise = fromOrdinal((this.ordinal + 5) % 6)
  val (x, y, z) = (q, r, s)
  def toPos3D = (q, r, s)
  def toVec3 = (x, y, z)