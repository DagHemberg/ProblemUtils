package problemutils.extensions

import problemutils.*
import scala.reflect.ClassTag
import Console.*

extension [A](a: A)
  def getType(implicit ct: ClassTag[A]): String = ct.toString
  
  /** Logs any object `a` in the console and then returns the object without modifying it. */
  def log = 
    println(s"[${YELLOW}*${RESET}] $a")
    a

  /** Logs any attribute of object `a` in the console and then returns the object without modifying it. */
  def logAttr[B](f: A => B) = 
    println(s"[${YELLOW}*${RESET}] ${f(a)}")
    a

  /** Logs any object `a` in the console without the "[*]" prefix and then returns the object without modifying it. */
  def lg = 
    println(a)
    a
  
  /** Prints an empty line in the console and returns the object without modifying it. Useful for debugging. */
  def space = 
    println()
    a

  /** Logs any object `a` in the console *if* the condition `p` is satisfied and then returns the object without modifying it. */
  def logIf(p: A => Boolean) = 
    if p(a) then println(s"[${RED}!${RESET}] $a")
    a

  /** Logs any attribute of object `a` in the console *if* the condition `p` is satisfied and then returns the object without modifying it. */
  def logAttrIf[B](f: A => B)(p: B => Boolean) = 
    if p(f(a)) then println(s"[${RED}!${RESET}] ${f(a)}")
    a

  /** Similar to `log`, but takes a color parameter. */
  def logCol(color: String = "yellow") =
    val col = color.toLowerCase match
      case "cyan" => CYAN
      case "red" => RED
      case "green" => GREEN
      case "yellow" => YELLOW
      case "blue" => BLUE
      case "magenta" => MAGENTA
      case _ => CYAN
    println(s"[${col}+${RESET}] $a")
    a