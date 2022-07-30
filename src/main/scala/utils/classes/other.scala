package utils.classes
import utils.*
import utils.extensions.*

sealed trait Example[A](val solution: A)
case object Skip extends Example[Any](None)
case class Primary[A](override val solution: A) extends Example[A](solution)
case class Secondary[A](override val solution: A) extends Example[A](solution)

/** A simple wrapper class that includes the result of an evaluation and the time (in seconds) it took to evaluate it
 * @param result The final evaluation
 * @param time Time elapsed while evaluating, in seconds
 */
case class TimedEval[A](duration: Double, result: A)
object TimedEval:
  /** Times the evaluation of a block of code */
  def time[A](block: => A): TimedEval[A] =
    val start = System.nanoTime()
    val result = block
    val duration = (System.nanoTime() - start) / 1E9
    TimedEval(duration, result)

  def logTime[A](block: => A) = time(block).logAttr(_.duration).result

object Testing:
  def read(folder: String, year: String, day: String) = 
    os.read.lines(os.pwd / "src" / "main" / "resources" / "input" / folder / year / s"$day.txt").toList