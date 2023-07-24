package problemutils.extensions

import problemutils.*

extension [A](a: A)
  /** Applies any function `A => Unit` on any object `a` and then returns the object without modifying it. */  
  def tap(f: A => Unit): A = 
    f(a)
    a

  /** Applies any function `A => B` on any object `a` and then returns the result. */
  def pipe[B](f: A => B): B = f(a)

  /** Applies any function `A => A` on any object `n` times. If `n` is less than or equal to 0, the original object is returned without function application. */
  def iterate(f: A => A)(n: Int): A = 
    if n <= 0 then a
    else f(a).iterate(f)(n - 1)

  /** Recursively applies a function `f: A => A` on any object `a` until the predicate `p` is satisfied. */
  def doUntil(f: A => A)(p: A => Boolean): A = 
    if p(a) then a
    else f(a).doUntil(f)(p)

  /** Recursively applies a function `f: A => A` on any object `a` until `f(a)` is equal to `a`. */
  def converge(f: A => A): A = 
    val fa = f(a)
    if a == fa then a
    else fa.converge(f)

  /** Returns a set of all elements reachable from the starting point `a` using a "neighbour function" `f: A => IterableOnce[A]`. */
  def expand(f: A => IterableOnce[A]): Set[A] = 
    val set = Set(a)
    def helper(collected: Set[A], prev: Set[A]) = 
      (collected.diff(prev).flatMap(f).union(collected), collected)

    (set flatMap f, set).converge(helper).head