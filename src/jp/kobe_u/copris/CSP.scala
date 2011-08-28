package jp.kobe_u.copris

import scala.collection._

/**
 * Abstract class of expressions.
 * [[jp.kobe_u.copris.Term]]s and [[jp.kobe_u.copris.Constraint]]s are expressions.
 */
abstract class Expr

/**
 * Abstract class of terms.
 *
 * Operators defined in this class create a new expression.
 * For example, `x + y` returns a new term `Add(x, y)`
 * when `x` and `y` are terms.
 */
sealed abstract class Term extends Expr {
  /** Returns [[jp.kobe_u.copris.Neg]] of Term */
  def unary_- = Neg(this)
  /** Returns [[jp.kobe_u.copris.Add]] of Terms */
  def + (x: Term) = Add(this, x)
  /** Returns [[jp.kobe_u.copris.Add]] of Term with Int */
  def + (a: Int) = Add(this, Num(a))
  /** Returns [[jp.kobe_u.copris.Sub]] of Terms */
  def - (x: Term) = Sub(this, x)
  /** Returns [[jp.kobe_u.copris.Sub]] of Term with Int */
  def - (a: Int) = Sub(this, Num(a))
  /** Returns [[jp.kobe_u.copris.Mul]] of Terms */
  def * (x: Term) = Mul(this, x)
  /** Returns [[jp.kobe_u.copris.Mul]] of Term with Int */
  def * (a: Int) = Mul(this, Num(a))
  /** Returns [[jp.kobe_u.copris.Div]] of Terms */
  def / (x: Term) = Div(this, x)
  /** Returns [[jp.kobe_u.copris.Div]] of Term by Int */
  def / (a: Int) = Div(this, Num(a))
  /** Returns [[jp.kobe_u.copris.Mod]] of Terms */
  def % (x: Term) = Mod(this, x)
  /** Returns [[jp.kobe_u.copris.Mod]] of Term by Int */
  def % (a: Int) = Mod(this, Num(a))
  /** Returns [[jp.kobe_u.copris.Max]] of Terms */
  def max (x: Term) = Max(this, x)
  /** Returns [[jp.kobe_u.copris.Max]] of Term and Int */
  def max (a: Int) = Max(this, Num(a))
  /** Returns [[jp.kobe_u.copris.Min]] of Terms */
  def min (x: Term) = Min(this, x)
  /** Returns [[jp.kobe_u.copris.Min]] of Term and Int */
  def min (a: Int) = Min(this, Num(a))

  /** Returns [[jp.kobe_u.copris.Eq]] of Terms */
  def === (x: Term) = Eq(this, x)
  /** Returns [[jp.kobe_u.copris.Eq]] of Term and Int */
  def === (a: Int) = Eq(this, Num(a))
  /** Returns [[jp.kobe_u.copris.Ne]] of Terms */
  def !== (x: Term) = Ne(this, x)
  /** Returns [[jp.kobe_u.copris.Ne]] of Term and Int */
  def !== (a: Int) = Ne(this, Num(a))
  /** Returns [[jp.kobe_u.copris.Le]] of Terms */
  def <= (x: Term) = Le(this, x)
  /** Returns [[jp.kobe_u.copris.Le]] of Term and Int */
  def <= (a: Int) = Le(this, Num(a))
  /** Returns [[jp.kobe_u.copris.Lt]] of Terms */
  def < (x: Term) = Lt(this, x)
  /** Returns [[jp.kobe_u.copris.Lt]] of Term and Int */
  def < (a: Int) = Lt(this, Num(a))
  /** Returns [[jp.kobe_u.copris.Ge]] of Terms */
  def >= (x: Term) = Ge(this, x)
  /** Returns [[jp.kobe_u.copris.Ge]] of Term and Int */
  def >= (a: Int) = Ge(this, Num(a))
  /** Returns [[jp.kobe_u.copris.Gt]] of Terms */
  def > (x: Term) = Gt(this, x)
  /** Returns [[jp.kobe_u.copris.Gt]] of Term and Int */
  def > (a: Int) = Gt(this, Num(a))
}
/**
 * Object of `NIL` term.
 */
object NIL extends Term {
  override def toString = "nil"
}
/**
 * Case class of number terms.
 * @param value the value of the number term
 */
case class Num(value: Int) extends Term {
  override def toString = value.toString
}
/**
 * Object of number term 0.
 */
object ZERO extends Num(0)
/**
 * Object of number term 1.
 */
object ONE extends Num(1)
/**
 * Case class of integer variables.
 * @param name the primary name of the variable
 * @param is the indices of the variable (optional)
 */
case class Var(name: String, is: String*) extends Term with Ordering[Var] {
  /** Returns a new variable with extra indices given by `is1` */
  def apply(is1: Any*) =
    Var(name, is ++ is1.map(_.toString): _*)
  /** Compares variables */
  def compare(x1: Var, x2: Var) = {
    if (x1.name != x2.name)
      x1.name.compare(x2.name)
    else if (x1.is.size != x2.is.size)
      x1.is.size.compare(x2.is.size)
    else if (x1.is == x2.is)
      0
    else
      (0 until x1.is.size).map(i => x1.is(i).compare(x2.is(i))).
        find(_ != 0).getOrElse(0)
  }
  override def toString =
    if (is.size == 0) name else is.mkString(name + "(", ",", ")")
}
/**
 * Case class for absolute value of term.
 */
case class Abs(x0: Term) extends Term
/**
 * Case class for negation of term.
 */
case class Neg(x0: Term) extends Term
/**
 * Case class for addition of terms.
 * Companion object provies other factory methods.
 */
case class Add(xs: Term*) extends Term {
  override def toString = xs.mkString(productPrefix + "(", ",", ")")
}
/**
 * Factory for addition of terms.
 */
object Add {
  def apply(xs: Iterable[Term]) = new Add(xs.toSeq: _*)
}
/**
 * Case class for subtraction of terms.
 * Companion object provies other factory methods.
 */
case class Sub(xs: Term*) extends Term {
  override def toString = xs.mkString(productPrefix + "(", ",", ")")
}
/**
 * Factory for subtraction of terms.
 */
object Sub {
  def apply(xs: Iterable[Term]) = new Sub(xs.toSeq: _*)
}
/**
 * Case class for multiplication of terms.
 * Companion object provies other factory methods.
 */
case class Mul(xs: Term*) extends Term {
  override def toString = xs.mkString(productPrefix + "(", ",", ")")
/**
 * Factory for multiplication of terms.
 */
}
object Mul {
  def apply(xs: Iterable[Term]) = new Mul(xs.toSeq: _*)
}
/**
 * Case class for division of terms.
 */
case class Div(x0: Term, x1: Term) extends Term
/**
 * Case class for remainder of terms.
 */
case class Mod(x0: Term, x1: Term) extends Term
/**
 * Case class for maximum of terms.
 * Companion object provies other factory methods.
 */
case class Max(xs: Term*) extends Term {
  override def toString = xs.mkString(productPrefix + "(", ",", ")")
}
/**
 * Factory for maximum of terms.
 */
object Max {
  def apply(xs: Iterable[Term]) = new Max(xs.toSeq: _*)
}
/**
 * Case class for minimum of terms.
 * Companion object provies other factory methods.
 */
case class Min(xs: Term*) extends Term {
  override def toString = xs.mkString(productPrefix + "(", ",", ")")
}
/**
 * Factory for minimum of terms.
 */
object Min {
  def apply(xs: Iterable[Term]) = new Min(xs.toSeq: _*)
}
/**
 * Case class for if expressions.
 */
case class If(c: Constraint, x0: Term, x1: Term) extends Term

/**
 * Abstract class of constraints.
 *
 * Operators defined in this class create a new expression.
 * For example, `c && d` returns a new term `And(c, d)`
 * when `c` and `d` are constraints.
 */
sealed abstract class Constraint extends Expr {
  /** Returns [[jp.kobe_u.copris.Not]] of Constraint */
  def unary_! = Not(this)
  /** Returns [[jp.kobe_u.copris.And]] of Constraints */
  def && (c: Constraint) = And(this, c)
  /** Returns [[jp.kobe_u.copris.Or]] of Constraints */
  def || (c: Constraint) = Or(this, c)
  /** Returns [[jp.kobe_u.copris.Imp]] of Constraints */
  def ==> (c: Constraint) = Imp(this, c)
  /** Returns [[jp.kobe_u.copris.Xor]] of Constraints */
  def ^ (c: Constraint) = Xor(this, c)
  /** Returns [[jp.kobe_u.copris.Iff]] of Constraints */
  def <==> (c: Constraint) = Iff(this, c)
}
/**
 * Abstract class of global constraints.
 */
sealed abstract class GlobalConstraint extends Constraint

/**
 * Object of `FALSE` constraint.
 */
object FALSE extends Constraint {
  override def toString = "false"
}
/**
 * Object of `TRUE` constraint.
 */
object TRUE extends Constraint {
  override def toString = "true"
}
/**
 * Case class of Boolean variables.
 * @param name the primary name of the variable
 * @param is the indices of the variable (optional)
 */
case class Bool(name: String, is: String*) extends Constraint with Ordering[Bool] {
  /** Returns a new variable with extra indices given by `is1` */
  def apply(is1: Any*) =
    Bool(name, is ++ is1.map(_.toString): _*)
  /** Compares variables */
  def compare(x1: Bool, x2: Bool) = {
    if (x1.name != x2.name)
      x1.name.compare(x2.name)
    else if (x1.is.size != x2.is.size)
      x1.is.size.compare(x2.is.size)
    else if (x1.is == x2.is)
      0
    else
      (0 until x1.is.size).map(i => x1.is(i).compare(x2.is(i))).
        find(_ != 0).getOrElse(0)
  }
  override def toString =
    if (is.size == 0) name else is.mkString(name + "(", ",", ")")
}
/**
 * Case class for logical negation of constaint.
 */
case class Not(c0: Constraint) extends Constraint
/**
 * Case class for conjuction of constaints.
 * Companion object provies other factory methods.
 */
case class And(cs: Constraint*) extends Constraint {
  override def toString = cs.mkString(productPrefix + "(", ",", ")")
}
/**
 * Factory for conjunction of terms.
 */
object And {
  def apply(xs: Iterable[Constraint]) = new And(xs.toSeq: _*)
}
/**
 * Case class for disjuction of constaints.
 * Companion object provies other factory methods.
 */
case class Or(cs: Constraint*) extends Constraint {
  override def toString = cs.mkString(productPrefix + "(", ",", ")")
}
/**
 * Factory for disjunction of terms.
 */
object Or {
  def apply(xs: Iterable[Constraint]) = new Or(xs.toSeq: _*)
}
/**
 * Case class for implication of constaint.
 */
case class Imp(c0: Constraint, c1: Constraint) extends Constraint
/**
 * Case class for exclusive-or of constaint.
 */
case class Xor(c0: Constraint, c1: Constraint) extends Constraint
/**
 * Case class for if-and-only-if of constaint.
 */
case class Iff(c0: Constraint, c1: Constraint) extends Constraint
/**
 * Case class for equals constraints.
 */
case class Eq(x0: Term, x1: Term) extends Constraint
/**
 * Case class for not-equals constraints.
 */
case class Ne(x0: Term, x1: Term) extends Constraint
/**
 * Case class for less-than-or-equals constraints.
 */
case class Le(x0: Term, x1: Term) extends Constraint
/**
 * Case class for less-than constraints.
 */
case class Lt(x0: Term, x1: Term) extends Constraint
/**
 * Case class for greater-than-or-equals constraints.
 */
case class Ge(x0: Term, x1: Term) extends Constraint
/**
 * Case class for greater-than constraints.
 */
case class Gt(x0: Term, x1: Term) extends Constraint

/**
 * Case class for Alldifferent global constraints.
 * Companion object provies other factory methods.
 */
case class Alldifferent(xs: Term*) extends GlobalConstraint {
  override def toString = xs.mkString(productPrefix + "(", ",", ")")
}
/**
 * Factory for Alldifferent global constraints.
 */
object Alldifferent {
  def apply(xs: Iterable[Term]) = new Alldifferent(xs.toSeq: _*)
}
/**
 * Case class for Weightedsum global constraints.
 */
case class Weightedsum(axs: Seq[(Int,Term)], cmp: String, b: Term) extends GlobalConstraint {
  assert(cmp.matches("eq|ne|lt|le|gt|ge"))
  override def toString =
    productPrefix + "(" + axs + "," + cmp + "," + b + ")"
}
/**
 * Case class for Cumulative global constraints.
 */
case class Cumulative(tasks: Seq[(Term,Term,Term,Term)], limit: Term) extends GlobalConstraint {
  override def toString =
    productPrefix + "(" + tasks + "," + limit + ")"
}
/**
 * Case class for Element global constraints.
 */
case class Element(i: Term, xs: Seq[Term], xi: Term) extends GlobalConstraint {
  override def toString =
    productPrefix + "(" + i + "," + xs + "," + xi + ")"
}
/**
 * Case class for Disjunctive global constraints.
 * Companion object provies other factory methods.
 */
case class Disjunctive(tasks: (Term,Term)*) extends GlobalConstraint {
  override def toString =
    productPrefix + "(" + tasks + ")"
}
/**
 * Factory for Disjunctive global constraints.
 */
object Disjunctive {
  def apply(tasks: Iterable[(Term,Term)]) = new Disjunctive(tasks.toSeq: _*)
}
/**
 * Case class for LexLess global constraints.
 */
case class LexLess(xs: Seq[Term], ys: Seq[Term]) extends GlobalConstraint {
  override def toString =
    productPrefix + "(" + xs + "," + ys + ")"
}
/**
 * Case class for LexLesseq global constraints.
 */
case class LexLesseq(xs: Seq[Term], ys: Seq[Term]) extends GlobalConstraint {
  override def toString =
    productPrefix + "(" + xs + "," + ys + ")"
}
/**
 * Case class for Nvalue global constraints.
 */
case class Nvalue(count: Term, xs: Seq[Term]) extends GlobalConstraint {
  override def toString =
    productPrefix + "(" + count + "," + xs + ")"
}
/**
 * Case class for GlobalCardinality global constraints.
 */
case class GlobalCardinality(xs: Seq[Term], card: Seq[(Int,Term)]) extends GlobalConstraint {
  override def toString =
    productPrefix + "(" + xs + "," + card + ")"
}
/**
 * Case class for GlobalCardinalityWithCosts global constraints.
 */
case class GlobalCardinalityWithCosts(xs: Seq[Term], card: Seq[(Int,Term)],
                                      table: Seq[(Int,Int,Int)], cost: Term) extends GlobalConstraint {
  override def toString =
    productPrefix + "(" + xs + "," + card  + "," + table  + "," + cost + ")"
}
/**
 * Case class for Count global constraints.
 */
case class Count(value: Term, xs: Seq[Term], cmp: String, count: Term) extends GlobalConstraint {
  assert(cmp.matches("eq|ne|lt|le|gt|ge"))
  override def toString =
    productPrefix + "(" + value + "," + xs + "," + cmp + "," + count + ")"
}

/**
 * Abstract class of domains.
 * Companion object provies other factory methods.
 */
abstract class Domain {
  /** Returns lower bound value */
  def lb: Int
  /** Returns upper bound value */
  def ub: Int
}
/**
 * Case class of interval domain
 */
case class IntervalDomain(lo: Int, hi: Int) extends Domain {
  assert(lo <= hi)
  def lb = lo
  def ub = hi
  override def productPrefix = "Domain"
}
/**
 * Case class of set domain
 */
case class SetDomain(values: SortedSet[Int]) extends Domain {
  def lb = values.min
  def ub = values.max
  override def productPrefix = "Domain"
}
/**
 * Factory for creating domains.
 */
object Domain {
  /** Returns [[jp.kobe_u.copris.IntervalDomain]] */
  def apply(lo: Int, hi: Int) =
    IntervalDomain(lo, hi)
  /** Returns [[jp.kobe_u.copris.IntervalDomain]] with singleton value */
  def apply(value: Int) =
    IntervalDomain(value, value)
  /** Returns [[jp.kobe_u.copris.SetDomain]] */
  def apply(values: Set[Int]) =
    SetDomain(SortedSet(values.toSeq: _*))
}

/**
 * Trait of CSP (Constraint Satisfaction Problem)
 */
trait CSPTrait {
  /** Adds an integer variable */
  def int(x: Var, d: Domain): Var
  /** Adds an integer variable */
  def int(x: Var, d: Set[Int]): Var =
    int(x, Domain(d))
  /** Adds an integer variable */
  def int(x: Var, lo: Int, hi: Int): Var =
    int(x, Domain(lo, hi))
  /** Adds an integer variable */
  def int(x: Var, value: Int): Var =
    int(x, Domain(value))
  /** Adds integer variables */
  def int(xs: Iterable[Term], d: Domain): Iterable[Term] = {
    xs.foreach(_ match {
      case x: Var => int(x, d)
      case _ =>
        throw new IllegalArgumentException("argument of int declaration should be a Var")
    })
    xs
  }
  /** Adds integer variables */
  def int(xs: Iterable[Term], d: Set[Int]): Iterable[Term] =
    int(xs, Domain(d))
  /** Adds integer variables */
  def int(xs: Iterable[Term], lo: Int, hi: Int): Iterable[Term] =
    int(xs, Domain(lo, hi))
  /** Adds integer variables */
  def int(xs: Iterable[Term], value: Int): Iterable[Term] =
    int(xs, Domain(value))

  /** Adds a Boolean variable */
  def bool(p: Bool): Bool
  /** Adds Boolean variables */
  def bool(ps: Iterable[Bool]): Iterable[Bool] =
    { ps.foreach(bool(_)); ps }

  /** Adds a constraint */
  def add(cs: Constraint*): Unit
  /** Adds constraints */
  def add(cs: Iterable[Constraint]): Unit =
    add(cs.toSeq: _*)

  /** Specifies objective variable to be minimized */
  def minimize(x: Var): Var
  /** Specifies objective variable to be maximized */
  def maximize(x: Var): Var
}

/**
 * Case class of CSP (Constraint Satisfaction Problem)
 * @param variables integer variables
 * @param bools Boolean variables
 * @param dom domains of integer variables
 * @param constraints constraints
 */
case class CSP(var variables: Seq[Var] = Seq.empty,
               var bools: Seq[Bool] = Seq.empty,
               var dom: Map[Var,Domain] = Map.empty,
               var constraints: Seq[Constraint] = Seq.empty)
  extends CSPTrait {
  private var variablesSize = 0
  private var boolsSize = 0
  private var constraintsSize = 0
  /** Objective variable.  `null` if not defined */
  var objective: Var = null
  private var target = 0

  /**
   * Creates a copy of the given CSP
   * @param csp0 original CSP
   */
  def this(csp0: CSP) = {
    this(csp0.variables, csp0.bools, csp0.dom, csp0.constraints)
    objective = csp0.objective
    target = csp0.target
    commit
  }
  /**
   * Resets the CSP by setting variables, bools, dom, and
   * constraints to be empty.
   */
  def init: Unit = {
    variables = Seq.empty; bools = Seq.empty
    dom = Map.empty; constraints = Seq.empty
  }
  /**
   * Adds an integer variable
   */
  def int(x: Var, d: Domain): Var = {
    if (variables.contains(x))
      throw new IllegalArgumentException("duplicate int " + x)
    variables :+= x; dom += x -> d; x
  }
  /**
   * Adds a Boolean variable
   */
  def bool(p: Bool): Bool = {
    if (bools.contains(p))
      throw new IllegalArgumentException("duplicate bool " + p)
    bools :+= p; p
  }
  /**
   * Adds constraints
   */
  def add(cs: Constraint*): Unit =
    constraints = constraints ++ cs
  /**
   * Commits the changes made for the CSP.
   */
  def commit: Unit = {
    variablesSize = variables.size
    boolsSize = bools.size
    constraintsSize = constraints.size
  }
  /**
   * Cancels the changes made for the CSP.
   */
  def cancel: Unit = {
    variables = variables.take(variablesSize)
    bools = bools.take(boolsSize)
    constraints = constraints.take(constraintsSize)
  }
  /**
   * Returns the integer variables added after the last commit.
   */
  def variablesDelta =
    variables.drop(variablesSize)
  /**
   * Returns the Boolean variables added after the last commit.
   */
  def boolsDelta =
    bools.drop(boolsSize)
  /**
   * Returns the constraints added after the last commit.
   */
  def constraintsDelta =
    constraints.drop(constraintsSize)
  /**
   * Specifies the objective variable to be minimized
   */
  def minimize(x: Var): Var = {
    objective = x
    target = -1
    x
  }
  /**
   * Specifies the objective variable to be maximized
   */
  def maximize(x: Var): Var = {
    objective = x
    target = 1
    x
  }
  /**
   * Returns true when the minimization is specified
   */
  def isMinimize = target < 0
  /**
   * Returns true when the maximization is specified
   */
  def isMaximize = target > 0
  /**
   * Returns the readable String representation of the CSP
   */
  def output: String = {
    val sb = new StringBuilder()
    for (x <- variables) dom(x) match {
      case d: IntervalDomain =>
        sb.append("int(" + x + "," + d.lo + "," + d.hi + ")\n")
      case d: SetDomain =>
        sb.append("int(" + x + "," + d + ")\n")
    }
    for (p <- bools)
      sb.append("bool(" + p + ")\n")
    for (c <- constraints)
      sb.append(c.toString + "\n")
    if (isMinimize)
      sb.append("minimize(" + objective + ")\n")
    else if (isMaximize)
      sb.append("maximize(" + objective + ")\n")
    sb.toString
  }
}

/**
 * Case class for solutions
 */
case class Solution(intValues: Map[Var,Int], boolValues: Map[Bool,Boolean])

/**
 * Object Timer (experimental)
 */
object Timer {
  var count = 0
}
/**
 * Class Timer (experimental)
 */
class Timer(val timeout: Long) {
  val deadline: Long =
    if (timeout > 0) currentTime + timeout else 0
  def currentTime: Long =
    scala.compat.Platform.currentTime
  def restTime: Long =
    if (deadline > 0) deadline - currentTime else Long.MaxValue
  var timerThread: Thread = null
  var mainThread: Thread = null
  var timeoutTask: () => Unit = null
  var count = {Timer.count = Timer.count + 1; Timer.count}
  println("Timer " + count + " new")
  def setTimeoutTask(task: => Unit) = {
    timeoutTask = () => task
  }
  def start = {
    mainThread = Thread.currentThread
    timerThread = null
    if (deadline > 0) {
      timerThread = new Thread() {
        override def run = {
          println("Timer " + count + " start " + timeout)
          while (timerThread != null && currentTime < deadline) {
            Thread.sleep(10)
          }
          if (timerThread != null) {
            timerThread = null
            // Interrupt
            println("Timer " + count + " interrupt")
	    if (timeoutTask != null) {
	      timeoutTask()
	      timeoutTask = null
	    }
            mainThread.interrupt
          } else {
            // Done already
          }
          println("Timer " + count + " end")
        }
      }
      timerThread.start
    }
  }
  def stop = {
    println("Timer " + count + " stop")
    timeoutTask = null
    timerThread = null
  }
  def raiseTimeout = {
    println("Timer " + count + " timeout")
    if (timeoutTask != null) {
      timeoutTask()
      stop
    }
    throw new InterruptedException("Timeout (" + timeout + ") exceeded")
  }
  def checkTimeout = {
    if (restTime <= 0)
      raiseTimeout
  }
}

/**
 * Trait for CSP solvers
 */
trait SolverTrait {
  /** Initializes the solver */
  def init: Unit
  /** Finds the first solution */
  def find: Boolean
  /** Finds the next solution */
  def findNext: Boolean
  /** Finds the optimum solution */
  def findOpt: Boolean
  /** Returns the current solution */
  def solution: Solution

  /** Returns the integer variable value of the current solution */
  def value(x: Var): Int = solution.intValues(x)
  /** Returns the Boolean variable value of the current solution */
  def value(p: Bool): Boolean = solution.boolValues(p)
  /** Returns the integer variable values of the current solution */
  def values(x: Var, xs: Var*): Seq[Int] =
    value(x) +: xs.map(value(_))
  /** Returns the Boolean variable values of the current solution */
  def values(p: Bool, ps: Bool*): Seq[Boolean] =
    value(p) +: ps.map(value(_))
}

/**
 * Abstract class for CSP solvers
 */
abstract class AbstractSolver(csp: CSP) extends SolverTrait {
  /** Options of the solver */
  var options: Map[String,String] = Map.empty
  /** Timeout value in miliseconds (experimental) */
  var timeout: Long = 0
  /** Timer (experimental) */
  var timer: Timer = null
  /** Starts the timer (experimental) */
  def startTimer(t: Long) = {
    if (t > 0) {
      timeout = t
      timer = new Timer(timeout)
      timer.start
    }
  }
  /** Stops the timer (experimental) */
  def stopTimer = {
    if (timer != null)
      timer.stop
    timer = null
  }
  /** Checks the timeout (experimental) */
  def checkTimeout = {
    if (timer != null)
      timer.checkTimeout
  }
  /** Specifies the clean-up tasks of the timeout (experimental) */
  def setTimeoutTask(task: => Unit) = {
    if (timer != null)
      timer.setTimeoutTask(task)
  }
  /** Raises the interrupted exception for timeout (experimental) */
  def raiseTimeout = {
    if (timer != null)
      timer.raiseTimeout
    throw new InterruptedException("Timeout (" + timeout + ") exceeded")
  }

  /** Status of the solver (experimental) */
  var solverStats: Seq[Map[String,Map[String,Number]]] =
    Seq(Map.empty)
  /** Shifts the status (experimental) */
  def shiftSolverStats =
    solverStats = solverStats :+ Map.empty
  /** Gets the last status of ~name~ (experimental) */
  def getSolverStat(name: String): Map[String,Number] =
    if (solverStats.last.contains(name))
      solverStats.last(name)
    else
      Map.empty
  /** Sets the current status of `name` (experimental) */
  def addSolverStat(name: String, stat: Map[String,Number]): Unit = {
    val stat1 = getSolverStat(name) ++ stat
    solverStats = solverStats.init :+ (solverStats.last + (name -> stat1))
  }
  /** Adds the current status of `name` (experimental) */
  def addSolverStat(name: String, key: String, value: Number): Unit =
    addSolverStat(name, Map(key -> value))
  /** Measures the time spent for executing `block` (experimental) */
  def measureTime[T](name: String, key: String)(block: => T) = {
    try {
      val time0 = scala.compat.Platform.currentTime
      val value = block
      Thread.sleep(1)
      val time1 = scala.compat.Platform.currentTime
      addSolverStat(name, key, time1 - time0)
      value
    } catch {
      case e: InterruptedException => {
        println(e)
        // e.printStackTrace
        raiseTimeout
      }
      case e: java.nio.channels.ClosedByInterruptException => {
        // e.printStackTrace
        println(e)
        raiseTimeout
      }
    }
  }

  /** Body of the `find` method */
  def findBody: Boolean
  /** Body of the `findNext` method */
  def findNextBody: Boolean
  /** Body of the `findOpt` method */
  def findOptBody: Boolean
  /** Body of the `findOptBound` method */
  def findOptBoundBody(lb: Int, ub: Int): Boolean
  /* */
  def find = {
    addSolverStat("csp", "variables", csp.variables.size)
    addSolverStat("csp", "bools", csp.bools.size)
    addSolverStat("csp", "constraints", csp.constraints.size)
    measureTime("time", "find") {
      init
      val result = findBody
      addSolverStat("result", "find", if (result) 1 else 0)
      result
    }
  }
  /* */
  def findNext = {
    shiftSolverStats
    measureTime("time", "findNext") {
      val result = findNextBody
      addSolverStat("result", "find", if (result) 1 else 0)
      result
    }
  }
  /* */
  def findOpt = {
    addSolverStat("csp", "variables", csp.variables.size)
    addSolverStat("csp", "bools", csp.bools.size)
    addSolverStat("csp", "constraints", csp.constraints.size)
    measureTime("time", "findOpt") {
      init
      val result = findOptBody
      shiftSolverStats
      addSolverStat("result", "find", if (result) 1 else 0)
      if (result && (csp.isMinimize || csp.isMaximize)) {
        addSolverStat("result", "objective", value(csp.objective))
      }
      result
    }
  }
  /** Finds a solution within the given bounds */
  def findOptBound(lb: Int, ub: Int) = {
    shiftSolverStats
    addSolverStat("csp", "lb", lb)
    addSolverStat("csp", "ub", ub)
    measureTime("time", "findOptBound") {
      val result = findOptBoundBody(lb, ub)
      addSolverStat("result", "find", if (result) 1 else 0)
      result
    }
  }
}

/**
 * Factory for default solver.
 *
 * [[jp.kobe_u.copris.sugar.Solver]] is returned as the default solver.
 * @see [[jp.kobe_u.copris.sugar.Solver]]
 */
object DefaultSolver {
  def apply(csp: CSP): AbstractSolver =
    new sugar.Solver(csp)
}

/*
 * Statement (this trait is not used)
 * Constraint class is also a Statement
 */
/*
trait Statement
case class IntStatement(x: Var, d: Domain) extends Statement
case class BoolStatement(p: Bool) extends Statement
case class ConstraintStatement(c: Constraint) extends Statement
case class MinimizeStatement(x: Var) extends Statement
case class MaximizeStatement(x: Var) extends Statement
*/
