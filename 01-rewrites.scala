// # Preface
//
// This is a Scala CLI file
// To run it, use
//
//  scala-cli 01-rewrites.scala
//
// or (from Scala 3.5 onwards)
//
//  scala 01-rewrites.scala


// # Rewrite Systems: Introduction
//
// When we see an equality, like
//
//   a = b
//
// it tells us that we can replace a with b *or* b with a.
//
// A rewriting system is very similar, but we only replace the left hand side
// with the right hand side. If we interpret
//
//   a -> b
//
// as a rewrite rule that tells us that we can replace a with b. Rewrite systems
// define a Turing complete system of computation (in other words, they can
// compute anything that a computer can compute) with a model that is very
// similar to functional programming.
//
// Let's see a simple example, calculating the derivative
// We can define the following rules:
//
//   R1: D_X(X) -> 1
//   R2: D_X(Y) -> 0
//   R3: D_X(u + v) -> D_X(u) + D_X(v)
//   R4: D_X(u * v) -> (u * D_X(v)) + (D_X(u) * v)
//
// If we have the expression
//
//   D_X(X * X) (i.e. x^2)
//
// we can apply
//
//   R4 to give (X * D_X(X)) + (D_X(X) * X)
//   R1 to the LHS to give (X * 1) + (D_X(X) * X)
//   R1 to the RHS to give (X * 1) + (1 * X)
//
// Note we made the choice to apply R1 first to the left then the right. This
// choice was arbitrary. We could have applied R1 from right to left, or even to
// both sides simultaneously, and have achieved the same result.
//
// How would we implement this as code? Conceptually there are two parts to a
// rewrite:
//
// 1. the *pattern* it matches, which is the left hand side of the rule; and
// 2. the output it produces, if it successfully matches.
//
// The input and output are of the same type in the example above, but this is
// not always the case. So in Scala we can write.

trait Rewrite[A, B] {
  def apply(in: A): Option[B]

  def orElse(that: Rewrite[A, B]): Rewrite[A, B] =
    val self = this
    new Rewrite {
      def apply(in: A): Option[B] =
        self(in).orElse(that(in))
    }
}
object Rewrite {
  def lift[A, B](f: PartialFunction[A, B]): Rewrite[A, B] =
    new Rewrite {
      val lifted = f.lift
      def apply(in: A): Option[B] =
        lifted(in)
    }
}

// I defined a method `orElse` to compose two `Rewrites`. `r1.orElse(r2)` tries
// first `r1`, and, if that fails, tries `r2`. I also defined a utility to
// convert a `PartialFunction` to a `Rewrite`, as I know this will be useful in
// the future.

// Let's implement the derivative rules above. First we define the type of
// expressions (or terms).

enum Expr {
  case Var(name: String)
  case Const(value: Int)
  case Add(left: Expr, right: Expr)
  case Mul(left: Expr, right: Expr)
  case Deriv(variable: Var, expr: Expr)

  def +(that: Expr): Expr =
    Add(this, that)

  def *(that: Expr): Expr =
    Mul(this, that)

  override def toString(): String =
    this match {
      case Var(name) => name
      case Const(value) => value.toString()
      case Add(left, right) => s"($left + $right)"
      case Mul(left, right) => s"($left * $right)"
      case Deriv(variable, expr) => s"D_${variable.name}($expr)"
    }
}

// Now we implement the rules.

object Rules {
  import Expr.*

  val r1: Rewrite[Expr, Expr] =
    Rewrite.lift{ case Deriv(Var(v1), Var(v2)) if v1 == v2 => Const(1) }

  val r2: Rewrite[Expr, Expr] =
    Rewrite.lift{ case Deriv(Var(v1), Var(v2)) if v1 != v2 => Const(0) }

  val r3: Rewrite[Expr, Expr] =
    Rewrite.lift{ case Deriv(x, Add(u, v)) => Deriv(x, u) + Deriv(x, v) }

  val r4: Rewrite[Expr, Expr] =
    Rewrite.lift{ case Deriv(x, Mul(u, v)) => (u * Deriv(x, v)) + (Deriv(x, u) * v) }
}

// Now we need a method to apply a rule to an expression.

def rewrite(expr: Expr, rule: Rewrite[Expr, Expr]): Expr =
  import Expr.*
  expr match {
    case Var(name) => rule(expr).getOrElse(expr)

    case Const(value) => rule(expr).getOrElse(expr)

    case Add(left, right) =>
      val e = rewrite(left, rule) + rewrite(right, rule)
      rule(e).getOrElse(e)

    case Mul(left, right) =>
      val e = rewrite(left, rule) * rewrite(right, rule)
      rule(e).getOrElse(e)

    case Deriv(variable, expr) =>
      val e = Deriv(variable, rewrite(expr, rule))
      rule(e).getOrElse(e)
  }

// I arbitrarily choose to apply rules from the inside out. We could have also
// done the reverse and would still end up with the same result (for these
// rules).
//
// Let's step through an example and show it works as expected.

def iterate(expr: Expr, rule: Rewrite[Expr, Expr]): Expr = {
  println(expr)

  val e = rewrite(expr, rule)
  if e == expr then e
  else iterate(e, rule)
}

import Rules.*
import Expr.*

val rule: Rewrite[Expr, Expr] = r1.orElse(r2).orElse(r3).orElse(r4)

val expr = Deriv(Var("x"), (Var("x") * Var("x")))

// This will print the output
//
// D_x((x * x))
// ((x * D_x(x)) + (D_x(x) * x))
// ((x * 1) + (1 * x))
//
// which shows the process of calculating the derivative of x^2
@main def go(): Unit =
  iterate(expr, rule)
