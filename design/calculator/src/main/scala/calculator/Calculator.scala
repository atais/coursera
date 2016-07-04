package calculator

import scala.util.{Failure, Success, Try}

sealed abstract class Expr

final case class Literal(v: Double) extends Expr

final case class Ref(name: String) extends Expr

final case class Plus(a: Expr, b: Expr) extends Expr

final case class Minus(a: Expr, b: Expr) extends Expr

final case class Times(a: Expr, b: Expr) extends Expr

final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
                     namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map { case (ex, signal) =>
      val v = Signal(eval(signal(), namedExpressions))
      (ex, v)
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case s: Literal => s.v
      case s: Ref => {
        val t = Try(references.get(s.name).map(e => eval(e(), references)))
        t match {
          case Success(c) => c.getOrElse(Double.NaN)
          case Failure(_) => Double.NaN
        }
      }
      case s: Plus => eval(s.a, references) + eval(s.b, references)
      case s: Minus => eval(s.a, references) - eval(s.b, references)
      case s: Times => eval(s.a, references) * eval(s.b, references)
      case s: Divide => eval(s.a, references) / eval(s.b, references)
    }
  }

  /** Get the Expr for a referenced variables.
    * If the variable is not known, returns a literal NaN.
    */
  private def getReferenceExpr(name: String,
                               references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
