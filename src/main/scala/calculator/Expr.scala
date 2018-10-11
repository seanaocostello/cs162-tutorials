
package edu.ucsb.cs.cs162.tuts.calculator

// A mathematical expression.
sealed trait Expr

// A variable expression with a name.
final case class Var(name: String) extends Expr

// A number expression with a numeric value.
final case class Num(value: Double) extends Expr

// A unary operation expression (eg. -5 is UnOp("-", Num(5))).
final case class UnOp(op: String, value: Expr) extends Expr

// A binary operation expression (eg. 2+3 is BinOp("+", Num(2), Num(3))))
final case class BinOp(op: String, left: Expr, right: Expr) extends Expr

// The calculator object.
object Calculator {

  // Simplifies the head of the expression (should not simplify recursively!).  
  def simplifyHead(expr: Expr): Expr = {
      expr match {
            case UnOp("-", UnOp("-", a)) => a
	    case BinOp("+", Num(0), a) => a
	    case BinOp("+", a, Num(0)) => a
	    case BinOp("*", Num(1), a) => a
	    case BinOp("*", a, Num(1)) => a
	    case BinOp(a, Var("DUP"), b) => BinOp(a, b, b)
	    case BinOp(a, b, Var("DUP")) => BinOp(a, b, b)
	    case BinOp("*", Num(0), a) => Num(0)
	    case BinOp("*", a, Num(0)) => Num(0)
	    case BinOp("-", a, b) if (a==b) => Num(0)
	    //case BinOp(_, Num(a), Num(b)) => Num(1)
     	    case _ => expr
      }
  }
  
  // Evaluates the expression to a numeric value.
  def evaluate(expr: Expr): Double = {
      simplifyHead(expr) match {
      	   case Num(a) => a
	   case Var(a) => 1
	   case UnOp("-", a) => - evaluate(a)
	   case UnOp(_,a) => 1
	   case BinOp("+", a, b) => evaluate(a) + evaluate(b)
	   case BinOp("-", a, b) => evaluate(a) - evaluate(b)
	   case BinOp("*", a, b) => evaluate(a) * evaluate(b)
	   case BinOp(_,a,b) => 1
      }
  }
}
