package cz.etn.scw3

abstract class Expr

case class Number(n: Int) extends Expr

case class Add(e1: Expr, e2: Expr) extends Expr

case class Sub(e1: Expr, e2: Expr) extends Expr

case class Let(bindings: List[(String, Expr)], body: Expr) extends Expr

case class Var(name: String) extends Expr

case class If(cond: Expr, trueBranch: Expr, falseBranch: Expr) extends Expr

case class Pair(e1:Expr, e2: Expr) extends Expr

case class First(e: Expr) extends Expr
case class Second(e: Expr) extends Expr

case class Fn(name: String, param: String, body: Expr) extends Expr

case class Call(function: Expr, arg: Expr) extends Expr

object Dummy {

  def eval(expr: Expr): Expr = {
    def eval(expr: Expr, env: Map[String, Expr]): Expr =
      expr match {
//      case value @ (Number(_)|Fn(_,_,_)) => value
        case n : Number => n
        case f: Fn => f 
        case Add(e1, e2) =>
          val (Number(n1), Number(n2)) = (eval(e1, env), eval(e2, env))
          Number(n1 + n2)
        case Sub(e1, e2) =>
          val (Number(n1), Number(n2)) = (eval(e1, env), eval(e2, env))
          Number(n1 - n2)
        case Var(name) => env(name)
        case Let(bindings, body) =>
          val nenv = bindings.foldLeft(env) { (innerEnv, binding) =>
            val (variable, expr) = binding
            innerEnv + (variable -> eval(expr, innerEnv))
          }
          eval(body, nenv)
        case If(cond, trueBranch, falseBranch) =>
          eval(cond, env) match { 
            case Number(0) => eval(falseBranch, env)
            case Number(_) => eval(trueBranch, env)
          }
        case Pair(e1, e2) => Pair(eval(e1, env), eval(e2, env)) 
        case First(e) => 
          val Pair(p1, p2) = eval(e, env)
          p1
        case Second(e) => 
          val Pair(p1, p2) = eval(e, env)
          p2
        case Call(fn, arg) =>
          val fun @ Fn(name, param, body) = eval(fn, env)
          eval(body, env + (param -> eval(arg, env)) + (name -> fun))
      }
    eval(expr, Map.empty)
  }
  	
}
