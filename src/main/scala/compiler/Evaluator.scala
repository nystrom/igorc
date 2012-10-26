package compiler

object Evaluator {
  import Trees._

  def eval(p: Program): Exp = {
    p.funs collectFirst {
      case Fun('main, Nil, body) => eval(body, new Env(p, Map.empty))
    } get
  }

  class Env(val p: Program, var vars: Map[Symbol, Exp]) {
  }

  def eval(e: Exp, env: Env): Exp = e match {
    case IntLit(value) => IntLit(value)
    
    case Bin(IntLit(x), '+, IntLit(y)) => IntLit(x + y)
    case Bin(IntLit(x), '-, IntLit(y)) => IntLit(x - y)
    case Bin(IntLit(x), '*, IntLit(y)) => IntLit(x * y)
    case Bin(IntLit(x), '/, IntLit(y)) => IntLit(x / y)
    case Bin(IntLit(x), '<, IntLit(y)) => IntLit(if (x < y) 1 else 0)
    case Bin(IntLit(x), '>, IntLit(y)) => IntLit(if (x > y) 1 else 0)
    case Bin(IntLit(x), '<=, IntLit(y)) => IntLit(if (x <= y) 1 else 0)
    case Bin(IntLit(x), '>=, IntLit(y)) => IntLit(if (x >= y) 1 else 0)
    case Bin(IntLit(x), '==, IntLit(y)) => IntLit(if (x == y) 1 else 0)
    case Bin(IntLit(x), '!=, IntLit(y)) => IntLit(if (x != y) 1 else 0)
    case Bin(x, op, y) => eval(Bin(eval(x, env), op, eval(y, env)), env)

    case Invoke(f, args) =>
      env.p.funs collectFirst {
        case Fun(name, params, body) if name == f =>
          val env2 = new Env(env.p, (params zip (args map (a => eval(a, env)))).toMap)
          eval(body, env2)
      } get

    case If(cond, e1, e2) => eval(cond, env) match {
      case IntLit(0) => eval(e2, env)
      case _ => eval(e1, env)
    }
    case While(cond, body) =>
      eval(If(cond, Seq(body :: While(cond, body) :: Nil), Seq(Nil)), env)

    case Var(x) => env.vars(x)
    case Assign(x, e) =>
      val v = eval(e, env)
      env.vars += (x -> v)
      v

    case Seq(Nil) => IntLit(0)
    case Seq(s :: Nil) => eval(s, env)
    case Seq(s :: ss) =>
      eval(s, env)
      eval(Seq(ss), env)
  }
}