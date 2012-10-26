package compiler

object Main extends App {
  run(args.mkString(" "))

  import Trees._
  import Evaluator._

  def peval(t: Program) = eval(t)

  def run(arg: String): Exp = {
    Parser.parse(arg) map { p =>
      check(p)
      peval(p)
    } head
  }

  def codegen(p: Program): Unit = {
  }
  
  def check(p: Program): Unit = {
    p.funs foreach {
      case Fun(name, args, body) =>
        check(Env(p, Map.empty ++ args.map(a => (a, ())).toMap))(body)
    }
    if (p.funs exists (_.name == 'main))
      ()
    else throw new RuntimeException("no function main found")
  }

  def check(env: Env)(e: Exp): Unit = e match {
    case Var(x) => if (env.vars contains x) () else throw new RuntimeException("not defined " + x)
    case Assign(x, e) =>
      check(env)(e)
      if (env.vars contains x) () else throw new RuntimeException("not defined " + x)
    case Seq(es) => es foreach { check(env) }
    case If(cond, e1, e2) => List(cond, e1, e2) foreach { check(env) }
    case While(cond, e) => List(cond, e) foreach { check(env) }
    case Bin(e1, op, e2) => List(e1, e2) foreach { check(env) }
    case Invoke(f, es) =>
      es foreach { check(env) }
      val funs = env.p.funs collect {
        case d @ Fun(name, params, body) if f == name => d
      }
      funs match {
        case Nil => throw new RuntimeException("function " + f + " not found")
        case Fun(name, params, body) :: Nil if params.length != es.length => throw new RuntimeException("function " + f + " called with wrong number of arguments")
        case f :: Nil => ()
        case fs => throw new RuntimeException("too many functions named " + f)
      }
    case _ =>
  }

  case class Env(val p: Program, val vars: Map[Symbol, Unit])

  val next: () => Int = {
    var n = 0
    () => {
      n += 1
      n
    }
  }

  def gensym(prefix: String = "_") = Symbol(prefix + next())
}
