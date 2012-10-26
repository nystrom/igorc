package compiler

object Trees {
  trait Tree
  
  case class Program(funs: List[Fun]) extends Tree
  case class Fun(name: Symbol, formal: List[Symbol], exp: Exp) extends Tree
  
  trait Exp extends Tree
  case class IntLit(value: Int) extends Exp
  case class Bin(e1: Exp, op: Symbol, e2: Exp) extends Exp
  case class Var(x: Symbol) extends Exp
  case class If(cond: Exp, e1: Exp, e2: Exp) extends Exp
  case class Invoke(f: Symbol, args: List[Exp]) extends Exp
  case class While(cond: Exp, body: Exp) extends Exp
  case class Assign(x: Symbol, e: Exp) extends Exp
  case class Seq(es: List[Exp]) extends Exp
}