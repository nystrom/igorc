package compiler

object OldMain extends App {
  run(args.mkString(" "))

  trait Tree
  case class Program(funs: List[Fun]) extends Tree
  case class Fun(name: Symbol, formals: List[Symbol], exp: Exp) extends Tree

  trait Exp extends Tree
  case class IntLit(value: Int) extends Exp
  case class Var(x: Symbol) extends Exp
  case class Bin(e1: Exp, op: Symbol, e2: Exp) extends Exp
  case class Invoke(fun: Symbol, args: List[Exp]) extends Exp
  case class If(cond: Exp, e1: Exp, e2: Exp) extends Exp

  class Env(val funs: Map[Symbol, Fun], val vars: Map[Symbol, Exp]) {
    def this(funs: List[Fun]) = this(funs.groupBy(_.name).mapValues(_.head), Map.empty)
  }

  def eval(t: Exp)(env: Env): Exp = {
    println(t)

    t match {
      case IntLit(v) =>
        IntLit(v)
      case Var(x) =>
        env.vars(x)
      case Invoke(f, es) =>
        val vs = es map { e => eval(e)(env) }
        env.funs.get(f) match {
          case Some(Fun(_, formals, body)) if formals.length == vs.length =>
            eval(body)(new Env(env.funs, (formals zip vs).toMap))
          case Some(_) => throw new RuntimeException("wrong num args to " + f.name)
          case None => throw new RuntimeException(f.name + " not found")
        }
      case Bin(IntLit(x), '+, IntLit(y)) => IntLit(x+y)
      case Bin(IntLit(x), '-, IntLit(y)) => IntLit(x-y)
      case Bin(IntLit(x), '*, IntLit(y)) => IntLit(x*y)
      case Bin(IntLit(x), '/, IntLit(y)) => IntLit(x/y)
      case Bin(IntLit(x), '<, IntLit(y)) => IntLit(if (x < y) 1 else 0)
      case Bin(IntLit(x), '>, IntLit(y)) => IntLit(if (x > y) 1 else 0)
      case Bin(IntLit(x), '<=, IntLit(y)) => IntLit(if (x <= y) 1 else 0)
      case Bin(IntLit(x), '>=, IntLit(y)) => IntLit(if (x >= y) 1 else 0)
      case Bin(IntLit(x), '==, IntLit(y)) => IntLit(if (x == y) 1 else 0)
      case Bin(IntLit(x), '!=, IntLit(y)) => IntLit(if (x != y) 1 else 0)
      case Bin(x, op, y) => eval(Bin(eval(x)(env), op, eval(y)(env)))(env)
      case If(IntLit(0), e1, e2) => eval(e2)(env)
      case If(IntLit(_), e1, e2) => eval(e1)(env)
      case If(e0, e1, e2) => eval(If(eval(e0)(env), e1, e2))(env)
    }
  }

  def peval(p: Program): Unit = p match {
    case Program(funs) =>
      funs collect {
        case Fun('main, Nil, exp) =>
          val r = eval(exp)(new Env(funs))
          println(r)
      }
  }

  def run(arg: String) = {
    parse(arg) map { p =>
      peval(p)
      // val t = lower(p)
      // peval(t)
    }
  }

  trait Insn
  trait Loc
  case class IList(insns: List[Insn]) extends Exp
  case class Adr(offset: Int, x: Symbol) extends Loc
  case class Reg(x: Symbol) extends Loc
  case class Imm(v: Int) extends Loc
  case class MOV(tgt: Loc, src: Loc) extends Insn
  case class LABEL(label: Symbol) extends Insn
  case class B(label: Symbol) extends Insn
  case class BZ(x: Loc, label: Symbol) extends Insn
  case class OP(op: Symbol, tgt: Loc, src1: Loc, src2: Loc) extends Insn
  case class CALL(f: Symbol, args: List[Loc]) extends Insn
  case class RETURN(x: Loc) extends Insn
  
  def lower(e: Exp)(result: Symbol): List[Insn] = e match {
    case IntLit(v) => MOV(Reg(result), Imm(v)) :: Nil
    case Var(x) => MOV(Reg(result), Reg(x)) :: Nil
    case Bin(e1, op, e2) => 
      val r1 = gensym()
      val r2 = gensym()
      lower(e1)(r1) ++ lower(e2)(r2) :+ OP(op, Reg(result), Reg(r1), Reg(r2))
    case If(e0, e1, e2) =>
      val r0 = gensym()
      val r1 = gensym()
      val r2 = gensym()
      val F = gensym()
      val J = gensym()
      lower(e0)(r0) ++ List(BZ(Reg(r0), F)) ++ lower(e1)(result) ++ List(B(J), LABEL(F)) ++ lower(e2)(result) ++ List(LABEL(J))
    case Invoke(f, args) =>
      val regs: List[Symbol] = args map { _ => gensym() }
      (args zip regs).flatMap {
        case (e: Exp, r: Symbol) => lower(e)(r)
      } ++ List(CALL(f, regs map Reg))
  }
  
  def lower(p: Program): Program = p match {
    case Program(funs) =>
      Program(funs map {
        case Fun(f, formals, exp) =>
          val result = gensym()
          val end = gensym()
          Fun(f, formals, IList(lower(exp)(result) :+ RETURN(Reg(result))))
      })
  }

  val next: () => Int = {
    var n = 0
    () => {
      n += 1
      n
    }
  }
  
  def gensym(prefix: String = "_") = Symbol(prefix + next())

  trait Parser[+A] {
    self =>

    def parse(s: String): Option[(String, A)]

    def ~[B](p: => Parser[B]) = new Sequence(this, p)
    def |[B >: A](p: => Parser[B]) = new Union[B](this, p)
    def /[B >: A](p: => Parser[B]) = new Alt[B](this, p)
    def * = new Star(this)
    def + = new Sequence(this, this*) map { case (a, as) => a :: as }
    def ? = new Opt(this)

    def map[B](f: A => B): Parser[B] = new Parser[B] {
      def parse(s: String) = {
        self.parse(s) map {
          case (s1, a1) => (s1, f(a1))
        }
      }
    }
    def flatMap[B](f: A => Parser[B]): Parser[B] = new Parser[B] {
      def parse(s: String) = {
        self.parse(s) flatMap {
          case (s1, a1) => f(a1).parse(s1)
        }
      }
    }
    def filter(f: A => Boolean): Parser[A] = new Parser[A] {
      def parse(s: String) = {
        self.parse(s) match {
          case Some((s1, a1)) if f(a1) => Some((s1, a1))
          case _ => None
        }
      }
    }
  }

  object Eof extends Parser[Unit] {
    def parse(s: String) = {
      if (s.isEmpty) Some("", ())
      else None
    }
  }
  
  object Any extends Parser[Char] {
    def parse(s: String) = s.headOption map { a => (s.tail, a) }
  }

  case class Ch(ch: Char) extends Parser[Char] {
    def parse(s: String) = s.headOption flatMap {
      case a if a == ch => Some((s.tail, a))
      case _ => None
    }
  }

  case class St(st: String) extends Parser[String] {
    def parse(s: String) = if (s.startsWith(st)) Some((s.drop(st.length), st)) else None
  }

  case class AnyOf(cs: Seq[Char]) extends Parser[Char] {
    def parse(s: String) = s.headOption flatMap {
      case ch if cs contains ch => Some((s.tail, ch))
      case _ => None
    }
  }

  // Sequence
  class Sequence[+A, +B](pa: => Parser[A], pb: => Parser[B]) extends Parser[(A, B)] {
    def parse(s: String) = {
      pa.parse(s) flatMap {
        case (s1, a) => pb.parse(s1) map {
          case (s2, b) => (s2, (a, b))
        }
      }
    }
  }

  // Ordered choice
  class Alt[+A](pa: => Parser[A], pb: => Parser[A]) extends Parser[A] {
    def parse(s: String) = {
      pa.parse(s) match {
        case Some((s1, a1)) => Some((s1, a1))
        case None => pb.parse(s) match {
          case Some((s2, a2)) => Some((s2, a2))
          case None => None
        }
      }
    }
  }

  // Unordered choice -- take the longest
  class Union[+A](pa: => Parser[A], pb: => Parser[A]) extends Parser[A] {
    def parse(s: String) = {
      (pa.parse(s), pb.parse(s)) match {
        case (Some((s1, a1)), Some((s2, a2))) if (s1.length <= s2.length) => Some((s1, a1))
        case (Some((s1, a1)), Some((s2, a2))) => Some((s2, a2))
        case (Some((s1, a1)), None) => Some((s1, a1))
        case (None, Some((s2, a2))) => Some((s2, a2))
        case (None, None) => None
      }
    }
  }

  class Star[+A](pa: => Parser[A]) extends Parser[List[A]] {
    def parse(s: String): Option[(String, List[A])] = {
      pa.parse(s) match {
        case Some((s1, a1)) =>
          new Star(pa).parse(s1) map {
            case (s2, as) =>
              (s2, a1 :: as)
          }
        case None =>
          Some((s, Nil))
      }
    }
  }

  class Opt[+A](pa: => Parser[A]) extends Parser[Option[A]] {
    def parse(s: String): Option[(String, Option[A])] = {
      pa.parse(s) match {
        case Some((s1, a)) =>
          Some((s1, Some(a)))
        case None =>
          Some((s, None))
      }
    }
  }

  def parse(file: String): Option[Program] = {
    val source = try {
      scala.io.Source.fromFile(file).toArray.mkString
    } catch {
      case ex: java.io.FileNotFoundException => file
    }
    println("unparsed: " + source)

    lazy val ws: Parser[Unit] = {
      // careful! don't use the implicit str2token because it'll be recursive
      (Ch(' ') / Ch('\n') / Ch('\t')).* map { _ => () }
    }

    def token[A](p: => Parser[A]) = {
      ws ~ p map { case (_, a) => a }
    }

    implicit def str2token(s: String): Parser[String] = token(new St(s))

    lazy val upper = AnyOf('A' to 'Z')
    lazy val lower = AnyOf('a' to 'z')
    lazy val digit = AnyOf('0' to '9')
    lazy val letter = upper | lower
    lazy val ident: Parser[Symbol] = token { (letter ~ (letter / digit).*) map { case (x, xs) => (x :: xs).mkString } map { s => Symbol(s) } }
    lazy val number: Parser[Exp] = token { (digit ~ digit.*) map { case (x, xs) => (x :: xs).mkString } map { s => IntLit(s.toInt) } }
    lazy val exp: Parser[Exp] = bin / ite / app / par / (ident map Var) / number
    lazy val par: Parser[Exp] = "(" ~ exp ~ ")" map { case ((_, e), _) => e }
    lazy val ite: Parser[Exp] = "(" ~ "if" ~ exp ~ exp ~ exp ~ ")" map { case (((((_, _), e0), e1), e2), _) => If(e0, e1, e2) }
    lazy val app: Parser[Exp] = "(" ~ ident ~ exp.* ~ ")" map { case (((_, e1), e2), _) => Invoke(e1, e2) }
    lazy val bin: Parser[Exp] = "(" ~ op ~ exp ~ exp ~ ")" map { case ((((_, op), e1), e2), _) => Bin(e1, Symbol(op), e2) }
    lazy val op: Parser[String] = { "+" / "-" / "*" / "/" / "<=" / "<" / "==" / "!=" / ">=" / ">" }
    lazy val fun: Parser[Fun] = "(" ~ "define" ~ ident ~ "(" ~ ident.* ~ ")" ~ exp ~ ")" map { case (((((((_, _), name), _), xs), _), e), _) => Fun(name, xs, e) }
    lazy val funs: Parser[Program] = (fun.*) map { case fs => Program(fs) }
    lazy val parser: Parser[Program] = (funs ~ ws ~ Eof) map { case ((e, _), _) => e }

    parser.parse(source) flatMap {
      case ("", t) => println("parsed " + t); Some(t)
      case (s, p) => println("remaining input: " + s); println(p); None
      case x => println("huh " + x); None
    }
  }
}
