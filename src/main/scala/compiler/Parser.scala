package compiler

object Parser {
  import Trees._

  /*
  case class Program(funs: List[Fun]) extends Tree
  case class Fun(name: Symbol, formal: List[Symbol], stm: Exp) extends Tree
  case class IntLit(value: Int) extends Exp
  case class Bin(e1: Exp, op: Symbol, e2: Exp) extends Exp
  case class Var(x: Symbol) extends Exp
  case class If(cond: Exp, e1: Exp, e2: Exp) extends Exp
  case class Invoke(f: Symbol, args: List[Exp]) extends Exp
  case class While(cond: Exp, body: Exp) extends Exp
  case class Assign(x: Symbol, e: Exp) extends Exp
   */

  type M[A] = Parser[A]

  trait Parser[+A] {
    self =>

    def parse(s: String): Option[(String, A)]

    def ~[B](p: => Parser[B]) = new Seq(this, p)
    def |[B >: A](p: => Parser[B]) = new Alt[B](this, p)
    def |||[B >: A](p: => Parser[B]) = new AltAlt[B](this, p)
    def * : Parser[List[A]] = new Star(this)
    def + : Parser[List[A]] = new Plus(this)
    def ? : Parser[Option[A]] = new Optional(this)

    def map[B](f: A => B): Parser[B] = new Parser[B] {
      def parse(s: String) = self.parse(s) map {
        case (s1, a) => (s1, f(a))
      }
    }

    def flatMap[B](f: A => Parser[B]) = new Parser[B] {
      def parse(s: String) = self.parse(s) match {
        case Some((s1, a)) => f(a).parse(s1)
      }
    }
  }

  implicit def ch2p(ch: Char) = new Ch(ch)
  implicit def st2p(s: String) = new Str(s)

  class ChRange(from: Char, to: Char) extends Parser[Char] {
    def parse(s: String) = s.headOption match {
      case Some(c) if from <= c && c <= to => Some((s.tail, c))
      case _ => None
    }
  }

  // 'c'
  class Ch(ch: Char) extends Parser[Char] {
    def parse(s: String) = s.headOption match {
      case Some(c) if c == ch => Some((s.tail, c))
      case _ => None
    }

    def --(ch2: Char) = new ChRange(ch, ch2)
  }

  // "t"
  class Str(t: String) extends Parser[String] {
    def parse(s: String) = {
      if (s startsWith t) {
        Some((s drop t.length, t))
      } else {
        None
      }
    }
  }

  // ab
  class Seq[+A, +B](pa: => Parser[A], pb: => Parser[B]) extends Parser[(A, B)] {
    def parse(s: String) = pa.parse(s) match {
      case Some((s1, a)) => pb.parse(s1) match {
        case Some((s2, b)) => Some((s2, (a, b)))
        case None => None
      }
      case None => None
    }
  }

  // a / b
  class Alt[+A](pa: => Parser[A], pb: => Parser[A]) extends Parser[A] {
    def parse(s: String) = pa.parse(s) match {
      case Some((s1, a)) => Some((s1, a))
      case None => pb.parse(s)
    }
  }

  // a | b
  class AltAlt[+A](pa: => Parser[A], pb: => Parser[A]) extends Parser[A] {
    def parse(s: String) = (pa.parse(s), pb.parse(s)) match {
      case (Some((s1, a)), None) => Some((s1, a))
      case (None, Some((s2, a))) => Some((s2, a))
      case (Some((s1, a1)), Some((s2, a2))) =>
        if (s1.length <= s2.length) Some((s1, a1))
        else Some((s2, a2))
      case (None, None) => None
    }
  }

  // p?
  class Optional[+A](p: => Parser[A]) extends Parser[Option[A]] {
    def parse(s: String) = p.parse(s) match {
      case Some((s1, a)) => Some((s1, Some(a)))
      case None => Some((s, None))
    }
  }

  // p*
  class Star[+A](p: => Parser[A]) extends Parser[List[A]] {
    def parse(s: String) = p.parse(s) match {
      case Some((s1, a)) => this.parse(s1) map {
        case (s2, as) => (s2, a :: as)
      }
      case None => Some((s, Nil))
    }
  }

  // p+
  class Plus[+A](p: => Parser[A]) extends Parser[List[A]] {
    def parse(s: String) = p.parse(s) match {
      case Some((s1, a)) => (new Star(p)).parse(s1) map {
        case (s2, as) => (s2, a :: as)
      }
      case None => None
    }
  }

    object Eof extends Parser[Unit] {
    def parse(s: String) = {
      if (s.isEmpty) Some("", ())
      else None
    }
  }

  // p+ == p p*
  //  def Plus[A](p: Parser[A]) = new Seq(p, new Star(p))

  /*
   * program ::= fun*
   * fun     ::= 'def' id '(' param ** ',' ')' '=' stm
   * stm     ::= n | id | 'if' '(' stm ')' stm 'else' stm | id '(' stm ** ',' ')' | '(' stm ')' | '{' stm ** ';' '}' | id '=' stm | stm op stm
   * op      ::= + | - | * | < | == | ...
   * param   ::= id
   */

  def parse(file: String): Option[Program] = {
    val source = try {
      scala.io.Source.fromFile(file).toArray.mkString
    } catch {
      case ex: java.io.FileNotFoundException => file
    }
    println("unparsed: " + source)

    lazy val space = new Ch(' ') | new Ch('\t') | new Ch('\n') | new Ch('\r') | new Ch('\f')
    lazy val WS = space.*

    lazy val digit = '0' -- '9'
    lazy val number = WS ~ digit.+ map { case (_, xs) => xs.mkString.toInt }
    lazy val lower = 'a' -- 'z'
    lazy val upper = 'A' -- 'Z'
    lazy val letter = lower | upper
    lazy val ident = WS ~ letter ~ (letter | digit).* map { case ((_, first), rest) => Symbol((first :: rest).mkString) }
    lazy val numexp = number map { (x: Int) => IntLit(x) }
    lazy val varexp = ident map Var
    lazy val `(` = WS ~ "(" map { case (x,y) => Symbol(y) }
    lazy val `)` = WS ~ ")" map { case (x,y) => Symbol(y) }
    lazy val `if` = WS ~ "if" map { case (x,y) => Symbol(y) }
    lazy val `else` = WS ~ "else" map { case (x,y) => Symbol(y) }
    lazy val `while` = WS ~ "while" map { case (x,y) => Symbol(y) }
    lazy val `def` = WS ~ "def" map { case (x,y) => Symbol(y) }
    lazy val `=` = WS ~ "=" map { case (x,y) => Symbol(y) }
    lazy val plus = WS ~ "+" map { case (x,y) => Symbol(y) }
    lazy val minus = WS ~ "-" map { case (x,y) => Symbol(y) }
    lazy val `*` = WS ~ "*" map { case (x,y) => Symbol(y) }
    lazy val `==` = WS ~ "==" map { case (x,y) => Symbol(y) }
    lazy val `<` = WS ~ "<" map { case (x,y) => Symbol(y) }
    lazy val `<=` = WS ~ "<=" map { case (x,y) => Symbol(y) }
    lazy val `>=` = WS ~ ">=" map { case (x,y) => Symbol(y) }
    lazy val `>` = WS ~ ">" map { case (x,y) => Symbol(y) }
    lazy val `!=` = WS ~ "!=" map { case (x,y) => Symbol(y) }
    lazy val `;` = WS ~ ";" map { case (x,y) => Symbol(y) }
    lazy val `,` = WS ~ "," map { case (x,y) => Symbol(y) }
    lazy val `{` = WS ~ "{" map { case (x,y) => Symbol(y) }
    lazy val `}` = WS ~ "}" map { case (x,y) => Symbol(y) }
    lazy val parexp: Parser[Exp] = `(` ~ cmpexp ~ `)` map { case ((_, e), _) => e }
    lazy val block: Parser[Exp] = `{` ~ seq ~ `}` map { case ((_, e), _) => e }
    lazy val seq: Parser[Exp] = stm.* map { case s::Nil => s case ss => Seq(ss) } 
    lazy val stm: Parser[Exp] = block | ifexp | whileexp | assign | callstm
    lazy val assign: Parser[Exp] = ident ~ `=` ~ cmpexp ~ `;` map { case (((x, _), e), _) => Assign(x, e) }
    lazy val callstm: Parser[Exp] = cmpexp ~ `;` map (_._1)
    lazy val cmpexp: Parser[Exp] = addexp ~ (cmpop ~ addexp).* map { case (e1, es) => es.foldLeft(e1) { case (left, (op, e)) => Bin(left, op, e) } }
    lazy val addexp: Parser[Exp] = mulexp ~ (addop ~ mulexp).* map { case (e1, es) => es.foldLeft(e1) { case (left, (op, e)) => Bin(left, op, e) } }
    lazy val mulexp: Parser[Exp] = primary ~ (mulop ~ primary).* map { case (e1, es) => es.foldLeft(e1) { case (left, (op, e)) => Bin(left, op, e) } }
    lazy val mulop: Parser[Symbol] = `*` 
    lazy val addop: Parser[Symbol] = plus | minus
    lazy val cmpop: Parser[Symbol] = `<=` | `<` | `>=` | `>` | `==` | `!=`
    lazy val ifexp: Parser[Exp] = `if` ~ `(` ~ cmpexp ~ `)` ~ stm ~ `else` ~ stm map { case ((((((_, _), cond), _), e1), _), e2) => If(cond, e1, e2) }
    lazy val whileexp: Parser[Exp] = `while` ~ `(` ~ cmpexp ~ `)` ~ stm map { case ((((_, _), cond), _), body) => While(cond, body) }
    lazy val primary: Parser[Exp] = numexp | parexp | callexp | varexp
    lazy val callexp: Parser[Exp] = ident ~ `(` ~ exps.? ~ `)` map { case (((x, _), oes), _) => Invoke(x, oes.toList.flatten) }
    lazy val exps: Parser[List[Exp]] = cmpexp ~ (`,` ~ cmpexp).* map { case (e1, es) => es.foldLeft[List[Exp]](e1::Nil) { case (left, (_, e)) => left :+ e } }
    lazy val fun: Parser[Fun] = `def` ~ ident ~ `(` ~ params.? ~ `)` ~ `=` ~ stm map { case ((((((_, name), _), params), _), _), e) => Fun(name, params.toList.flatten, e) }
    lazy val params: Parser[List[Symbol]] = ident ~ (`,` ~ ident).* map { case (e1, es) => es.foldLeft[List[Symbol]](e1::Nil) { case (left, (_, e)) => left :+ e } }
    lazy val parser: Parser[Program] = fun.+ ~ WS ~ Eof map { case ((funs, _), _) => Program(funs) }

    parser.parse(source) match {
      case Some(("", x)) => println("got " + x); Some(x)
      case Some((s, x)) => println("got program, but some input remains: " + s + " ==> " + x); None
      case _ => println("failed"); None
    }
  }

}