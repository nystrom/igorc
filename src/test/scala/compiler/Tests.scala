package compiler

import org.junit._
import Assert._

class Tests extends App {
  import Main._
  import Trees._

  @Test
  def test1 {
    val p = Parser parse """
def main() = 0;
"""
    assertEquals(Some(Program(Fun('main, Nil, IntLit(0))::Nil)), p)
  }

  @Test
  def test1a {
    val v = Main run """
def main() = 345789;
"""
    assertEquals(IntLit(345789), v)
  }

  @Test
  def test9 {
	  val p = Parser parse """
def fact(n) = if (n) n * fact(n-1); else 1;
			  """
			  assertEquals(Some(Program(List(Fun('fact,List('n),If(Var('n),Bin(Var('n),'*,Invoke('fact,List(Bin(Var('n),'-,IntLit(1))))),IntLit(1)))))), p)
  }

  @Test
  def test2 {
    val p = Parser parse """
def main() = 0;
def fact(n) = { if (n) n * fact(n - 1); else 1; }
"""
    assertEquals(Some(Program(List(Fun('main,List(),IntLit(0)), Fun('fact,List('n),If(Var('n),Bin(Var('n),'*,Invoke('fact,List(Bin(Var('n),'-,IntLit(1))))),IntLit(1)))))), p)
  }

  @Test
  def test3 {
    val p = Parser parse """
def main() = fact(10);
def fact(n) = { if (n) n * fact(n - 1); else 1; }
"""
    assertEquals(Some(Program(List(Fun('main,List(),Invoke('fact, List(IntLit(10)))), Fun('fact,List('n),If(Var('n),Bin(Var('n),'*,Invoke('fact,List(Bin(Var('n),'-,IntLit(1))))),IntLit(1)))))), p)
  }

  @Test
  def test4 {
    val v = 
    Main run """
def main() = fact(10);
def fact(n) = { if (n) n * fact(n - 1); else 1; }
"""
    
    assertEquals(IntLit(10*9*8*7*6*5*4*3*2), v)
  }
}
