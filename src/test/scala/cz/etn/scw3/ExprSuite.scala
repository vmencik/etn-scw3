package cz.etn.scw3

import scala.language.implicitConversions
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class ExprSuite extends FunSuite with ShouldMatchers {
  implicit def int2Number(n: Int): Number = Number(n)
  
  implicit def str2Var(name: String): Var = Var(name)
  
  test("Eval constant") {
    val expr = Number(10)
    Dummy.eval(expr) should be(Number(10))
  }

  test("Eval add") {
    val expr = Add(Number(1), Number(2))
    Dummy.eval(expr) should be(Number(3))
  }
  
  test("Eval sub") {
    val expr = Sub(Number(1), Number(2))
    Dummy.eval(expr) should be(Number(-1))
  }

  test("Eval let constant") {
    val expr = Let(List(), Number(14))
    Dummy.eval(expr) should be(Number(14))
  }
  
  test("Eval let Addition") {
    val expr = Let(List(), Add(Number(1), Number(5)))
    Dummy.eval(expr) should be(Number(6))
  }

  test("Eval let var") {
    val expr = Let(List(("x", Number(10))), Var("x"))
    Dummy.eval(expr) should be(Number(10))
  }

  test("Eval let complex") {
    val expr = Let(List(("x", Number(10)), ("y", Add(Var("x"), Number(3)))), Add(Var("y"), Var("x")))
    Dummy.eval(expr) should be(Number(23))
  }
  
  test("Eval if") {
    Dummy.eval(If(Number(0), Number(1), Number(2))) should be (Number(2))
    Dummy.eval(If(Number(1), Number(10), Number(20))) should be (Number(10))
  }
  
  test("Pair") {
    val pair = Pair(Add(Number(1), Number(2)), Sub(Number(3), Number(2)))
    Dummy.eval(pair) should be(Pair(Number(3), Number(1)))
  }
  
  test("Pair first") {
    val pairFirst = First(Pair(1, 2))
    Dummy.eval(pairFirst) should be(Number(1))
  }
  
  test("Pair second") {
    val pairSecond = Second(Pair(1, 2))
    Dummy.eval(pairSecond) should be(Number(2))
  }
  
  test("Function definition"){
    val fn = Fn("f","p",Number(1))
    Dummy.eval(fn) should be(fn)
  }
  
  test("Function call") {
    val fn = Fn("f","p",Number(1))
    val expr = Call(fn, Number(0))
    Dummy.eval(expr) should be(Number(1))
  }
  
  test("Function call with param") {
    val fn = Fn("f","p",Var("p"))
    val expr = Call(fn, Number(0))
    Dummy.eval(expr) should be(Number(0))
  }
  
  test("suma"){
    val sum = Fn("sum", "n",
        If(
        	"n",
        	Add("n", Call("sum", Sub("n",1))),
        	0
        )
    )
    Dummy.eval(Call(sum, 3)) should be (Number(6))
  }
  
  test("fibonacci in Josef") {
	  val fibJosef = Fn("fib", "index",
	    Let(List("fibTuple" -> Fn("fibInternal", "n",
	      If(
			    Sub("n", 1),
			    If(
			      Sub("n", 2),
			      Let(
			        List(
			          "ftuple" -> Call("fibInternal", Sub("n", 1)),
			          "n-1" -> First("ftuple"),
			          "n-2" -> Second("ftuple")
			        ),
			        Pair(Add("n-1", "n-2"), "n-1")
			      ),
			      Pair(1, 0)
			    ),
			    Pair(0, 0)
	  		)
	    )),
	    First(Call("fibTuple", "index"))
	  )
	)
	Dummy.eval(Call(fibJosef, 1)) should be (Number(0))
	Dummy.eval(Call(fibJosef, 2)) should be (Number(1))
	Dummy.eval(Call(fibJosef, 3)) should be (Number(1))
	Dummy.eval(Call(fibJosef, 10)) should be (Number(34))  
	Dummy.eval(Call(fibJosef, 40)) should be (Number(63245986))
	  
  }
}