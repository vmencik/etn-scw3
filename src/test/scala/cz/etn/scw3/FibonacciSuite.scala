package cz.etn.scw3

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class FibonacciSuite extends FunSuite with ShouldMatchers {

  test("fib(1) = 0") {
    Fib(1) should be(0)
  }
  
  test("fib(2) = 1") {
    Fib(2) should be(1)
  }
  
  test("fib(3) = 1") {
    Fib(3) should be(1)
  }
  
  test("fib(8) = 13") {
	  Fib(8) should be(13)
  }
}