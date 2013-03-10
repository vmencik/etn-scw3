package cz.etn.scw3

object Fib {

  def apply(index: Int): Long = {

    def fibTuple(n: Int): (Long, Long) = {
      if (n == 1) (0, 0)
      else if (n == 2) (0, 1)
      else {
        val (n2, n1) = fibTuple(n - 1)
        (n1, n2 + n1)
      } 
    }
    
    fibTuple(index)._2

  }

}

