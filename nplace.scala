object NumberPlace {
  val size = 9
  val blockSize = 3

  /**
   * @param initial initial number list.
   * @return solved list if found, otherwise null.
   */
  def solve(initial: List[Int]): List[Int] = {
    sub(List(), initial.reverse)
  }

  /**
   * @param list temporary solved list.
   * @param initial a part initial number list that is not solved.
   * @return solved list if found, otherwise null.
   */
  private def sub(list: List[Int], initial: List[Int]): List[Int] =
    if (initial.isEmpty) {
      list
    } else {
      val head = initial.head
      val tail = initial.tail
      if (head > 0 && head <= size) {
        sub(head :: list, tail)
      } else {
        (1 to size).view.filter(isValid(initial.reverse ::: list, tail.size, _)).map(x => sub(x :: list, tail)).find(_ != null).orNull
      }
    }

  /**
   * gets wheter a number can be put into specific square.
   * @param list number list.
   * @param id square index to be put a number.
   * @param n number to be put.
   * @return true if a passed number can be put into passed square, otherwise false.
   */
  def isValid(list: List[Int], id: Int, n: Int): Boolean = {
    /**
     * @param i square index.
     * @return (column index, row index, block index).
     */
    def props(i: Int) = {
      val x = i % size
      val y = i / size
      (x, y, y / blockSize * blockSize + x / blockSize)
    }
    val (x, y, z) = props(id)
    list.zipWithIndex.forall(t => {
      val (m, i) = t
      /* valid if same number is not in same row, column, and block. */
      m != n || {
        val (a, b, c) = props(i)
        a != x && b != y && c != z
      }
    })
  }
}

def printSquares(list: List[Int]) = {
  list.map(x => x match {
    case 0 => "_"
    case _ => x.toString
  }).grouped(NumberPlace.size).foreach(x => println(x.mkString))
}
if (args.length > 0) {
  try {
    val list = args(0).toList.map(_.toString.toInt)
    println("input:")
    printSquares(list)
    val result = NumberPlace.solve(args(0).toList.map(_.toString.toInt))
    println("result:")
    printSquares(result)
  } catch {
    case ex: NumberFormatException => println("invalid argument")
  }
} else {
  println("no argument")
}
