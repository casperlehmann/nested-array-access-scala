object MultiplicationTable {
  def main(args: Array[String]): Unit = {
    println(getValueAtIndex(2, 3, 9))
    println(getValueAtIndexForNDArray(multiTable, indices = 2, 3, 9))
  }

  def multiTable: Array[Array[Array[Int]]] = {
    // Make a multiplication table, like in elementary school. Except it's 3D.
    var multiTable = Array.ofDim[Int](10, 10, 10)
    for (x <- 0 to 9; y <- 0 to 9; z <- 0 to 9) {
      multiTable(x)(y)(z) = x * y * z
    }
    multiTable
  }

  def getValueAtIndex(x: Int, y: Int, z: Int): Int = {
    // Simple indexing into nested array, three layers deep
    multiTable(x)(y)(z)
  }

  def getValueAtIndexForNDArray[X](t: X, indices: Int*): Int = {
    // Recursive accessing at index for n-dimensional array
    t match {
      case t: Int => t: Int
      case t: Array[_] => {
        val rest = indices.drop(1)
        val into = t(indices(0))
        getValueAtIndexForNDArray(into, rest: _*)
      }
    }
  }
}
