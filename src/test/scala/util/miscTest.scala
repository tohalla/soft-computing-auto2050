package util

import org.scalatest.FunSuite

class miscTest extends FunSuite {

  test("should calculate euclidean distance correctly") {
    assert(misc.getDistance(Seq(4, 5, 12), Seq(2, 9, 8)) == 6)
  }

}
