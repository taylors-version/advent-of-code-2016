import org.scalatest.funsuite.AnyFunSuite


class Day08Test extends AnyFunSuite{

    test("row") {
        val expected: Array[Array[Boolean]] = Array.ofDim[Boolean](6, 50)
        expected(0)(4) = true
        expected(0)(5) = true
        expected(0)(6) = true
        expected(1)(0) = true
        expected(1)(2) = true
        expected(2)(1) = true
        val result: Array[Array[Boolean]] = Array.ofDim[Boolean](6, 50)
        Day08.applyCommand(result, "rect 3x2")
        Day08.applyCommand(result, "rotate row y=0 by 4")
        Day08.applyCommand(result, "rotate column x=1 by 1")
        assert(result(0).sameElements(expected(0)) &&
            result(1).sameElements(expected(1)) &&
            result(2).sameElements(expected(2)) &&
            result(3).sameElements(expected(3)) &&
            result(4).sameElements(expected(4)) &&
            result(5).sameElements(expected(5)))
    }


}
