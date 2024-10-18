import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.Seq

class Day12Test extends AnyFunSuite{
    
    test("cpy 41 a produces 41 on a") {
        val start: Seq[Int] = Seq(0, 0, 0, 0)
        val expected: Seq[Int] = Seq(41, 0, 0, 0)
        val actual = Day12.runInstruction(start, "cpy 41 a")
        assert(expected == actual)
    }

    test("cpy a b") {
        val start: Seq[Int] = Seq(10, 0, 0, 0)
        val expected: Seq[Int] = Seq(10, 10, 0, 0)
        val actual = Day12.runInstruction(start, "cpy a b")
        assert(expected == actual)
    }

    test("inc c") {
        val start: Seq[Int] = Seq(0, 0, 0, 0)
        val expected: Seq[Int] = Seq(0, 0, 1, 0)
        val actual = Day12.runInstruction(start, "inc c")
        assert(expected == actual)
    }

    test("dec d") {
        val start: Seq[Int] = Seq(0, 0, 0, 9)
        val expected: Seq[Int] = Seq(0, 0, 0, 8)
        val actual = Day12.runInstruction(start, "dec d")
        assert(expected == actual)
    }
    
}
