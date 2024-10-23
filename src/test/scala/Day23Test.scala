import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.Seq

class Day23Test extends AnyFunSuite{
    
    test("Example produces 3") {
        val commands: Seq[String] = "cpy 2 a\ntgl a\ntgl a\ntgl a\ncpy 1 a\ndec a\ndec a".split("\\n")
        val start: Seq[Int] = Seq(0, 0, 0, 0)
        val expected: Int = 3
        val actual = Day23.part1(commands)
        assert(expected == actual)
    }
    
}
