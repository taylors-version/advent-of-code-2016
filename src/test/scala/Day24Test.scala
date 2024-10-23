import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.Seq

class Day24Test extends AnyFunSuite{
    val input = "###########\n#0.1.....2#\n#.#######.#\n#4.......3#\n###########"
    
    test("Example produces 14") {
        val expected = 14
        val actual = Day24.part1(input.split("\\n"))
        assert(expected == actual)
    }
    
}
