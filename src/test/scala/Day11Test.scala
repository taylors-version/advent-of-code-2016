import org.scalatest.funsuite.AnyFunSuite

class Day11Test extends AnyFunSuite{

    test("Example") {
        val input: Seq[String] = "The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.\nThe second floor contains a hydrogen generator.\nThe third floor contains a lithium generator.\nThe fourth floor contains nothing relevant.".split("\n")
        assert(Day11.part1(input) == 11)
    }
    
    
}
