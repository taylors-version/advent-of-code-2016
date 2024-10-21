import Day15.Congruence
import org.scalatest.funsuite.AnyFunSuite

class Day15Test extends AnyFunSuite{
    
    val testInput = "Disc #1 has 5 positions; at time=0, it is at position 4.\nDisc #2 has 2 positions; at time=0, it is at position 1."

    test("test input parsing") {
        val input = testInput.split("\n")
        val expected: Seq[Congruence] = Seq(
            Congruence(0, 5),
            Congruence(1, 2)
        )
        assert(Day15.parseInput(input) == expected)
    }
    
    test("test sample"){
        val input = testInput.split("\n")
        assert(Day15.part1(input) == 5)
    }

}
