import org.scalatest.funsuite.AnyFunSuite

class Day19Test extends AnyFunSuite{

    test("5 chairs means 3 wins") {
        assert(Day19.part1(5) == 3)
    }

}
