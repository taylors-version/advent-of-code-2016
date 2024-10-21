import org.scalatest.funsuite.AnyFunSuite

class Day19Test extends AnyFunSuite{

    test("5 chairs means 3 wins") {
        assert(Day19.part1(5) == 3)
    }

    test("5 chairs mark 2 means 2 wins") {
        assert(Day19.part2(5) == 2)
    }

    test("6 chairs mark 2 means 3 wins") {
        assert(Day19.part2(6) == 3)
    }

}
