import org.scalatest.funsuite.AnyFunSuite


class Day01Test extends AnyFunSuite{

    test("Example R1 = 1") {
        assert(Day01.part1("R1") == 1)
    }

    test("Example R2 = 2") {
        assert(Day01.part1("R2") == 2)
    }

    test("Example 1 = 5") {
        assert(Day01.part1("R2, L3") == 5)
    }

    test("Example 2 = 2") {
        assert(Day01.part1("R2, R2, R2") == 2)
    }

    test("Example 3 = 12") {
        assert(Day01.part1("R5, L5, R5, R3") == 12)
    }

    test("part 2 example = 4") {
        assert(Day01.part2("R8, R4, R4, R8") == 4)
    }

}
