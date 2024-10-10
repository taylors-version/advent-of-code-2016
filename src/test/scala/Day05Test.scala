import org.scalatest.funsuite.AnyFunSuite


class Day05Test extends AnyFunSuite{

    test("abc is 18f47a30") {
        assert(Day05.part1("abc") == "18f47a30")
    }

    test("part2 abc is 3231929") {
        assert(Day05.part2("abc") == "05ace8e3")
    }
}
