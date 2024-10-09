import org.scalatest.funsuite.AnyFunSuite


class Day03Test extends AnyFunSuite{

    test("5, 10, 25 = 0") {
        assert(Day03.part1(Seq("5 10 25")) == 0)
    }

    test("16, 10, 25 = 0") {
        assert(Day03.part1(Seq("16 10 25")) == 1)
    }

    test("16,  10, 25 = 0") {
        assert(Day03.part1(Seq("16  10 25")) == 1)
    }

    test("part2") {
        val input = Seq("101 301 501",
          "102 302 502",
          "103 303 503",
          "201 401 601",
          "202 402 602", "203 403 603")
        assert(Day03.part2(input) > 0)
    }

}
