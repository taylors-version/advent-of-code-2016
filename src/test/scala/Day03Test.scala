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

}
