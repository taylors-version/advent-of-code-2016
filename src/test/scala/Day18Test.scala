import org.scalatest.funsuite.AnyFunSuite

class Day18Test extends AnyFunSuite{

    test("'..^^.' for 3 rows = 6 safe tiles") {
        assert(Day18.part1("..^^.", 3) == 6)
    }

    test(".^^.^.^^^^' for 10 rows = 38 safe tiles") {
        assert(Day18.part1(".^^.^.^^^^", 10) == 38)
    }
}
