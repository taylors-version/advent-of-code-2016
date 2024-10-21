import org.scalatest.funsuite.AnyFunSuite

class Day16Test extends AnyFunSuite{

    test("'1' becomes '100'") {
        assert(Day16.dragon("1", 2) == "100")
    }

    test("'0' becomes '001'") {
        assert(Day16.dragon("0", 2) == "001")
    }

    test("'11111' becomes '11111000000'") {
        assert(Day16.dragon("11111", 10) == "11111000000")
    }

    test("'111100001010' becomes '1111000010100101011110000'") {
        assert(Day16.dragon("111100001010", 15) == "1111000010100101011110000")
    }

    test("'110010110100' has checksum '100'") {
        assert(Day16.checksum("110010110100") == "100")
    }

    test("'10000', disk size 20 has checksum '01100") {
        assert(Day16.part1("10000", 20) == "01100")
    }

}
