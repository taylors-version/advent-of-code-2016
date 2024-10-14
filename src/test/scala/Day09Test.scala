import org.scalatest.funsuite.AnyFunSuite


class Day09Test extends AnyFunSuite{

    test("ADVENT stays the same") {
        val actual = Day09.decompressString("ADVENT")
        assert(actual.equals("ADVENT"))
    }

    test("'A(1x5)BC' becomes 'ABBBBBC'") {
        val actual = Day09.decompressString("A(1x5)BC")
        assert(actual.equals("ABBBBBC"))
    }

    test("'(3x3)XYZ' becomes 'ABBBBBC'") {
        val actual = Day09.decompressString("(3x3)XYZ")
        assert(actual.equals("XYZXYZXYZ"))
    }

    test("'A(2x2)BCD(2x2)EFG' becomes 'ABCBCDEFEFG'") {
        val actual = Day09.decompressString("A(2x2)BCD(2x2)EFG")
        assert(actual.equals("ABCBCDEFEFG"))
    }

    test("'(6x1)(1x3)A' becomes '(1x3)A'") {
        val actual = Day09.decompressString("(6x1)(1x3)A")
        assert(actual.equals("(1x3)A"))
    }

    test("'X(8x2)(3x3)ABCY' becomes 'X(3x3)ABC(3x3)ABCY'") {
        val actual = Day09.decompressString("X(8x2)(3x3)ABCY")
        assert(actual.equals("X(3x3)ABC(3x3)ABCY"))
    }

    test("'(3x3)XYZ' has length 9") {
        assert(Day09.decompressV2String("(3x3)XYZ") == "XYZXYZXYZ".length)
    }

    test("'X(8x2)(3x3)ABCY' has length 20") {
        assert(Day09.decompressV2String("X(8x2)(3x3)ABCY") == "XABCABCABCABCABCABCY".length)
    }

}
