import org.scalatest.funsuite.AnyFunSuite

class Day14Test extends AnyFunSuite{

    test("abc18 is triple") {
        assert(Day14.containsTriple(Day14.getmd5("abc18")).nonEmpty)
    }

    test("abc18 is not a key"){
        assert(!Day14.isKey("abc", 18, Day14.getmd5))
    }

    test("abc816 is a f"){
        assert(Day14.containsFive(Day14.getmd5("abc816"), 'e'))
    }

    test("abc39 is a key"){
        assert(Day14.isKey("abc", 39, Day14.getmd5))
    }

    test("Example = 22728") {
        assert(Day14.part1("abc") == 22728)
    }

    test("stretched Hash of abc0 is a107ff634856bb300138cac6568c0f24"){
        assert(Day14.getStretchedmd5("abc0").equals("a107ff634856bb300138cac6568c0f24"))
    }

    test("Example Stretched = 22551") {
        assert(Day14.part2("abc") == 22551)
    }
    

}
