import org.scalatest.funsuite.AnyFunSuite

class Day14Test extends AnyFunSuite{
    
    test("abc18 is triple") {
        assert(Day14.containsTriple(Day14.getmd5("abc18")).nonEmpty)
    }
    
    test("abc18 is not a key"){
        assert(!Day14.isKey("abc", 18))
    }
    
    test("abc816 is a f"){
        assert(Day14.containsFive(Day14.getmd5("abc816"), 'e'))
    }
    
    test("abc39 is a key"){
        assert(Day14.isKey("abc", 39))
    }
    
    test("Example = 22728") {
        assert(Day14.part1("abc") == 22728)
    }

}
