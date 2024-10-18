import Day13.Coord
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.Seq

class Day13Test extends AnyFunSuite{
    
    test("Example = 11") {
        assert(Day13.part1(10, Coord(7,4)) == 11)
    }

}
