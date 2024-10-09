import org.scalatest.funsuite.AnyFunSuite


class Day02Test extends AnyFunSuite{
    val exampleInput:Seq[String] = "ULL\nRRDDD\nLURDL\nUUUUD".split("\n").toSeq

    test("Example = 1985") {
        assert(Day02.part1(exampleInput) == "1985")
    }

}
