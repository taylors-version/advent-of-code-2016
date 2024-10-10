import org.scalatest.funsuite.AnyFunSuite


class Day06Test extends AnyFunSuite{
    val sampleInput: Seq[String] = "eedadn\ndrvtee\neandsr\nraavrd\natevrs\ntsrnev\nsdttsa\nrasrtv\nnssdts\nntnada\nsvetve\ntesnvt\nvntsnd\nvrdear\ndvrsen\nenarar".split("\n")

    test("abc is 18f47a30") {
        assert(Day06.part1(sampleInput) == "easter")
    }

}
