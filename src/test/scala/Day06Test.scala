import org.scalatest.funsuite.AnyFunSuite


class Day06Test extends AnyFunSuite{
    val sampleInput: Seq[String] = "eedadn\ndrvtee\neandsr\nraavrd\natevrs\ntsrnev\nsdttsa\nrasrtv\nnssdts\nntnada\nsvetve\ntesnvt\nvntsnd\nvrdear\ndvrsen\nenarar".split("\n")

    test("sample is easter") {
        assert(Day06.part1(sampleInput) == "easter")
    }

    test("sample part 2 is advent") {
        assert(Day06.part2(sampleInput) == "advent")
    }

}
