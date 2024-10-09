import org.scalatest.funsuite.AnyFunSuite


class Day04Test extends AnyFunSuite{

    test("Sample input 1") {
        assert(Day04.part1(Seq("aaaaa-bbb-z-y-x-123[abxyz]")) == 123)
    }

    test("Other sample inputs"){
        val input: Seq[String] = Seq("a-b-c-d-e-f-g-h-987[abcde]",
        "not-a-real-room-404[oarel]",
        "totally-real-room-200[decoy]")
        assert(Day04.part1(input) == 1391)
    }

}
