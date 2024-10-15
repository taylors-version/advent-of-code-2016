import Day10.Bot
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.Seq


class Day10Test extends AnyFunSuite{

    test("process input bins with separate bots") {
        val input: Seq[String] = Seq("value 5 goes to bot 2", "value 3 goes to bot 1")
        val actual = Day10.processInputBins(input)
        val expected = Seq(Bot(2, Seq(5)), Bot(1, Seq(3)))
        assert(actual == expected)
    }

    test("process input bins with duplicat bot") {
        val input: Seq[String] = Seq("value 5 goes to bot 2", "value 3 goes to bot 2")
        val actual = Day10.processInputBins(input)
        val expected = Seq(Bot(2, Seq(5,3)))
        assert(actual == expected)
    }

    test("process bots with new bot") {
        val input: Seq[String] = Seq("bot 1 gives low to bot 202 and high to bot 185",
            "bot 5 gives low to bot 4 and high to bot 3")
        val bot5: Bot = Bot(5, Seq(13, 10))
        val bots = Seq(Bot(99, Seq.empty), Bot(100, Seq.empty))
        val actual = Day10.processBot(bot5, bots, input)
        val expected = bots ++ Seq(Bot(4, Seq(10)), Bot(3, Seq(13)))
        assert(actual == expected)
    }

    test("process bots with existing bot") {
        val input: Seq[String] = Seq("bot 1 gives low to bot 202 and high to bot 185",
            "bot 5 gives low to bot 4 and high to bot 3")
        val bot5: Bot = Bot(1, Seq(13, 10))
        val bots: Seq[Bot] = Seq(Bot(202, Seq(99)), Bot(500, Seq.empty))
        val actual = Day10.processBot(bot5, bots, input)
        val expected = Seq(Bot(500, Seq.empty), Bot(202, Seq(99, 10)), Bot(185, Seq(13)))
        assert(actual == expected)
    }

}
