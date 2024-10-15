import scala.annotation.tailrec

object Day10:
    case class Bot(id: Int, chips: Seq[Int])

    def processInputBins(input: Seq[String]): Seq[Bot] = {
        val initial = input.filter(_.startsWith("value")).map(line => {
            val digits = line.split("\\D+")
            Bot(digits(2).toInt, Seq(digits(1).toInt))
        })
        initial.collect({
            case bot if initial.count(_.id == bot.id) > 1 =>
                val chips = initial.filter(_.id == bot.id).flatMap(_.chips)
                Bot(bot.id, chips)
            case bot if initial.count(_.id == bot.id) == 1 => bot
        }).distinct
    }

    def processBot(bot: Bot, bots: Seq[Bot], input: Seq[String]): Seq[Bot] = {
        if bots.size < 2 then bots
        else if bot.chips == Seq(61, 17) then Seq(bot)
        else
            val digits = input.find(_.startsWith("bot " + bot.id + " ")).get.split("\\D+")
            val outputs: (Int, Int) = (digits(2).toInt, digits(3).toInt)
            val lowerBot: Bot = bots.find(_.id == outputs(0)).getOrElse(Bot(outputs(0), Seq.empty))
            val higherBot: Bot = bots.find(_.id == outputs(1)).getOrElse(Bot(outputs(1), Seq.empty))
            bots.filterNot(b => b.id == outputs(0) || b.id == outputs(1))
                .appended(lowerBot.copy(chips = lowerBot.chips.appended(bot.chips.min)))
                .appended(higherBot.copy(chips = higherBot.chips.appended(bot.chips.max))).filterNot(_.id == bot.id)
    }

    @tailrec
    private def process(bots: Seq[Bot], input: Seq[String]): Bot = {
        if bots.size == 1 then bots.head
        else process(processBot(bots.filter(_.chips.size == 2).head, bots, input), input)
    }

    def part1(input: Seq[String]): Int = {
        process(processInputBins(input), input).id
    }

    def main(args: Array[String]): Unit = {
        val data: Seq[String] = io.Source.fromResource("Day10.txt").getLines().toSeq
        println(part1(data))
    }