import scala.annotation.tailrec
import scala.collection.immutable.Seq

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

    def processBotV2(bot: Bot, bots: Seq[Bot], input: Seq[String]): Seq[Bot] = {
        val line = input.find(_.startsWith("bot " + bot.id + " ")).get
        val digits = line.split("\\D+")
        val outputs: (Int, Int) = (digits(2).toInt, digits(3).toInt)
        val lowerBotId: Int = if line.contains("low to output " + outputs(0) + " ") then outputs(0) + 1000 else outputs(0)
        val higherBotId: Int = if line.contains("high to output " + outputs(1) + "\n") then outputs(1) + 1000 else outputs(1)
        val lowerBot: Bot = bots.find(_.id == lowerBotId).getOrElse(Bot(lowerBotId, Seq.empty))
        val higherBot: Bot = bots.find(_.id == higherBotId).getOrElse(Bot(higherBotId, Seq.empty))
        bots.filterNot(b => b.id == lowerBotId || b.id == higherBotId).filterNot(_.id == bot.id)
            .appended(lowerBot.copy(chips = lowerBot.chips.appended(bot.chips.min)))
            .appended(higherBot.copy(chips = higherBot.chips.appended(bot.chips.max)))
    }

    def processBot(bot: Bot, bots: Seq[Bot], input: Seq[String]): Seq[Bot] = {
        if bots.size < 2 then bots
        else if bot.chips == Seq(61, 17) then Seq(bot)
        else
            val digits = input.find(_.startsWith("bot " + bot.id + " ")).get.split("\\D+")
            val outputs: (Int, Int) = (digits(2).toInt, digits(3).toInt)
            val lowerBot: Bot = bots.find(_.id == outputs(0)).getOrElse(Bot(outputs(0), Seq.empty))
            val higherBot: Bot = bots.find(_.id == outputs(1)).getOrElse(Bot(outputs(1), Seq.empty))
            bots.filterNot(b => b.id == outputs(0) || b.id == outputs(1)).filterNot(_.id == bot.id)
                .appended(lowerBot.copy(chips = lowerBot.chips.appended(bot.chips.min)))
                .appended(higherBot.copy(chips = higherBot.chips.appended(bot.chips.max)))
    }

    @tailrec
    private def processV2(bots: Seq[Bot], input: Seq[String]): Seq[Bot] = {
        if bots.count(b => b.id >= 1000 && b.id <=1002) == 3 then bots
        else {
            processV2(processBotV2(bots.filter(_.chips.size == 2).head, bots, input), input)
        }
    }

    @tailrec
    private def process(bots: Seq[Bot], input: Seq[String]): Bot = {
        if bots.size == 1 then bots.head
        else process(processBot(bots.filter(_.chips.size == 2).head, bots, input), input)
    }

    def part1(input: Seq[String]): Int = {
        process(processInputBins(input), input).id
    }
    
    def part2(input: Seq[String]): Int = {
        processV2(processInputBins(input), input).filter(b => b.id >= 1000 && b.id <=1002).foldLeft(1)((a,b) => a * b.chips.head)
    }

    def main(args: Array[String]): Unit = {
        val data: Seq[String] = io.Source.fromResource("Day10.txt").getLines().toSeq
        println(part1(data))
        println(part2(data))
    }