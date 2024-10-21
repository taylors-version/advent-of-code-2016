import scala.collection.immutable.Seq

object Day15:

    case class Congruence(a: BigInt, m: BigInt)

    def parseInput(input: Seq[String]): Seq[Congruence] = {
        input.map(s => {
            val digits = s.split("\\D+")
            val modulo = digits(2).toInt
            val remainder = BigInt(modulo - (digits(1).toInt + digits(4).toInt)).mod(modulo)
            Congruence(remainder, modulo)
        })
    }

    private def chineseRemainder(congruences: Seq[Congruence]): BigInt = {
        val modulo = congruences.map(_.m).product
        congruences.foldLeft(BigInt(0))((sum, congruence) =>
            val ni: BigInt = (modulo / congruence.m).modInverse(congruence.m)
            (sum + (congruence.a * (modulo / congruence.m)).mod(modulo)*ni).mod(modulo)
        )

    }

    def part1(input: Seq[String]): BigInt = {
        chineseRemainder(parseInput(input))
    }

    def main(args: Array[String]): Unit = {
        val data: Seq[String] = io.Source.fromResource("Day15.txt").getLines().toSeq
        println(part1(data))
    }