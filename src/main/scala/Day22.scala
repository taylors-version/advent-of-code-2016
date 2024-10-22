import scala.collection.immutable.Seq

object Day22:

    case class Node(x: Int, y: Int, used: Int, avail: Int)

    def parseInput(input: Seq[String]): Seq[Node] = {
        input.drop(2).map {l =>
            val numbers = l.split("\\D+")
            Node(numbers(1).toInt, numbers(2).toInt, numbers(4).toInt, numbers(5).toInt)
        }
    }

    def part1(input: Seq[String]): Int = {
        val nodes = parseInput(input)
        nodes.combinations(2).count(comb => (comb.head.used != 0 && comb.head.used <= comb.last.avail) || (comb.last.used != 0 && comb.last.used <= comb.head.avail))
    }

    def main(args: Array[String]): Unit = {
        val data: Seq[String] = io.Source.fromResource("Day22.txt").getLines().toSeq
        println(part1(data))
    }