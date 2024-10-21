import scala.annotation.tailrec
import scala.collection.immutable.Seq

object Day19:

    @tailrec
    private def stealPresents(chairs: Seq[Int], beginAtStart: Boolean = true): Int = {
        if chairs.length == 1 then chairs.head else
            val collectionRemainder = if beginAtStart then 1 else 0
            val newChairs = chairs.zipWithIndex.collect{case (x,i) if (i + 1) % 2 == collectionRemainder => x}
            stealPresents(newChairs, chairs.length%2 != 0 ^ beginAtStart)
    }

    @tailrec
    private def stealPresentsOpposite(chairs: Int, i: Int = 1): Int = {
        if i * 3 >= chairs then chairs -i else
        stealPresentsOpposite(chairs, i*3)
    }

    def part1(input: Int): Int = {
        stealPresents(1 to input)
    }

    def part2(input: Int): Int = {
        stealPresentsOpposite(input)
    }

    def main(args: Array[String]): Unit = {
        println(part1(3012210))
        println(part2(3012210))
    }