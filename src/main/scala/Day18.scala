import scala.annotation.tailrec

object Day18:

    @tailrec
    def determineTraps(input: Seq[String], rows: Int): Seq[String] = {
        if input.length >= rows then input else
            val newRow = ("." + input.last + ".").sliding(3).toSeq.map(slide =>
                if (slide.head == '^' && slide.last == '.') || (slide.head == '.' && slide.last == '^')  then '^' else '.'
            ).mkString
            determineTraps(input :+ newRow, rows)
    }

    def part1(input: String, rows: Int): Long = {
        determineTraps(Seq(input), rows).foldLeft(0L)((a, b) => a + b.count(_ == '.'))
    }

    def main(args: Array[String]): Unit = {
        val data: String = io.Source.fromResource("Day18.txt").mkString
        println(part1(data, 40))
        println(part1(data, 400000))// Yea I was lazy
    }