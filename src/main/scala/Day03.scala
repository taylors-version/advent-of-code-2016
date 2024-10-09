object Day03:

    def isTriangle(input: String): Boolean = {
        val values: Seq[Int] = input.trim.split("\\s+").toSeq.map(_.toInt).sorted
        values(0) + values(1) > values(2)
    }

    def part1(input: Seq[String]): Int = {
        input.count(isTriangle)
    }


    def main(args: Array[String]): Unit = {
        val data = io.Source.fromResource("Day03.txt").getLines().toSeq
        println(part1(data))
    }
