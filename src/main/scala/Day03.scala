object Day03:
    case class Bob(division: Int, modulos: Int)

    def isTriangle(input: String): Boolean = {
        val values: Seq[Int] = input.trim.split("\\s+").toSeq.map(_.toInt).sorted
        values(0) + values(1) > values(2)
    }

    def part1(input: Seq[String]): Int = {
        input.count(isTriangle)
    }

    def part2(input: Seq[String]): Int = {
        val flatInput: Seq[String] = input.flatMap(_.trim.split("\\s+").toSeq)
        val verticalInput = flatInput.zipWithIndex.groupBy((s, i) => Bob(i/9, i%3)).values.map(t => t.map(m => m._1)).map(t => t.toSeq.mkString(" ")).toSeq
        verticalInput.count(isTriangle)
    }

    def main(args: Array[String]): Unit = {
        val data = io.Source.fromResource("Day03.txt").getLines().toSeq
        println(part1(data))
        println(part2(data))
    }
