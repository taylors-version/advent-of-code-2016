object  Day01 :

    def part1(input: String): Int = 0


    def main(args: Array[String]): Unit = {
        val data = io.Source.fromResource("Day01.txt").mkString.trim
        println(part1(data))
    }
