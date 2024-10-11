import scala.util.matching.Regex
object Day08:
    private val rectangle: Regex = "rect.*".r
    private val row: Regex = "rotate row.*".r
    private val column: Regex = "rotate column.*".r

    def applyCommand(screen: Array[Array[Boolean]], command: String): Unit = {
        val foo = "bar"
        command match
            case rectangle() =>
                val Array(xLength, yLength) = command.split("\\D+").tail.map(_.toInt)
                for x <- 0 until xLength; y <- 0 until yLength do screen(y)(x) = true
                val ben = "boo"
            case row() =>
                val Array(yValue, length) = command.split("\\D+").tail.map(_.toInt)
                val newRow = Array.ofDim[Boolean](50)
                for x <- 0 until 50 do newRow((x + length) % 50) = screen(yValue)(x)
                screen(yValue) = newRow
                val ben = "b00"
            case column() =>
                val Array(xValue, length) = command.split("\\D+").tail.map(_.toInt)
                val newColumn = Array.ofDim[Boolean](6)
                for y <- 0 until 6 do newColumn((y + length) % 6) = screen(y)(xValue)
                for y <- 0 until 6 do screen(y)(xValue) = newColumn(y)
                val ben = "boo"
    }

    def part1(input: Seq[String]): Int = {
        val screen = Array.fill(6)(Array.fill(50)(false))
        input.foreach(applyCommand(screen, _))
        part2(screen)
        screen.foldLeft(0)((sum, r) => sum + r.count(x => x))
    }

    def part2(screen: Array[Array[Boolean]]): Unit = {
        screen.foreach(r => {
            r.foreach(c => {
                if c then print("#") else print(".")
            })
            println("")
        })
    }

    def main(args: Array[String]): Unit = {
        val data = io.Source.fromResource("Day08.txt").getLines().toSeq
        println(part1(data))
    }