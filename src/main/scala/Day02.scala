import scala.annotation.tailrec

object  Day02 :

    case class XY(x: Int, y: Int)
    private val keypad = Map(
        XY(0,0) -> "1",
        XY(1,0) -> "2",
        XY(2,0) -> "3",
        XY(0,1) -> "4",
        XY(1,1) -> "5",
        XY(2,1) -> "6",
        XY(0,2) -> "7",
        XY(1,2) -> "8",
        XY(2,2) -> "9"
    )

    @tailrec
    private def decode(input: String, position: XY = XY(1,1)): XY = {
        if input.length < 1 then position
        else {
            input.head match
                case 'U' => decode(input.tail, position.copy(y = 0.max(position.y - 1)))
                case 'D' => decode(input.tail, position.copy(y = 2.min(position.y + 1)))
                case 'R' => decode(input.tail, position.copy(x = 2.min(position.x + 1)))
                case 'L' => decode(input.tail, position.copy(x = 0.max(position.x - 1)))
        }
    }

    def part1(input: Seq[String]): String = {
        val keys = input.foldLeft(Seq[XY](XY(1,1)))((positions, instruction) => positions :+ decode(instruction, positions.last))
        keys.map(keypad).mkString.drop(1)
    }

    def main(args: Array[String]): Unit = {
        val data = io.Source.fromResource("Day02.txt").getLines().toSeq
        println(part1(data))
    }
