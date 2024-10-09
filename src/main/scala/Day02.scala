import scala.annotation.tailrec

object  Day02 :

    private case class XY(x: Int, y: Int)
    private val squareKeypad = Map(
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

    private val diamondKeypad = Map(
        XY(2, 0) -> "1",
        XY(1, 1) -> "2",
        XY(2, 1) -> "3",
        XY(3, 1) -> "4",
        XY(0, 2) -> "5",
        XY(1, 2) -> "6",
        XY(2, 2) -> "7",
        XY(3, 2) -> "8",
        XY(4, 2) -> "9",
        XY(1, 3) -> "A",
        XY(2, 3) -> "B",
        XY(3, 3) -> "C",
        XY(2, 4) -> "D"
    )

    @tailrec
    private def squareDecode(input: String, position: XY = XY(1,1)): XY = {
        if input.length < 1 then position
        else {
            input.head match
                case 'U' => squareDecode(input.tail, position.copy(y = 0.max(position.y - 1)))
                case 'D' => squareDecode(input.tail, position.copy(y = 2.min(position.y + 1)))
                case 'R' => squareDecode(input.tail, position.copy(x = 2.min(position.x + 1)))
                case 'L' => squareDecode(input.tail, position.copy(x = 0.max(position.x - 1)))
        }
    }

    @tailrec
    private def diamondDecode(input: String, position: XY = XY(0, 2)): XY = {
        if input.length < 1 then position
        else {
            val newPosition: XY = input.head match
                case 'U' => if diamondKeypad.contains(position.copy(y = position.y - 1)) then position.copy(y = position.y - 1) else position
                case 'D' => if diamondKeypad.contains(position.copy(y = position.y + 1)) then position.copy(y = position.y + 1) else position
                case 'R' => if diamondKeypad.contains(position.copy(x = position.x + 1)) then position.copy(x = position.x + 1) else position
                case 'L' => if diamondKeypad.contains(position.copy(x = position.x - 1)) then position.copy(x = position.x - 1) else position
                case _ => position.copy()
            diamondDecode(input.tail, newPosition)
        }
    }

    def part1(input: Seq[String]): String = {
        val keys = input.foldLeft(Seq[XY](XY(1,1)))((positions, instruction) => positions :+ squareDecode(instruction, positions.last))
        keys.map(squareKeypad).mkString.drop(1)
    }

    def part2(input: Seq[String]): String = {
        val keys = input.foldLeft(Seq[XY](XY(0, 2)))((positions, instruction) => positions :+ diamondDecode(instruction, positions.last))
        keys.map(diamondKeypad).mkString.drop(1)
    }

    def main(args: Array[String]): Unit = {
        val data = io.Source.fromResource("Day02.txt").getLines().toSeq
        println(part1(data))
        println(part2(data))
    }
