import java.security.MessageDigest
import scala.collection.mutable

object Day17:
    private val openDoors = Seq('b', 'c', 'd', 'e', 'f')

    private case class Coord(x: Int, y: Int, passcode: String, path: String):
        def goal: Boolean = x == 3 && y == 3
        def valid: Boolean =
            x >= 0 && y >= 0 && x<=3 && y<=3 && validHash(md5(passcode + path.dropRight(1)), path.takeRight(1))

        def next: Seq[Coord] = if goal then Seq.empty else Seq(Coord(x - 1, y, passcode, path + "L"), Coord(x + 1, y, passcode, path + "R"), Coord(x, y - 1, passcode, path + "U"), Coord(x, y + 1, passcode, path + "D"))
        private def validHash(hash: String, direction: String): Boolean =
            val compareChar = direction match
                case "U" => hash(0)
                case "D" => hash(1)
                case "L" => hash(2)
                case _ => hash(3)
            openDoors.contains(compareChar)

    private def md5(inputStr: String): String = {
        val md: MessageDigest = MessageDigest.getInstance("MD5")
        md.digest(inputStr.getBytes()).map(0xFF & _).map {
            "%02x".format(_)
        }.foldLeft("") {
            _ + _
        }
    }

    private def bfs(start: Coord): Coord = {
        val unvisitedStates = mutable.Queue[Coord](start)
        val visitedStates = collection.mutable.Map(start -> 0)

        while unvisitedStates.nonEmpty do
            val currentState = unvisitedStates.dequeue()
            if (currentState.goal) {
                return currentState
            }
            val count = visitedStates(currentState) + 1
            val next = currentState.next.filter(_.valid)
            next.foreach { n =>
                if!visitedStates.contains(n) || count < visitedStates(n) then
                    visitedStates(n) = count
                    unvisitedStates.enqueue(n)
            }
        start
    }

    private def bfsPart2(start: Coord): Coord = {
        val unvisitedStates = mutable.Queue[Coord](start)
        val visitedStates = collection.mutable.Map(start -> 0)

        while unvisitedStates.nonEmpty do
            val currentState = unvisitedStates.dequeue()
            val count = visitedStates(currentState) + 1
            val next = currentState.next.filter(_.valid)
            next.foreach { n =>
                if !visitedStates.contains(n) || count < visitedStates(n) then
                    visitedStates(n) = count
                    unvisitedStates.enqueue(n)
            }
            
        visitedStates.keys.filter(_.goal).toSeq.sortWith((a, b) => a.path.length.compareTo(b.path.length) < 0).last
    }


    def part1(input: String): String = {
        bfs(Coord(0, 0, input, "")).path
    }

    def part2(input: String): Int = {
        bfsPart2(Coord(0, 0, input, "")).path.length
    }

    def main(args: Array[String]): Unit = {
        println(part1("awrkjxxr"))
        println(part2("awrkjxxr"))
    }