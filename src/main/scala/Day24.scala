import scala.collection.immutable.Seq
import scala.collection.mutable

object Day24:
    case class Coord(x: Int, y: Int){
        def next: Seq[Coord] = Seq(Coord(x+1, y), Coord(x-1, y), Coord(x, y+1), Coord(x, y-1))
    }

    case class State(maze: Array[Array[Char]], position: Coord, charsCollected: Set[Char], steps: Int = 0) extends Ordered[State] {
        override def compare(that: State): Int = that.steps - this.steps
        override def equals(other: Any): Boolean = other match {
            case state: State =>
                this.position == state.position && this.charsCollected == state.charsCollected
            case _ => false
        }
        override def hashCode(): Int = (position.hashCode + charsCollected.hashCode()).hashCode()

        def next: Seq[State] = {
            position.next.filterNot(n => maze(n.x)(n.y) == '#').map(n =>
                State(maze, n, charsCollected + maze(n.x)(n.y), steps+1)
            )
        }
    }

    def parseInput(input: Seq[String]):Array[Array[Char]] = {
        val maze: Array[Array[Char]] = Array.fill(input.head.length)(Array.fill(input.length)('.'))
        input.zipWithIndex.foreach{(line,y) =>
            line.zipWithIndex.foreach((c, x) => maze(x)(y) = c)
        }
        maze
    }

    private def bfs(startState: State, digitCount: Int): Int = {
        val unvisitedStates = mutable.Queue[State](startState)
        val visitedStates = collection.mutable.Map(startState -> 0)

        while unvisitedStates.nonEmpty do
            val currentState = unvisitedStates.dequeue()
            val count = visitedStates(currentState) + 1
            val next = currentState.next
            next.foreach { n =>
                if !visitedStates.contains(n) || count < visitedStates(n) then
                    visitedStates(n) = count
                    unvisitedStates.enqueue(n)
            }
        visitedStates(visitedStates.keys.filter(_.charsCollected.size == digitCount).toSeq.minBy(_.steps))
    }

    def part1(input: Seq[String]): Int = {
        val maze = parseInput(input)
        val targetSize = input.foldLeft(0)((a,b) => a + b.count(c => c.isDigit)) + 1 //Adding the '.' to this to make life easier
        val startPosition = for {
            (x, i) <- maze.zipWithIndex
            (y, j) <- x.zipWithIndex
            if maze(i)(j)=='0'
        } yield Coord(i,j)
        val startState = State(maze, startPosition.head, Set('.', '0'))
        bfs(startState, targetSize)
    }
    def main(args: Array[String]): Unit = {
        val data: Seq[String] = io.Source.fromResource("Day24.txt").getLines().toSeq
        println(part1(data))
    }