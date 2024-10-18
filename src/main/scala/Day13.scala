import scala.collection.mutable

object Day13:

    case class Coord(x: Int, y: Int):
        def valid(number: Int): Boolean =
            x >= 0 && y >= 0 &&
            ((x*x + 3*x + 2*x*y + y + y*y + number).toBinaryString.count(_ == '1') % 2 == 0)
        def next: Seq[Coord] = Seq(Coord(x-1, y), Coord(x+1, y), Coord(x, y-1), Coord (x,y+1))

    case class State(coord: Coord, steps: Int) extends Ordered[State] {
        override def compare(that: State): Int = that.steps - this.steps
        override def equals(other: Any): Boolean = other match {
            case state: State =>
                this.coord == state.coord
            case _ => false
        }

        override def hashCode(): Int = coord.hashCode
        def valid(number: Int): Boolean = coord.valid(number)
        def next: Seq[State] = coord.next.map(State(_, steps + 1))
    }


    private def dijkstra(start: State, target: Coord, number: Int): State = {
        val unvisitedStates = mutable.PriorityQueue[State](start)
        val visitedStates = mutable.HashSet[State]()
        
        while (unvisitedStates.nonEmpty) {
            val currentState = unvisitedStates.dequeue()
            visitedStates.add(currentState)
            if (currentState.coord == target) {
                return currentState
            }
            else
                currentState.next.filter(_.valid(number)).filterNot(visitedStates.contains).foreach(s => {
                    unvisitedStates.enqueue(s)
                })
        }
        start.copy(steps = Int.MaxValue)
    }

    def part1(input: Int, target: Coord): Int = dijkstra(State(Coord(1,1), 0), target, input).steps



    def main(args: Array[String]): Unit = {
        println(part1(1358, Coord(31,39)))
    }