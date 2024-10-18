import scala.annotation.targetName
import scala.collection.immutable.Seq
import scala.collection.mutable

object Day11:
    private val chipPattern = "microchip".r
    private val generatorPattern = "generator".r

    private val moves = Seq(Floor(2, 0), Floor(1, 0), Floor(1, 1), Floor(0, 1), Floor(0, 2))
    private val nextFloors = Map(0 -> Seq(1), 1 -> Seq(0, 2), 2 -> Seq(1, 3), 3 -> Seq(2))

    case class Floor(chips: Int, generators: Int):
        def empty: Boolean = chips == 0 && generators == 0
        def legal: Boolean = (chips >=0 && generators >= 0) && (generators == 0 || chips <= generators)
        @targetName("plus")
        def +(other: Floor): Floor = Floor(chips + other.chips, generators + other.generators)
        @targetName("minus")
        def -(other: Floor): Floor = Floor(chips - other.chips, generators - other.generators)

    case class State(elevator: Int, floors: Seq[Floor]):
        def goal: Boolean = floors.take(3).forall(_.empty)
        def legal: Boolean = floors.forall(_.legal)
        def next: Seq[State] =
            for move <- moves
                floor <- nextFloors(elevator)
                yield State(floor, floors.updated(elevator, floors(elevator) - move).updated(floor, floors(floor) + move))

    private def parseInput(input: Seq[String]): State = {
        State(0, input.map(line => Floor(chipPattern.findAllMatchIn(line).size, generatorPattern.findAllMatchIn(line).size)))
    }


    private def bfs(startState: State): Int = {
        val unvisitedStates = mutable.Queue[State](startState)
        val visitedStates = collection.mutable.Map(startState -> 0)

        while unvisitedStates.nonEmpty do
            val currentState = unvisitedStates.dequeue()
            val count = visitedStates(currentState) + 1
            val next = currentState.next.filter(_.legal)
            next.foreach { n =>
                if!visitedStates.contains(n) || count < visitedStates(n) then
                    visitedStates(n) = count
                    unvisitedStates.enqueue(n)
            }
        var ben = visitedStates
        visitedStates(visitedStates.keys.filter(_.goal).head)
    }

    def part1(input: Seq[String]): Int = {
        val initialState = parseInput(input)
        bfs(initialState)
    }


    def main(args: Array[String]): Unit = {
        val data: Seq[String] = io.Source.fromResource("Day11.txt").getLines().toSeq
        println(part1(data))
    }