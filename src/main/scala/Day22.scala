import scala.collection.immutable.Seq

object Day22:
    private val next = Set((-1, 0), (1, 0), (0, -1), (0, 1))

    case class Node(x: Int, y: Int, used: Int, avail: Int, isGoalData: Boolean = false):
        def canMoveTo(other: Node): Boolean = other.avail >= used


    def parseInput(input: Seq[String]): Seq[Node] = {
        input.drop(2).map {l =>
            val numbers = l.split("\\D+")
            Node(numbers(1).toInt, numbers(2).toInt, numbers(4).toInt, numbers(5).toInt)
        }
    }

    private def printMap(nodes: Seq[Node]): Unit = {
        for (y <- 0 to 27){
            for(x <- 0 to 37) {
                print(nodeChar(nodes.find(n => n.x == x && n.y == y).get))
            }
            println()
        }
    }

    private def nodeChar(node: Node): Char = {
        if node.used == 0 then '_'
        else if node.isGoalData then '*'
        else if node.used > 100 then '#'
        else '.'
    }

    def part1(input: Seq[String]): Int = {
        val nodes = parseInput(input)
        nodes.combinations(2).count(comb => (comb.head.used != 0 && comb.head.used <= comb.last.avail) || (comb.last.used != 0 && comb.last.used <= comb.head.avail))
    }

    def part2(input: Seq[String]): Unit = {
        val nodes = parseInput(input)
        val dataNode = nodes.find(n => n.y == 0 && n.x == 37).get
        val index = nodes.indexOf(dataNode)
        val nodesWithEmpty = nodes.updated(index, dataNode.copy(isGoalData = true))
        printMap(nodesWithEmpty)
        //From printed map 26+26+29+5*36 = 261
    }

    def main(args: Array[String]): Unit = {
        val data: Seq[String] = io.Source.fromResource("Day22.txt").getLines().toSeq
        println(part1(data))
        part2(data)
    }