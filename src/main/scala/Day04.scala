object Day04:
    case class Room(encrypted: String, id: Int, checksum: String)

    private def isRealRoom(room: Room): Boolean = {
        room.encrypted.groupBy(identity).view.mapValues(_.length).toSeq
        .sortBy((char: Char, count: Int) => (count * -1, char))
        .take(5).map(_.head).mkString
        .equals(room.checksum)
    }

    def part1(input: Seq[String]): Int = {
        input.map(l => {
            val split = l.split("-")
            Room(split.dropRight(1).mkString, split.last.filter(_.isDigit).toInt, split.last.substring(split.last.length - 6, split.last.length-1))
        }).filter(isRealRoom).foldLeft(0)((a,b) => a + b.id)
    }

    def main(args: Array[String]): Unit = {
        val data = io.Source.fromResource("Day04.txt").getLines().toSeq
        println(part1(data))
    }
