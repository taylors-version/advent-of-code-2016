object Day04:
    private case class Room(encrypted: String, id: Int, checksum: String)

    private def isRealRoom(room: Room): Boolean = {
        room.encrypted.groupBy(identity).view.mapValues(_.length).toSeq
        .sortBy((char: Char, count: Int) => (count * -1, char))
        .take(5).map(_.head).mkString
        .equals(room.checksum)
    }

    private def decryption(room: Room): String = {
        room.encrypted.map(shift(_, room.id))
    }

    private def shift(char: Char, diff: Int): Char = {
        ((((char - 96) + (diff % 26)) % 26) + 96).toChar
    }

    def part1(input: Seq[String]): Int = {
        input.map(l => {
            val split = l.split("-")
            Room(split.dropRight(1).mkString, split.last.filter(_.isDigit).toInt, split.last.substring(split.last.length - 6, split.last.length-1))
        }).filter(isRealRoom).foldLeft(0)((a,b) => a + b.id)
    }

    def part2(input: Seq[String]): Int = {
        input.map(l => {
            val split = l.split("-")
            Room(split.dropRight(1).mkString, split.last.filter(_.isDigit).toInt, split.last.substring(split.last.length - 6, split.last.length-1))
        }).filter(decryption(_).equalsIgnoreCase("northpoleobjectstorage")).head.id
    }

    def main(args: Array[String]): Unit = {
        val data = io.Source.fromResource("Day04.txt").getLines().toSeq
        println(part1(data))
        println(part2(data))
    }
