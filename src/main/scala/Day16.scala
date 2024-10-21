import scala.annotation.tailrec

object Day16:

    @tailrec
    def dragon(data: String, minSize: Int): String = {
        if data.length >= minSize then data else
            dragon(data + '0' + data.reverse.map(c =>
            c match
                case '1' => '0'
                case _ => '1'), minSize)
    }

    @tailrec
    def checksum(data: String): String = {
        val newCheck: String = data.sliding(2, 2).map {
            case "00" => "1"
            case "11" => "1"
            case _ => "0"
        }.mkString
        if newCheck.length % 2 == 0 then checksum(newCheck) else newCheck
    }

    def part1(input: String, diskSize: Int): String = {
        checksum(dragon(input, diskSize).take(diskSize))
    }

    def main(args: Array[String]): Unit = {
        println(part1("11100010111110100", 272))
        println(part1("11100010111110100", 35651584))
    }