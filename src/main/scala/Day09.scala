import scala.annotation.tailrec
import scala.util.matching.Regex

object Day09:
    private val markerRegEx = "\\(\\d+x\\d+\\)".r

    @tailrec
    def decompressString(input: String, acc: String = ""): String = {
        if input.isEmpty then acc
        else
            input.head match
                case '(' =>
                    val digits = input.split("\\D")
                    val chars = digits(1).toInt
                    val times = digits(2).toInt
                    val closeIndex = input.indexOf(')')
                    val repeatedString = input.substring(closeIndex + 1, closeIndex + 1 + chars) * times
                    decompressString(input.drop(closeIndex + 1 + chars), acc + repeatedString)
                case _ => decompressString(input.tail, acc + s"${input.head}")
    }
    
    def decompressV2String(input: String, acc: Long = 0): Long = {
        if input.isEmpty then acc
        else
            val firstMarker = markerRegEx.findFirstMatchIn(input)
            if firstMarker.isEmpty then input.length
            else
                val digits = firstMarker.get.group(0).split("\\D+")
                val chars = digits(1).toInt
                val times = digits(2).toInt
                val start = input.indexOf('(')
                val end = input.indexOf(')') + 1
                val firstSub = input.substring(0, start)
                val secondSub = input.substring(end, end + chars)
                val thirdSub = input.substring(end + chars)
                input.substring(0, start).length + times * decompressV2String(secondSub) + decompressV2String(thirdSub)
    }

    def part1(input: String): Int = {
        decompressString(input).length
    }

    def part2(input: String): Long = {
        decompressV2String(input)
    }

    def main(args: Array[String]): Unit = {
        val data = io.Source.fromResource("Day09.txt").mkString.trim
        println(part1(data))
        println(part2(data))
    }