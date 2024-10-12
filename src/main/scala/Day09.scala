import scala.annotation.tailrec

object Day09:

    @tailrec
    def decompressString(input: String, acc: String = ""): String = {
        if input.isEmpty then acc
        else
        input.head match
            case '(' => {
                val digits = input.split("\\D")
                val chars = digits(1).toInt
                val times = digits(2).toInt
                val closeIndex = input.indexOf(')')
                val repeatedString = input.substring(closeIndex + 1, closeIndex + 1 + chars) * times
                decompressString(input.drop(closeIndex + 1 + chars), acc + repeatedString)
            }
            case _ => decompressString(input.tail, acc + s"${input.head}")
    }

    def part1(input: String): Int = {
        decompressString(input).length
    }



    def main(args: Array[String]): Unit = {
        val data = io.Source.fromResource("Day09.txt").mkString.trim
        println(part1(data))
    }