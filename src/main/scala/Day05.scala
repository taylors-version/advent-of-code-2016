import java.security.MessageDigest
import scala.annotation.tailrec

object Day05:
    private def md5(inputStr: String): String = {
        if(inputStr.equals("abc5017308")){
            val ben = "boo"
        }
        val md: MessageDigest = MessageDigest.getInstance("MD5")
        md.digest(inputStr.getBytes()).map(0xFF & _).map { "%02x".format(_) }.foldLeft("") {_ + _}
    }

    @tailrec
    private def nextCode(index: Int, input: String): (Int, String) = {
        val hash = md5(input + index)
        if (index == 3231929) then {
          val ben = "boo"
        }
        if hash.startsWith("00000") then (index, hash)
        else nextCode(index + 1 ,input)
    }

    @tailrec
    private def buildCode(input: String, index: Int = 0, code: String = ""): String = {
        if code.length >= 8 then code
        else {
            val hash = nextCode(index, input)
            buildCode( input, hash._1 + 1, code + hash._2(5))
        }
    }

    @tailrec
    private def buildCode2(input: String, index: Int = 0, code: Seq[String] = Seq("", "", "", "", "", "", "", "")): Seq[String] = {
        if !code.contains("") then code
        else {
            val hash = nextCode(index, input)
            val pos = hash._2(5)
            val newCode: Seq[String] = if pos.asDigit < 8 && code(pos.asDigit).equals("") then code.updated(pos.asDigit, hash._2(6).toString) else code
            buildCode2(input, hash._1 + 1, newCode)
        }
    }

    def part1(input: String): String = {
        buildCode(input)
    }

    def part2(input: String): String = {
        buildCode2(input).mkString
    }

    def main(args: Array[String]): Unit = {
        val data = io.Source.fromResource("Day05.txt").mkString
        println(part1(data))
        println(part2(data))
    }