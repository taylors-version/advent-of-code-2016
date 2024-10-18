import java.security.MessageDigest
import scala.annotation.tailrec
import scala.collection.mutable

object Day14:
    private val hashes = collection.mutable.HashMap[String, String]()

    private def md5(inputStr: String): String = {
        val md: MessageDigest = MessageDigest.getInstance("MD5")
        md.digest(inputStr.getBytes()).map(0xFF & _).map {
            "%02x".format(_)
        }.foldLeft("") {
            _ + _
        }
    }

    def getmd5(inputStr: String): String = {
        hashes.getOrElseUpdate(inputStr, md5(inputStr))
    }

    def containsTriple(hash: String): Option[String] = hash.sliding(3).find(s => s(0) == s(1) && s(0) == s(2))
    def containsFive(hash: String, char: Char): Boolean = hash.contains(char.toString * 5)

    def isKey(prefix: String, suffix: Int): Boolean = {
        val hash = getmd5(prefix + suffix.toString)
        containsTriple(hash) match
            case None => false
            case Some(s) => (for i <- 1 to 1000 yield containsFive(getmd5(prefix + (suffix + i).toString), s.head)).foldLeft(false)((a,b) => a || b)
    }

    @tailrec
    private def findKeys(prefix: String, suffix: Int, keyCount: Int = 0, counter: Int = 0): Int = {
        if keyCount == 64 then counter - 1 else
            val newKeyCount = if isKey(prefix, suffix) then keyCount + 1 else keyCount
            findKeys(prefix, suffix + 1, newKeyCount, counter + 1)
    }

    def part1(input: String): Int = {
        findKeys(input, 0)
    }
    
    def main(args: Array[String]): Unit = {
        println(part1("ngcjuoqr"))
    }