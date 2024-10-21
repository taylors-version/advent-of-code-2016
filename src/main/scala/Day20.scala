import scala.annotation.tailrec
import scala.collection.immutable.Seq

object Day20:
    case class Forbidden(min: Long, max: Long):
        def contains(value: Long): Option[Long] = if value >= min && value <= max then Some(max) else None


    @tailrec
    private def findAllowedIP(forbiddenList: Seq[Forbidden], ip: Long = 0): Long = {
        val lowestForbidden = forbiddenList.head
        val contained: Option[Long] = lowestForbidden.contains(ip)
        contained match {
            case Some(value) => findAllowedIP(forbiddenList.filterNot(f => f.max <= lowestForbidden.max), lowestForbidden.max + 1)
            case None => ip
        }
    }

    def part1(input: Seq[String]): Long = {
        val forbiddenList: Seq[Forbidden] = input.map(l => {
            val numbers = l.split("\\D+")
            Forbidden(numbers.head.toLong, numbers.last.toLong)
        }).sortBy(f => f.min)
        findAllowedIP(forbiddenList)
    }

    def main(args: Array[String]): Unit = {
        val data: Seq[String] = io.Source.fromResource("Day20.txt").getLines().toSeq
        println(part1(data))
    }