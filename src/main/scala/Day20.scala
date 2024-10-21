import scala.annotation.tailrec
import scala.collection.immutable.Seq

object Day20:
    private val maxIP: Long = 4294967295L
    
    private case class Forbidden(min: Long, max: Long):
        def contains(value: Long): Option[Long] = if value >= min && value <= max then Some(max) else None


    private def parseInput(input: Seq[String]): Seq[Forbidden] = {
        input.map(l => {
            val numbers = l.split("\\D+")
            Forbidden(numbers.head.toLong, numbers.last.toLong)
        }).sortBy(f => f.min)
    }
    
    @tailrec
    private def findAllowedIP(forbiddenList: Seq[Forbidden], ip: Long = 0): Long = {
        val lowestForbidden = forbiddenList.head
        val contained: Option[Long] = lowestForbidden.contains(ip)
        contained match {
            case Some(value) => findAllowedIP(forbiddenList.filterNot(f => f.max <= lowestForbidden.max), lowestForbidden.max + 1)
            case None => ip
        }
    }

    @tailrec
    private def allowedIPs(forbiddenList: Seq[Forbidden], currentIP: Long = 0, allowed: Seq[Long] = Seq.empty): Seq[Long] = {
        if forbiddenList.isEmpty then allowed ++ (currentIP to maxIP)
        else
            val lowestForbidden = forbiddenList.head
            val contained: Option[Long] = lowestForbidden.contains(currentIP)
            contained match {
                case Some(value) => allowedIPs(forbiddenList.filterNot(f => f.max <= lowestForbidden.max), lowestForbidden.max + 1, allowed)
                case None => allowedIPs(forbiddenList, currentIP + 1, allowed :+ currentIP)
            }
    }

    def part1(input: Seq[String]): Long = {
        findAllowedIP(parseInput(input))
    }
    
    def part2(input: Seq[String]): Long = {
        allowedIPs(parseInput(input)).length
    }

    def main(args: Array[String]): Unit = {
        val data: Seq[String] = io.Source.fromResource("Day20.txt").getLines().toSeq
        println(part1(data))
        println(part2(data))
    }