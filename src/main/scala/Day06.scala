import scala.collection.mutable
import scala.collection.mutable.Map
object Day06:

    def part1(input: Seq[String]): String = {
        val charsByColumn: mutable.Map[Int, Seq[Char]] = mutable.Map()
        Range(0,input.head.length).foreach(i => charsByColumn+=(i -> Seq.empty))
        input.foreach(s => s.zipWithIndex.foreach((c,i) => charsByColumn(i) = charsByColumn(i).appended(c)))

        charsByColumn.foldLeft(""){case (s, (i, sequence)) => s + sequence.groupBy(identity).maxBy(_._2.size)._1}
    }

    def part2(input: Seq[String]): String = {
      val charsByColumn: mutable.Map[Int, Seq[Char]] = mutable.Map()
      Range(0, input.head.length).foreach(i => charsByColumn += (i -> Seq.empty))
      input.foreach(s => s.zipWithIndex.foreach((c, i) => charsByColumn(i) = charsByColumn(i).appended(c)))

      charsByColumn.foldLeft("") { case (s, (i, sequence)) => s + sequence.groupBy(identity).minBy(_._2.size)._1 }
    }


    def main(args: Array[String]): Unit = {
        val data = io.Source.fromResource("Day06.txt").getLines().toSeq
        println(part1(data))
        println(part2(data))
    }