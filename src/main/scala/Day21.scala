import scala.annotation.tailrec
import scala.collection.immutable.Seq

object Day21:
    @tailrec
    private def scramble(password: String, commands: Seq[String]): String = {
        if commands.isEmpty then password
        else
            val newPassword = commands.head match
                case s"swap position $firstPos with position $lastPos" =>
                    password.updated(firstPos.toInt, password(lastPos.toInt)).updated(lastPos.toInt, password(firstPos.toInt))
                case s"swap letter $firstLetter with letter $lastLetter" =>
                    password.updated(password.indexOf(firstLetter), lastLetter.head).updated(password.indexOf(lastLetter), firstLetter.head)
                case s"reverse positions $firstPos through $lastPos" =>
                    val bitToReverse = password.subSequence(firstPos.toInt, lastPos.toInt + 1)
                    password.replace(bitToReverse, bitToReverse.toString.reverse)
                case s"rotate left $steps ste$ps" =>
                    password.drop(steps.toInt) + password.take(steps.toInt)
                case s"rotate right $steps ste$ps" =>
                    password.takeRight(steps.toInt) + password.dropRight(steps.toInt)
                case s"move position $posFrom to position $posTo" =>
                    (password.take(posFrom.toInt) + password.drop(posFrom.toInt + 1)).patch(posTo.toInt, password.charAt(posFrom.toInt).toString, 0)
                case s"rotate based on position of letter $letter" =>
                    val index = password.indexOf(letter)
                    val rotate = (if index >= 4 then index + 2 else index + 1) % password.length
                    password.takeRight(rotate) + password.dropRight(rotate)
                case _ => ""
            scramble(newPassword, commands.tail)
    }

    @tailrec
    private def unscramble(permutations: Seq[String], password: String, input: Seq[String]): String = {
        if scramble(permutations.head, input) == password then permutations.head else unscramble(permutations.tail, password, input)
    }

    def part1(initial: String, input: Seq[String]): String = {
        scramble(initial, input)
    }

    def part2(password: String, input: Seq[String]): String = {
        unscramble("abcdefgh".permutations.toSeq, password, input) //CPU go brr
    }

    def main(args: Array[String]): Unit = {
        val data: Seq[String] = io.Source.fromResource("Day21.txt").getLines().toSeq
        println(part1("abcdefgh", data))
        println(part2("fbgdceah", data))
    }