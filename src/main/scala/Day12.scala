import scala.annotation.tailrec
import scala.collection.immutable.Seq

object Day12:
    private val regMap = Map("a" -> 0, "b" -> 1, "c" -> 2, "d" -> 3)
    private val intPattern = """(-?[0-9]+)""".r
    private val cpy = "cpy"
    private val inc = "inc"
    private val dec = "dec"
    private val jnz = "jnz"

    def runInstruction(registers: Seq[Int], instruction: String): Seq[Int] = {
        instruction match
            case copy if copy.startsWith(cpy) =>
                val parts = copy.split(' ')
                registers.updated(regMap(parts(2)), value(registers, parts(1)))
            case incr if incr.startsWith(inc) =>
                val parts = incr.split(' ')
                registers.updated(regMap(parts(1)), registers(regMap(parts(1))) + 1)
            case decr if decr.startsWith(dec) =>
                val parts = decr.split(' ')
                registers.updated(regMap(parts(1)), registers(regMap(parts(1))) - 1)
            case _ => registers
    }

    def value(registers: Seq[Int], input: String): Int = {
        input match
            case intPattern(int) => int.toInt
            case _ => registers(regMap(input))
    }

    @tailrec
    private def assembunnyRunner(instructions: Seq[String], instructionNo: Int = 0, registers: Seq[Int] = Seq(0,0,0,0)): Seq[Int] = {
        if instructionNo >= instructions.size then registers
        else
            val instruction = instructions(instructionNo)
            instruction match
                case jump if instruction.startsWith(jnz) =>
                    val parts = jump.split(' ')
                    val offset = if value(registers, parts(1)) == 0 then 1 else value(registers, parts(2))
                    assembunnyRunner(instructions, instructionNo + offset, registers)
                case _ => assembunnyRunner(instructions, instructionNo + 1, runInstruction(registers, instruction))
    }

    def part1(input: Seq[String]): Int = {
        assembunnyRunner(input).head
    }

    def part2(input: Seq[String]): Int = {
        assembunnyRunner(input, 0, Seq(0,0,1,0)).head
    }


    def main(args: Array[String]): Unit = {
        val data: Seq[String] = io.Source.fromResource("Day12.txt").getLines().toSeq
        println(part1(data))
        println(part2(data))
    }