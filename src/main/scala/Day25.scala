import scala.annotation.tailrec
import scala.collection.immutable.Seq

object Day25:
    private val regMap = Map("a" -> 0, "b" -> 1, "c" -> 2, "d" -> 3, "o" -> 4)
    private val intPattern = """(-?[0-9]+)""".r
    private val cpy = "cpy"
    private val inc = "inc"
    private val dec = "dec"
    private val jnz = "jnz"
    private val out = "out"

    def runInstruction(registers: Seq[Long], instruction: String): Seq[Long] = {
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
            case out if out.startsWith(out) =>
                val parts = out.split(' ')
                registers.updated(4, registers(4) * 10 + registers(regMap(parts(1))))
            case _ => registers
    }

    def value(registers: Seq[Long], input: String): Int = {
        input match
            case intPattern(int) => int.toInt
            case _ => registers(regMap(input)).toInt
    }

    @tailrec
    private def assembunnyRunner(instructions: Seq[String], instructionNo: Int = 0, registers: Seq[Long] = Seq(0,0,0,0L)): Boolean = {
        if registers(4) == 10101010101010101L then true
        else if registers(4) > 10101010101010101L then false
        else
            val instruction = instructions(instructionNo)
            instruction match
                case jump if instruction.startsWith(jnz) =>
                    val parts = jump.split(' ')
                    val offset = if value(registers, parts(1)) == 0 then 1 else value(registers, parts(2))
                    assembunnyRunner(instructions, instructionNo + offset, registers)
                case _ => assembunnyRunner(instructions, instructionNo + 1, runInstruction(registers, instruction))
    }
    
    @tailrec
    private def findInputSignal(input: Seq[String], initial: Int = 1): Int = {
        val registers = Seq(initial.toLong, 0, 0, 0, 0L)
        if assembunnyRunner(input, registers = registers) then initial
        else findInputSignal(input, initial + 1)
    }

    def part1(input: Seq[String]): Int = {
        findInputSignal(input)
    }

    def main(args: Array[String]): Unit = {
        val data: Seq[String] = io.Source.fromResource("Day25.txt").getLines().toSeq
        println(part1(data))
    }