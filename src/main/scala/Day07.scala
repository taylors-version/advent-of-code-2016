object Day07:

    def isTLS(input: String): Boolean = {
        val inputSplit = input.split(Array('[', ']'))
        val supernet = inputSplit.zipWithIndex.filter((s, i) => i%2 == 0).map((s, i) => s)
        val hypernet = inputSplit.diff(supernet)

        supernet.count(isABBA) > 0 && hypernet.count(isABBA) == 0
    }

    def isSSL(input: String): Boolean = {
        val inputSplit = input.split(Array('[', ']'))
        val supernet = inputSplit.zipWithIndex.filter((s, i) => i % 2 == 0).map((s, i) => s).toSeq
        val hypernet = inputSplit.diff(supernet).toSeq

        val supernetABAs: Seq[String] = supernet.flatMap(ABAs)
        val hypernetABAs: Seq[String] = hypernet.flatMap(ABAs).map(s => "" + s(1) + s(0) + s(1))

        supernetABAs.diff(hypernetABAs).size < supernetABAs.size
    }

    def isABBA(input: String): Boolean = {
        val slide: Seq[String] = input.sliding(4).toSeq
        slide.exists(s => s(0).equals(s(3)) && s(1).equals(s(2)) && !s(0).equals(s(1)))
    }

    def ABAs(input: String): Seq[String] = input.sliding(3).toSeq.filter(s => s(0).equals(s(2)) && !s(0).equals(s(1)))

    def part1(input: Seq[String]): Int = {
        input.count(isTLS)
    }

    def part2(input: Seq[String]): Int = {
        input.count(isSSL)
    }

    def main(args: Array[String]): Unit = {
        val data = io.Source.fromResource("Day07.txt").getLines().toSeq
        println(part1(data))
        println(part2(data))
    }