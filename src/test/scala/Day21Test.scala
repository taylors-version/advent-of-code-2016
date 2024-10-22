import org.scalatest.funsuite.AnyFunSuite

class Day21Test extends AnyFunSuite{

    test("'abcde' swap position 4 with position 0 gives 'ebcda'") {
        val input: Seq[String] = Seq("swap position 0 with position 4")
        val expected = "ebcda"
        val actual = Day21.part1("abcde", input)
        assert(actual == expected)
    }

    test("'ebcda' swap letter d with letter b gives 'edcba'") {
        val input: Seq[String] = Seq("swap letter d with letter b")
        val expected = "edcba"
        val actual = Day21.part1("ebcda", input)
        assert(actual == expected)
    }

    test("'edcba' reverse positions 0 through 4 'abcde'") {
        val input: Seq[String] = Seq("reverse positions 0 through 4")
        val expected = "abcde"
        val actual = Day21.part1("edcba", input)
        assert(actual == expected)
    }

    test("'abcde' rotate left 1 step 'bcdea'") {
        val input: Seq[String] = Seq("rotate left 1 step")
        val expected = "bcdea"
        val actual = Day21.part1("abcde", input)
        assert(actual == expected)
    }

    test("'abcde' rotate right 1 step 'eabcd'") {
        val input: Seq[String] = Seq("rotate right 1 step")
        val expected = "eabcd"
        val actual = Day21.part1("abcde", input)
        assert(actual == expected)
    }

    test("'bcdea' move position 1 to position 4 'bdeac'") {
        val input: Seq[String] = Seq("move position 1 to position 4")
        val expected = "bdeac"
        val actual = Day21.part1("bcdea", input)
        assert(actual == expected)
    }

    test("'bdeac' move position 3 to position 0 'abdec'") {
        val input: Seq[String] = Seq("move position 3 to position 0")
        val expected = "abdec"
        val actual = Day21.part1("bdeac", input)
        assert(actual == expected)
    }

    test("'abdec' rotate based on position of letter b 'ecabd'") {
        val input: Seq[String] = Seq("rotate based on position of letter b")
        val expected = "ecabd"
        val actual = Day21.part1("abdec", input)
        assert(actual == expected)
    }

    test("'ecabd' rotate based on position of letter d 'decab'") {
        val input: Seq[String] = Seq("rotate based on position of letter d")
        val expected = "decab"
        val actual = Day21.part1("ecabd", input)
        assert(actual == expected)
    }
    
    test("Example") {
        val input: Seq[String] = Seq(
            "swap position 4 with position 0",
            "swap letter d with letter b",
            "reverse positions 0 through 4",
            "rotate left 1 step", 
            "move position 1 to position 4",
            "move position 3 to position 0",
            "rotate based on position of letter b",
            "rotate based on position of letter d")
        val expected = "decab"
        val actual = Day21.part1("abcde", input)
        assert(actual == expected)
    }

}
