import org.scalatest.funsuite.AnyFunSuite


class Day07Test extends AnyFunSuite{

    test("abba[mnop]qrst is TLS") {
        assert(Day07.isTLS("abba[mnop]qrst"))
    }

    test("abcd[bddb]xyyx is not TLS") {
        assert(!Day07.isTLS("abcd[bddb]xyyx"))
    }

    test("aaaa[qwer]tyui is not TLS") {
        assert(!Day07.isTLS("aaaa[qwer]tyui"))
    }

    test("ioxxoj[asdfgh]zxcvbn is TLS") {
        assert(Day07.isTLS("ioxxoj[asdfgh]zxcvbn"))
    }

    test("aba[bab]xyz is SSL") {
        assert(Day07.isSSL("aba[bab]xyz"))
    }

    test("xyx[xyx]xyx is not SSL") {
        assert(!Day07.isSSL("xyx[xyx]xyx"))
    }

    test("aaa[kek]eke is SSL") {
        assert(Day07.isSSL("aaa[kek]eke"))
    }

    test("zazbz[bzb]cdb is SSL") {
        assert(Day07.isSSL("zazbz[bzb]cdb"))
    }

}
