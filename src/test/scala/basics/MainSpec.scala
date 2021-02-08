package basics

import java.io.{ByteArrayInputStream, PrintStream}

import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MainSpec extends AnyWordSpec with Matchers {

  "Main" should {

    "read line commands and execute results" in {
      test("divide 4 5", "4 divided by 5 is 0.8\n")
      test("sum 5 5 6 8.5", "the sum of 5 5 6 8.5 is 24.5\n")
      test("average 4 3 8.5 4", "the average of 4 3 8.5 4 is 4.875\n")
      test("min 4 -3 -17", "the minimum of 4 -3 -17 is -17\n")
      test("max 4 -3 -17", "the maximum of 4 -3 -17 is 4\n")
    }

    "read line commands and handle errors" in {
      test("abc", "Error: Command not found\n")
      test("divide 4", "Error: Not enough numbers for operation, expected 2\n")
      test("divide 4 2 92", "Error: Too many numbers for operation, expected 2\n")
      test("sum ", "Error: There are no any number\n")
    }

    "read line commands and handle very strange input " in {
      test("sum ;sirpeiw[qr qwo[e[rp l fas;ldkf 5 aslkdfj  5 asldjflk  6 aksdf;lajsdf 8.5", "the sum of 5 5 6 8.5 is 24.5\n")
      test("sum ;sirpeiw[qr divide qwo[e[rp l fas;ldkf 5 aslkdfj  5 asldjflk  6 aksdf;lajsdf 8.5", "the sum of 5 5 6 8.5 is 24.5\n")
      test("sum 5                   5 6 8.5", "the sum of 5 5 6 8.5 is 24.5\n")
      test("5 5 Sum 6 8.5", "the sum of 5 5 6 8.5 is 24.5\n")
      test(" dslkjflksaf 5  aslkdjf 5 Sum 6 asdjf 8.5", "the sum of 5 5 6 8.5 is 24.5\n")
    }

  }

  private def test(input: String, expectedOutput: String): Assertion = {
    System.setIn(new ByteArrayInputStream(input.getBytes))
    val stream = new java.io.ByteArrayOutputStream()
    Console.withOut(stream) {
      Main.main(Array.empty)
    }
    val actualOutput = new String(stream.toByteArray)
    actualOutput should equal(expectedOutput)
  }

}
