package basics

import basics.ConsoleHelper.{composeResult, error}

import scala.io.Source

object Main {

  def main(args: Array[String]): Unit =
    Source.stdin.getLines().map { line =>
      val (operation, numbers) = ConsoleHelper.parse(line)
      operation.fold(error("Command not found")) { op =>
        op.calculate(numbers).fold(e => error(e),r => composeResult(op, numbers, r))
      }
    }.foreach(println)

}
