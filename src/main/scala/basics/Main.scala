package basics

import basics.ConsoleHelper.{composeResult, error}

import scala.io.Source

object Main {

  def main(args: Array[String]): Unit =
    Source.stdin.getLines().map { line =>
      val (operation, numbers) = ConsoleHelper.parse(line)
      operation match {
        case Some(op) =>
          op.calculate(numbers) match {
            case Left(e) => error(e)
            case Right(answer) => composeResult(op, numbers, answer)
          }
        case None => error("Command not found")
      }
    }.foreach{l => println(l)}

}
