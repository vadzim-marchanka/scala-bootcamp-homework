package basics

import basics.Operations.{Average, Divide, Max, Min, Operation, Sum}

object ConsoleHelper {

  private val supportedOperations: Seq[Operation] = Seq(Divide, Sum, Min, Max, Average)

  def parse(line: String): (Option[Operation], Seq[BigDecimal]) = {
    val nameAndOperation = supportedOperations.map(op => operationName(op) -> op).toMap
    val numberRegex = """[-+]?\d+(\.\d+)?""".r
    val operationsRegex = s"(${supportedOperations.map(operationName).mkString("|")})".r
    val operation = operationsRegex.findAllIn(line.toLowerCase).take(1).toSeq.headOption.flatMap(nameAndOperation.get)
    val numbers = numberRegex.findAllIn(line).toSeq.map(BigDecimal(_))
    (operation, numbers)
  }

  def composeResult(operation: Operation, numbers: Seq[BigDecimal], result: BigDecimal): String = {
    def composition(operationName: String) = s"the $operationName of ${numbers.mkString(" ")} is $result"

    operation match {
      case Divide => s"${numbers(0)} divided by ${numbers(1)} is $result"
      case Sum => composition("sum")
      case Average => composition("average")
      case Min => composition("minimum")
      case Max => composition("maximum")
    }
  }

  def error(msg: String) = s"Error: $msg"

  private def operationName(operation: Operation) : String = operation match {
    case Divide => "divide"
    case Sum => "sum"
    case Min => "min"
    case Max => "max"
    case Average => "average"
  }

}
