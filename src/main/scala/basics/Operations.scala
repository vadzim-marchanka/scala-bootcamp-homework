package basics

object Operations {

  sealed trait Operation {

    def calculate(arguments: Seq[BigDecimal]): Either[String, BigDecimal] =
      validate(checkNonEmpty(Right(arguments))).map(calculateWithoutValidation)

    protected def validate(arguments: Either[String, Seq[BigDecimal]]): Either[String, Seq[BigDecimal]]

    protected def calculateWithoutValidation(arg: Seq[BigDecimal]): BigDecimal

    protected def checkNonEmpty(arg: Either[String, Seq[BigDecimal]]): Either[String, Seq[BigDecimal]] =
      arg.flatMap { a => if (a.isEmpty) Left("There are no any number") else Right(a)}

  }

  final case object Divide extends Operation {
    protected override def calculateWithoutValidation(arg: Seq[BigDecimal]): BigDecimal = arg(0) / arg(1)

    override protected def validate(arg: Either[String, Seq[BigDecimal]]): Either[String, Seq[BigDecimal]] =
      validateDivisorNotNull(validateExactlyTwoArguments(arg))

    private def validateExactlyTwoArguments(arg: Either[String, Seq[BigDecimal]]): Either[String, Seq[BigDecimal]] =
      arg.flatMap { a =>
        a.size match {
          case size if size < 2 => Left("Not enough numbers for operation, expected 2")
          case size if size > 2 => Left("Too many numbers for operation, expected 2")
          case size if size == 2 => Right(a)
        }
      }

    private def validateDivisorNotNull(arg: Either[String, Seq[BigDecimal]]): Either[String, Seq[BigDecimal]] =
      arg.flatMap { a =>
        a.lift(1).filter(_ == 0).fold[Either[String, Seq[BigDecimal]]](Right(a)){ _ =>
          Left("The second value can not be null to perform operation")
        }
      }
  }

  final case object Sum extends Operation {
    override protected def validate(arg: Either[String, Seq[BigDecimal]]): Either[String, Seq[BigDecimal]] = arg
    override protected def calculateWithoutValidation(arg: Seq[BigDecimal]): BigDecimal = arg.sum
  }

  final case object Average extends Operation {
    override protected def validate(arg: Either[String, Seq[BigDecimal]]): Either[String, Seq[BigDecimal]] = arg
    override protected def calculateWithoutValidation(arg: Seq[BigDecimal]): BigDecimal = arg.sum / arg.size
  }

  final case object Min extends Operation {
    override protected def validate(arg: Either[String, Seq[BigDecimal]]): Either[String, Seq[BigDecimal]] = arg
    override protected def calculateWithoutValidation(arg: Seq[BigDecimal]): BigDecimal = arg.min
  }

  final case object Max extends Operation {
    override protected def validate(arg: Either[String, Seq[BigDecimal]]): Either[String, Seq[BigDecimal]] = arg
    override protected def calculateWithoutValidation(arg: Seq[BigDecimal]): BigDecimal = arg.max
  }

}
