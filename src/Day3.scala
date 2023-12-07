import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Positional

case class Number(value: String) extends Positional
case class Symbol(value: String) extends Positional
case class EngineLine(tokens: List[Symbol | Number])

object EngineLineParser extends RegexParsers:
  def numberToken: Parser[Number] =
    positioned("""(0|[1-9]\d*)""".r ^^ { Number.apply })

  def symbolToken: Parser[Symbol] =
    positioned("""[^0-9.]""".r ^^ { Symbol.apply })

  def line: Parser[EngineLine] =
    repsep(rep(".") ~> (symbolToken | numberToken: Parser[Symbol | Number]) <~ rep("."), rep(".")) ^^ { EngineLine.apply }

  def apply(input: String): EngineLine = parseAll(line, input) match
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)

def partitionTokens(engineLine: EngineLine): (List[Number], List[Symbol]) =
  engineLine.tokens.foldLeft((List.empty[Number], List.empty[Symbol])):
    case ((numberAcc, symbolAcc), n @ Number(_)) => (n :: numberAcc, symbolAcc)
    case ((numberAcc, symbolAcc), s @ Symbol(_)) => (numberAcc, s :: symbolAcc)

def isAdjacent(number: Number, symbol: Symbol): Boolean =
  val numberAdjacencyRangeStart = number.pos.column - 1
  val numberAdjacencyRangeEnd   = number.pos.column + number.value.length
  numberAdjacencyRangeStart <= symbol.pos.column && symbol.pos.column <= numberAdjacencyRangeEnd

def firstSolutionDay3(): Unit =
  project.withInputOfDay(3): lines =>
    val rows = lines.map(line => partitionTokens(EngineLineParser(line)))
    val (preGuard, postGuard) = Iterator((Nil, Nil)).duplicate
    val partNumberSums = (preGuard ++ rows ++ postGuard).sliding(3).map:
      case Seq(
        (_, previousSymbols), (currentNumbers, currentSymbols), (_, nextSymbols)
      ) =>
        val partNumbers = for
          number <- currentNumbers
          if (previousSymbols ++ currentSymbols ++ nextSymbols).exists(symbol => isAdjacent(number, symbol))
        yield number.value.toInt

        partNumbers.sum

    println(partNumberSums.sum)

@main
def day3(): Unit =
  firstSolutionDay3()
