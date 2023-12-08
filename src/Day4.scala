import scala.util.parsing.combinator.RegexParsers

case class ScratchCard(winningNumbers: List[Int], numbersYouHave: Set[Int])

object ScratchCardParser extends RegexParsers:
  def number: Parser[Int] = """(0|[1-9]\d*)""".r ^^ { _.toInt }
  def scratchCard: Parser[ScratchCard] = "Card" ~ number ~ ":" ~ rep(number) ~ "|" ~ rep(number) ^^ {
    case _ ~ winningNumbers ~ _ ~ numbersYouHave => ScratchCard(winningNumbers, numbersYouHave.toSet)
  }
  def apply(input: String): ScratchCard = parseAll(scratchCard, input) match
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)

def firstSolutionDay4(): Unit =
  project.withInputOfDay(4): lines =>
    val points = for
      ScratchCard(winningNumbers, numbersYouHave) <- lines.map(ScratchCardParser(_))
      matches = winningNumbers.count(numbersYouHave.contains) if matches > 0
    yield math.pow(2, matches - 1)

    println(points.sum.toInt)

def secondSolutionDay4(): Unit = ???

@main
def day4(): Unit =
  firstSolutionDay4()
