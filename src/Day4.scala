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

case class DuplicativeCard(numberOfInstances: Int, remainingCopiesToCreateBelow: Int)

def secondSolutionDay4(): Unit =
  project.withInputOfDay(4): lines =>
    val cardInstances = lines.foldLeft(List.empty[DuplicativeCard]):
      case (previousCards, line) =>
        val ScratchCard(winningNumbers, numbersYouHave) = ScratchCardParser(line)
        val matchesCount = winningNumbers.count(numbersYouHave.contains)
        val currentCardInstances = 1 + previousCards.filter(_.remainingCopiesToCreateBelow > 0).map(_.numberOfInstances).sum
        val previousCardsApplied = previousCards.map: card =>
          DuplicativeCard(card.numberOfInstances, card.remainingCopiesToCreateBelow - 1)

        DuplicativeCard(currentCardInstances, matchesCount) :: previousCardsApplied

    println(cardInstances.map(_.numberOfInstances).sum)

@main
def day4(): Unit =
  firstSolutionDay4()
  secondSolutionDay4()
