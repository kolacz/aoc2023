import scala.util.parsing.combinator.RegexParsers

case class Game(number: Int, draws: List[List[Cubes]])
case class Cubes(amount: Int, color: Color)

enum Color:
  case red, green, blue

object GameParser extends RegexParsers:
  def game: Parser[Game] = "Game" ~ number ~ ":" ~ repsep(draw, ";") ^^ {
    case _ ~ gameNumber ~ _ ~ draws => Game(gameNumber, draws)
  }
  def draw: Parser[List[Cubes]] = repsep(number ~ color, ",") ^^ {
    _.map { case amount ~ color => Cubes(amount, color) }
  }
  def color:  Parser[Color] = """red|green|blue""".r ^^ { Color.valueOf }
  def number: Parser[Int]   = """(0|[1-9]\d*)""".r   ^^ { _.toInt }

  def apply(input: String): Game = parseAll(game, input) match
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)

def firstSolutionDay2(): Unit =
  project.withInputOfDay(2): lines =>
    val limits = Map(Color.red -> 12, Color.green -> 13, Color.blue -> 14)
    val possibleGames = for
      line <- lines
      Game(number, draws) = GameParser(line)
      if draws.forall: draw =>
        draw.forall: cubes =>
          cubes.amount <= limits(cubes.color)
    yield
      number
    println(possibleGames.sum)

def secondSolutionDay2(): Unit =
  project.withInputOfDay(2): lines =>
    val powers = for
      line <- lines
      Game(_, draws) = GameParser(line)
    yield
      draws.flatten.groupMap(_.color)(_.amount).foldLeft(1):
        case (power, (_, amounts)) => power * amounts.max
    println(powers.sum)

@main
def day2(): Unit =
  firstSolutionDay2()
  secondSolutionDay2()
