def firstSolutionDay1(): Unit =
  project.withInputOfDay(1): lines =>
    val total = lines.map { line =>
      val digits = line.filter(_.isDigit)
      s"${digits.head}${digits.last}".toInt
    }.sum
    println(total)

def secondSolutionDay1(): Unit =
  project.withInputOfDay(1): lines =>
    val digits = Map(
      "one" -> "1",
      "two" -> "2",
      "three" -> "3",
      "four" -> "4",
      "five" -> "5",
      "six" -> "6",
      "seven" -> "7",
      "eight" -> "8",
      "nine" -> "9"
    )

    def replaceDigits(line: String): String =
      digits.foldLeft(line):
        case (line, (letters, number)) =>
          line.replace(letters, s"${letters.head}$number${letters.last}")

    val total =
      lines.map { line =>
        val digits = replaceDigits(line).filter(_.isDigit)
        s"${digits.head}${digits.last}".toInt
      }.sum

    println(total)

@main
def day1(): Unit =
  firstSolutionDay1()
  secondSolutionDay1()
