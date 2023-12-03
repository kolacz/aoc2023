//> using scala "3.3.1"
//> using dep "com.lihaoyi::os-lib:0.9.2"
//> using dep "org.scala-lang.modules::scala-parser-combinators:2.3.0"

object project:
  def withInputOfDay(day: Int)(block: Iterator[String] => Unit): Unit =
    val file = scala.io.Source.fromFile(s"../src/resources/input$day.txt")
    try
      block(file.getLines())
    finally
      file.close
