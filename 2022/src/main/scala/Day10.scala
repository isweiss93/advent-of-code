import scala.io.Source

/**
 *
 *
 *
 */
object Day10 {

  def main(args: Array[String]): Unit = {
    val instructions = Source.fromResource("day10.txt").getLines.toSeq

    val cycles = instructions.foldLeft(Seq((1, 1))) { case (c, instruction) =>
      val (cycle, register) = c.last
      instruction match {
        case "noop" => c :+ (cycle + 1, register)
        case s"addx ${value}" =>
          c ++ Seq((cycle + 1, register), (cycle + 2, register + value.toInt))
      }
    }

    val totalSignalStrength =
      cycles.tail
        .filter { case (cycle, _) => cycle % 40 == 20 }
        .map { case (cycle, register) => cycle * register }
        .sum

    val screen = cycles.foldLeft(Seq("")) { case (pixels, (cycle, middle)) =>
      val pixel = (cycle - 1) % 40
      val updated =
        if ((middle - 1).to(middle + 1).contains(pixel)) pixels.init :+ (pixels.last + '#')
        else pixels.init :+ (pixels.last + '.')

      if (pixel == 39) updated :+ "" else updated
    }

    println(totalSignalStrength)
    println(screen.mkString("\n"))
  }
}
