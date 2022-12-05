import scala.io.Source

object Day01 extends App {
  private val file = Source.fromResource("day01.txt")

  private val caloriesList = file.getLines().toSeq.foldLeft(Seq(0L)) { case (acc, line) =>
    line.toLongOption match {
      case Some(calories) => acc.init :+ (acc.last + calories)
      case None           => acc :+ 0
    }
  }

  println(caloriesList.max)
  println(caloriesList.sorted.takeRight(3).sum)
}
