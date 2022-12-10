import scala.io.Source

/**
 *
 *
 *
 *
 */
object Day08 {

  def main(args: Array[String]): Unit = {
    val trees =
      Source.fromResource("day08.txt").getLines.toList.map(_.toCharArray.toList.map(_ - '0'))
    val numOutside = trees.length * 2 + trees.head.length * 2 - 4
    val numInside = (for {
      i <- 1.until(trees.length - 1)
      j <- 1.until(trees.head.length - 1)
    } yield {
      val row    = trees(i)
      val column = trees.map(_(j))
      val tree   = trees(i)(j)
      row.take(j).forall(_ < tree) ||
      row.drop(j + 1).forall(_ < tree) ||
      column.take(i).forall(_ < tree) ||
      column.drop(i + 1).forall(_ < tree)
    }).count(t => t)

    val highestScenicScore = (for {
      i <- 1.until(trees.length - 1)
      j <- 1.until(trees.head.length - 1)
    } yield {
      val row    = trees(i)
      val column = trees.map(_(j))
      val tree   = trees(i)(j)
      val (leftScore, _) = row.take(j).foldRight((0, true)) { case (t, (score, continue)) =>
        if (t < tree && continue) (score + 1, continue)
        else if (t >= tree && continue) (score + 1, false)
        else (score, continue)
      }
      val (rightScore, _) = row.drop(j + 1).foldLeft((0, true)) { case ((score, continue), t) =>
        if (t < tree && continue) (score + 1, continue)
        else if (t >= tree && continue) (score + 1, false)
        else (score, continue)
      }
      val (upScore, _) = column.take(i).foldRight((0, true)) { case (t, (score, continue)) =>
        if (t < tree && continue) (score + 1, continue)
        else if (t >= tree && continue) (score + 1, false)
        else (score, continue)
      }
      val (downScore, _) = column.drop(i + 1).foldLeft((0, true)) { case ((score, continue), t) =>
        if (t < tree && continue) (score + 1, continue)
        else if (t >= tree && continue) (score + 1, false)
        else (score, continue)
      }

      leftScore * rightScore * upScore * downScore
    }).max

    println(numInside + numOutside)
    println(highestScenicScore)
  }
}
