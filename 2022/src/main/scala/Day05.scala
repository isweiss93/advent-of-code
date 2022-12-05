import scala.io.Source

/**
 *
 *
 * 
 */
object Day05 {

  def main(args: Array[String]): Unit = {
    val file = Source.fromResource("day05.txt").getLines.toList

    val (stacksInput, moves) = {
      val (stacks, moves) =
        file.splitAt(file.zipWithIndex.find { case (s, _) => s == "" }.map(_._2).get)
      (stacks.init, moves.tail)
    }

    val stacks = stacksInput
      .map { line =>
        for {
          i <- 1.until(line.length, 4).toList
        } yield {
          line.charAt(i)
        }
      }
      .transpose
      .map(_.reverse)
      .map(_.filterNot(_ == ' '))

    val finalStacksP1 = moves.foldLeft(stacks) {
      case (oldStacks, s"move ${num} from ${start} to ${end}") =>
        val newStacks = moveCratesOneAtATime(oldStacks, num.toInt, start.toInt - 1, end.toInt - 1)
        newStacks
    }

    val messageP1 = finalStacksP1.foldLeft("") { case (s, stack) => s :+ stack.last }

    val finalStacksP2 = moves.foldLeft(stacks) {
      case (oldStacks, s"move ${num} from ${start} to ${end}") =>
        val newStacks = moveCratesTogether(oldStacks, num.toInt, start.toInt - 1, end.toInt - 1)
        newStacks
    }

    val messageP2 = finalStacksP2.foldLeft("") { case (s, stack) => s :+ stack.last }

    println(messageP1)
    println(messageP2)
  }

  private def moveCratesOneAtATime(
    stacks: List[List[Char]],
    num: Int,
    start: Int,
    end: Int
  ): List[List[Char]] =
    0.until(num).foldLeft(stacks) { case (oldStacks, _) =>
      val crate         = oldStacks(start).last
      val updatedStacks = oldStacks.updated(start, oldStacks(start).init)
      updatedStacks.updated(end, oldStacks(end) :+ crate)
    }

  private def moveCratesTogether(
    stacks: List[List[Char]],
    num: Int,
    start: Int,
    end: Int
  ): List[List[Char]] = {
    val crates        = stacks(start).takeRight(num)
    val updatedStacks = stacks.updated(start, stacks(start).dropRight(num))
    updatedStacks.updated(end, stacks(end) ++ crates)
  }
}
