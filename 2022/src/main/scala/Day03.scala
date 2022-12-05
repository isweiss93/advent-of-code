import scala.io.Source

/** Adding
 * comment
 * block to hide
 * solution when sent
 */
object Day03 extends App {

  val rucksacks: Seq[Seq[Char]] =
    Source.fromResource("day03.txt").getLines.toSeq.map(_.toCharArray.toSeq)

  val mistakes: Seq[Char] = rucksacks.map(r => r.splitAt(r.length / 2)).map { case (r1, r2) =>
    r1.intersect(r2).head
  }
  val totalMistakePriority: Long = mistakes.map(getPriority).sum

  val groups: Seq[Seq[Seq[Char]]] =
    rucksacks.zipWithIndex
      .map { case (r, i) => (r, i / 3) }
      .groupBy { case (_, i) => i }
      .toSeq
      .map { case (_, rWithIndex) =>
        rWithIndex.map { case (r, _) => r }
      }

  val badges: Seq[Char] = groups.map { group =>
    group
      .foldLeft(Seq.empty[Char]) { case (intersection, r) =>
        if (intersection.isEmpty) r
        else intersection.intersect(r)
      }
      .head
  }
  val totalBadgePriority: Long = badges.map(getPriority).sum

  println(totalMistakePriority)
  println(totalBadgePriority)

  private def getPriority(tpe: Char): Long = {
    if (tpe.isLower) tpe - 'a' + 1
    else tpe - 'A' + 27
  }
}
