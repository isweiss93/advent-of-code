import scala.io.Source

object Day04 extends App {
  val pairs = Source.fromResource("day04.txt").getLines.toList.map { case s"${start1}-${end1},${start2}-${end2}" =>
    val elf1 = start1.toInt.to(end1.toInt).toList
    val elf2 = start2.toInt.to(end2.toInt).toList

    (elf1, elf2)
  }

  println(pairs.count { case (elf1, elf2) => elf1.containsSlice(elf2) || elf2.containsSlice(elf1) })
  println(pairs.count { case (elf1, elf2) => elf1.intersect(elf2).nonEmpty })
}
