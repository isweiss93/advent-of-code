import scala.io.Source

/**
 *
 *
 *
 */
object Day13 {

  def main(args: Array[String]): Unit = {
    val input =
      Source
        .fromResource("day13.txt")
        .getLines
        .toList
        .filter(_.nonEmpty)

    val packets = input.map(s => PacketEntry.parse(s.slice(1, s.length - 1)))

    val compared = packets.grouped(2).map(pair => pair.head.compare(pair.last))
    val sorted = (packets ++ List(PacketEntry.divider2, PacketEntry.divider6)).sortWith {
      case (l, r) => l.compare(r) < 0
    }

    println(compared.zipWithIndex.filter { case (c, _) => c == -1 }.map(_._2 + 1).sum)
    println(
      (sorted.indexWhere(_ == PacketEntry.divider2) + 1)
        * (sorted.indexWhere(_ == PacketEntry.divider6) + 1)
    )
  }
}

sealed trait PacketEntry {
  val nonEmpty: Boolean
}

object PacketEntry {

  val divider2 = PacketList(List(PacketList(List(PacketInt(2)))))
  val divider6 = PacketList(List(PacketList(List(PacketInt(6)))))

  def parse(input: String): PacketList = {
    val (list, finalNum) = input.foldLeft((List(PacketList.empty), "")) { case ((stack, acc), c) =>
      c match {
        case '[' => (stack :+ PacketList.empty, acc)
        case ']' =>
          val completedList =
            if (acc.nonEmpty) stack.last :+ PacketInt(acc.toInt)
            else stack.last
          val listToUpdate = stack.init.last
          (stack.dropRight(2) :+ (listToUpdate :+ completedList), "")
        case ',' =>
          if (acc.nonEmpty) (stack.init :+ (stack.last :+ PacketInt(acc.toInt)), "")
          else (stack, acc)
        case d => (stack, acc + d)
      }
    }
    if (finalNum.nonEmpty) list.head :+ PacketInt(finalNum.toInt) else list.head
  }
}

case class PacketList(list: List[PacketEntry]) extends PacketEntry {

  val nonEmpty: Boolean                  = list.nonEmpty
  def :+(other: PacketEntry): PacketList = PacketList(list :+ other)

  def compare(other: PacketList): Int = {
    val otherList = other.list
    val comparisonList = list.zip(otherList).map {
      case (l: PacketInt, r: PacketInt)   => l.compare(r)
      case (l: PacketInt, r: PacketList)  => l.asList.compare(r)
      case (l: PacketList, r: PacketInt)  => l.compare(r.asList)
      case (l: PacketList, r: PacketList) => l.compare(r)
    }

    val comparison =
      if (comparisonList.nonEmpty)
        comparisonList.reduce((c1: Int, c2: Int) => if (c1 == 0) c2 else c1)
      else 0

    if (comparison == 0)
      if (list.length < otherList.length) -1
      else if (list.length > otherList.length) 1
      else 0
    else comparison
  }
  override def toString: String = list.map(_.toString).mkString("[", ",", "]")
}

object PacketList {
  def empty: PacketList = PacketList(List.empty)
}

case class PacketInt(value: Int) extends PacketEntry {
  override val nonEmpty: Boolean = true

  def asList: PacketList = PacketList(List(this))

  def compare(other: PacketInt): Int =
    if (value < other.value) -1
    else if (value > other.value) 1
    else 0

  override def toString: String = value.toString
}
