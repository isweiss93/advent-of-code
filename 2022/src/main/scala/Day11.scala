import scala.io.Source

/**
 *
 *
 *
 */
object Day11 {

  def main(args: Array[String]): Unit = {
    val monkeys = Source
      .fromResource("day11.txt")
      .getLines
      .filterNot(_ == "")
      .grouped(6)
      .toList
      .map(_.map(_.filter(c => c.isDigit || c == '*' || c == '+' || c == ',')))
      .map { m =>
        val op: (Long, Long) => Long = if (m(2).head == '*') _ * _ else _ + _
        Monkey(
          id           = m.head.toInt,
          items        = m(1).split(",").toList.map(_.toLong),
          op           = (w: Long) => op(w, m(2).tail.toLongOption.getOrElse(w)),
          div          = m(3).toLong,
          idIfTrue     = m(4).toInt,
          idIfFalse    = m(5).toInt,
          numInspected = 0
        )
      }

    val rounds20    = observe(monkeys, 20, _ / 3)
    val rounds10000 = observe(monkeys, 10000, _ % monkeys.map(_.div).product)

    println(rounds20.map(_.numInspected).sorted.takeRight(2).product)
    println(rounds10000.map(_.numInspected).sorted.takeRight(2).product)
  }

  private def observe(monkeys: List[Monkey], rounds: Int, scaleDown: Long => Long): List[Monkey] =
    1.to(rounds).foldLeft(monkeys) { case (m, _) =>
      m.foldLeft(m) { case (list, monkey) => list(monkey.id).takeTurn(list, scaleDown) }
    }
}

case class Monkey(
  id: Int,
  items: List[Long],
  op: Long => Long,
  div: Long,
  idIfTrue: Int,
  idIfFalse: Int,
  numInspected: Long
) {

  def takeTurn(monkeys: List[Monkey], scaleDown: Long => Long): List[Monkey] = {
    items
      .foldLeft(monkeys) { case (list, item) =>
        val worry  = scaleDown(op(item))
        val target = if (worry % div == 0) idIfTrue else idIfFalse
        list.updated(target, list(target).copy(items = list(target).items :+ worry))
      }
      .updated(id, this.copy(items = List.empty, numInspected = numInspected + items.length))
  }
}
