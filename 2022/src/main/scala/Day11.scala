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
        val id: Int                  = m.head.toInt
        val items: List[Long]        = m(1).split(",").toList.map(_.toLong)
        val op: (Long, Long) => Long = if (m(2).head == '*') _ * _ else _ + _
        val mod: Option[Long]        = m(2).tail.toLongOption
        val div: Long                = m(3).toLong
        val yes: Int                 = m(4).toInt
        val no: Int                  = m(5).toInt
        (
          id,
          Monkey(
            id           = id,
            items        = items,
            div          = div,
            op           = (w: Long) => op(w, mod.getOrElse(w)),
            test         = (i: Long) => if (i % div == 0) yes else no,
            numInspected = 0
          )
        )
      }
      .toMap

    val rounds20 = 1.to(20).foldLeft(monkeys) { case (m, _) =>
      m.keys.toList.sorted.foldLeft(m) { case (map, id) =>
        map(id).takeTurn(map, _ / 3)
      }
    }

    val scaleDown = monkeys.values.map(_.div).product

    val rounds10000 = 1.to(10000).foldLeft(monkeys) { case (m, _) =>
      m.keys.toList.sorted.foldLeft(m) { case (map, id) =>
        map(id).takeTurn(map, _ % scaleDown)
      }
    }

    println(rounds20.values.toList.map(_.numInspected).sorted.takeRight(2).product)
    println(rounds10000.values.toList.map(_.numInspected).sorted.takeRight(2).product)
  }
}

case class Monkey(
  id: Int,
  items: List[Long],
  op: Long => Long,
  div: Long,
  test: Long => Int,
  numInspected: Long
) {

  def takeTurn(monkeys: Map[Int, Monkey], scaleDown: Long => Long): Map[Int, Monkey] = {
    val map = items.foldLeft(monkeys) { case (map, item) =>
      val worry  = scaleDown(op(item))
      val target = test(worry)
      val monkey = map(target)
      map.updated(target, monkey.copy(items = monkey.items :+ worry))
    }
    map.updated(id, this.copy(items = List.empty, numInspected = numInspected + items.length))
  }
}
