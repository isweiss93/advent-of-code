import scala.io.Source
import enumeratum._

/**
 *
 *
 *
 *
 */
object Day09 {

  def main(args: Array[String]): Unit = {
    val moves = Source.fromResource("day09.txt").getLines.toSeq.map { case s"${d} ${m}" =>
      Move(Direction.withName(d), m.toInt)
    }

    val knotsP1 = Seq.fill[Position](2)(Position(0, 0))
    val knotsP2 = Seq.fill[Position](10)(Position(0, 0))

    val (seenP1, _) = moves.foldLeft((Set.empty[Position], knotsP1)) { case ((seen, knots), move) =>
      val (newSeen, newKnots) = move.move(knots)
      (seen ++ newSeen, newKnots)
    }

    val (seenP2, _) = moves.foldLeft((Set.empty[Position], knotsP2)) { case ((seen, knots), move) =>
      val (newSeen, newKnots) = move.move(knots)
      (seen ++ newSeen, newKnots)
    }

    println(seenP1.size)
    println(seenP2.size)
  }
}

final case class Move(direction: Direction, numMoves: Int) {

  def move(knots: Seq[Position]): (Set[Position], Seq[Position]) =
    1.to(numMoves).foldLeft((Set.empty[Position], knots)) { case ((seenLast, k), _) =>
      val updatedKnots = direction.update(k)
      (seenLast + updatedKnots.last, updatedKnots)
    }
}

sealed trait Direction extends EnumEntry {
  val xModifier: Int = 0
  val yModifier: Int = 0

  private lazy val modifier: Position = Position(xModifier, yModifier)

  def update(knots: Seq[Position]): Seq[Position] =
    knots.foldLeft(Seq.empty[Position]) { case (updated, knot) =>
      if (updated.isEmpty) Seq(knot.add(modifier))
      else {
        val prevKnot = updated.last
        val newPos = if (!knot.isTouching(prevKnot)) knot.add(knot.diff(prevKnot).reduced) else knot
        updated :+ newPos
      }
    }
}

object Direction extends Enum[Direction] {
  val values = findValues

  override def withName(name: String): Direction = name match {
    case "U" => Up
    case "D" => Down
    case "L" => Left
    case "R" => Right
  }

  case object Up extends Direction {
    override val yModifier: Int = 1
  }

  case object Down extends Direction {
    override val yModifier: Int = -1
  }

  case object Left extends Direction {
    override val xModifier: Int = -1
  }

  case object Right extends Direction {
    override val xModifier: Int = 1
  }
}

final case class Position(x: Int, y: Int) {

  def reduced: Position =
    Position(if (x != 0) x / math.abs(x) else 0, if (y != 0) y / math.abs(y) else 0)

  def diff(other: Position): Position =
    Position(other.x - x, other.y - y)

  def add(other: Position): Position =
    Position(other.x + x, other.y + y)

  def isTouching(other: Position): Boolean =
    math.abs(x - other.x) <= 1 && math.abs(y - other.y) <= 1
}
