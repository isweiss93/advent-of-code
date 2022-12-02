import enumeratum._
import scala.io.Source

object Day02 extends App {
  private val guide = Source.fromResource("day02.txt").getLines.toSeq

  private val scoresPart1 = guide.map { case s"${opponent} ${me}" =>
    val myWeapon  = Weapon.withName(me)
    val oppWeapon = Weapon.withName(opponent)

    myWeapon.fight(oppWeapon)
  }

  private val scoresPart2 = guide.map { case s"${opponent} ${result}" =>
    val oppWeapon = Weapon.withName(opponent)
    val expectedResult = Result.withName(result)

    oppWeapon.chooseWeapon(expectedResult).fight(oppWeapon)
  }

  println(scoresPart1.sum)
  println(scoresPart2.sum)
}

sealed abstract class Weapon(baseScore: Long)
    extends EnumEntry {

  val Beats: Weapon
  val isBeatenBy: Weapon
  private val Self = this

  def fight(opp: Weapon): Long = {
    val result: Long = opp match {
      case Beats => 6L
      case Self  => 3L
      case _     => 0L
    }
    baseScore + result
  }

  def chooseWeapon(result: Result): Weapon = {
    import Result._

    result match {
      case Win => isBeatenBy
      case Draw => this
      case Lose => Beats
    }
  }
}

private object Weapon extends Enum[Weapon] {

  override def withName(name: String): Weapon =
    name match {
      case "A" | "X" => Rock
      case "B" | "Y" => Paper
      case "C" | "Z" => Scissors
      case _         => throw new RuntimeException("No matching weapon!")
    }

  val values = findValues
  case object Rock extends Weapon(1) {
    override lazy val Beats = Scissors
    override lazy val isBeatenBy: Weapon = Paper
  }
  case object Paper extends Weapon(2) {
    override lazy val Beats = Rock
    override lazy val isBeatenBy: Weapon = Scissors
  }
  case object Scissors extends Weapon(3) {
    override lazy val Beats = Paper
    override lazy val isBeatenBy: Weapon = Rock
  }
}

sealed trait Result extends EnumEntry

object Result extends Enum[Result] {
  override def withName(name: String): Result =
    name match {
      case "X" => Lose
      case "Y" => Draw
      case "Z" => Win
      case _ => throw new RuntimeException("No matching result!")
    }
  val values = findValues
  case object Win extends Result
  case object Draw extends Result
  case object Lose extends Result
}
