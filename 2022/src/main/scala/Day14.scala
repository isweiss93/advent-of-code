import scala.collection.mutable
import scala.io.Source

object Day14 {

  def main(args: Array[String]): Unit = {
    val paths = Source
      .fromResource("day14.txt")
      .getLines
      .toList
      .map(_.split(" -> ").toList.map { pair =>
        val p = pair.split(",").map(_.toInt)
        (p.head, p.last)
      })

    val maxY = paths.flatten.map(_._2).max

    val map = mutable.Seq.fill(maxY + 2)(mutable.Seq.fill[Char](1000)('.'))

    paths.foreach { path =>
      path.foldLeft(Option.empty[(Int, Int)]) {
        case (Some((oldX, oldY)), (x, y)) =>
          val deltaX: Int = (x - oldX).sign
          val deltaY: Int = (y - oldY).sign
          var i = 1
          while (map(y)(x) != '#') {
            map(oldY + deltaY * i)(oldX + deltaX * i) = '#'
            i += 1
          }
          Some(x, y)
        case (None, (x, y)) =>
          map(y)(x) = '#'
          Some(x, y)
      }
    }

    var filling = true
    while (filling) {
      var (posX, posY) = (500, 0)
      var resting = false
      while (!resting) {
        if(posY + 1 >= map.length) {
          resting = true
          filling = false
        }
        else if (map(posY + 1)(posX) == '.')
          posY += 1
        else if (map(posY + 1)(posX - 1) == '.') {
          posY += 1
          posX -= 1
        }
        else if (map(posY + 1)(posX + 1) == '.') {
          posY += 1
          posX += 1
        }
        else resting = true
      }
      if (filling) map(posY)(posX) = 'O'
    }

    println(map.flatten.count(_ == 'O'))

    val mapP2 = map :+ mutable.Seq.fill(1000)('#')

    while (mapP2.head(500) != 'O') {
      var (posX, posY) = (500, 0)
      var resting = false
      while (!resting) {
        if (posY + 1 >= map.length) {
          resting = true
          filling = false
        }
        else if (mapP2(posY + 1)(posX) == '.')
          posY += 1
        else if (mapP2(posY + 1)(posX - 1) == '.') {
          posY += 1
          posX -= 1
        }
        else if (mapP2(posY + 1)(posX + 1) == '.') {
          posY += 1
          posX += 1
        }
        else resting = true
      }
      mapP2(posY)(posX) = 'O'
    }

    println(mapP2.flatten.count(_ == 'O'))
  }
}
