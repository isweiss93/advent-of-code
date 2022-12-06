import scala.io.Source

/**
 *
 *
 *
 *
 */
object Day06 {

  def main(args: Array[String]): Unit = {
    val datastream = Source.fromResource("day06.txt").getLines.toList.flatMap(_.toCharArray.toList)
    val startOfPacketMarker  = getMarker(datastream, 4)
    val startOfMessageMarker = getMarker(datastream, 14)

    println(startOfPacketMarker)
    println(startOfMessageMarker)
  }

  private def getMarker(datastream: List[Char], threshold: Int): Int = {
    val (_, marker) =
      datastream.zipWithIndex.drop(threshold - 1).foldLeft((datastream.take(threshold - 1), 0)) {
        case ((runner, marker), (c, i)) =>
          val newRunner = runner :+ c
          val newMarker =
            if (newRunner.distinct.length == threshold && marker < 1) i + 1
            else marker
          (newRunner.tail, newMarker)
      }
    marker
  }
}
