import scala.io.Source

/**
 *
 *
 *
 */
object Day15 {

  def main(args: Array[String]): Unit = {
    val (sensors, beacons) = Source
      .fromResource("day15.txt")
      .getLines
      .toList
      .map { case s"Sensor at x=${xs}, y=${ys}: closest beacon is at x=${xb}, y=${yb}" =>
        (
          (xs.toLong, ys.toLong, distance(xs.toLong, ys.toLong, xb.toLong, yb.toLong)),
          (xb.toLong, yb.toLong)
        )
      }
      .unzip

    val row              = 2000000
    val beaconlessRanges = getBeaconlessRanges(sensors, row)
    val numNoBeacon = beaconlessRanges.map { case (min, max) =>
      max - min + 1
    }.sum - beacons.distinct.count(_._2 == row)

    println(numNoBeacon)

    val minVal = 0
    val maxVal = 4000000

    val allBeaconlessRanges =
      minVal.to(maxVal).map(getBeaconlessRanges(sensors, _, Some(minVal, maxVal)))
    val targetRow    = allBeaconlessRanges.indexWhere(_.length == 2)
    val targetColumn = allBeaconlessRanges(targetRow).head._2 + 1

    println(targetRow)
    println(targetColumn * maxVal + targetRow)
  }

  private def distance(x1: Long, y1: Long, x2: Long, y2: Long): Long =
    math.abs(x1 - x2) + math.abs(y1 - y2)

  private def getBeaconlessRanges(
    sensors: Seq[(Long, Long, Long)],
    row: Long,
    range: Option[(Long, Long)] = None
  ): Seq[(Long, Long)] =
    sensors
      .filter { case (_, y, d) => (y - d).to(y + d).contains(row) }
      .foldLeft(Seq.empty[(Long, Long)]) { case (seq, (x, y, d)) =>
        val distanceToRow     = math.abs(y - row)
        val distanceRemaining = d - distanceToRow
        val xRange = range
          .map { case (min, max) =>
            (math.max(x - distanceRemaining, min), math.min(x + distanceRemaining, max))
          }
          .getOrElse(x - distanceRemaining, x + distanceRemaining)
        seq :+ xRange
      }
      .sortBy(_._1)
      .foldLeft(Seq.empty[(Long, Long)]) { case (seq, (min, max)) =>
        if (seq.isEmpty) Seq((min, max))
        else if (seq.last._2 + 1 >= min) seq.init :+ (seq.last._1, math.max(seq.last._2, max))
        else seq :+ (min, max)
      }
}
