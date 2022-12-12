import scala.collection.mutable
import scala.io.Source

/**
 *
 *
 *
 */
object Day12 {

  def main(args: Array[String]): Unit = {
    val heightmap = Source.fromResource("day12.txt").getLines.toList.map(_.toCharArray.toList)

    println(findMinSteps(heightmap, 'S', 'E', _ <= 1))
    println(findMinSteps(heightmap, 'E', 'a', _ >= -1))
  }

  private def findMinSteps(
    heightmap: List[List[Char]],
    start: Char,
    end: Char,
    check: Int => Boolean
  ): Int = {
    val (startJ, startI) =
      heightmap.map(_.indexWhere(_ == start)).zipWithIndex.find { case (x, _) => x != -1 }.get
    val visited       = mutable.HashSet((startI, startJ))
    val queue         = mutable.Queue.empty[Entry]
    var minSteps: Int = Int.MaxValue
    addValidNeighbors(heightmap, Entry(startI, startJ, 0), queue, visited, check)

    while (queue.nonEmpty) {
      val entry = queue.dequeue()
      if (heightmap(entry.i)(entry.j) == end)
        minSteps = math.min(minSteps, entry.steps)
      addValidNeighbors(heightmap, entry, queue, visited, check)
    }
    minSteps
  }

  private def addValidNeighbors(
    heightmap: List[List[Char]],
    entry: Entry,
    queue: mutable.Queue[Entry],
    visited: mutable.HashSet[(Int, Int)],
    check: Int => Boolean
  ): Unit =
    entry.neighbors.foreach { case (i, j) =>
      val target = Entry(i, j, entry.steps + 1)
      if (
        heightmap.indices.contains(i) &&
        heightmap.head.indices.contains(j) &&
        check(getChar(target, heightmap) - getChar(entry, heightmap)) &&
        !visited.contains((i, j))
      ) {
        queue.enqueue(target)
        visited.add((i, j))
      }
    }

  private def getChar(entry: Entry, heightmap: List[List[Char]]): Char =
    if (heightmap(entry.i)(entry.j) == 'S') 'a'
    else if (heightmap(entry.i)(entry.j) == 'E') 'z'
    else heightmap(entry.i)(entry.j)
}

final case class Entry(i: Int, j: Int, steps: Int) {
  val neighbors = Seq((i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1))
}
