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

    part1(heightmap)
    part2(heightmap)
  }

  private def part1(heightmap: List[List[Char]]): Unit = {
    val (startJ, startI) =
      heightmap.map(_.indexWhere(_ == 'S')).zipWithIndex.find { case (x, _) => x != -1 }.get

    println(findMinSteps(heightmap, startI, startJ))
  }

  private def part2(heightmap: List[List[Char]]): Unit = {
    val starts = heightmap
      .map(_.zipWithIndex.filter { case (char, _) => Seq('a', 'S').contains(char) }.map(_._2))
      .zipWithIndex
      .filter(_._1.nonEmpty)
      .flatMap { case (list, i) => list.map(j => (i, j)) }

    val shortestPaths = starts.map { case (startI, startJ) =>
      findMinSteps(heightmap, startI, startJ)
    }

    println(shortestPaths.min)
  }

  private def findMinSteps(heightmap: List[List[Char]], startI: Int, startJ: Int): Int = {
    val visited       = mutable.HashSet((startI, startJ))
    val queue         = mutable.Queue.empty[Entry]
    var minSteps: Int = Int.MaxValue
    addValidNeighbors(heightmap, Entry(startI, startJ, 0), queue, visited)

    while (queue.nonEmpty) {
      val entry = queue.dequeue()
      if (heightmap(entry.i)(entry.j) == 'E')
        minSteps = math.min(minSteps, entry.steps)
      addValidNeighbors(heightmap, entry, queue, visited)
    }
    minSteps
  }

  private def addValidNeighbors(
    heightmap: List[List[Char]],
    entry: Entry,
    queue: mutable.Queue[Entry],
    visited: mutable.HashSet[(Int, Int)]
  ): Unit =
    entry.neighbors.foreach { case (i, j) =>
      val target = Entry(i, j, entry.steps + 1)
      if (
        heightmap.indices.contains(i) &&
        heightmap.head.indices.contains(j) &&
        getChar(target, heightmap) - getChar(entry, heightmap) <= 1 &&
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
