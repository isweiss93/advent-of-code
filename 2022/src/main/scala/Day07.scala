import scala.io.Source

/**
 *
 *
 *
 *
 */
object Day07 {

  def main(args: Array[String]): Unit = {
    val terminal = Source.fromResource("day07.txt").getLines.toSeq

    terminal.foldLeft(Directory.root) { case (curr, line) =>
      val next = line match {
        case "$ cd /"  => curr.toRoot
        case "$ cd .." => curr.parent
        case s"$$ cd ${directoryName}" =>
          curr.children.get(directoryName) match {
            case Some(d: Directory) => d
            case f                  => throw new RuntimeException(s"Cannot cd to ${f}!")
          }
        case s"$$ ls" => curr
        case s"dir ${directoryName}" =>
          curr.children =
            curr.children + (directoryName -> Directory(directoryName, curr, Map.empty))
          curr
        case s"${fileSize} ${fileName}" =>
          curr.children = curr.children + (fileName -> File(fileName, fileSize.toLong))
          curr
      }
      next
    }

    val directorySizes = getSizes(Directory.root)
    val max100000      = directorySizes.filter(_ <= 100000)
    val total100000    = max100000.sum

    val freeSpace         = 70000000 - Directory.root.size
    val neededSpace       = 30000000 - freeSpace
    val sizeOfDirToDelete = directorySizes.filter(_ >= neededSpace).min

    println(total100000)
    println(sizeOfDirToDelete)
  }

  private def getSizes(dir: Directory): Seq[Long] =
    dir.children.values.toSeq.collect { case d: Directory => d }.flatMap(getSizes) :+ dir.size
}

sealed trait SystemObject {
  val name: String
  val size: Long
}

case class Directory(
  override val name: String,
  var parent: Directory,
  var children: Map[String, SystemObject]
) extends SystemObject {

  override lazy val size: Long = children.values.map(_.size).sum

  def toRoot: Directory =
    if (name == "/") this
    else parent.toRoot
}

object Directory {
  var root = Directory("/", null, Map.empty)
}

case class File(
  override val name: String,
  override val size: Long
) extends SystemObject
