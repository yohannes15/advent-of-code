/** --- Day 3: Perfectly Spherical Houses in a Vacuum ---
  *
  * Santa is delivering presents to an infinite two-dimensional grid of houses.
  *
  * He begins by delivering a present to the house at his starting location, and
  * then an elf at the North Pole calls him via radio and tells him where to
  * move next. Moves are always exactly one house to the north (^), south (v),
  * east (>), or west (<). After each move, he delivers another present to the
  * house at his new location.
  *
  * However, the elf back at the north pole has had a little too much eggnog,
  * and so his directions are a little off, and Santa ends up visiting some
  * houses more than once. How many houses receive at least one present?
  *
  * For example:
  *
  *   - > delivers presents to 2 houses: one at the starting location, and one
  *     to the east.
  *   - ^>v< delivers presents to 4 houses in a square, including twice to the
  *     house at his starting/ending location.
  *   - ^v^v^v^v^v delivers a bunch of presents to some very lucky children at
  *     only 2 houses.
  */
object Day3HousesReceivingAtLeastOnePresent:

  val movements = Map(
    '>' -> (1, 0), // east
    '<' -> (-1, 0), // west
    '^' -> (0, -1), // north
    'v' -> (0, 1) // south
  )
  final case class Point(x: Int, y: Int):
    def delta(dx: Int, dy: Int) = Point(x + dx, y + dy)

  def deliver(input: Seq[Char]): Seq[Point] =
    input.scanLeft(Point(0, 0))({
      case (location, next) =>
        val (dx, dy) = movements(next)
        location.delta(dx, dy)
    })

  @main def housesReceivingAtLeastOnePresent(): Int =
    val directions = os.read(os.pwd / "AOC" / "15DAY03.txt").mkString.trim
    val allPointsMovedTo = deliver(directions)
    println(s"AllPointsMoved to \n \n ${allPointsMovedTo.mkString(" \n ")} \n")
    val result = allPointsMovedTo.distinct.size
    println(s"HousesReceivingAtLeastOnePresent: $result")
    result

  /** --- Part Two ---
    *
    * The next year, to speed up the process, Santa creates a robot version of
    * himself, Robo-Santa, to deliver presents with him.
    *
    * Santa and Robo-Santa start at the same location (delivering two presents
    * to the same starting house), then take turns moving based on instructions
    * from the elf, who is eggnoggedly reading from the same script as the
    * previous year.
    *
    * This year, how many houses receive at least one present?
    *
    * For example:
    *
    * ^v delivers presents to 3 houses, because Santa goes north, and then
    * Robo-Santa goes south. ^>v< now delivers presents to 3 houses, and Santa
    * and Robo-Santa end up back where they started. ^v^v^v^v^v now delivers
    * presents to 11 houses, with Santa going one direction and Robo-Santa going
    * the other.
    */
  @main def housesReceivingAtLeastOnePresentWithRoboSanta(): Unit =
    val directions = os.read(os.pwd / "AOC" / "15DAY03.txt").mkString.trim
    println(deliverWithRoboSantaScanLeftSoln(directions) ==
      deliverWithRoboSantaPartitionMap(directions))

  def deliverWithRoboSantaScanLeftSoln(input: String): Int =
    val allPointsMovedToBySantaAndRoboSanta =
      input.zipWithIndex.scanLeft((Point(0, 0), Point(0, 0)))({
        case ((santaLocation, roboLocation), (next, idx)) =>
          val (dx, dy) = movements(next)
          if idx % 2 == 0 then
            (santaLocation.delta(dx, dy), roboLocation)
          else
            (santaLocation, roboLocation.delta(dx, dy))
      })
    println(
      s"AllPointsMoved to \n \n ${allPointsMovedToBySantaAndRoboSanta.mkString(" \n ")} \n"
    )
    val result = allPointsMovedToBySantaAndRoboSanta.flatMap {
      case (santa, robo) => Seq(santa, robo)
    }.distinct.size
    println(s"HousesReceivingAtLeastOnePresentWithRoboSantaScanLeft: $result")
    result

  def deliverWithRoboSantaPartitionMap(input: String): Int =
    val (santaDirs, robotDirs) = input.zipWithIndex.partitionMap((char, idx) =>
      if idx % 2 == 0 then Left(char) else Right(char)
    )
    val result = (deliver(santaDirs) ++ deliver(robotDirs)).distinct.size
    println(s"HousesReceivingAtLeastOnePresentWithRoboSantaPMap: $result")
    result
