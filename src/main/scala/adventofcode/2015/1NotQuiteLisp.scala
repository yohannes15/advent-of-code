/** https://adventofcode.com/2015/day/1
  *
  * --- Day 1: Not Quite Lisp ---
  *
  * Here's an easy puzzle to warm you up.
  *
  * Santa is trying to deliver presents in a large apartment building, but he
  * can't find the right floor - the directions he got are a little confusing.
  * He starts on the ground floor (floor 0) and then follows the instructions
  * one character at a time.
  *
  * An opening parenthesis, (, means he should go up one floor, and a closing
  * parenthesis, ), means he should go down one floor.
  *
  * The apartment building is very tall, and the basement is very deep; he will
  * never find the top or bottom floors.
  *
  * For example:
  *
  * (()) and ()() both result in floor 0.
  *
  * ((( and (()(()( both result in floor 3.
  *
  * ))((((( also results in floor 3.
  *
  * ()) and ))( both result in floor -1 (the first basement level).
  *
  * ))) and )())()) both result in floor -3.
  *
  * To what floor do the instructions take Santa?
  */
@main def notQuiteLispPart1(): Int =
  val string = os.read(os.pwd / "AOC" / "15DAY01.txt")
  val floor = string.foldLeft(0) {
    case (acc, char) => if char == '(' then acc + 1 else acc - 1
  }
  println(s"Santa should go to floor $floor")
  floor

/** --- Part Two ---
  *
  * Now, given the same instructions, find the position of the first character
  * that causes him to enter the basement (floor -1). The first character in the
  * instructions has position 1, the second character has position 2, and so on.
  *
  * For example:
  *
  * ) causes him to enter the basement at character position 1. ()()) causes him
  * to enter the basement at character position 5.
  *
  * What is the position of the character that causes Santa to first enter the
  * basement?
  */
@main def notQuiteLispPart2(): Int =
  val string = os.read(os.pwd / "AOC" / "15DAY01.txt")
  val positionNegative = string.scanLeft(0) {
    case (acc, char) => if char == '(' then acc + 1 else acc - 1
  }.indexOf(-1)
  println(
    s"Position of character that causes Santa to enter basement is $positionNegative"
  )
  positionNegative
