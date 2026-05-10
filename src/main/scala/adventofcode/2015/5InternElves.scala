import scala.util.matching.Regex

/** --- Day 5: Doesn't He Have Intern-Elves For This? ---
  *
  * Santa needs help figuring out which strings in his text file are naughty or
  * nice.
  *
  * A nice string is one with all of the following properties:
  *
  *   - It contains at least three vowels (aeiou only), like aei, xazegov, or
  *     aeiouaeiouaeiou.
  *   - It contains at least one letter that appears twice in a row, like xx,
  *     abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
  *   - It does not contain the strings ab, cd, pq, or xy, even if they are part
  *     of one of the other requirements.
  *
  * For example:
  *
  *   - ugknbfddgicrmopn is nice because it has at least three vowels
  *     (u...i...o...), a double letter (...dd...), and none of the disallowed
  *     substrings.
  *   - aaa is nice because it has at least three vowels and a double letter,
  *     even though the letters used by different rules overlap.
  *   - jchzalrnumimnmhp is naughty because it has no double letter.
  *   - haegwjzuvuyypxyu is naughty because it contains the string xy.
  *   - dvszwmarrgswjxmb is naughty because it contains only one vowel.
  *
  * How many strings are nice?
  */
object Day05NumberOfNiceStrings:

  def regexSoln(lines: Seq[String]): Int =

    val (vowels, pair, naughty) =
      (
        "[aeiou].*[aeiou].*[aeiou]".r.unanchored,
        "(.)\\1".r.unanchored,
        "ab|cd|pq|xy".r.unanchored
      )

    val noOfNices = lines.count(line =>
      vowels.matches(line) && pair.matches(line) && !naughty.matches(line)
    )
    println(s"Number of nices: $noOfNices")
    noOfNices

  def slidingAndContainsSoln(lines: Seq[String]): Int =

    def isNiceString(s: String): Boolean =
      val isNice =
        !(
          // Doesn't contain any of these substrings
          s.contains("ab") ||
            s.contains("cd") ||
            s.contains("pq") ||
            s.contains("xy")
        ) &&
          (
            // Contains atleast three vowles
            s.sliding(1).count(x =>
              x == "a" ||
                x == "e" ||
                x == "i" ||
                x == "o" ||
                x == "u"
            ) >= 3
          ) &&
          (
            s.sliding(2).count(x => x.size == 2 && x(0) == x(1)) > 0
          )
      println(s"$s is nice? $isNice")
      isNice

    val noOfNices = lines.foldLeft(0)({
      case (nices, next) => if isNiceString(next) then nices + 1 else nices
    })
    println(s"Number of nices: $noOfNices")
    noOfNices

  @main def numberOfNiceStrings(): Int =
    val lines: Seq[String] = os.read.lines(os.pwd / "AOC" / "15DAY05.txt")
    // First Pass Solution
    // slidingAndContainsSoln(lines)
    // Regex Solution
    regexSoln(lines)
