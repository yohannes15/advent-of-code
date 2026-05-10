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

  /** --- Part Two ---
    *
    * Realizing the error of his ways, Santa has switched to a better model of
    * determining whether a string is naughty or nice. None of the old rules
    * apply, as they are all clearly ridiculous.
    *
    * Now, a nice string is one with all of the following properties:
    *
    *   - It contains a pair of any two letters that appears at least twice in
    *     the string without overlapping, like xyxy (xy) or aabcdefgaa (aa), but
    *     not like aaa (aa, but it overlaps).
    *   - It contains at least one letter which repeats with exactly one letter
    *     between them, like xyx, abcdefeghi (efe), or even aaa.
    *
    * For example:
    *
    *   - qjhvhtzxzqqjkmpb is nice because is has a pair that appears twice (qj)
    *     and a letter that repeats with exactly one letter between them (zxz).
    *   - xxyxx is nice because it has a pair that appears twice and a letter
    *     that repeats with one between, even though the letters used by each
    *     rule overlap.
    *   - uurcxstgmygtbstg is naughty because it has a pair (tg) but no repeat
    *     with a single letter between them.
    *   - ieodomkazucvgmuy is naughty because it has a repeating letter with one
    *     between (odo), but no pair that appears twice.
    *
    * How many strings are nice under these new rules?
    */
  @main def numberOfNiceStringsPart2(): Int =
    val lines: Seq[String] = os.read.lines(os.pwd / "AOC" / "15DAY05.txt")
    // sliding solution
    solnPart2(lines)
    // regex solution
    // regexSolnPart2(lines)

  def solnPart2(lines: Seq[String]): Int =
    def isNiceString(s: String): Boolean =
      // Check for a pair of any two letters that appears at least twice without overlapping
      val hasTwoPair = s.sliding(2).zipWithIndex.exists { case (pair, i) =>
        // Search for the same pair starting 2 positions after the current one to avoid overlap
        s.indexOf(pair, i + 2) != -1
      }
      // Check for at least one letter which repeats with exactly one letter between them
      val hasTriple = s.sliding(3).exists(window =>
        window.size == 3 && window(0) == window(2)
      )
      hasTwoPair && hasTriple

    val noOfNices = lines.count(isNiceString)
    println(s"Number of nices (non-regex): $noOfNices")
    noOfNices

  def regexSolnPart2(lines: Seq[String]): Int =
    val (twoPair, triple) = (
      "(..).*\\1".r.unanchored,
      "(.).\\1".r.unanchored
    )
    val noOfNices =
      lines.count(line => twoPair.matches(line) && triple.matches(line))
    println(s"Number of nices: $noOfNices")
    noOfNices

/** NOTE: REGEX SOLUTIONS EXPLAINED
  *
  * Part 1 Regexes
  *
  * These are defined in the regexSoln method:
  *
  *   1. [aeiou].*[aeiou].*[aeiou] (vowels)
  *      - Purpose: Checks for at least three vowels.
  *      - Format: [aeiou] matches a single vowel. .* matches any sequence of
  *        characters. By repeating [aeiou] three times with .* in between, it
  *        ensures there are at least three vowels anywhere in the string.
  *   2. (.)\1 (pair)
  *      - Purpose: Checks for at least one letter that appears twice in a row
  *        (e.g., aa, bb).
  *      - Format: (.) is a capturing group that matches any single character.
  *        \1 is a backreference that matches the exact same character captured
  *        by the first group.
  *   3. ab|cd|pq|xy (naughty)
  *      - Purpose: Checks for disallowed substrings.
  *      - Format: The pipe | acts as an OR operator, matching any of the
  *        literal strings ab, cd, pq, or xy.
  *
  * Part 2 Regexes
  *
  * These are defined in the regexSolnPart2 method:
  *
  *   1. (..).*\1 (twoPair)
  *      - Purpose: Checks for a pair of any two letters that appears at least
  *        twice without overlapping (e.g., xyxy).
  *      - Format: (..) captures any two characters. .* matches any characters
  *        in between. \1 is a backreference to the same two characters captured
  *        at the start.
  *   2. (.).\1 (triple)
  *      - Purpose: Checks for a letter that repeats with exactly one letter
  *        between them (e.g., xyx, aaa).
  *      - Format: (.) captures any single character. The . matches any single
  *        character in the middle. \1 is a backreference to the first captured
  *        character.
  *
  * Note on .unanchored
  *
  * All regexes in this file use the .unanchored method. In Scala, this allows
  * the regex to match any substring of the input, rather than requiring the
  * pattern to match the entire string from start to finish.
  */
