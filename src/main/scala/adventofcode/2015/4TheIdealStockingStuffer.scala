import java.security.MessageDigest

/** --- Day 4: The Ideal Stocking Stuffer ---
  *
  * Santa needs help mining some AdventCoins (very similar to bitcoins) to use
  * as gifts for all the economically forward-thinking little girls and boys.
  *
  * To do this, he needs to find MD5 hashes which, in hexadecimal, start with at
  * least five zeroes. The input to the MD5 hash is some secret key (your puzzle
  * input, given below) followed by a number in decimal. To mine AdventCoins,
  * you must find Santa the lowest positive number (no leading zeroes: 1, 2, 3,
  * ...) that produces such a hash.
  *
  * https://en.wikipedia.org/wiki/MD5
  *
  * For example:
  *
  * If your secret key is abcdef, the answer is 609043, because the MD5 hash of
  * abcdef609043 starts with five zeroes (000001dbbfa...), and it is the lowest
  * such number to do so. If your secret key is pqrstuv, the lowest number it
  * combines with to make an MD5 hash starting with five zeroes is 1048970; that
  * is, the MD5 hash of pqrstuv1048970 looks like 000006136ef....
  *
  * Your puzzle input is `iwrupvqb`.
  *
  * FURTHER DESCRIPTION
  *
  *   1. What is MD5?
  *
  * MD5 is a mathematical function that takes any input (like a string of text)
  * and turns it into a fixed-size "fingerprint" called a hash.
  *
  *   - The Format: The output is always a 128-bit number, but we almost always
  *     look at it as a 32-character hexadecimal string.
  *   - Hexadecimal: This means the string only uses characters 0-9 and a-f.
  *   - Consistency: The same input will always produce the exact same hash.
  *     Even changing one letter in the input will result in a completely
  *     different hash.
  *
  * Example: The MD5 hash of hello is 5d41402abc4b2a76b9719d911017c592.
  *
  *   2. The Problem Breakdown
  *
  * Santa is "mining" coins. To "mine" a coin, he needs to find a hash that
  * starts with a certain number of zeroes.
  *
  * Your Goal: Find the lowest positive integer that you can stick onto the end
  * of your secret key (iwrupvqb) so that the MD5 hash of that combined string
  * starts with at least five zeroes.
  *
  * How it works step-by-step:
  *   1. Take key: iwrupvqb.
  *   2. Start with the number 1.
  *   3. Combine them: iwrupvqb1.
  *   4. Calculate the MD5 hash of iwrupvqb1.
  *   5. Check the first 5 characters of that hash. Are they 00000?
  *      - If yes, you found it! The answer is 1.
  *      - If no, try the next number: iwrupvqb2.
  *   6. Repeat this until you find the first number that works.
  *
  * Example from the description: If the secret key was abcdef:
  *   - abcdef1 -> hash starts with e80b... (No)
  *   - abcdef2 -> hash starts with 07e0... (No)
  *   - ... (keep going) ...
  *   - abcdef609043 -> hash starts with 000001db... (Yes! Five zeroes.)
  *   - The answer for abcdef would be 609043.
  *
  * You need to do this same process for your input: iwrupvqb.
  */
object Day04TheIdealStockingStuffer:

  def md5(s: String): String =
    val md5 = MessageDigest.getInstance("MD5")
    val digest: Array[Byte] = md5.digest(s.getBytes("UTF-8"))
    // Correctly calculates the 128-bit hash as a Byte array.
    // need to format the result into a human-readable hexadecimal string
    digest.map("%02x".format(_)).mkString

  @main def lowestIdealNumber(input: String): Int =
    val indexSatisfyingCondition = Iterator.from(1).indexWhere(index =>
      md5(s"$input$index").startsWith("00000")
    ) + 1
    println(
      s"lowestIdealNumberForHashStartingWith00000 is $indexSatisfyingCondition"
    )
    indexSatisfyingCondition

  @main def lowestIdealNumberPart2(input: String): Int =
    val indexStatisfyingCondition = Iterator.from(1).indexWhere(index =>
      md5(s"$input$index").startsWith("000000")
    ) + 1
    println(
      s"lowestIdealNumberForHasStartingWith000000 is $indexStatisfyingCondition"
    )
    indexStatisfyingCondition
