package de.unruh.rendermath
package latex

import de.unruh.rendermath.latex.Tokenizer.{mergeTokens, tokenize}
import org.scalatest.funsuite.AnyFunSuite

class ParserTest extends AnyFunSuite {

  test("evaluateMacros") {
    val text = "1\\pm \\TESTduplicate 2 \\TESTduplicate {hello}"
    val tokens = tokenize(text)
    println("TOKENS", tokens)
    val mathTokens = Parser.evaluateMacros(tokens)
    println("MATHTOKENS")
    for (m <- mathTokens)
      println(m)
  }

  def testParse(text: String, expected: String) = {
      val tokens = tokenize(text)
      println("TOKENS", tokens)
      val math = Parser.parse(tokens)
      println("MATH " + math)
      assert(math.toString == expected)
  }

  test("parse 1+2") {
    testParse("1+2", "arith1.plus(1, 2)")
  }

  test("parse \\frac") {
    testParse("\\frac{1}{2}", "arith1.divide(1, 2)")
  }

  test("parse {}") {
    testParse("0*{1+2}", "arith1.times(0, arith1.plus(1, 2))")
  }

  test("parse priority") {
    testParse("0+1*2", "arith1.plus(0, arith1.times(1, 2))")
  }

  test("numbers") {
    testParse("12324243", "12324243")
  }

  test("parse with whitespace") {
    testParse("1 + 1", "arith1.plus(1, 1)")
  }

  test(raw"parse with command \cdot") {
    testParse(raw"1 \cdot 2", "arith1.times(1, 2)")
  }
}
