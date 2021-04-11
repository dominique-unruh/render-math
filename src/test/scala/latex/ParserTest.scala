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

  test("parse 1+2") {
    val text = "1+2"
    val tokens = tokenize(text)
    println("TOKENS", tokens)
    val math = Parser.parse(tokens)
    println("MATH " + math)
    assert(math.toString == "arith1.plus(1, 2)")
  }

  test("parse \\frac") {
    val text = "\\frac{1}{2}"
    val tokens = tokenize(text)
    println("TOKENS", tokens)
    val math = Parser.parse(tokens)
    println("MATH " + math)
    assert(math.toString == "arith1.divide(1, 2)")
  }
}
