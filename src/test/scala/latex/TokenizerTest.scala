package de.unruh.rendermath
package latex

import de.unruh.rendermath.latex.Tokenizer.{Command, Letter, Whitespace, mergeTokens, tokenize}
//import fastparse.internal.Instrument
//import fastparse.{P, Parsed, ParserInputSource, parse}
import org.scalatest.funsuite.AnyFunSuite

class TokenizerTest extends AnyFunSuite {
  /*def testParse[T](input: String,
                   rest: String,
                   parser: P[_] => P[T],
                   expected: T): Unit = {
    val result = parse(input + rest, parser(_))
    println(s"Parsed ${input+rest}, got $result")
    result match {
      case Parsed.Success(value, index) =>
        assert(index == input.length)
        assert(value == expected)
      case failure: Parsed.Failure =>
        fail(failure.toString())
    }
  }*/

  test("tokenizer: command") {
    assert(tokenize("\\test    abc").head == Command("test","\\test    "))
//    testParse("\\test    ", "abc", Tokenizer.command(_), Command("test","\\test    "))
    assert(tokenize("\\test1abc").head == Command("test","\\test"))
//    testParse("\\test", "1abc", Tokenizer.command(_), Command("test", "\\test"))
    assert(tokenize("\\, abc").head == Command(",","\\,"))
//    testParse("\\,", " abc", Tokenizer.command(_), Command(",", "\\,"))
    assert(tokenize("\\,abc").head == Command(",","\\,"))
//    testParse("\\,", "abc", Tokenizer.command(_), Command(",", "\\,"))
    assert(tokenize("\\ abc").head == Command(" ","\\ "))
//    testParse("\\ ", "abc", Tokenizer.command(_), Command(" ", "\\ "))
    assert(tokenize("\\  abc").head == Command(" ","\\ "))
//    testParse("\\ ", " abc", Tokenizer.command(_), Command(" ", "\\ "))
  }

  test("tokenizer: whitespace") {
    assert(tokenize(" \nx").head == Whitespace(" \n"))
//    testParse(" \n","x", Tokenizer.whitespace(_), Whitespace(" \n"))
  }

  test("tokenizer: letter") {
    assert(tokenize("sx").head == Letter("s"))
//    testParse("s","x", Tokenizer.letter(_), Letter("s"))
  }

  test("tokenize") {
    val text = "\\frac{1 + 2}{x}\n"
    val result = tokenize(text)
    print(result)
    assert(mergeTokens(result) == text)
  }

}
