package de.unruh.rendermath


import de.unruh.rendermath.Grammar.{RhsElement, Rule}
import org.scalatest.funsuite.AnyFunSuite

class GrammarTest extends AnyFunSuite {
  private def exampleConstructor(name: String)(args: Seq[String]) = s"$name(${args.mkString(",")})"

  def TerminalPiece(name: String): RhsElement = RhsElement(name, Grammar.smallestPriority)

  val exampleGrammarRules = List(
    Rule("expr", 1000, List(TerminalPiece("("), RhsElement("expr", 0), TerminalPiece(")")), exampleConstructor("paren")),
    Rule("expr", 1000, List(TerminalPiece("0")), exampleConstructor("zero")),
    Rule("expr", 0, List(RhsElement("expr", 0), TerminalPiece("+"), RhsElement("expr", 1)), exampleConstructor("plus")),
    Rule("expr", 2, List(RhsElement("expr", 3), TerminalPiece("*"), RhsElement("expr", 2)), exampleConstructor("times")),
    Rule("expr", 3, List(TerminalPiece("-"), RhsElement("expr", 3)), exampleConstructor("minus"))
  )

  val exampleGrammar: Grammar[String, String] = {
    val empty = Grammar[String, String]().setTokenizer(_.split(' ').map { t => (t,t) })
    exampleGrammarRules.foldRight(empty) { (rule, grammar) => grammar.addRule(rule) }
  }

  test("match 0") {
    assert(exampleGrammar.matches(nonterminal = "expr", text = "0") == List("zero(0)"))
  }

  test("match 0+0") {
    assert(exampleGrammar.matches(nonterminal = "expr", text = "0 + 0") == List("plus(zero(0),+,zero(0))"))
  }

  test("experiments") {
    exampleGrammar.printGrammar()

    val text = "0 + 0"
    println(s"MATCHES for $text")
    for (m <- exampleGrammar.matches(nonterminal = "expr", text = text))
      println(m)
  }

}
