package de.unruh.rendermath

import Grammar.Result

import org.scalatest.funsuite.AnyFunSuite

class GrammarTest extends AnyFunSuite {
  private def exampleConstructor(name: Result)(args: List[Result]) = s"$name(${args.mkString(",")})"

  val exampleGrammarRules = List(
    Rule("expr", 1000, exampleConstructor("paren"), TerminalPiece("("), NonterminalPiece("expr", 0), TerminalPiece(")")),
    Rule("expr", 1000, exampleConstructor("zero"), TerminalPiece("0")),
    Rule("expr", 0, exampleConstructor("plus"), NonterminalPiece("expr", 0), TerminalPiece("+"), NonterminalPiece("expr", 1)),
    Rule("expr", 2, exampleConstructor("times"), NonterminalPiece("expr", 3), TerminalPiece("*"), NonterminalPiece("expr", 2)),
    Rule("expr", 3, exampleConstructor("minus"), TerminalPiece("-"), NonterminalPiece("expr", 3))
  )

  val exampleGrammar: Grammar = exampleGrammarRules.foldRight(Grammar.empty) { (rule, grammar) => grammar.addRule(rule) }

  test("match 0") {
    assert(exampleGrammar.matches("expr", 0, List("0")) == List("zero()"))
  }

  test("no match 0 at high priority") {
    assert(exampleGrammar.matches("expr", 1001, List("0")) == Nil)
  }

  test("recover rules") {
    assert(exampleGrammar.rules.toSet == exampleGrammarRules.toSet)
  }

  test("experiments") {
//    for (rule <- exampleGrammar.rules)
//      println(rule)

    val tokens = "0 + 0".split(' ').toList
    println(s"MATCHES for $tokens")
    for (m <- exampleGrammar.matches("expr", 0, tokens))
      println(m)
  }

}
