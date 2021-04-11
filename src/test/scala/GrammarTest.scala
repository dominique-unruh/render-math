package de.unruh.rendermath


import org.scalatest.funsuite.AnyFunSuite

class GrammarTest extends AnyFunSuite {
//  private def exampleConstructor(name: Result)(args: List[Result]) = s"$name(${args.mkString(",")})"

  def TerminalPiece(name: String): RhsElement = RhsElement(name, Grammar.smallestPriority)

  val exampleGrammarRules = List(
    Rule("expr", 1000, List(TerminalPiece("("), RhsElement("expr", 0), TerminalPiece(")")), "paren"),
    Rule("expr", 1000, List(TerminalPiece("0")), "zero"),
    Rule("expr", 0, List(RhsElement("expr", 0), TerminalPiece("+"), RhsElement("expr", 1)), "plus"),
    Rule("expr", 2, List(RhsElement("expr", 3), TerminalPiece("*"), RhsElement("expr", 2)), "times"),
    Rule("expr", 3, List(TerminalPiece("-"), RhsElement("expr", 3)), "minus")
  )

  val exampleGrammar: Grammar = exampleGrammarRules.foldRight(Grammar.empty) { (rule, grammar) => grammar.addRule(rule) }
    .setTokenizer(_.split(' ').map { t => (t,t) })

  test("match 0") {
    assert(exampleGrammar.matches(nonterminal = "expr", text = "0") == List("zero()"))
  }

//  test("no match 0 at high priority") {
//    assert(exampleGrammar.matches("expr", 1001, List("0")) == Nil)
//  }

/*  test("recover rules") {
    assert(exampleGrammar.rules.toSet == exampleGrammarRules.toSet)
  }*/

  test("experiments") {
//    for (rule <- exampleGrammar.rules)
//      println(rule)

    exampleGrammar.printGrammar()

//    val tokens = "0 + 0".split(' ').toList
    val text = "0 + 0"
    println(s"MATCHES for $text")
    for (m <- exampleGrammar.matches(nonterminal = "expr", text = text))
      println(m)
  }

}
