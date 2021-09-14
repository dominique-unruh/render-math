package de.unruh.rendermath.latex

import de.unruh.rendermath.{Application, Error, Grammar, Math, Number, SymbolName, Variable}
import de.unruh.rendermath.Grammar.{Constructor, Priority, RhsElement, Rule, largestPriority, smallestPriority}
import de.unruh.rendermath.SymbolName.arith1
import de.unruh.rendermath.SymbolName.rendermath.parseerror
import de.unruh.rendermath.latex.Tokenizer.{Brace, Command, Token, Whitespace}

import java.util.regex.Pattern
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

object Parser {
  def scanGroup(tokens: List[Token]) : (List[Token], List[Token], Brace) = {
    val argTokens = ListBuffer[Token]()
    var remainingTokens = tokens
    var level = 1
    var closingBrace = null : Brace
    while (level > 0)
      remainingTokens match {
        case (b : Brace) :: more =>
          if (b.open) level += 1; else level -= 1
          if (level > 0) argTokens += b
          else closingBrace = b
          remainingTokens = more
        case token :: more =>
          argTokens += token
          remainingTokens = more
        case Nil => throw new IllegalArgumentException(s"End of string inside {}-group")
      }
    (remainingTokens, argTokens.result(), closingBrace)
  }

  @tailrec
  def takeArgument(command: Command, tokens: List[Token]): (List[Token], Seq[Token]) = tokens match {
    case Nil => throw new IllegalArgumentException(s"LaTeX macro \\$command reached end of string")
    case (b : Brace) :: _ if !b.open => throw new IllegalArgumentException(s"LaTeX macro \\$command found }")
    case (b : Brace) :: more if b.open =>
      val (newTokens, arg, _) = scanGroup(more)
      (newTokens, arg)
    case (_ : Whitespace) :: rest => takeArgument(command, rest)
    case token :: rest => (rest, Seq(token))
  }

  def takeArguments(command: Command, argnum: Int, tokens: List[Token]): (List[Token], List[Seq[Token]]) = {
    var tokens2 = tokens
    val args = ListBuffer[Seq[Token]]()
    for (_ <- 1 to argnum) {
      val (tokens3, arg) = takeArgument(command, tokens2)
      tokens2 = tokens3
      args += arg
    }
    (tokens2, args.result())
  }

//  val mathNonterminal: RhsElement = RhsElement("MATH", smallestPriority)
//  def commandNonterminal(name: String): RhsElement = RhsElement(s"CMD-$name", smallestPriority)
//  def characterNonterminal(name: String): RhsElement = RhsElement(s"CHAR-$name", smallestPriority)
//  def expr(priority: Priority): RhsElement = RhsElement("expr", priority)

  val parseRuleRegex: Regex = raw"""([a-zA-Z][a-zA-Z_0-9]+?|\\[^a-zA-Z]|\\[a-zA-Z]+|.)(\.-?[0-9]+)?""".r

  def parseRule(rule: String, constructor: Constructor[Math]): Rule[Math] = {
    def parseNonterminal(nonterminal: String, defaultPriority: Priority): RhsElement = {
      nonterminal match {
        case parseRuleRegex(name, priority) =>
          val priority2 = if (priority!=null) priority.tail.toInt else defaultPriority
          val name2 =
            if (name.length == 1) s"CHAR-$name"
            else if (name.head == '\\') s"CMD-${name.tail}"
            else name
          RhsElement(name2, priority2)
      }
    }
    val parts = rule.split(' ').filter(_.nonEmpty)
    if (parts.length < 3)
      throw new IllegalArgumentException(s"Rule must have format 'nonterminal ::= nonterminal nonterminal ...' (too short): $rule")
    val Seq(head, assign, rhs @ _*) = parts.toSeq
    if (!Seq("::=", ":=", "->").contains(assign))
      throw new IllegalArgumentException(s"Rule must have format 'nonterminal ::= nonterminal nonterminal ...' (wrong symbol $assign): $rule")
    val headElem = parseNonterminal(head, largestPriority)
    val rhsElems = rhs.map(parseNonterminal(_,smallestPriority))
    Rule(headElem.nonterminal, headElem.minPriority, rhsElems.toList, constructor)
  }

  def copyConstructor[A](items: Seq[A]): A = items.head
  def infixConstructor(symbol: SymbolName)(items: Seq[Math]): Math = items match {
    case Seq(a,_,b) => Application(symbol, a, b)
  }

  val grammarRules: Seq[Rule[Math]] = List[(String, Constructor[Math])] (
    "expr.0 ::= expr.1 + expr.0" -> infixConstructor(arith1.plus),
    "expr.0 ::= expr.1 - expr.0" -> infixConstructor(arith1.minus),
    "expr.2 ::= expr.3 * expr.2" -> infixConstructor(arith1.times),
    "expr.2 ::= expr.3 \\cdot expr.2" -> infixConstructor(arith1.times),
    "expr.2 ::= expr.3 / expr.2" -> infixConstructor(arith1.divide),

    "expr ::= ( expr )" -> { case Seq(_, a, _) => a },
    "expr ::= MATH" -> copyConstructor,
    "expr ::= number" -> copyConstructor,
    "expr ::= variable" -> copyConstructor,

    "number ::= digit" -> copyConstructor,
    "number ::= number digit" -> { case Seq(Number(a), Number(b)) => Number(a*10+b) },
    "digit ::= 0" -> { _ => Number(0) },
    "digit ::= 1" -> { _ => Number(1) },
    "digit ::= 2" -> { _ => Number(2) },
    "digit ::= 3" -> { _ => Number(3) },
    "digit ::= 4" -> { _ => Number(4) },
    "digit ::= 5" -> { _ => Number(5) },
    "digit ::= 6" -> { _ => Number(6) },
    "digit ::= 7" -> { _ => Number(7) },
    "digit ::= 8" -> { _ => Number(8) },
    "digit ::= 9" -> { _ => Number(9) },

    "variable ::= x" -> { _ => Variable("x") }
  ) map {
    case (rule, constructor) => parseRule(rule, constructor)
  }

  val grammar: Grammar[Seq[MathParserToken], Math] = {
    val empty = Grammar[Seq[MathParserToken], Math]().setTokenizer(_.map {
      case LatexToken(token) => token match {
        case cmd : Command => (s"CMD-${cmd.name}", Error(parseerror, "raw command symbol", cmd))
        case _ : Whitespace => ???
        case char: Tokenizer.Character => (s"CHAR-${char.character}", Error(parseerror, "raw character", char))
      }
      case token: ErrorToken => ("ERROR", Error(parseerror, token))
      case GroupToken(opening, tokens, closing) =>
        ??? // Should be evaluted already in parse
      case MathToken(math) => ("MATH", math)
    })
    grammarRules.foldRight(empty) { (rule, grammar) => grammar.addRule(rule) }
  }

  def parse(tokens: Seq[Token]): Math = {
    val evaluated = evaluateMacros(tokens)
    parseMathParserTokens(evaluated)
  }

  def parseMathParserTokens(tokens: Seq[MathParserToken]): Math = {
    val evaluated = evaluateGroups(tokens)
//    grammar.printGrammar()
    val matches = grammar.matches(nonterminal = "expr", text = evaluated)
    if (matches.isEmpty)
      throw new RuntimeException(s"Could not parse formula $tokens")
    if (matches.length > 1)
      throw new RuntimeException(s"Could not parse formula $tokens, ${matches.length}x ambiguous")
    matches.head
  }

  trait Macro {
    def apply(command: Command, tokens: List[Token]): (List[Token], Seq[MathParserToken])
  }
  case object Passthrough extends Macro {
    override def apply(command: Command, tokens: List[Token]): (List[Token], Seq[MathParserToken]) = (tokens, Seq(LatexToken(command)))
  }
  /** Optional args etc not supported */
  abstract class SubstitutionMacro(argnum: Int) extends Macro {
    override def apply(command: Command, tokens: List[Token]): (List[Token], Seq[MathParserToken]) = {
      val (tokens2, args) = takeArguments(command, argnum, tokens)
      val newTokens = evaluate(command, args).toList
      (newTokens ++ tokens2, Nil)
    }
    def evaluate(command: Command, args: Seq[Seq[Token]]): Seq[Token]
  }

  abstract class MathMacro(argnum: Int) extends Macro {
    override def apply(command: Command, tokens: List[Token]): (List[Token], Seq[MathParserToken]) = {
      val (tokens2, args) = takeArguments(command, argnum, tokens)
      val args2 = args map { tokens => parse(tokens) }
      val math = evaluate(command, args2)
      (tokens2, Seq(MathToken(math)))
    }
    def evaluate(command: Command, args: Seq[Math]): Math
  }


  val macros: Map[String, Macro] = Map[String, Macro](
    "pm" -> Passthrough,
    "cdot" -> Passthrough,
    "TESTduplicate" -> new SubstitutionMacro(1) { def evaluate(command: Command, args: Seq[Seq[Token]]): Seq[Token] = args.head ++ args.head },
    "frac" -> new MathMacro(2) { def evaluate(command: Command, args: Seq[Math]): Math = args.head / args(1) },
  )

  sealed trait MathParserToken
  final case class LatexToken(token: Tokenizer.Token) extends MathParserToken
  final case class ErrorToken(msg: String) extends MathParserToken
  final case class GroupToken(opening: Brace, tokens: Seq[MathParserToken], closing: Brace) extends MathParserToken
  final case class MathToken(math: Math) extends MathParserToken

  def evaluateGroups(tokens: Seq[MathParserToken]): Seq[MathParserToken] = tokens map {
    case GroupToken(_, tokens, _) => MathToken(parseMathParserTokens(tokens))
    case token => token
  }

  def evaluateMacros(tokens: Seq[Tokenizer.Token]): List[MathParserToken] = {
    val result = ListBuffer[MathParserToken]()
    var toks = tokens.toList
    while (toks.nonEmpty) {
      val token = toks.head
      toks = toks.tail
      token match {
        case command @ Command(name, _) =>
          val macr = macros.getOrElse(name, throw new IllegalArgumentException(s"Unknown LaTeX command/macro \\$name"))
          val (remainingTokens, res) = macr(command, toks)
          toks = remainingTokens
          result ++= res
        case _ : Whitespace =>
        case b : Brace =>
          if (!b.open)
            throw new IllegalArgumentException("Unmatched }")
          val (tokens, content, closing) = scanGroup(toks)
          toks = tokens
          val content2 = evaluateMacros(content)
          result += GroupToken(b, content2, closing)
        case token : Tokenizer.Character => result += LatexToken(token)
      }
    }
    result.result()
  }
}
