package de.unruh.rendermath.latex

import de.unruh.rendermath.latex.Tokenizer.{Brace, Command, Token, Whitespace}

import scala.collection.mutable.ListBuffer

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

  def parse(tokens: Seq[Token]) = {
    ???
  }

  trait Macro {
    def apply(command: Command, tokens: List[Token]): (List[Token], Seq[MathToken])
  }
  case object Passthrough extends Macro {
    override def apply(command: Command, tokens: List[Token]): (List[Token], Seq[MathToken]) = (tokens, Seq(LatexToken(command)))
  }
  /** Optional args etc not supported */
  abstract class SubstitutionMacro(argnum: Int) extends Macro {
    def takeArgument(command: Command, tokens: List[Token]): (List[Token], Seq[Token]) = tokens match {
      case Nil => throw new IllegalArgumentException(s"LaTeX macro \\$command reached end of string")
      case (b : Brace) :: _ if !b.open => throw new IllegalArgumentException(s"LaTeX macro \\$command found }")
      case (b : Brace) :: more if b.open =>
        val (newTokens, arg, _) = scanGroup(more)
        (newTokens, arg)
      case (_ : Whitespace) :: rest => takeArgument(command, rest)
      case token :: rest => (rest, Seq(token))
    }
    override def apply(command: Command, tokens: List[Token]): (List[Token], Seq[MathToken]) = {
      var tokens2 = tokens
      val args = ListBuffer[Seq[Token]]()
      for (_ <- 1 to argnum) {
        val (tokens3, arg) = takeArgument(command, tokens2)
        tokens2 = tokens3
        args += arg
      }
      val newTokens = evaluate(command, args.result()).toList
      (newTokens ++ tokens2, Nil)
    }
    def evaluate(command: Command, args: Seq[Seq[Token]]): Seq[Token]
  }

  val macros: Map[String, Macro] = Map[String, Macro](
  "pm" -> Passthrough,
    "TESTduplicate" -> new SubstitutionMacro(1) { def evaluate(command: Command, args: Seq[Seq[Token]]): Seq[Token] = args.head ++ args.head }
  )

  sealed trait MathToken
  final case class LatexToken(token: Tokenizer.Token) extends MathToken
  final case class ErrorToken(msg: String) extends MathToken
  final case class GroupToken(opening: Brace, tokens: Seq[MathToken], closing: Brace) extends MathToken

  def evaluateMacros(tokens: Seq[Tokenizer.Token]): List[MathToken] = {
    val result = ListBuffer[MathToken]()
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
