package de.unruh.rendermath
package latex


import fastparse._
import NoWhitespace._

object Tokenizer {
  sealed trait Token {
    def rawtext: String
  }
  final case class Command(name: String, rawtext: String) extends Token
  final case class Whitespace(rawtext: String) extends Token
  sealed trait Character extends Token {
    assert(rawtext.length==1)
    val character: Char = rawtext.head
  }
  final case class Letter(override val rawtext: String) extends Character
  final case class Other(override val rawtext: String) extends Character
  final case class Brace(override val rawtext: String) extends Character {
    val open: Boolean = character match {
      case '{' => true
      case '}' => false
    }
  }
  final case class Digit(override val rawtext: String) extends Character

  def transparentCapture[T](p: => P[T])(implicit ctx: P[_]): P[(T, String)] = {
    val startPos = ctx.index
    val res = p
    val endPos = res.index
    val slice = res.input.slice(startPos, endPos)
    res.successValue = (res.successValue, slice)
    res.asInstanceOf[P[(T, String)]]
  }

  def letterC[_: P]: P[Unit] = P {  CharIn("a-zA-Z")  }

  def whitespaceC[_: P]: P[Unit] = P {  CharIn(" \n\r\t")  }

  def command(implicit ctx : P[_]): P[Command] = { P {  transparentCapture("\\" ~/ ((letterC.rep(1).! ~ whitespaceC.rep) | AnyChar.!))  } }
    .map { case (name,rawtext) => Command(name,rawtext) }.opaque("TeX command name")

  def letter[_: P]: P[Letter] = P { letterC.! }
    .map(Letter).opaque("letter")

  def whitespace[_: P]: P[Whitespace] = P { whitespaceC.rep(1).! }
    .map(Whitespace).opaque("whitespace")

  def brace[_ : P]: P[Brace] = P { CharIn("{}").! }
    .map(Brace).opaque("brace")

  def digit[_: P]: P[Digit] = P { CharIn("0-9").! }
    .map(Digit).opaque("digit")

  /** Matches everythign, even if not "other". */
  def other[_: P]: P[Other] = P{ AnyChar.! }
    .map(Other).opaque("other")

  def token[_: P]: P[Token] = P{ command | whitespace | letter | brace | digit | other }
  def tokensTillEnd[_: P]: P[Seq[Token]] = P{ token.rep ~ End }

  def tokenize(latex: String): Seq[Token] = parse(latex, tokensTillEnd(_)) match {
    case Parsed.Success(value, _) => value
    case failure: Parsed.Failure => failure.get
  }

  def mergeTokens(tokens: Seq[Token]): String = tokens.map(_.rawtext).mkString
}
