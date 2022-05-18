package de.unruh.rendermath
package document

class Paragraph(val text : Seq[Inline]) extends Element {
  // TODO: this should insert spaces between elements but only after \command
  override def renderAsLatex: String = text.map(_.renderAsLatex).mkString("")
}

object Paragraph {
  def apply(text : (Inline|String)*) : Paragraph =
    new Paragraph(text.map {
      case str : String => Text(str)
      case inline : Inline => inline
    })
}
