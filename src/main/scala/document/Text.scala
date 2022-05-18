package de.unruh.rendermath
package document

case class Text(text: String) extends Inline {
  // TODO: should do appropriate quoting since this is plain text
  override def renderAsLatex: String = text
}
