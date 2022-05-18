package de.unruh.rendermath
package document

class Document(paragraphs: Seq[Paragraph]) extends Element {
  def renderAsLatex =
    raw"""
       \documentclass{article}

       \begin{document}

        ${paragraphs.map(_.renderAsLatex).mkString("\n\n")}

       \end{document}
       """.stripMargin

}

object Document {
  def apply(paragraphs: Paragraph*): Document = new Document(paragraphs)
}
