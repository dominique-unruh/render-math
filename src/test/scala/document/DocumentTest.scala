package de.unruh.rendermath
package document

class DocumentTest extends org.scalatest.funsuite.AnyFunSuite {
  val testDocument = Document(
    Paragraph("This is a test."),
    Paragraph("And another paragraph.")
  )

  test ("render as LaTeX") {
    print(testDocument.renderAsLatex)
  }
}
