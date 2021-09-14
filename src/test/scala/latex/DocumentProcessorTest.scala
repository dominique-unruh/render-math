package de.unruh.rendermath
package latex

import java.nio.file.Path

class DocumentProcessorTest extends org.scalatest.funsuite.AnyFunSuite {
  test("readFormulas") {
    val processor = new DocumentProcessor(Path.of("test.tex"))
    processor.processFormulas()
  }
}
