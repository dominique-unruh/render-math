package de.unruh.rendermath
package latex

import de.unruh.rendermath.latex.DocumentProcessor.{LineIteratorClosing, logger}
import org.log4s

import java.io.{BufferedWriter, FileReader, FileWriter}
import java.nio.file.Path
import scala.collection.AbstractIterator
import scala.io.Source
import scala.util.Using

import de.unruh.rendermath.processors.Processor

class DocumentProcessor(texfile: Path) {
//  val directory = texfile.getParent
  def basename = texfile.getFileName.toString.stripSuffix(".tex")
  def withExtension(extension: String) = texfile.resolveSibling(s"$basename.$extension")
  def formulasOut = withExtension("formulas.out")
  def formulasIn = withExtension("formulas.in")

  def processFormulas() = {
    Using(new BufferedWriter(new FileWriter(formulasIn.toFile))) { writer =>
      for (line <- new LineIteratorClosing(scala.io.Source.fromFile(formulasOut.toFile)))
        line match {
          case DocumentProcessor.formulaLineRegex(numberStr, formula) =>
            val number = numberStr.toInt
            var math = Parser.parse(Tokenizer.tokenize(formula))
            logger.debug((number, formula, math).toString)
            for (processor <- Processor.processors) {
              math = processor.process(math)
              logger.debug(s"Processor $processor -> $math")
            }
            val rendered = Renderer.toLaTeX(math)
            writer.write(s"\\csdef{formulas@F@$number}{$formula}\n")
            writer.write(s"\\csdef{formulas@R@$number}{$rendered}\n")
        }
    }.get
  }
}

object DocumentProcessor {
  private val logger = log4s.getLogger
  private val formulaLineRegex = raw"^FORMULA\[([0-9]+)\] = (.*)$$".r

  class LineIteratorClosing(val source: Source) extends AbstractIterator[String] {
    private val iter = source.getLines()
    override def hasNext: Boolean = {
      if (iter.hasNext) true
      else {
        source.close()
        false
      }
    }
    override def next(): String = iter.next()
  }
}