package de.unruh.rendermath

import SymbolName.{arith1, rendermath}

import de.unruh.rendermath.Grammar.{Constructor, Nonterminal, Priority, Result, Terminal}
import de.unruh.rendermath.Utils.updatedWithDefault
import org.scilab.forge.jlatexmath.{TeXConstants, TeXFormula}

import java.awt.image.BufferedImage
import java.awt.{Color, Image}
import scala.collection.mutable.ListBuffer


object LaTeX {
  private val Parenthesis = Math.attributePattern(rendermath.parenthesis)


  def toLaTeX(math: Math): String = math match {
    case Parenthesis(value, math) => value match {
      case false => s"\\left(${toLaTeX(math)}\\right)"
    }
    case Application(arith1.plus, args @ _*) => args.map(toLaTeX).mkString("+")
    case Application(arith1.times, args @ _*) => args.map(toLaTeX).mkString("*")
    case Application(arith1.divide, arg1, arg2) =>
      s"\\frac{${toLaTeX(arg1)}}{${toLaTeX(arg2)}}"
    case Symbol(name) =>
      throw new IllegalArgumentException("Don't know how to render symbol "+name)
    case Application(head, _ @ _*) =>
      throw new IllegalArgumentException("Don't know how to render applications with head "+head)
    case Number(number) => number.toString
    case Variable(name) => name
  }

  def latexToImage(latex: String): BufferedImage =
    new TeXFormula(latex).createBufferedImage(TeXConstants.STYLE_TEXT, 30, Color.black, Color.white).asInstanceOf[BufferedImage]

  // Based on https://stackoverflow.com/a/7099022/2646248
  def imageToAsciiArt(image: BufferedImage): String = {
    val sb = new StringBuilder
    val lines = new ListBuffer[String]
    for (y <- 0 until image.getHeight) {
      sb.clear()
      for (x <- 0 until image.getWidth) {
        val color = image.getRGB(x, y) & 0xFF // assumes B&W picture with not alpha
        val char = {
          if (color > 200) ' '
          else if (color > 130) '·'
          else if (color > 50) '*'
          else '#'
        }
        sb.append(char)
      }
      lines.append(sb.toString.stripTrailing)
    }

    val lines2 = lines.toList.dropWhile(_.isEmpty)
      .reverse.dropWhile(_.isEmpty).reverse
    lines2.mkString("\n")
  }

  def toAsciiArt(math: Math): String =
    imageToAsciiArt(latexToImage(toLaTeX(math)))
}
