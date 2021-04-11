package de.unruh.rendermath

import LaTeX.{toAsciiArt, toLaTeX}

import org.scalatest.funsuite.AnyFunSuite
import Implicits._

import de.unruh.rendermath.SymbolName.rendermath
import org.scilab.forge.jlatexmath.{Atom, FractionAtom, TeXFormula, TeXParser}

class LaTeXTest extends AnyFunSuite {
  test ("render simple") {
    assert(toLaTeX(Number(3)) == "3")
    assert(toLaTeX((3:Math) + 4) == "3+4")
    assert(toLaTeX((3:Math) + ((4:Math) * 5)) == "3+4*5") // Note: render does not insert parentheses
    assert(toLaTeX((-3:Math) + "x") == "-3+x")
    assert(toLaTeX(Number(3).setAttribute(rendermath.parenthesis, true)) == "\\left(3\\right)")
  }

  test("ASCII art") {
    println(toAsciiArt((3:Math)/4))
  }

}
