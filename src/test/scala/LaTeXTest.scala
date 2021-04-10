package de.unruh.rendermath

import LaTeX.render

import org.scalatest.funsuite.AnyFunSuite
import Implicits._

import de.unruh.rendermath.SymbolName.rendermath

class LaTeXTest extends AnyFunSuite {
  test ("render simple") {
    assert(render(Number(3)) == "3")
    assert(render((3:Math) + 4) == "3+4")
    assert(render((3:Math) + ((4:Math) * 5)) == "3+4*5") // Note: render does not insert parentheses
    assert(render((-3:Math) + "x") == "-3+x")
    assert(render(Number(3).setAttribute(rendermath.parenthesis, true)) == "\\left(3\\right)")
  }
}
