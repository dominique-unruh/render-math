package de.unruh.rendermath

import de.unruh.rendermath.SymbolName.arith1
import org.scalatest.funsuite.AnyFunSuite

class SymbolNameTest extends AnyFunSuite {
  test ("hardcoded names") {
    assert(arith1.plus.cdname == "arith1.plus")
  }
}
