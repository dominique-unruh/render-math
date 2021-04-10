package de.unruh.rendermath

import SymbolName.{arith1, rendermath}

object LaTeX {
  private val Parenthesis = Math.attributePattern(rendermath.parenthesis)

  def render(math: Math): String = math match {
    case Parenthesis(value, math) => value match {
      case false => s"\\left(${render(math)}\\right)"
    }
    case Application(arith1.plus, args @ _*) => args.map(render).mkString("+")
    case Application(arith1.times, args @ _*) => args.map(render).mkString("*")
    case Symbol(name) =>
      throw new IllegalArgumentException("Don't know how to render symbol "+name)
    case Application(head, _ @ _*) =>
      throw new IllegalArgumentException("Don't know how to render applications with head "+head)
    case Number(number) => number.toString
    case Variable(name) => name
  }

}
