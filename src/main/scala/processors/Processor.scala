package de.unruh.rendermath
package processors

import org.log4s
import SymbolName.{arith1, rendermath}

abstract class Processor {
  def process(math: Math): Math
}

case object AddParentheses extends Processor {
  override def process(math: Math): Math = process(math, parent=rendermath.invalid)

  val parenthesisPairs = Set(
    arith1.times -> arith1.plus
  )

  private def process(math: Math, parent: SymbolName): Math = math match {
    case (app @ Application(head, args @ _*)) : Application =>
      val args2 = args.map { a => process(a, head) }
      val argsChanged = args.zip(args2).exists { (a,b) => a ne b }
      val app2 = if (argsChanged) app.setArguments(args2) else app
      val needParens = parenthesisPairs.contains(parent -> head) && !math.getAttribute(rendermath.parenthesis).getOrElse(false)
      val app3 = if (needParens) app2.setAttribute(rendermath.parenthesis, true) else app2
      logger.debug(s"$parent->$head ($argsChanged,$needParens)   $app")
      app3
    case _: Symbol => math
    case _: Number => math
    case _: Variable => math
    case _: Error => math
  }

  private val logger = log4s.getLogger
}

object Processor {
  val processors : List[Processor] = List(AddParentheses)
}