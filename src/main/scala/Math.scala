package de.unruh.rendermath

import SymbolName.arith1

import scala.language.implicitConversions

sealed trait Math {
  def removeAttribute[A](name: AttributeName[A]): Math =
    setAttributes(attributes.removed(name.symbolName))

  val attributes : Map[SymbolName, Any]

  def setAttributes(attributes: Map[SymbolName, Any]): Math
  final def setAttribute[A](name: AttributeName[A], value: A): Math =
    setAttributes(attributes.updated(name.symbolName, value))

  // Convenience methods
  final def +(other: Math): Application = Application(arith1.plus, this, other)
  final def -(other: Math): Application = Application(arith1.minus, this, other)
  final def *(other: Math): Application = Application(arith1.times, this, other)
  final def /(other: Math): Application = Application(arith1.divide, this, other)
}

object Math {
  class AttributePattern[A](name: AttributeName[A]) {
    def unapply(math: Math): Option[(Any,Math)] = math.attributes.get(name.symbolName) match {
      case Some(value : A) => Some((value, math.removeAttribute(name)))
      case None => None
    }
  }
  def attributePattern[A](name: AttributeName[A]) = new AttributePattern(name)
}

final class Symbol private (val name:SymbolName, override val attributes: Map[SymbolName, Any]) extends Math {
  override def setAttributes(attributes: Map[SymbolName, Any]): Math = new Symbol(name, attributes)
}

/** cdname is of the form cd.name */
class SymbolName(val cdname: String) extends AnyVal
class AttributeName[A] private (private val cdname: String) extends AnyVal {
  def symbolName: SymbolName = new SymbolName(cdname)
}
object AttributeName {
  def apply[A](symbolName: SymbolName) = new AttributeName[A](symbolName.cdname)
}

object SymbolName {
  private def s(implicit name: sourcecode.FullName): SymbolName = {
    val names = name.value.split('.')
    new SymbolName(names(names.length-2) + "." + names(names.length-1))
  }
  private def a[A](implicit name: sourcecode.FullName): AttributeName[A] =
    AttributeName[A](s(name))

  //noinspection TypeAnnotation
  object arith1 {
    val plus = s
    val minus = s
    val times = s
    val divide = s
  }

  //noinspection TypeAnnotation
  object rendermath {
    val parenthesis = a[Boolean]
  }
}

object Symbol {
  def apply(name: SymbolName) : Symbol = new Symbol(name, Map.empty)
  def unapply(arg: Math): Option[SymbolName] = arg match {
    case symbol: Symbol => Some(symbol.name)
    case _ => None
  }
}

final class Application private (val head:Math, val args:List[Math], override val attributes: Map[SymbolName, Any]) extends Math {
  override def setAttributes(attributes: Map[SymbolName, Any]): Math = new Application(head, args, attributes)
}
object Application {
  def apply(head:Math, args:Math*) : Application = new Application(head, args.toList, Map.empty)
  def apply(head:SymbolName, args:Math*) : Application = new Application(Symbol(head), args.toList, Map.empty)

  def unapplySeq(arg: Math): Option[(SymbolName, List[Math])] = arg match {
    case apply : Application => apply.head match {
      case Symbol(name) => Some((name, apply.args))
      case _ => None
    }
    case _ => None
  }
}

/** Represents an integer. OpenMath also supports IEEE doubles, and Content MathML supports IEEE double and
 * decimal fractions. We do not support any of these, explicit fractions and/or infinity or NaN symbols must be used for them. */
final class Number private (val number: BigInt, override val attributes: Map[SymbolName, Any]) extends Math {
  override def setAttributes(attributes: Map[SymbolName, Any]): Math = new Number(number, attributes)
}

object Number {
  def apply(number: Int): Number = new Number(BigInt(number), Map.empty)
  def apply(number: BigInt): Number = new Number(number, Map.empty)
  def unapply(arg: Math): Option[BigInt] = arg match {
    case number: Number => Some(number.number)
    case _ => None
  }
}

final class Variable private (val name:String, override val attributes: Map[SymbolName, Any]) extends Math {
  override def setAttributes(attributes: Map[SymbolName, Any]): Math = new Variable(name, attributes)
}

object Variable {
  def apply(name: String): Variable = new Variable(name, Map.empty)
  def unapply(arg: Math): Option[String] = arg match {
    case variable: Variable => Some(variable.name)
    case _ => None
  }
}

object Implicits {
  implicit def fromInt(number: Int): Number = Number(number)
  implicit def fromBigInt(number: BigInt): Number = Number(number)
  implicit def fromString(name: String): Variable = Variable(name)
}
