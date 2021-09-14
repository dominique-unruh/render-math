package de.unruh.rendermath

import Grammar.{Len1Lhs, Len1Rhs, Len2Lhs, Len2Rhs, Nonterminal, ParseResult, ParseResultNormal, ParseResultSynthetic, Priority, RhsElement, Rule, largestPriority, priorityString, smallestPriority, syntheticPriority, uniqueNumber}
import Utils.updatedWithDefault

import org.jetbrains.annotations.Nullable

object Grammar {
  type Nonterminal = String
  type Priority = Int

  private sealed trait ParseResult[+Result] {
    val nonterminal: Nonterminal
    val maxPriority: Priority
  }
  private final case class ParseResultNormal[+Result](override val nonterminal: Nonterminal, override val maxPriority: Priority, result: Result) extends ParseResult[Result] {
    override def toString: String = s"$nonterminal[$maxPriority]=$result"
  }
  private final case class ParseResultSynthetic[+Result](override val nonterminal: Nonterminal, override val maxPriority: Priority, result: List[Result]) extends ParseResult[Result] {
    override def toString: String = s"$nonterminal[$maxPriority]=$result"
  }

  case class Rule[Result](nonterminal: Nonterminal, maxPriority: Int, pieces: List[RhsElement],
                          constructor: Constructor[Result]) {
    override def toString: Nonterminal = s"$nonterminal[$maxPriority] -> ${pieces.mkString(" ")}"
  }

  final case class RhsElement(nonterminal: Nonterminal, minPriority: Priority) {
    override def toString: Nonterminal = s"$nonterminal[$minPriority]"
  }

  private var counter = 0
  private def uniqueNumber(): Int = { counter += 1; counter }

  private def priorityString(priority: Priority): String =
    if (priority == smallestPriority) "*"
    else if (priority == syntheticPriority) "@"
    else priority.toString

  private final case class Len2Rhs(leftNonterminal: Nonterminal,
                           rightNonterminal: Nonterminal)
  private final case class Len1Rhs(nonterminal: Nonterminal)

  type Constructor[Result] = Seq[Result] => Result

  private final case class Len2Lhs[Result](rhsLeftMinPriority: Priority, rhsRightMinPriority: Priority,
                                   nonterminal: Nonterminal, maxPriority: Priority,
                                   @Nullable constructor: Constructor[Result])
  private final case class Len1Lhs[Result](rhsMinPriority: Priority, nonterminal: Nonterminal, maxPriority: Priority,
                                   @Nullable constructor: Constructor[Result])

  def apply[Text, Result]() : Grammar[Text, Result] =
    new Grammar(Map.empty, Map.empty,
      { _ => throw new UnsupportedOperationException("No tokenizer configured") })

  val smallestPriority: Priority = Int.MinValue
  val largestPriority: Priority = Int.MaxValue
  private val syntheticPriority: Priority = 1212121212
}


final class Grammar[Text, Result] private (private val inverted2Rules: Map[Len2Rhs, List[Len2Lhs[Result]]],
                                           private val inverted1Rules: Map[Len1Rhs, List[Len1Lhs[Result]]],
                                           private val tokenizer: Text => Seq[ParseResult[Result]]) {
  private type ParseResult = Grammar.ParseResult[Result]
  private type ParseResultNormal = Grammar.ParseResultNormal[Result]
  private type ParseResultSynthetic = Grammar.ParseResultSynthetic[Result]
  private type Len2Lhs = Grammar.Len2Lhs[Result]
  private type Len1Lhs = Grammar.Len1Lhs[Result]
  type Rule = Grammar.Rule[Result]

  private def copy(inverted2Rules: Map[Len2Rhs, List[Len2Lhs]] = inverted2Rules,
                   inverted1Rules: Map[Len1Rhs, List[Len1Lhs]] = inverted1Rules,
                   tokenizer: Text => Seq[ParseResult] = tokenizer) =
    new Grammar[Text,Result](inverted2Rules, inverted1Rules, tokenizer)

  def setTokenizer(tokenizer: Text => Seq[(Nonterminal, Result)]): Grammar[Text, Result] =
    copy(tokenizer = { text => tokenizer(text) map { case (nt, tag) =>
      ParseResultNormal(nt, largestPriority, tag) } })

  def addRule(rule: Rule): Grammar[Text, Result] = rule match {
    case Rule(nonterminal, maxPriority, Seq(singleRhs), constructor) =>
      val rhs = Len1Rhs(nonterminal = singleRhs.nonterminal)
      val lhs = Len1Lhs(nonterminal = nonterminal, maxPriority = maxPriority,
        constructor = constructor,
        rhsMinPriority = singleRhs.minPriority)
      this.copy(inverted1Rules = updatedWithDefault(inverted1Rules, Nil, rhs) { lhs :: _ })
    case Rule(_, _, Seq(), _) =>
      throw new UnsupportedOperationException("Empty production rules are not supported")
    case Rule(nonterminal, maxPriority, rhs, constructor) =>
      var grammar: Grammar[Text, Result] = this
      val rhsTailRev = rhs.tail.reverse
      var rhsEnd = rhsTailRev.head
      for (rhsElem <- rhsTailRev.tail) {
        val rhs2 = Len2Rhs(leftNonterminal = rhsElem.nonterminal,
          rightNonterminal = rhsEnd.nonterminal)
        // TODO reuse existing ones
        // TODO: Use compact name (or different type for synthetic nonterminals)
        val lhs2 : Len2Lhs = Len2Lhs(
          rhsLeftMinPriority = rhsElem.minPriority, rhsRightMinPriority = rhsEnd.minPriority,
          nonterminal=s"#${uniqueNumber()}", maxPriority=syntheticPriority, constructor = null)
        rhsEnd = RhsElement(lhs2.nonterminal, lhs2.maxPriority)
        grammar = grammar.copy(inverted2Rules =
          updatedWithDefault(grammar.inverted2Rules, Nil, rhs2) { lhs2 :: _ })
      }

      val rhsHead = rhs.head
      val rhs2 = Len2Rhs(leftNonterminal = rhsHead.nonterminal,
        rightNonterminal = rhsEnd.nonterminal)
      val lhs2 = Len2Lhs(nonterminal = nonterminal, maxPriority = maxPriority, constructor = constructor,
        rhsLeftMinPriority = rhsHead.minPriority, rhsRightMinPriority = rhsEnd.minPriority)
      grammar.copy(inverted2Rules =
        updatedWithDefault(grammar.inverted2Rules, Nil, rhs2) { lhs2 :: _ })
  }

  def matches(nonterminal: Nonterminal, minPriority: Priority = smallestPriority, text: Text): List[Result] = {
    matchesNonterminalList(nonterminal, minPriority, tokenizer(text))
  }

  private def matchesNonterminalList(nonterminal: Nonterminal, minPriority: Priority = smallestPriority,
                                     text: Seq[ParseResult]): List[Result] = {
    def apply1Rules(results: Seq[ParseResult]): List[ParseResult] = {
      // Apply len1 rules
      var results1 : List[ParseResult] = Nil
      var results2ToAdd = results.toList
      while (results2ToAdd.nonEmpty) {
        val resultToAdd = results2ToAdd.head
        results2ToAdd = results2ToAdd.tail
        results1 = resultToAdd :: results1
//        println(s"apply1: $resultToAdd")
        for (lhs <- inverted1Rules.getOrElse(Len1Rhs(resultToAdd.nonterminal), Nil)
             if lhs.rhsMinPriority <= resultToAdd.maxPriority) {
//          println(s"rule: $lhs")
          val newResult = ParseResultNormal(lhs.nonterminal, lhs.maxPriority,
            lhs.constructor(Seq(resultToAdd.asInstanceOf[ParseResultNormal].result)))
          results2ToAdd = newResult :: results2ToAdd
        }
      }
      results1
    }

    val length = text.length
    /** table[row][col] are all parse results for text[col]..text[col+row] */
    val table : Array[Array[Seq[ParseResult]]] = new Array(length)
    val bottomRow : Array[Seq[ParseResult]] = Array.from(text.iterator.map { pr =>
      apply1Rules(Seq(pr)) })

//    println("bottomRow "+ bottomRow.toList)

    table.update(0, bottomRow)
    for (row <- 1 until length) {
      table.update(row, new Array(length - row))
      for (col <- 0 until length - row) {
//        println(s"Row=$row, col=$col (text $col .. ${row+col})")
        // Result from applying len2 rules
        val results2 =
          for (i <- 0 until row;
//               _ = println(s"i=$i");
               left <- table(i)(col);
               right <- table(row-i-1)(col+i+1);
//               _ = println(s"left=$left, right=$right");
               rhs2 = Len2Rhs(left.nonterminal, right.nonterminal);
//               _ = println(s"rhs2=$rhs2");
               lhs2 <- inverted2Rules.getOrElse(rhs2, Nil);
//               _ = println(s"lhs2=$lhs2")
               if lhs2.rhsRightMinPriority <= right.maxPriority
               if lhs2.rhsLeftMinPriority <= left.maxPriority;
               result = applyConstructor(lhs2, left, right))
            yield result

        val results1 = apply1Rules(results2)
        table(row).update(col, results1)
        }
      }
    val finalResults = table(length-1)(0)

    finalResults.toList collect {
      case ParseResultNormal(`nonterminal`, maxPriority, result) if maxPriority >= minPriority => result
    }
  }

  private def applyConstructor(lhs2: Len2Lhs, left: ParseResult, right: ParseResult): ParseResult = {
    val leftResult = left.asInstanceOf[ParseResultNormal].result
    val constructorArgs = right match {
      case Grammar.ParseResultNormal(_, _, result) => List(leftResult, result)
      case Grammar.ParseResultSynthetic(_, _, result) => leftResult :: result
    }

    lhs2.constructor match {
      case null => ParseResultSynthetic(lhs2.nonterminal, lhs2.maxPriority, constructorArgs)
      case constructor => ParseResultNormal(lhs2.nonterminal, lhs2.maxPriority, constructor(constructorArgs))
    }
  }

  def printGrammar(): Unit = {
    println("Len 1 rules:")
    for ((rhs, lhss) <- inverted1Rules;
         lhs <- lhss)
      println(s"  ${lhs.nonterminal}[${priorityString(lhs.maxPriority)}] -> ${rhs.nonterminal}[${priorityString(lhs.rhsMinPriority)}]")
      println("Len 2 rules:")
    for ((rhs, lhss) <- inverted2Rules;
         lhs <- lhss)
      println(s"  ${lhs.nonterminal}[${priorityString(lhs.maxPriority)}] -> " +
        s"${rhs.leftNonterminal}[${priorityString(lhs.rhsLeftMinPriority)}] " +
        s"${rhs.rightNonterminal}[${priorityString(lhs.rhsRightMinPriority)}]")
  }
}

object Utils {
  def updatedWithDefault[A,B](map: Map[A,B], default: B, key: A)(f: B=>B): Map[A, B] =
    map.updatedWith(key) { case None => Some(f(default)); case Some(value) => Some(f(value)) }
}


