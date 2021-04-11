package de.unruh.rendermath

import Grammar.{Branch, Branch1, DemoTree, Leaf, Len1Lhs, Len1Rhs, Len2Lhs, Len2Rhs, Nonterminal, ParseResult, Priority, Text, TreeTag, largestPriority, smallestPriority, syntheticPriority, uniqueNumber}
import Utils.updatedWithDefault

object Grammar {
  type Terminal = String
  type Nonterminal = String
  type Priority = Int
  type TreeTag = String
  type Text = String

  type NonterminalIdx = Int

  final case class ParseResult(nonterminal: Nonterminal, maxPriority: Priority, result: DemoTree) {
    override def toString: Terminal = s"$nonterminal[$maxPriority]=$result"
  }

  private var counter = 0
  def uniqueNumber(): Int = { counter += 1; counter }

  def priorityString(priority: Priority): String =
    if (priority == smallestPriority) "*"
    else if (priority == syntheticPriority) "@"
    else priority.toString


  final case class Len2Rhs(leftNonterminal: Nonterminal/*, leftMinPriority: Priority*/,
                           rightNonterminal: Nonterminal/*, rightMinPriority: Priority*/) {
//    override def toString: String = s"$leftNonterminal[${priorityString(leftMinPriority)}] $rightNonterminal[${priorityString(rightMinPriority)}]"
  }
  final case class Len1Rhs(nonterminal: Nonterminal/*, minPriority: Priority*/) {
//    override def toString: String = s"$nonterminal[${priorityString(minPriority)}]"
  }

  sealed trait DemoTree
  final case class Leaf(name: TreeTag) extends DemoTree {
    override def toString: Terminal = name
  }
  final case class Branch(name: TreeTag, left: DemoTree, right: DemoTree) extends DemoTree {
    override def toString: Terminal = s"$name($left,$right)"
  }
  final case class Branch1(name: TreeTag, child: DemoTree) extends DemoTree {
    override def toString: Terminal = s"$name($child)"
  }

  final case class Len2Lhs(rhsLeftMinPriority: Priority, rhsRightMinPriority: Priority,
                           nonterminal: Nonterminal, maxPriority: Priority, treeTag: String) {
//    override def toString: String = s"$nonterminal[${priorityString(maxPriority)}] /*$treeTag*/"
  }
  final case class Len1Lhs(rhsMinPriority: Priority, nonterminal: Nonterminal, maxPriority: Priority, treeTag: String) {
//    override def toString: String = s"$nonterminal[${priorityString(maxPriority)}] /*$treeTag*/"
  }

  val empty = new Grammar(Map.empty, Map.empty,
    { _ => throw new UnsupportedOperationException("No tokenizer configured") })

  val smallestPriority: Priority = Int.MinValue
  val largestPriority: Priority = Int.MaxValue
  val syntheticPriority: Priority = 1212121212
}


final case class Grammar private (private val inverted2Rules: Map[Len2Rhs, List[Len2Lhs]],
                                  private val inverted1Rules: Map[Len1Rhs, List[Len1Lhs]],
                                  private val tokenizer: Text => Seq[ParseResult]) {
  def setTokenizer(tokenizer: Text => Seq[(Nonterminal, TreeTag)]): Grammar =
    copy(tokenizer = { text => tokenizer(text) map { case (nt, tag) => ParseResult(nt, largestPriority, Leaf(tag)) } })

  def addRule(rule: Rule): Grammar = rule match {
    case Rule(nonterminal, maxPriority, Seq(singleRhs), treeTag) =>
      val rhs = Len1Rhs(nonterminal = singleRhs.nonterminal)
      val lhs = Len1Lhs(nonterminal = nonterminal, maxPriority = maxPriority, treeTag = treeTag,
        rhsMinPriority = singleRhs.minPriority)
      this.copy(inverted1Rules = updatedWithDefault(inverted1Rules, Nil, rhs) { lhs :: _ })
    case Rule(nonterminal, maxPriority, Seq(), treeTag) =>
      ???
    case Rule(nonterminal, maxPriority, rhs, treeTag) =>
      var grammar: Grammar = this
      val rhsTailRev = rhs.tail.reverse
      var rhsEnd = rhsTailRev.head
      for (rhsElem <- rhsTailRev.tail) {
        val rhs2 = Len2Rhs(leftNonterminal = rhsElem.nonterminal,
          rightNonterminal = rhsEnd.nonterminal)
        // TODO reuse existing ones
        // TODO: Use compact name (or different type for synthetic nonterminals)
        val lhs2 = Len2Lhs(
          rhsLeftMinPriority = rhsElem.minPriority, rhsRightMinPriority = rhsEnd.minPriority,
          nonterminal=s"#${uniqueNumber()}", maxPriority=syntheticPriority, treeTag="SYNTHETIC")
        rhsEnd = RhsElement(lhs2.nonterminal, lhs2.maxPriority)
        grammar = grammar.copy(inverted2Rules =
          updatedWithDefault(grammar.inverted2Rules, Nil, rhs2) { lhs2 :: _ })
      }

      val rhsHead = rhs.head
      val rhs2 = Len2Rhs(leftNonterminal = rhsHead.nonterminal,
        rightNonterminal = rhsEnd.nonterminal)
      val lhs2 = Len2Lhs(nonterminal = nonterminal, maxPriority = maxPriority, treeTag = treeTag,
        rhsLeftMinPriority = rhsHead.minPriority, rhsRightMinPriority = rhsEnd.minPriority)
      grammar.copy(inverted2Rules =
        updatedWithDefault(grammar.inverted2Rules, Nil, rhs2) { lhs2 :: _ })
  }

  def matches(nonterminal: Nonterminal, minPriority: Priority = smallestPriority, text: Text) = {
    matchesNonterminalList(nonterminal, minPriority, tokenizer(text))
  }

  def matchesNonterminalList(nonterminal: Nonterminal, minPriority: Priority = smallestPriority,
                             text: Seq[ParseResult]): List[DemoTree] = {
    def apply1Rules(results: Seq[ParseResult]): List[ParseResult] = {
      // Apply len1 rules
      var results1 : List[ParseResult] = Nil
      var results2ToAdd = results.toList
      while (results2ToAdd.nonEmpty) {
        val resultToAdd = results2ToAdd.head
        results2ToAdd = results2ToAdd.tail
        results1 = resultToAdd :: results1
        for (lhs <- inverted1Rules.getOrElse(Len1Rhs(resultToAdd.nonterminal), Nil);
             if lhs.rhsMinPriority <= resultToAdd.maxPriority) {
          val newResult = ParseResult(lhs.nonterminal, lhs.maxPriority, Branch1(lhs.treeTag, resultToAdd.result))
          println(s"newResult=$newResult")
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
    table.update(0, bottomRow)
    for (row <- 1 until length) {
      table.update(row, new Array(length - row))
      for (col <- 0 until length - row) {
        println(s"Row=$row, col=$col (text $col .. ${row+col})")
        // Result from applying len2 rules
        val results2 =
          for (i <- 0 until row;
               _ = println(s"i=$i");
               left <- table(i)(col);
               right <- table(row-i-1)(col+i+1);
               _ = println(s"left=$left, right=$right");
               rhs2 = Len2Rhs(left.nonterminal, right.nonterminal);
               _ = println(s"rhs2=$rhs2");
               lhs2 <- inverted2Rules.getOrElse(rhs2, Nil);
               _ = println(s"lhs2=$lhs2");
               if lhs2.rhsRightMinPriority <= right.maxPriority;
               if lhs2.rhsLeftMinPriority <= left.maxPriority;
               result = Branch(lhs2.treeTag, left.result, right.result);
               parseResult = ParseResult(lhs2.nonterminal, lhs2.maxPriority, result))
            yield parseResult

        println(s"Results2=$results2")

        val results1 = apply1Rules(results2)

        println(s"results1=$results1")
        table(row).update(col, results1)
        }
      }
    val finalResults = table(length-1)(0)
    finalResults.withFilter(_.maxPriority >= minPriority).map(_.result).toList
    }


  def printGrammar(): Unit = {
    println("Len 1 rules:")
    for ((rhs, lhss) <- inverted1Rules;
         lhs <- lhss)
      println(s"  $lhs -> $rhs")
    println("Len 2 rules:")
    for ((rhs, lhss) <- inverted2Rules;
         lhs <- lhss)
      println(s"  $lhs -> $rhs")
  }
}

object Utils {
  def updatedWithDefault[A,B](map: Map[A,B], default: B, key: A)(f: B=>B): Map[A, B] =
    map.updatedWith(key) { case None => Some(f(default)); case Some(value) => Some(f(value)) }
}

case class Rule(nonterminal: Nonterminal, maxPriority: Int, pieces: List[RhsElement], treeTag: TreeTag) {
  override def toString: Nonterminal = s"$nonterminal[$maxPriority] -> ${pieces.mkString(" ")}"
}

final case class RhsElement(nonterminal: Nonterminal, minPriority: Priority) {
  override def toString: Nonterminal = s"$nonterminal[$minPriority]"
}

