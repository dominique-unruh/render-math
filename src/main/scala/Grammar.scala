package de.unruh.rendermath

import Grammar.{Constructor, Nonterminal, Priority, Result, Terminal}

import de.unruh.rendermath.Utils.updatedWithDefault

object Grammar {
  type Terminal = String
  type Nonterminal = String
  type Priority = Int
  type Result = String
  type Constructor = List[Result] => Result

  val empty = new Grammar(Map.empty)
}


final class Grammar private (val ruleMap: Map[Nonterminal, RuleSet]) {
  def matches(nonterminal: Nonterminal, priority: Priority, tokens: List[Terminal]): List[Result] = {
    val ruleset = ruleMap.getOrElse(nonterminal, throw new IllegalArgumentException(s"Unknown nonterminal $nonterminal"))
    for ((_, result, Nil) <- ruleset.matches(this, priority, tokens, Nil))
      yield result()
  }

  def addRule(rule: Rule): Grammar = {
    val newRules = updatedWithDefault(ruleMap, RuleSet.empty, rule.nonterminal) {
      _.addRule(rule.priority, rule.constructor, rule.pieces.toList) }
    new Grammar(newRules)
  }

  def rules: List[Rule] =
    for ((nt, ruleset) <- ruleMap.toList;
         rule <- ruleset.rules(nt, Nil))
    yield rule
}

object Utils {
  def updatedWithDefault[A,B](map: Map[A,B], default: B, key: A)(f: B=>B): Map[A, B] =
    map.updatedWith(key) { case None => Some(f(default)); case Some(value) => Some(f(value)) }
}


/** Represents a multiset of production rule right hand sides. The multiset contains:
 * - For each (terminal, ruleset), and each rule in ruleset: terminal :: rule
 * - For each (pri, contructor): empty rule
 * - For each (nonterminal, priority, ruleset) in nonterminals and each rule in ruleset: (nonterminal, priority) :: rule
 * */
final class RuleSet private (terminals: Map[Terminal, RuleSet],
                             finals: List[(Priority, Constructor)],
                             nonterminals: Map[Nonterminal, Map[Priority, RuleSet]]) {
  /**
   * Tries to match all the rules in this ruleset.
   * Matches only with those rules that have a priority >= `minPriority`
   * Returns a list of all matches, each together with the priority of the matching rule, and with any unparsed tokens
   * @param minPriority
   * @param tokens
   * @param revPrefix addinational arguments to pass to the constructor of the rule (prefix, reversed)
   * @return
   */
  // TODO: Fast stop if there are no rules of suitable priority
  def matches(grammar: Grammar, minPriority: Priority, tokens: List[Terminal], revPrefix: List[() => Result]): List[(Priority, () => Result, List[Terminal])] = {
    val matches1 = finals collect { case (pri, constructor) if pri >= minPriority =>
      (pri, () => constructor(revPrefix.reverse.map(_())), tokens) }

    println(tokens, this.rules("?", Nil))

    val matches2 = tokens match {
      case Nil => Nil
      case token :: tokens =>
        terminals.get(token) match {
          case None => Nil
          case Some(ruleset) => ruleset.matches(grammar, minPriority, tokens, revPrefix)
        }
    }

    val matches3 = for ((nt, restRulesets) <- nonterminals.toList;
                            ntRuleset = grammar.ruleMap.getOrElse(nt, throw new IllegalArgumentException(s"Nonexisting nonterminal $nt"));
                            ntMinPri = restRulesets.keys.min;
                            (ntMatchPri, ntMatchResult, restTokens) <- ntRuleset.matches(grammar, ntMinPri, tokens, Nil);
                            (ntAnnotatedPri, restRuleset) <- restRulesets;
                            if ntAnnotatedPri <= ntMatchPri;
                            result <- restRuleset.matches(grammar, minPriority, restTokens, ntMatchResult :: revPrefix))
        yield result
    matches1 ::: matches2 ::: matches3
  }

  def rules(nonterminal: Nonterminal, prefixRev: List[Piece]): List[Rule] = {
    val rules1 =
      for ((terminal, ruleset) <- terminals.toList;
           rule <- ruleset.rules(nonterminal, TerminalPiece(terminal) :: prefixRev))
      yield rule
    val rules2 =
      for ((priority, constructor) <- finals)
        yield Rule(nonterminal, priority, constructor, prefixRev.reverse :_*)
    val rules3 =
      for ((nt, priRuleset) <- nonterminals.toList;
           (pri, rules) <- priRuleset.toList;
           rule <- rules.rules(nonterminal, NonterminalPiece(nt, pri) :: prefixRev))
      yield rule
    rules1 ::: rules2 ::: rules3
  }

  def addRule(priority: Priority, constructor: Constructor, pieces: List[Piece]): RuleSet = pieces match {
    case Nil =>
      val newFinals = (priority, constructor) :: finals
      new RuleSet(terminals, newFinals, nonterminals)
    case TerminalPiece(terminal) :: pieces =>
      val newTerminals = Utils.updatedWithDefault(terminals, RuleSet.empty, terminal) {
        _.addRule(priority, constructor, pieces) }
      new RuleSet(newTerminals, finals, nonterminals)
    case NonterminalPiece(name, ntPriority) :: pieces =>
      val newNonterminals = {
        updatedWithDefault(nonterminals, Map.empty[Priority, RuleSet], name) {
          updatedWithDefault(_, RuleSet.empty, ntPriority) {
            _.addRule(priority, constructor, pieces)
          }
        }
      }
      new RuleSet(terminals, finals, newNonterminals)
  }
  /*
    val maxPriority : Priority = {
      val maxTerminalPri = (for (rulesets <- terminals.values; ruleset <- rulesets) yield ruleset.maxPriority).max
      val maxFinalsPri = (for ((pri,_) <- finals) yield pri).max
      val maxNTPri = (for ((_,_,ruleset) <- nonterminals) yield ruleset.maxPriority).max
      Seq(maxTerminalPri,maxFinalsPri,maxNTPri).max
    }
  */

  /*
    val minPriority : Priority = {
      val minTerminalPri = (for (rulesets <- terminals.values; ruleset <- rulesets) yield ruleset.minPriority).min
      val minFinalsPri = (for ((pri,_) <- finals) yield pri).min
      val minNTPri = (for ((_,_,ruleset) <- nonterminals) yield ruleset.minPriority).min
      Seq(minTerminalPri,minFinalsPri,minNTPri).min
    }
  */
}

object RuleSet {
  val empty = new RuleSet(Map.empty, Nil, Map.empty)
}

case class Rule(nonterminal: Nonterminal, priority: Int, constructor: Constructor, pieces: Piece*) {
  override def toString: Nonterminal = s"$nonterminal[$priority] -> ${pieces.mkString(" ")}"
}

sealed trait Piece
final case class NonterminalPiece(name: Nonterminal, priority: Int) extends Piece {
  override def toString: Nonterminal = s"$name[$priority]"
}
final case class TerminalPiece(value: Terminal) extends Piece {
  override def toString: Nonterminal = value
}

