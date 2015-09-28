package model.parser

import scala.util.parsing.combinator.RegexParsers
import model.data.Rule

/**
 * A general parser for data quality rules which identifies rule type and selects correct parser object
 */
object RuleParser {
  
  /**
   * Parses a rule instance and returns a ParsedRule data structure instance
   * 
   * @param ruleData Rule instance to be parsed
   */
  def apply(ruleData: Rule) : ParsedRule = {
    val mdRulePattern = """\A(md).*""".r
    val cfdRulePattern = """\A(cfd).*""".r
    var ruleInstance: ParsedRule = null
    ruleData.rule match {
      case mdRulePattern(m) => { 
        val parser = new MdRuleParser(); 
        parser.parse(ruleData.rule) match { case (result, md) => ruleInstance = md } 
      }
      case cfdRulePattern(m) => {
        val parser = new CfdRuleParser();
        parser.parse(ruleData.rule) match { case (result, cfd) => ruleInstance = cfd }
      }
    }
    return ruleInstance
  }
}