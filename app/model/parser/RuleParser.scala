package model.parser

import scala.util.parsing.combinator.RegexParsers
import model.data.Rule

object RuleParser {
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