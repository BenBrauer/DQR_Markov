package model.markovLogic

import model.data.Rule
import model.parser._

/**
 * 
 */
object RuleCompiler {
  def apply(rule: Rule): String = {
    RuleParser(rule) match {
      case cfdRule: CfdRule => CfdRuleCompiler(cfdRule)
      case mdRule: MdRule => MdRuleCompiler(mdRule)  
    
    }
  } 
}