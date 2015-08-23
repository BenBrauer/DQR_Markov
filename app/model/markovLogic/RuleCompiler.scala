package model.markovLogic

import model.data.Rule
import model.parser._

/**
 * Is a compiler to generate markov logic for data quality rules
 */
object RuleCompiler {
  
  /**
   * Generates markov logic for the rule
   * 
   * @param rule the rule markov logic should be generated for
   */
  def apply(rule: Rule): String = {
    RuleParser(rule) match {
      case cfdRule: CfdRule => CfdRuleCompiler(cfdRule)
      case mdRule: MdRule => MdRuleCompiler(mdRule)  
    
    }
  } 
}