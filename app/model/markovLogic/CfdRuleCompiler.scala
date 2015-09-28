package model.markovLogic

import model.parser.CfdRule

/**
 * Is a compiler for CfdRules to markov logic
 */
object CfdRuleCompiler {
 
  /**
   * Compiles CfdRules to markov mogic
   * 
   * @param rule the CfdRule object to be compiled to markov logic
   */
  def apply(rule: CfdRule): String = {
    //create attribute value pairs 
    val cons =  rule.consequentAttributeList zip rule.consequentValueList
    val cond = rule.conditionalAttributeList zip rule.conditionalValueList
    //create Markov Logic for conditional attribute value pairs   
    val condPart = cond.foldLeft("")((condTotal, condTuple) => {
      val condAttr = condTuple._1
      val condVal = condTuple._2
      condTotal + { if (condTotal.length > 0)  "^" else "" } + 
        rule.relationName + "-" + condAttr + "(id1," + { if(condVal == "_") "val" + condAttr + "1"  else condVal } + ")^" +
        rule.relationName + "-" + condAttr + "(id2," + { if(condVal == "_") "val" + condAttr + "2" else condVal } + ")"
    })
    //create Markov Logic for consequent attribute value pairs
    val mlRules = for ((consAttr,consVal) <- cons) yield condPart + "->" + {if (consVal == "_") "eq" + rule.relationName + "-" + consAttr + "(id1,id2)" 
     else rule.relationName + "-" + consAttr + "(id1," + consVal + ")^" + rule.relationName + "-" + consAttr + "(id2," + consVal  }
    return mlRules.foldLeft("")(_+ "\n" +_) 
   }
  
}