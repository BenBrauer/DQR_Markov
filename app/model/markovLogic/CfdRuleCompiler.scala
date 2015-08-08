package model.markovLogic

import model.parser.CfdRule

object CfdRuleCompiler {
  
  //Regular expression for validation
  val RuleRegEx = ""
  
  def apply(rule: CfdRule): String = {
    val cons =  rule.consequentAttributeIdentifier zip rule.consequentValueIdentifier
    val cond = rule.conditionalAttributeIdentifier zip rule.conditionalValueIdentifier
    val condPart = cond.foldLeft("")((condTotal, condTuple) => condTotal + { if (condTotal.length > 0)  "^" else "" } 
      + condTuple._1 + "-" + rule.relationName + "(id1," + { if(condTuple._2 == "_") "val" + condTuple._1 + "1"  else condTuple._2 } + ")^"
      + condTuple._1 + "-" + rule.relationName + "(id2," + { if(condTuple._2 == "_") "val" + condTuple._1 + "2" else condTuple._2} + ")")
    val mlRules = for ((consAttr,consVal) <- cons) yield condPart + "->" + {if (consVal == "_") "eq" + rule.relationName + "-" + consAttr + "(id1,id2)" 
     else consAttr + "(id1," + consVal + ")^" + consAttr + "(id2," + consVal  }
    return mlRules.foldLeft("")(_+ "\n" +_) 
   }
  
  def validate(rule: String): Boolean = {
    return true
  }
  
}