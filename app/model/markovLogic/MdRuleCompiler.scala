package model.markovLogic

import model.parser.MdRule
import model.parser.Expr
import model.parser.Equals
import model.parser.And

/**
 * Is a compiler to create markov logic for a MdRule 
 */
object MdRuleCompiler {
   
  /**
   * Compiles MdRule to markov logic
   * 
   * @param rule MdRule to be compiled into markov logic
   */
  def apply(rule: MdRule): String = {
     return exprToMarkovLogic(rule.conditionalExpr) + " => " + 
      { 
        if(rule.consequentMatchAttribute1 == rule.consequentMatchAttribute2) "match" + rule.consequentMatchAttribute1 + "(" +
          rule.consequentMatchRelationName1 + "id," + rule.consequentMatchRelationName2 + "id)"
        else "match" + rule.consequentMatchAttribute1 + rule.consequentMatchAttribute2 + "(" +
          rule.consequentMatchRelationName1 + "id," + rule.consequentMatchRelationName2 + "id)"
      }
   }
  
  /**
   * Splits a relation identifier into a tuple of relation name and attribute name
   */
  private def splitUpRelationIdentifier(relationIdentifier: String): (String, String) =  {
    val relationParts = relationIdentifier.split("\\[")
    val relationName = relationParts(0)
    val attributeName = relationParts(1).replaceAll("\\]", "")
    (relationName, attributeName)
  }
  
  /**
   * Generates markov logic für an Expr instance
   */
  private def exprToMarkovLogic(expr: Expr): String = {
    expr match {
      case And(e1: Expr, e2:Expr) => exprToMarkovLogic(e1) + " ^ " + exprToMarkovLogic(e2)
      case Equals(lhs: String, operator: String, rhs: String) => { 
        return equationToMarkovLogic(lhs, operator, rhs)
      }
    }
  }
  
  /**
   * Generates markov logic for an equationb
   */
  private def equationToMarkovLogic(lhs: String, op: String, rhs: String): String = {
    val (lhRelationName, lhAttributeName) = splitUpRelationIdentifier(lhs)
    val (rhRelationName, rhAttributeName) = splitUpRelationIdentifier(rhs)
    val lhMarkovLogic =
      if (op == "!=") lhRelationName + "-" + lhAttributeName + "(" + lhRelationName + "id,val" + lhAttributeName + "1) ^ !" +
        lhRelationName + "-" + lhAttributeName + "(" + lhRelationName + "id,val" + lhAttributeName + "2)" 
      else lhRelationName + "-" + lhAttributeName + "(" + lhRelationName + "id,val" + lhAttributeName + ")"
    val rhMarkovLogic = 
      if (op == "!=")  rhRelationName + "-" + rhAttributeName + "(" + rhRelationName + "id,val" + rhAttributeName + "2) ^ !" +
        rhRelationName + "-" + rhAttributeName + "(" + rhRelationName + "id,val" + rhAttributeName + "1)"
      else  rhRelationName + "-" + rhAttributeName + "(" + rhRelationName + "id,val" + rhAttributeName + ")"
    return lhMarkovLogic + " ^ " + rhMarkovLogic
  }
  
  
}