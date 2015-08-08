package model.parser

sealed abstract class Expr
case class Equals(lhs: String, operator: String, rhs: String) extends Expr
case class And(e1: Expr, e2: Expr) extends Expr

/**
 * A Matching Dependency Data Quality Rule
 */
class MdRule(ruleText: String) extends ParsedRule(ruleText) {
 
  var conditionalExpr: Expr = null
  var consequentMatchRelationName1 = "" 
  var consequentMatchAttribute1 = ""
  var consequentMatchRelationName2 = ""
  var consequentMatchAttribute2 = ""
  
  def setRule(conditionalExpr: Expr, consequentMatchRelation1: String, consequentMatchRelation2: String): MdRule = {
      this.conditionalExpr = conditionalExpr
      splitUpRelationIdentifier(consequentMatchRelation1) match  {  
        case (relationName: String, attributeName: String) 
          => consequentMatchRelationName1 = relationName; consequentMatchAttribute1 = attributeName
      }
      splitUpRelationIdentifier(consequentMatchRelation2) match  {  
        case (relationName: String, attributeName: String) 
          => consequentMatchRelationName2 = relationName; consequentMatchAttribute2 = attributeName
      }
      return this
  }
  
  private def splitUpRelationIdentifier(relationIdentifier: String): (String, String) =  {
    val relationParts = relationIdentifier.split("\\[")
    val relationName = relationParts(0)
    val attributeName = relationParts(1).replaceAll("\\]", "")
    (relationName, attributeName)
  }
  
}