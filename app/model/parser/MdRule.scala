package model.parser


sealed abstract class Expr
case class Equals(lhs: String, op: String, rhs: String) extends Expr
case class And(e1: Expr, e2: Expr) extends Expr

/**
 * A Matching Dependency Data Quality Rule
 */
class MdRule(ruleText: String) extends ParsedRule(ruleText) {
 
  private var _conditionalExpr: Expr = null
  private var _consequentMatchRelationName1 = "" 
  private var _consequentMatchAttribute1 = ""
  private var _consequentMatchRelationName2 = ""
  private var _consequentMatchAttribute2 = ""
  
  def conditionalExpr = _conditionalExpr
  def consequentMatchRelationName1 = _consequentMatchRelationName1
  def consequentMatchAttribute1 =  _consequentMatchAttribute1 
  def consequentMatchRelationName2 = _consequentMatchRelationName2 
  def consequentMatchAttribute2 =_consequentMatchAttribute2 
  
  def setRule(conditionalExpr: Expr, consequentMatchRelation1: String, consequentMatchRelation2: String): MdRule = {
      _conditionalExpr = conditionalExpr
      splitUpRelationIdentifier(consequentMatchRelation1) match  {  
        case (relationName: String, attributeName: String) 
          => _consequentMatchRelationName1 = relationName; _consequentMatchAttribute1 = attributeName
      }
      splitUpRelationIdentifier(consequentMatchRelation2) match  {  
        case (relationName: String, attributeName: String) 
          => _consequentMatchRelationName2 = relationName; _consequentMatchAttribute2 = attributeName
      }
      return this
  }
  
  private def splitUpRelationIdentifier(relationIdentifier: String): (String, String) =  {
    val relationParts = relationIdentifier.split("\\[")
    val relationName = relationParts(0)
    val attributeName = relationParts(1).replaceAll("\\]", "")
    (relationName, attributeName)
  }
  
  
  
  override def toString(): String = {
    /*val foldList = (attrList: String, attr: String) => attrList + { if (attrList.length > 0) "," else ""} + attr
    _conditionalEqualRelationName1 + "[" + _conditionalEqualAttributeList1.foldLeft("")(foldList) + "]" + "=" + 
    _conditionalEqualRelationName2 + "[" + _conditionalEqualAttributeList2.foldLeft("")(foldList) + "]" + "^" +
    _conditionalNotEqualRelationName1 + "[" + _conditionalNotEqualAttributeList1.foldLeft("")(foldList) + "]" + "!=" +
    _conditionalNotEqualRelationName2 + "[" + _conditionalNotEqualAttributeList2.foldLeft("")(foldList) + "]" + "->" +
    _consequentMatchRelationName1+ "[" + _consequentMatchAttributeList1.foldLeft("")(foldList) + "]" + "<->" +
    _consequentMatchRelationName2+ "[" + _consequentMatchAttributeList2.foldLeft("")(foldList) + "]"*/
    ""
  }
  
  
}