package model.data

import org.mockito.internal.stubbing.ConsecutiveStubbing


sealed abstract class Expr
case class Equals(lhs: String, op: String, rhs: String) extends Expr
case class And(e1: Expr, e2: Expr) extends Expr


class MdRule(id: Long, label: String, rule: String, dataset_id: Long) extends Rule(id, label, rule, dataset_id) {
  
 
  var _conditionalExpr: Expr = null
  var _consequentMatchRelationName1 = "" 
  var _consequentMatchAttribute1 = ""
  var _consequentMatchRelationName2 = ""
  var _consequentMatchAttribute2 = ""
  
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
  
  private def exprToMarkovLogic(expr: Expr): String = {
    expr match {
      case And(e1: Expr, e2:Expr) => exprToMarkovLogic(e1) + " ^ " + exprToMarkovLogic(e2)
      case Equals(lhs: String, op: String, rhs: String) => { 
        return equationToMarkovLogic(lhs, op, rhs)
      }
    }
  }
  
  private def equationToMarkovLogic(lhs: String, op: String, rhs: String): String = {
    //TODO: Operators = and !=
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
    //val additionalMarkovLogic = if (op == "~") " ^ similar(val" + newValCounter1 + ",val" + newValCounter2 + ")"
    return lhMarkovLogic + " ^ " + rhMarkovLogic
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
  
  def toMarkovLogic(): String = {
      return exprToMarkovLogic(_conditionalExpr) + " => " + 
      { 
        if(_consequentMatchAttribute1 == _consequentMatchAttribute2) "match" + _consequentMatchAttribute1 + "(" +
          _consequentMatchRelationName1 + "id," + _consequentMatchRelationName2 + "id)"
        else "match" + _consequentMatchAttribute1 + _consequentMatchAttribute2 + "(" +
          _consequentMatchRelationName1 + "id," + _consequentMatchRelationName2 + "id)"
      }
  }
  
}