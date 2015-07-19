package model.data


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
    var valCounter = 0
    expr match {
      case And(e1: Expr, e2:Expr) => exprToMarkovLogic(e1) + " ^ " + exprToMarkovLogic(e2)
      case Equals(lhs: String, op: String, rhs: String) => { 
        val (equationMarkovLogic, newValCounter) = equationToMarkovLogic(lhs, op, rhs, valCounter) 
        valCounter = newValCounter
        equationMarkovLogic
      }
    }
  }
  
  private def equationToMarkovLogic(lhs: String, op: String, rhs: String, valCounter: Int): (String,Int) = {
    //TODO: Operators = and !=
    val (lhRelationName, lhAttributeName) = splitUpRelationIdentifier(lhs)
    val newValCounter1 = valCounter + 1 
    val lhMarkovLogic = lhRelationName + "-" + lhAttributeName + "(id1,val"  + 
      newValCounter1.toString() + ")"
    val (rhRelationName, rhAttributeName) = splitUpRelationIdentifier(rhs)
    val newValCounter2 = valCounter + 1 
    val rhMarkovLogic = rhRelationName + "-" + rhAttributeName + "(id2,val"  + 
      newValCounter2.toString() + ")"
    val additionalMarkovLogic = if (op == "~") " ^ similar(val" + newValCounter1 + ",val" + newValCounter2 + ")"
    return (lhMarkovLogic + " ^ " + rhMarkovLogic + additionalMarkovLogic, newValCounter2)
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
        if(_consequentMatchAttribute1 == _consequentMatchAttribute2) "match" + _consequentMatchAttribute1 + "(id1,id2)"
        else "match" + _consequentMatchAttribute1 + _consequentMatchAttribute2 + "(id1,id2)"
      }
  }
  
}