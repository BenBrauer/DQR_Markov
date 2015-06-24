package model.markovLogic

class MdRule {
  
  var _conditionalEqualRelationName1 = "" 
  var _conditionalEqualAttributeList1 = new Array[String](0)
  var _conditionalEqualRelationName2 = ""
  var _conditionalEqualAttributeList2 = new Array[String](0)
  
  var _conditionalNotEqualRelationName1 = "" 
  var _conditionalNotEqualAttributeList1 = new Array[String](0)
  var _conditionalNotEqualRelationName2 = ""
  var _conditionalNotEqualAttributeList2 = new Array[String](0)
  
  var _consequentMatchRelationName1 = "" 
  var _consequentMatchAttributeList1 = new Array[String](0)
  var _consequentMatchRelationName2 = ""
  var _consequentMatchAttributeList2 = new Array[String](0)
  
  def setRule(conditionalEqualRelation1: String, conditionalEqualRelation2: String,
    conditionalNotEqualRelation1: String, conditionalNotEqualRelation2: String,
    consequentMatchRelation1: String, consequentMatchRelation2: String): MdRule = {
      splitUpRelationIdentifier(conditionalEqualRelation1) match  {  
        case (relationName: String, attributeList: Array[String]) 
          => _conditionalEqualRelationName1 = relationName; _conditionalEqualAttributeList1 = attributeList
      }
      splitUpRelationIdentifier(conditionalEqualRelation2) match  {  
        case (relationName: String, attributeList: Array[String]) 
          => _conditionalEqualRelationName2 = relationName; _conditionalEqualAttributeList2 = attributeList
      }
      splitUpRelationIdentifier(conditionalNotEqualRelation1) match  {  
        case (relationName: String, attributeList: Array[String]) 
          => _conditionalNotEqualRelationName1 = relationName; _conditionalNotEqualAttributeList1 = attributeList
      }
      splitUpRelationIdentifier(conditionalNotEqualRelation1) match  {  
        case (relationName: String, attributeList: Array[String]) 
          => _conditionalNotEqualRelationName2 = relationName; _conditionalNotEqualAttributeList2 = attributeList
      }
      splitUpRelationIdentifier(consequentMatchRelation1) match  {  
        case (relationName: String, attributeList: Array[String]) 
          => _consequentMatchRelationName1 = relationName; _consequentMatchAttributeList1 = attributeList
      }
      splitUpRelationIdentifier(consequentMatchRelation2) match  {  
        case (relationName: String, attributeList: Array[String]) 
          => _consequentMatchRelationName2 = relationName; _consequentMatchAttributeList2 = attributeList
      }
      return this
  }
  
  private def splitUpRelationIdentifier(relationIdentifier: String): (String, Array[String]) =  {
    val relationParts = relationIdentifier.split("\\[")
    val relationName = relationParts(0)
    val attributeList = relationParts(1).replaceAll("\\]", "").split(",")
    (relationName, attributeList)
  }
  
  override def toString(): String = {
    val foldList = (attrList: String, attr: String) => attrList + { if (attrList.length > 0) "," else ""} + attr
    _conditionalEqualRelationName1 + "[" + _conditionalEqualAttributeList1.foldLeft("")(foldList) + "]" + "=" + 
    _conditionalEqualRelationName2 + "[" + _conditionalEqualAttributeList2.foldLeft("")(foldList) + "]" + "^" +
    _conditionalNotEqualRelationName1 + "[" + _conditionalNotEqualAttributeList1.foldLeft("")(foldList) + "]" + "!=" +
    _conditionalNotEqualRelationName2 + "[" + _conditionalNotEqualAttributeList2.foldLeft("")(foldList) + "]" + "->" +
    _consequentMatchRelationName1+ "[" + _consequentMatchAttributeList1.foldLeft("")(foldList) + "]" + "<->" +
    _consequentMatchRelationName2+ "[" + _consequentMatchAttributeList2.foldLeft("")(foldList) + "]" 
  }
  
  override def toMarkovLogic(): String = {
    
  }
  
}