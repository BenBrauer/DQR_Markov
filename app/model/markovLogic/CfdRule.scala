package model.markovLogic

import model.data._

//TODO Dataset
class CfdRule() {
  
  private var _relationName = ""
  def relationName_= (value:String):String = {
    _relationName = value
    ""
  } 
  
  var _conditionalAttributeIdentifier: Array[String] 
 
  var _consequentAttributeIdentifier: Array[String] 
  
  def setConditionalExpression(conditionalAttributeIdentifier: String, consequentAttributeIdentifier: String): String = {
    var condAttId = conditionalAttributeIdentifier.replace("[", "").replace("]", "")
    var consAttId = consequentAttributeIdentifier.replace("[", "").replace("]", "")
    _conditionalAttributeIdentifier = condAttId.split(",")
    _consequentAttributeIdentifier = consAttId.split(",")
    return ""
  }
  
  private var _tupleName = ""
  
  var _conditionalValueIdentifier: Array[String] 
  
  var _consequentValueIdentifier: Array[String]
  
  def setTuple(tupleName: String, conditionalValueIdentifier: String, consequentValueIdentifier: String): String = {
    _tupleName = tupleName
    _conditionalValueIdentifier = conditionalValueIdentifier.replace("[", "").replace("]", "").split(",")
    _consequentValueIdentifier = consequentValueIdentifier.replace("[", "").replace("]", "").split(",")
    ""
  }
  //TODO:
  def isValid = false
  
  override def toString = _relationName + "(" + _conditionalAttributeIdentifier + "->" + _consequentAttributeIdentifier +
                          "," + _tupleName + "=(" + _conditionalValueIdentifier + "||" + _consequentValueIdentifier + ")" 
  
  def toMarkovLogic: Array[String] = {
    val cons = _consequentAttributeIdentifier zip _consequentValueIdentifier
    val cond = _conditionalAttributeIdentifier zip _conditionalValueIdentifier
    val condPart = cond.foldLeft("")((condTotal, condTuple) => condTotal + { if (condTotal.length > 0)  "^" else "" } 
      + condTuple._1 + "(id1," + { if(condTuple._2 == "_") "value"  else condTuple._2 } + ")^"
      + condTuple._1  + "(id2," + {if(condTuple._2 == "_") "value" else condTuple._2} + ")")
   for ((consAttr,consVal) <- cons) yield condPart + "->" + {if (consVal == "_") "SAME" + consAttr + "(id1,id2)" 
     else consAttr + "(id1," + consVal + ")^" + consAttr + "(id2," + consVal  }
  } 
                              
  
                          
}


