package model.markovLogic

import model.data._

//TODO Dataset
class CfdRule() {
  
  private var _relationName = ""
  def relationName_= (value:String):String = {
    _relationName = value
    ""
  } 
  
  private var _conditionalAttributeIdentifier = ""
 
  private var _consequentAttributeIdentifier = ""
  
  def setConditionalExpression(conditionalAttributeIdentifier: String, consequentAttributeIdentifier: String): String = {
    _conditionalAttributeIdentifier = conditionalAttributeIdentifier
    _consequentAttributeIdentifier = consequentAttributeIdentifier
    return ""
  }
  
  private var _tupleName = ""
  
  private var _conditionalValueIdentifier = ""
  
  private var _consequentValueIdentifier = ""
  
  def setTuple(tupleName: String, conditionalValueIdentifier: String, consequentValueIdentifier: String): String = {
    _tupleName = tupleName
    _conditionalValueIdentifier = conditionalValueIdentifier
    _consequentValueIdentifier = consequentValueIdentifier
    ""
  }
  //TODO:
  def isValid = false
  
  override def toString = _relationName + "(" + _conditionalAttributeIdentifier + "->" + _consequentAttributeIdentifier +
                          "," + _tupleName + "=(" + _conditionalValueIdentifier + "||" + _consequentValueIdentifier + ")" 
  
}


