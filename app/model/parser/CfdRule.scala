package model.parser

/*
 * A Conditional functional dependency Data Quality rule. 
 */
class CfdRule(ruleText: String) extends ParsedRule(ruleText) {
  
  private var _relationName = ""
  private var _conditionalAttributeIdentifier = new Array[String](0) 
  private var _conditionalValueIdentifier= new Array[String](0) 
  private var _tupleName = ""
  private var _consequentAttributeIdentifier = new Array[String](0)  
  private var _consequentValueIdentifier = new Array[String](0) 
  
  def relationName = _relationName
  def relationName_= (value:String):CfdRule = {
    _relationName = value
    return this
  } 
  
  def conditionalAttributeIdentifier = _conditionalAttributeIdentifier
  def conditionalValueIdentifier = _conditionalValueIdentifier
  def consequentAttributeIdentifier = _consequentAttributeIdentifier
  def consequentValueIdentifier = _consequentValueIdentifier
  
  def setConditionalExpression(conditionalAttributeIdentifier: String, consequentAttributeIdentifier: String): CfdRule = {
    _conditionalAttributeIdentifier =  conditionalAttributeIdentifier.replace("[", "").replace("]", "").split(",")
    _consequentAttributeIdentifier = consequentAttributeIdentifier.replace("[", "").replace("]", "").split(",")
    return this
  }
  
  def setTuple(tupleName: String, conditionalValueIdentifier: String, consequentValueIdentifier: String): CfdRule = {
    _tupleName = tupleName
    _conditionalValueIdentifier = conditionalValueIdentifier.replace("[", "").replace("]", "").split(",")
    _consequentValueIdentifier = consequentValueIdentifier.replace("[", "").replace("]", "").split(",")
    return this
  }    
  
  override def toString = {
    val combineCommaSep = (total: String, value: String) => total + {if (total.length > 0) "," else ""} + value
    "cfd: " +_relationName + "([" + conditionalAttributeIdentifier.foldLeft("")(combineCommaSep) + "]->[" + 
    _consequentAttributeIdentifier.foldLeft("")(combineCommaSep) + "]," +
     _tupleName + "=(" + _conditionalValueIdentifier.foldLeft("")(combineCommaSep) + "||" + 
     _consequentValueIdentifier.foldLeft("")(combineCommaSep) + ")" 
  }
  
  
  
}


