package model.parser

/*
 * A Conditional functional dependency Data Quality rule. 
 */
class CfdRule(ruleText: String) extends ParsedRule(ruleText) {
  
  var relationName = ""
  var conditionalAttributeIdentifier = new Array[String](0) 
  var conditionalValueIdentifier= new Array[String](0) 
  var tupleName = ""
  var consequentAttributeIdentifier = new Array[String](0)  
  var consequentValueIdentifier = new Array[String](0) 
  
  
  def setConditionalExpression(conditionalAttributeIdentifier: String, consequentAttributeIdentifier: String): CfdRule = {
    this.conditionalAttributeIdentifier =  conditionalAttributeIdentifier.replace("[", "").replace("]", "").split(",")
    this.consequentAttributeIdentifier = consequentAttributeIdentifier.replace("[", "").replace("]", "").split(",")
    return this
  }
  
  def setTuple(tupleName: String, conditionalValueIdentifier: String, consequentValueIdentifier: String): CfdRule = {
    this.tupleName = tupleName
    this.conditionalValueIdentifier = conditionalValueIdentifier.replace("[", "").replace("]", "").split(",")
    this.consequentValueIdentifier = consequentValueIdentifier.replace("[", "").replace("]", "").split(",")
    return this
  }    
  
  override def toString = {
    val combineCommaSep = (total: String, value: String) => total + {if (total.length > 0) "," else ""} + value
    "cfd: " +relationName + "([" + conditionalAttributeIdentifier.foldLeft("")(combineCommaSep) + "]->[" + 
    consequentAttributeIdentifier.foldLeft("")(combineCommaSep) + "]," +
     tupleName + "=(" + conditionalValueIdentifier.foldLeft("")(combineCommaSep) + "||" + 
     consequentValueIdentifier.foldLeft("")(combineCommaSep) + ")" 
  }
  
  
  
}


