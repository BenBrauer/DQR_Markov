package model.parser

/*
 * Is a Conditional functional dependency Data Quality rule. 
 */
class CfdRule(ruleText: String) extends ParsedRule(ruleText) {
  
  var relationName = ""
  var conditionalAttributeList = new Array[String](0) 
  var conditionalValueList= new Array[String](0) 
  var tupleName = ""
  var consequentAttributeList = new Array[String](0)  
  var consequentValueList = new Array[String](0) 
  
  /**
   * Sets the functional expression part of a conditional functional dependency
   * 
   * @param conditionalAttributeIdentifier the identifier for the conditional attribute 
   * @param consequentAttributeIdentifier the identifier for the consequent attribute
   */
  def setFunctionalExpression(conditionalAttributeIdentifier: String, consequentAttributeIdentifier: String): CfdRule = {
    this.conditionalAttributeList =  conditionalAttributeIdentifier.replace("[", "").replace("]", "").split(",")
    this.consequentAttributeList = consequentAttributeIdentifier.replace("[", "").replace("]", "").split(",")
    return this
  }
  
  /**
   * Sets the tuple part of a conditional functional dependency
   * 
   * @param tupleName name of the tuple 
   * @param conditionalValueIdentifier the identifier for the conditional value
   * @param consequentValueIdentifier the identifier for the consequent value
   */
  def setTuple(tupleName: String, conditionalValueIdentifier: String, consequentValueIdentifier: String): CfdRule = {
    this.tupleName = tupleName
    this.conditionalValueList = conditionalValueIdentifier.replace("[", "").replace("]", "").split(",")
    this.consequentValueList = consequentValueIdentifier.replace("[", "").replace("]", "").split(",")
    return this
  }    
  
  /**
   * Returns rule in first order logic notation
   */
  override def toString = {
    val combineCommaSep = (total: String, value: String) => total + {if (total.length > 0) "," else ""} + value
    "cfd: " +relationName + "([" + conditionalAttributeList.foldLeft("")(combineCommaSep) + "]->[" + 
    consequentAttributeList.foldLeft("")(combineCommaSep) + "]," +
     tupleName + "=(" + conditionalValueList.foldLeft("")(combineCommaSep) + "||" + 
     consequentValueList.foldLeft("")(combineCommaSep) + ")" 
  }
  
  
  
}


