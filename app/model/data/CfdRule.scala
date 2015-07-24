package model.data

class CfdRule(id: Long, label: String, rule: String, dataset_id: Long) extends Rule(id,label,rule,dataset_id) {
  
  var _relationName = ""
  var _conditionalAttributeIdentifier = new Array[String](0) 
  var _conditionalValueIdentifier= new Array[String](0) 
  var _tupleName = ""
  var _consequentAttributeIdentifier = new Array[String](0)  
  var _consequentValueIdentifier = new Array[String](0) 
  
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
  
  def toMarkovLogic: Array[String] = {
    val cons = _consequentAttributeIdentifier zip _consequentValueIdentifier
    val cond = _conditionalAttributeIdentifier zip _conditionalValueIdentifier
    val condPart = cond.foldLeft("")((condTotal, condTuple) => condTotal + { if (condTotal.length > 0)  "^" else "" } 
      + condTuple._1 + "-" + _relationName + "(id1," + { if(condTuple._2 == "_") "val" + condTuple._1 + "1"  else condTuple._2 } + ")^"
      + condTuple._1 + "-" + _relationName + "(id2," + { if(condTuple._2 == "_") "val" + condTuple._1 + "2" else condTuple._2} + ")")
   for ((consAttr,consVal) <- cons) yield condPart + "->" + {if (consVal == "_") "eq" + _relationName+ "-" + consAttr + "(id1,id2)" 
     else consAttr + "(id1," + consVal + ")^" + consAttr + "(id2," + consVal  }
  } 
  
}

object CfdRule {
	
  /*val parser = new CfdRuleParser()
  
  def validate(rule: String, dataset: Dataset): (Boolean, String) = {
    val (parseSuccess, cfdRule) = parser.parse(rule)
    if (!parseSuccess) return (false, "Incorrect syntax: rule cannot be parsed.")
    val relation = dataset.relations().find { r => r.label == cfdRule.relationName }
    val relationExists = relation.isDefined
    val condColumnNumberMatch = 
      cfdRule.conditionalAttributeIdentifier.length == cfdRule.conditionalValueIdentifier.length
    val consColumnNumberMatch = 
      cfdRule.consequentAttributeIdentifier.length == cfdRule.consequentValueIdentifier.length
    val valid = consColumnNumberMatch && condColumnNumberMatch
    if (!valid) return return (false, "Number of columns and values in rule do not match.")
    return (true, "Rule is valid.")
  }
  
  def validate(rule: String): (Boolean, String) = {
    val (parseSuccess, cfdRule) = parser.parse(rule)
    if (!parseSuccess) return (false, "Incorrect syntax: rule cannot be parsed.")
    val condColumnNumberMatch = 
      cfdRule.conditionalAttributeIdentifier.length == cfdRule.conditionalValueIdentifier.length
    val consColumnNumberMatch = 
      cfdRule.consequentAttributeIdentifier.length == cfdRule.consequentValueIdentifier.length
    val valid = consColumnNumberMatch && condColumnNumberMatch
    if (!valid) return return (false, "Number of columns and values in rule do not match.")
    return (true, "Rule is valid.")
  }*/
}
