package model.parser

import scala.util.parsing.combinator._

/**
 * A parser for conditional functional dependency data quality rules in first order logic form
 */
class CfdRuleParser() extends RegexParsers {
  
  private var _cfdRule: CfdRule = null
  
  //conditional part of a CFD Rule
  private val attributeIdentifier = """\[([a-zA-Z0-9-_]+,)*[a-zA-Z0-9-_]+\]""".r 
  private val functionalExpression: Parser[CfdRule] = attributeIdentifier ~ """-\>""".r  ~ attributeIdentifier ^^ { 
    case condAI ~ _ ~ consAI => _cfdRule.setFunctionalExpression(condAI, consAI)  }
  
  //tuple part of a CFD Rule
  private val valueIdentifier = """(([a-zA-Z0-9-]+|_),?)*([a-zA-Z0-9-]+|_)""".r 
  private val tupleIdentifier = """t[0-9]+""".r 
  private val tuple: Parser[CfdRule] = 
    tupleIdentifier ~ """=\(""".r ~ valueIdentifier ~"""\|\|""".r ~ valueIdentifier ~ """\)""".r ^^ {
      case ti ~ _ ~ condVI ~ _ ~ consVI ~ _ => _cfdRule.setTuple(ti, condVI, consVI)}
  
  private val relationIdentifier = """[a-zA-Z0-9]+""".r ^^ {
    case ri => {
      _cfdRule.relationName = ri
      _cfdRule
    }
  } 
  
  private val rulePrefix = """cfd[0-9]*:\s*""".r 
  
  private val rule: Parser[CfdRule] = rulePrefix ~ relationIdentifier ~
    """\(""".r ~ functionalExpression ~ """,""".r  ~ tuple ~ """\)""".r ^^ { _ => this._cfdRule}
  
  def parse(ruleText: String): (Boolean, CfdRule) ={ 
    _cfdRule = new CfdRule(ruleText)
    parseAll(rule, _cfdRule.ruleText) match {
      case Success(result, _) => (true,result)
      case failure : NoSuccess => (false, null)
    }
  }
  
  
  
}