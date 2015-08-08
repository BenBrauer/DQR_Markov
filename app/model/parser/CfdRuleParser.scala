package model.parser

import scala.util.parsing.combinator._

/**
 * A parser for conditional functional dependency data quality rules in first order logic form
 */
class CfdRuleParser() extends RegexParsers {
  
  private var _cfdRule: CfdRule = null
  
  //conditional part of a CFD Rule
  private def attributeIdentifier = """\[([a-zA-Z0-9-_]+,)*[a-zA-Z0-9-_]+\]""".r 
  private def conditionalExpression: Parser[CfdRule] = attributeIdentifier ~ """-\>""".r  ~ attributeIdentifier ^^ { 
    case condAI ~ _ ~ consAI => _cfdRule.setConditionalExpression(condAI, consAI)  }
  
  //tuple part of a CFD Rule
  private def valueIdentifier = """(([a-zA-Z0-9-]+|_),?)*([a-zA-Z0-9-]+|_)""".r 
  private def tupleIdentifier = """t[0-9]+""".r 
  private def tuple: Parser[CfdRule] = 
    tupleIdentifier ~ """=\(""".r ~ valueIdentifier ~"""\|\|""".r ~ valueIdentifier ~ """\)""".r ^^ {
      case ti ~ _ ~ condVI ~ _ ~ consVI ~ _ => _cfdRule.setTuple(ti, condVI, consVI)}
  
  private def relationIdentifier = """[a-zA-Z0-9]+""".r ^^ {
    case ri => {
      _cfdRule.relationName = ri
      _cfdRule
    }
  } 
  
  private def rule: Parser[CfdRule] = """cfd[0-9]*:\s*""".r ~ relationIdentifier ~
    """\(""".r ~ conditionalExpression ~ """,""".r  ~ tuple ~ """\)""".r ^^ { _ => this._cfdRule}
  
  def parse(ruleText: String): (Boolean, CfdRule) ={ 
    _cfdRule = new CfdRule(ruleText)
    parseAll(rule, _cfdRule.ruleText) match {
      case Success(result, _) => (true,result)
      case failure : NoSuccess => (false, null)
    }
  }
  
  
  
}