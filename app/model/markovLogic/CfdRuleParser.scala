package model.markovLogic

import scala.util.parsing.combinator._
import model.data._

class CfdRuleParser() extends RegexParsers {
  
  var _cfdRule: CfdRule = null
  
  //conditional part of a CFD Rule
  def attributeIdentifier = """\[([a-zA-Z0-9-_]+,)*[a-zA-Z0-9-_]+\]""".r 
  def conditional = """-\>""".r 
  def conditionalExpression: Parser[CfdRule] = attributeIdentifier ~ conditional ~ attributeIdentifier ^^ { 
    case condAI ~ _ ~ consAI => _cfdRule.setConditionalExpression(condAI, consAI)  }
  
  //tuple part of a CFD Rule
  def valueIdentifier = """(([a-zA-Z0-9-]+|_),?)*([a-zA-Z0-9-]+|_)""".r 
  def tupleIdentifier = """t[0-9]+""".r 
  def tuple: Parser[CfdRule] = 
    tupleIdentifier ~ """=\(""".r ~ valueIdentifier ~"""\|\|""".r ~ valueIdentifier ~ """\)""".r ^^ {
      case ti ~ _ ~ condVI ~ _ ~ consVI ~ _ => _cfdRule.setTuple(ti, condVI, consVI)}
  
  def relationIdentifier: Parser[CfdRule] = """[a-zA-Z0-9]+""".r ^^ {
    case ri => _cfdRule.relationName_=(ri)
  } 
  def rule: Parser[CfdRule] = """cfd[0-9]*:\s*""".r ~ relationIdentifier ~
    """\(""".r ~ conditionalExpression ~ """,""".r  ~ tuple ~ """\)""".r ^^ { _ => this._cfdRule}
  
  def parse(cfdRule: CfdRule): (Boolean, CfdRule) ={ 
    _cfdRule = cfdRule
    parseAll(rule, _cfdRule.rule) match {
      case Success(result, _) => (true,result)
      case failure : NoSuccess => (false, null)
    }
  }
  
  
  
}