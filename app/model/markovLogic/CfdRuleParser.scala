package model.markovLogic

import scala.util.parsing.combinator._

class CfdRuleParser(cfdRule: CfdRule) extends RegexParsers { 
  def attributeIdentifier = """\[([a-zA-Z0-9-_]+,)*[a-zA-Z0-9-_]+\]""".r 
  def conditional = """-\>""".r 
  def conditionalExpression: Parser[String] = attributeIdentifier ~ conditional ~ attributeIdentifier ^^ { 
    case condAI ~ _ ~ consAI => cfdRule.setConditionalExpression(condAI, consAI)  }
  def valueIdentifier: Parser[String] = """(("[a-zA-Z0-9-]+"|_),?)*("[a-zA-Z0-9-]+"|_)""".r ^^ {_.toString}
  def tupleIdentifier: Parser[String] = """t[0-9]+""".r ^^ {_.toString}
  def tuple: Parser[String] = tupleIdentifier ~ """=\(""".r ~ valueIdentifier ~ """\|\|""".r ~ valueIdentifier ~ """\)""".r ^^ {
    case ti ~ _ ~ condVI ~ _ ~ consVI ~ _ => cfdRule.setTuple(ti, condVI, consVI)}
  def relationIdentifier: Parser[String] = """[a-zA-Z0-9]+""".r ^^ {
    case ri => cfdRule.relationName_=(ri)
  } 
  def rule: Parser[CfdRule] = relationIdentifier ~ """\(""".r ~ conditionalExpression ~ """,""".r  ~ tuple ~ """\)""".r ^^ { _ => this.cfdRule}
  
  def apply(input: String): CfdRule = parseAll(rule, input) match {
    case Success(result, _) => result
    case failure : NoSuccess => scala.sys.error(failure.msg)
}