package model.markovLogic

import scala.util.parsing.combinator._

class MdRuleParser() extends RegexParsers {
	def relationIdentifier = """[a-zA-Z0-9-_]+\[([a-zA-Z0-9-_]+,)*[a-zA-Z0-9-_]+\]""".r
	def conditional = """-\>""".r
  def logicalAnd = """^""".r
  def similar = """~""".r
  def matchOperator = """<->""".r
  
  def rule: Parser[MdRule] = """md[0-9]*:\s*""".r ~ relationIdentifier ~ """=""".r ~ 
    relationIdentifier ~ logicalAnd ~  relationIdentifier ~ similar ~ relationIdentifier ~ conditional ~ 
    relationIdentifier ~ matchOperator ~ relationIdentifier ^^ { _ => new MdRule()}
  
	
 }