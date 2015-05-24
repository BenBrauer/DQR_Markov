package model.markovLogic

import scala.util.parsing.combinator._

class CfdRuleParser extends RegexParsers {
  def attribute: Parser[String] = """\[[a-zA-Z0-9-_]+\]""".r  ^^{_.toString} //TODO: restrict to known attributes of a dataset/relation
  def conditional: Parser[String] = """-\>""".r ^^{_.toString}
  def conditionalExpression: Parser[String] = attribute ~ (conditional ~ attribute) ^^ { _.toString }
  def value: Parser[String] = """"[a-zA-Z0-9-_]+"""".r ^^ {_.toString}
  def valueTuple: Parser[String] = """\(""".r ~ value ~ """,""".r ~ value ^^ {_.toString}
  def tupleIdentifier = """t[0-9]+""".r ^^ {_.toString}
  def tuple = tupleIdentifier ~ """=""".r ~ valueTuple ^^ {_.toString}
  def rule = 
}