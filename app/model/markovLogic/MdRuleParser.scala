package model.markovLogic

import scala.util.parsing.combinator._
import model.data._

class MdRuleParser() extends JavaTokenParsers {
  
  var _mdRule: MdRule = null
  
	def relationIdentifier = """[a-zA-Z0-9-_]+\[[a-zA-Z0-9-_]+\]""".r
  //TODO:!=
  def equationIdentifier = """[=~]""".r
  
  def combination: Parser[Expr] = chainl1(equation, "^" ^^^ And)
  def equationRegex = relationIdentifier ~ equationIdentifier ~ relationIdentifier 
  def equation = equationRegex ^^ { case lhs ~ op ~ rhs => Equals(lhs, op, rhs) } | combination 
  
 /* def rule: Parser[MdRule] = """md[0-9]*:\s*""".r ~ relationIdentifier ~ """=""".r ~ 
    relationIdentifier ~ """^""".r ~  relationIdentifier ~  """!=""".r ~ relationIdentifier ~  """-\>""".r ~ 
    relationIdentifier ~ """<->""".r ~ relationIdentifier ^^ { 
      case _ ~  condRelationEqual1 ~ _ ~ condRelationEqual2 ~
      _  ~ condRelationNotEqual1 ~ _ ~ condRelationNotEqual2 ~ 
      _ ~ consRelation1 ~ _ ~ consRelation2  => mdRule.setRule(condRelationEqual1, condRelationEqual1, 
          condRelationNotEqual1, condRelationEqual2, consRelation1, consRelation2)}*/
  def rule = """md[0-9]*:\s*""".r ~ combination  ~ """-\>""".r ~ 
    relationIdentifier ~ """<->""".r ~ relationIdentifier ^^ {
    //TODO: setRule
    case _ ~ ast ~ _ ~ lhs ~ _ ~ rhs => _mdRule.setRule(ast, lhs, rhs)
  }
  
  
   def parse(mdRule: MdRule): (Boolean, MdRule) ={ 
    _mdRule = mdRule
    parseAll(rule, _mdRule.rule) match {
      case Success(result, _) => (true,result)
      case failure : NoSuccess => (false, null)
    }
  }
	
 }