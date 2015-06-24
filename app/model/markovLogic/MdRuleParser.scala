package model.markovLogic

import scala.util.parsing.combinator._

class MdRuleParser() extends RegexParsers {
  
  var mdRule: MdRule = null
  
	def relationIdentifier = """[a-zA-Z0-9-_]+\[([a-zA-Z0-9-_]+,)*[a-zA-Z0-9-_]+\]""".r
  
  def rule: Parser[MdRule] = """md[0-9]*:\s*""".r ~ relationIdentifier ~ """=""".r ~ 
    relationIdentifier ~ """^""".r ~  relationIdentifier ~  """!=""".r ~ relationIdentifier ~  """-\>""".r ~ 
    relationIdentifier ~ """<->""".r ~ relationIdentifier ^^ { 
      case _ ~  condRelationEqual1 ~ _ ~ condRelationEqual2 ~
      _  ~ condRelationNotEqual1 ~ _ ~ condRelationNotEqual2 ~ 
      _ ~ consRelation1 ~ _ ~ consRelation2  => mdRule.setRule(condRelationEqual1, condRelationEqual1, 
          condRelationNotEqual1, condRelationEqual2, consRelation1, consRelation2)}
  
  
   def parse(input: String): (Boolean, MdRule) ={ 
    mdRule = new MdRule()
    parseAll(rule, input) match {
      case Success(result, _) => (true,result)
      case failure : NoSuccess => (false, null)
    }
  }
	
 }