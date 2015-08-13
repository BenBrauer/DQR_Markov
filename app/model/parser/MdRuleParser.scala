package model.parser

import scala.util.parsing.combinator._

/**
 * A parser for matching dependency data quality rules in first order logic form
 */
class MdRuleParser() extends JavaTokenParsers {
  
  private var _mdRule: MdRule = null
  
	private val relationIdentifier = """[a-zA-Z0-9-_]+\[[a-zA-Z0-9-_]+\]""".r
  private val equalsIdentifier = """(=|!=)""".r
  
  private val combination: Parser[Expr] = chainl1(equation, "^" ^^^ And)
  
  private val equation = relationIdentifier ~ equalsIdentifier ~ relationIdentifier ^^ 
    { case lhs ~ op ~ rhs => Equals(lhs, op, rhs) } | combination 
  
  private val rulePrefix =  """md[0-9]*:\s*""".r
    
  private val rule = rulePrefix ~ combination  ~ """-\>""".r ~ 
    relationIdentifier ~ """<->""".r ~ relationIdentifier ^^ {
    case _ ~ ast ~ _ ~ lhs ~ _ ~ rhs => _mdRule.setRule(ast, lhs, rhs)
  }
  
  
   def parse(ruleText: String): (Boolean, MdRule) ={ 
    _mdRule = new MdRule(ruleText)
    parseAll(rule, _mdRule.ruleText) match {
      case Success(result, _) => (true,result)
      case failure : NoSuccess => (false, null)
    }
  }
	
 }