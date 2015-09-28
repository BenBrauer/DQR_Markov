package model.parser

import scala.util.parsing.combinator._

/**
 * A parser for matching dependency data quality rules in first order logic form
 */
class MdRuleParser() extends JavaTokenParsers {
  
  private var _mdRule: MdRule = null
  
	private def relationIdentifier = """[a-zA-Z0-9-_]+\[[a-zA-Z0-9-_]+\]""".r
  private def equalsIdentifier = """(=|!=)""".r
  
  private def combination: Parser[Expr] = chainl1(equation, "^" ^^^ And)
  
  private def equation = relationIdentifier ~ equalsIdentifier ~ relationIdentifier ^^ 
    { case lhs ~ op ~ rhs => Equals(lhs, op, rhs) } | combination 
  
  private def rulePrefix =  """md[0-9]*:\s*""".r
    
  private def rule = rulePrefix ~ combination  ~ """-\>""".r ~ 
    relationIdentifier ~ """<->""".r ~ relationIdentifier ^^ {
    case _ ~ ast ~ _ ~ lhs ~ _ ~ rhs => _mdRule.setRule(ast, lhs, rhs)
  }
  
  /**
   * Parses a matching dependency rule in first order logic notation into a MdRule object instance
   * 
   * @param ruleText matching dependency rule text in first order logic notation 
   */
  def parse(ruleText: String): (Boolean, MdRule) ={ 
    _mdRule = new MdRule(ruleText)
    parseAll(rule, _mdRule.ruleText) match {
      case Success(result, _) => (true,result)
      case failure : NoSuccess => (false, null)
    }
  }
	
 }