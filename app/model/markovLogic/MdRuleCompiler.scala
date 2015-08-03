package model.markovLogic

import model.data.MdRule

object MdRuleCompiler {
   
  //Tokens
  val ImplicationToken = "->"
  val EqualToken = "="
  val SimilarityToken = "~"
  val ConjunctionToken = "^"
  val DisjunctionToken = "v"
  val MatchToken = "<=>"
  
  //Regular expression for validation
  val RuleRegEx = ""
  
  def apply(rule: String): String = {
     //TODO: convert rule to markov logic
    return ""
   }
  
  def validate(rule: MdRule): Boolean = {
    return true
  }
  
}