package model.markovLogic

object RuleCompiler {
   
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
  
  def validate(rule: String): Boolean = {
    return true
  }
  
}