package model.data

import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import model.markovLogic._
import scala.util.parsing.combinator.RegexParsers

abstract case class Rule (id: Long, label: String, rule: String, dataset_id: Long) {
  def toMarkovLogic(): String
  /*def toMarkovLogic(): String = {
    val mdRulePattern = """\A(md).*""".r
    val cfdRulePattern = """\A(cfd).*""".r
    var markovLogic = "";
    this.rule match {
      case mdRulePattern(m) => { 
        val parser = new MdRuleParser(); 
        parser.parse(this.rule) match { case (result, md) => markovLogic = md.toMarkovLogic } 
      }
      case cfdRulePattern(m) => {
        val parser = new CfdRuleParser();
        parser.parse(this.rule) match { case (result, cfd) => markovLogic = 
          cfd.toMarkovLogic.foldLeft("")((ml,rule)=> ml + {if (ml.length > 0) "\n" else "" } + rule ) }
      }
    }
    return markovLogic
  }*/
}

object Rule {
  
  val rule = {
    get[Long]("id") ~ 
    get[String]("label") ~
    get[String]("rule") ~
    get[Long]("dataset_id")map {
      case id~label~rule~dataset_id => Rule.createNewInstance(id,label,rule,dataset_id)
      }
  }
  
  def createNewInstance(id: Long, label: String, rule: String, dataset_id: Long): Rule = {
    val mdRulePattern = """\A(md).*""".r
    val cfdRulePattern = """\A(cfd).*""".r
    var ruleInstance: Rule = null
    rule match {
      case mdRulePattern(m) => { 
        val parser = new MdRuleParser(); 
        parser.parse(new MdRule(id, label, rule, dataset_id)) match { case (result, md) => ruleInstance = md } 
      }
      case cfdRulePattern(m) => {
        val parser = new CfdRuleParser();
        parser.parse(new CfdRule(id, label, rule, dataset_id)) match { case (result, cfd) =>  
          ruleInstance = cfd
        }
      }
    }
    return ruleInstance
  }
  
  def create(label: String, rule: String, dataset_id: Long) = {
      DB.withConnection { implicit c => 
      SQL("INSERT INTO rule (label, rule, dataset_id) VALUES ({label},{rule},{dataset_id})")
        .on("label" -> label,
            "rule" -> rule,
            "dataset_id" -> dataset_id)
        .executeInsert()
      }
  }
  
  def update(id: Long, label: String, rule: String) = {
    DB.withConnection { implicit c => 
      SQL("UPDATE rule SET label = {label}, rule  = {rule}  WHERE id = {id}")
        .on("label" -> label,
            "rule" -> rule,
            "id" -> id)
        .executeUpdate()
      }
  }
  
  def delete(id: Long)  =  {
     DB.withConnection { implicit c => SQL("DELETE FROM rule WHERE id = {id}")
      .on("id" -> id)
      .execute()}
  }
  
   def byId(id: Long) : Rule = 
      DB.withConnection { implicit c => SQL("SELECT * FROM rule WHERE id = {id}").on("id" -> id).as(rule single)}
 
  
  
  
}