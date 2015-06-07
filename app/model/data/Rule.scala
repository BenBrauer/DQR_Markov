package model.data

import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current

case class Rule (id: Long, label: String, rule: String, relation_id: Long) {
  
}

object Rule {
  
  val rule = {
    get[Long]("id") ~ 
    get[String]("label") ~
    get[String]("rule") ~
    get[Long]("relation_id")map {
      case id~label~rule~relation_id => Rule(id,label,rule,relation_id)
      }
  }
  
  def create(label: String, rule: String, relation_id: Long) = {
      DB.withConnection { implicit c => 
      SQL("INSERT INTO rule (label, rule, relation_id) VALUES ({label},{rule},{relation_id})")
        .on("label" -> label,
            "rule" -> rule,
            "relation_id" -> relation_id)
        .executeInsert()
      }
  }
  
  def update(id: Long, label: String, rule: String) = {
    DB.withConnection { implicit c => 
      SQL("UPDATE rule SET label = {label}, rule  = {rule}  WHERE id = {id})")
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