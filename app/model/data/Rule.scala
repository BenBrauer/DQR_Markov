package model.data

import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import scala.util.parsing.combinator.RegexParsers

/**
 * A rule which can be stored in the database.
 * 
 * @constructor create a new rule with an id, label, rule and reference to a dataset-id
 * @param id the rule's id in the database
 * @param label the rule's label
 * @param firt order logic text of the rule
 * @param dataset-id the rule's reference to a dataset-id value in the database
 */

case class Rule (id: Long, label: String, rule: String, dataset_id: Long) {
 
}


/**
 * Rule companion object provides functionality to create, delete and read rules from the database
 */
object Rule {
  
  /**
   * A RowParser to parse from rule table in the database into a rule instance
   */
  val rule = {
    get[Long]("id") ~ 
    get[String]("label") ~
    get[String]("rule") ~
    get[Long]("dataset_id")map {
      case id~label~rule~dataset_id => Rule(id,label,rule,dataset_id)
      }
  }
  
  /**
   * Creates a new rule on the database
   * 
   * @param label label of the rule to be created
   * @param rule text of the rule to be created is expected in first order logic notation
   * @param dataset_id id of the dataset the rule should belong to
   */
  def create(label: String, rule: String, dataset_id: Long) = {
      DB.withConnection { implicit c => 
      SQL("INSERT INTO rule (label, rule, dataset_id) VALUES ({label},{rule},{dataset_id})")
        .on("label" -> label,
            "rule" -> rule,
            "dataset_id" -> dataset_id)
        .executeInsert()
      }
  }
  
  /**
   * Updates an existing rule on the database
   * 
   * @param id id of the rule to be updated
   * @param label new label of the rule to be updated
   * @param rule new rule text to be updated is expected in first order logic notation
   */
  def update(id: Long, label: String, rule: String) = {
    DB.withConnection { implicit c => 
      SQL("UPDATE rule SET label = {label}, rule  = {rule}  WHERE id = {id}")
        .on("label" -> label,
            "rule" -> rule,
            "id" -> id)
        .executeUpdate()
      }
  }
  
  /**
   * Deletes a rule on the database
   * 
   * @param id the id of the rule to be deleted
   */
  def delete(id: Long)  =  {
     DB.withConnection { implicit c => SQL("DELETE FROM rule WHERE id = {id}")
      .on("id" -> id)
      .execute()}
  }
  
  /**
   * Retrieves a rule from the database
   * 
   * @param id the id of the rule to be retrieved  
   */
   def byId(id: Long) : Rule = 
      DB.withConnection { implicit c => SQL("SELECT * FROM rule WHERE id = {id}").on("id" -> id).as(rule single)}
 
  
  
  
}