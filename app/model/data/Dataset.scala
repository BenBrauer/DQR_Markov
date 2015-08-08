package model.data

import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current

/**
 * A dataset which can be stored in the database
 * 
 * @constructur create new instance of dataset with an id and label
 * @param id the dataset's id in the database
 * @param label the dataset's label
 */
case class Dataset (id: Long, label: String) {
  
  def relations(): List[Relation] = DB.withConnection { implicit c =>
    SQL("SELECT * FROM relation WHERE dataset_id = {dataset_id}").on("dataset_id" -> this.id).as(Relation.relation *)
  }
  
  def rules(): List[Rule] = DB.withConnection { implicit c =>
    SQL("SELECT * FROM rule WHERE dataset_id = {dataset_id}").on("dataset_id" -> this.id).as(Rule.rule *)
  }
}

object Dataset {
  
  val dataset = {
    get[Long]("id") ~ 
    get[String]("label") map {
      case id~label => Dataset(id, label)
      }
  }
  
  def all(): List[Dataset] = DB.withConnection { implicit c =>
    SQL("SELECT * FROM dataset").as(dataset *)
  }
  
  def create(label: String) = {
    DB.withConnection { implicit c => 
      SQL("INSERT INTO dataset (label) VALUES ({label})")
        .on("label" -> label)
        .executeInsert()
      }
  }
  
  def delete(id: Long) = {
    DB.withConnection { implicit c => SQL("DELETE FROM dataset WHERE id = {id}")
      .on("id" -> id)
      .execute()}
    DB.withConnection { implicit c => SQL("DELETE FROM relation WHERE dataset_id = {id}")
      .on("id" -> id)
      .execute()}
    DB.withConnection { implicit c => SQL("DELETE FROM rule WHERE dataset_id = {id}")
      .on("id" -> id)
      .execute()}
  }
  
  def byId(id: Long): Dataset = 
    DB.withConnection { implicit c => SQL("SELECT * FROM dataset WHERE id = {id}").on("id" -> id).as(dataset single)}
  
    
  
}


