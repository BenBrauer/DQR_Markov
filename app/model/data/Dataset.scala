package model.data

import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current


case class Dataset (id: Long, label: String) {
  
  def relations(): List[Relation] = DB.withConnection { implicit c =>
    SQL("SELECT * FROM relation WHERE dataset_id = {dataset_id}").on("dataset_id" -> this.id).as(Relation.relation *)
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
  }
  
  def byLabel(label: String): Dataset = 
    DB.withConnection { implicit c => SQL("SELECT * FROM dataset WHERE label = {label}").on("label" -> label).as(dataset single)}
  
  
  def byId(id: Int): Dataset = 
    DB.withConnection { implicit c => SQL("SELECT * FROM dataset WHERE id = {id}").on("id" -> id).as(dataset single)}
  
    
  
}


