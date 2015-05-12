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
  
  def add(label: String) = {
    //TODO
  }
  
  def byLabel(label: String): Dataset = 
    DB.withConnection { implicit c => SQL("SELECT * FROM dataset WHERE label = {label}").on("label" -> label).as(dataset single)}
  
  
  def byId(id: Int): Dataset = 
    DB.withConnection { implicit c => SQL("SELECT * FROM dataset WHERE id = {id}").on("id" -> id).as(dataset single)}
  
    
  
}


