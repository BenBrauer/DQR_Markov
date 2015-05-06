package model.data

import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current


case class Dataset (id: Long, label: String) {
  
  def relations(): List[Relation] = {
    //TODO: Logic to retrieve from database
    List()
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
    
  }
}


