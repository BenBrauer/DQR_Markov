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
  
  /**
   * Returns a list of all relations belonging to this dataset
   */
  def relations(): List[Relation] = DB.withConnection { implicit c =>
    SQL("SELECT * FROM relation WHERE dataset_id = {dataset_id}").on("dataset_id" -> this.id).as(Relation.relation *)
  }
  
  /**
   * Returns a list of all rules belonging to this dataset
   */
  def rules(): List[Rule] = DB.withConnection { implicit c =>
    SQL("SELECT * FROM rule WHERE dataset_id = {dataset_id}").on("dataset_id" -> this.id).as(Rule.rule *)
  }
}

/**
 * Dataset companion object provides functionality to create, read and delete datasets in the database 
 */
object Dataset {
  
  /**
   * RowParser to parse rows from dataset table into a datsaset instance
   */
  val dataset = {
    get[Long]("id") ~ 
    get[String]("label") map {
      case id~label => Dataset(id, label)
      }
  }
  
  /**
   * Returns a list of all datasets in the database
   */
  def all(): List[Dataset] = DB.withConnection { implicit c =>
    SQL("SELECT * FROM dataset").as(dataset *)
  }
  
  /**
   * Creates a new dataset on the database
   * 
   * @param label label of the dataset to be created
   */
  def create(label: String) = {
    DB.withConnection { implicit c => 
      SQL("INSERT INTO dataset (label) VALUES ({label})")
        .on("label" -> label)
        .executeInsert()
      }
  }
  
  /**
   * Deletes a dataset on the database
   * 
   * @param id id of the dataset to be deleted
   */
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
  
  /**
   * Retrieves dataset by id value from the database
   * 
   * @param id id of the dataset to be retrieved
   */
  def byId(id: Long): Dataset = 
    DB.withConnection { implicit c => SQL("SELECT * FROM dataset WHERE id = {id}").on("id" -> id).as(dataset single)}
  
    
  
}


