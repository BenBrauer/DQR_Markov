package model.data

import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import util.CSV

/**
 * A relation which can be stored in the database.
 * 
 * @constructor create a new relation with an id, label, data and reference to a dataset-id
 * @param id the relation's id in the database
 * @param label the relation's label
 * @param data the relation's data in csv-format
 * @param dataset-id the relation's reference to a dataset-id value in the database
 */
case class Relation(id: Long, label: String, data: String, dataset_id: Long) {
  
  /**
   * Returns a list of maps which represent column value relations for each row of the relation data
   */
  def parseData() : List[Map[String,String]] = {
    val parser = new CSV()
    parser.parse(this.data)
  }
  
  /**
   * Returns a list of all column names of this relation
   */
  def columns: List[String] = {
    val data = this.parseData()
    val columns = data(0).keySet
    return columns.toList
  }
  
}

/**
 * Relation companion object provides functionality to create, delete and read relations from the database
 */
object Relation {
  
  /**
   * A RowParser to parse from relation table in the database into a relation instance
   */
  val relation = {
    get[Long]("id") ~ 
    get[String]("label") ~
    get[String]("data") ~
    get[Long]("dataset_id")map {
      case id~label~data~dataset_id => Relation(id,label,data,dataset_id)
      }
  }
  
  /**
   * Creates a new relation on the database
   * 
   * @param label label of the relation to be created
   * @param data data of the relation to be created is expected as csv string
   * @param dataset_id id of the dataset the relation should belong to
   */
  def create(label: String, data: String, dataset_id: Long) = {
    DB.withConnection { implicit c => 
      SQL("INSERT INTO relation (label, data, dataset_id) VALUES ({label},{data},{dataset_id})")
        .on("label" -> label,
            "data" -> data,
            "dataset_id" -> dataset_id)
        .executeInsert()
      }
  }
  
  /**
   * Deletes a relation from the database
   * 
   * @param id id of the relation to be deleted
   */
  def delete(id: Long) = {
    DB.withConnection { implicit c => SQL("DELETE FROM relation WHERE id = {id}")
      .on("id" -> id)
      .execute()}
  }
  
  /**
   * Retrieves a relation from the database
   * 
   * @param id id if the relation to be retrieved
   */
  def byId(id: Long) : Relation= 
    DB.withConnection { implicit c => SQL("SELECT * FROM relation WHERE id = {id}").on("id" -> id).as(relation single)}
  
}