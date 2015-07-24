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
  
  def parseData() : List[Map[String,String]] = {
    val parser = new CSV()
    parser.parse(this.data)
  }
  
  def toMarkovLogic(): String =  {
    val data = this.parseData()
    val (logic, index) = data.foldLeft(("",1))({
      case ((relationMarkovLogic,rowIndex),row) => {
        val rowMarkovLogic = row.foldLeft("")({
          case (totalLogic,(column, value)) => totalLogic + "\n" + this.label + "-" + column + 
            "(" + rowIndex.toString() + "," + value +")"
        })
        (relationMarkovLogic  + rowMarkovLogic, rowIndex + 1)
      }
    })
    return logic
  }
  
  def columns: List[String] = {
    val data = this.parseData()
    val columns = data(0).keySet
    return columns.toList
  }
  
}
object Relation {
  
  val relation = {
    get[Long]("id") ~ 
    get[String]("label") ~
    get[String]("data") ~
    get[Long]("dataset_id")map {
      case id~label~data~dataset_id => Relation(id,label,data,dataset_id)
      }
  }
  
  def create(label: String, data: String, dataset_id: Long) = {
    DB.withConnection { implicit c => 
      SQL("INSERT INTO relation (label, data, dataset_id) VALUES ({label},{data},{dataset_id})")
        .on("label" -> label,
            "data" -> data,
            "dataset_id" -> dataset_id)
        .executeInsert()
      }
  }
  
  def delete(id: Long) = {
    DB.withConnection { implicit c => SQL("DELETE FROM relation WHERE id = {id}")
      .on("id" -> id)
      .execute()}
  }
  
  def byId(id: Long) : Relation= 
    DB.withConnection { implicit c => SQL("SELECT * FROM relation WHERE id = {id}").on("id" -> id).as(relation single)}
  
}