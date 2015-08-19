package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import model.data._
import model.markovLogic.RelationCompiler

/**
 * A controller for operations on Datasets and Relations like create, delete and show
 */
object Browser extends Controller {
 
  /**
   * Stores all attributes of a Dataset
   */
  case class DatasetData(label: String)  
  
  /**
   * Form to map form input into DatasetData instance
   */
  val datasetForm = Form(
    mapping(
        "label" -> text
    )(DatasetData.apply)(DatasetData.unapply) 
  )
  
  /**
   * Returns an Action which creates a dataset
   */
  def createDataset() = Action { implicit request =>
    val datasetData = datasetForm.bindFromRequest.get
    Dataset.create(datasetData.label)
    Redirect("/browser")
  }
  
  /**
   * Returns an Action which deletes a Dataset
   * 
   * @param id the Dataset's id
   */
  def deleteDataset(id: Long) = Action {
    Dataset.delete(id)
    Redirect("/browser")
  }
 
  /**
   * Stores all attributes of a Relation
   */
  case class RelationData(label: String, data: String, dataset_id: Long)
  
  
  /**
   * Form to map form input into RelationData instance
   */ 
  val relationForm = Form(
      mapping(
        "label" -> text,
        "data" -> text,
        "dataset_id" -> longNumber
          )(RelationData.apply)(RelationData.unapply)
          
    )
  
  /**
   * Returns an Action which starts the input of a Relation
   * 
   * @param dataset_id the id of the Dataset the Relation belongs to
   */
  def inputRelation(dataset_id: Long) = Action {
	  Ok(views.html.relationForm(relationForm,dataset_id))
  }
    
  /**
   * Returns an Action which creates a Relation on the database
   * 
   */
  def createRelation() = Action { implicit request => 
    val relationData = relationForm.bindFromRequest.get
    Relation.create(relationData.label, relationData.data, relationData.dataset_id)
    Redirect("/browser")
  }
  
  /**
   * Returns an Action which starts the view to show a relation
   */
  def showRelation(id: Long) = Action {
    Ok(views.html.relation(Relation.byId(id)))
  }
   
  /**
   * Returns an Action which deletes a Relation
   * 
   * @param id the id of the Relation to be deleted
   */
  def deleteRelation(id: Long) = Action {
    Relation.delete(id)
    Redirect("/browser")
  }
  
  /**
   * Returns an Action which outputs Markov Logic of a Relation
   * 
   * @param id the id of the relation
   */
  def markovLogicRelation(id: Long) = Action {
    val relation = Relation.byId(id)
    Ok(RelationCompiler(relation))
  }
  
} 