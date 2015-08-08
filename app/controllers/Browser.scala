package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import model.data._
import model.markovLogic.RelationCompiler

/**
 * Browser is a controller for operations on Datasets like create, delete, show and on Relations like create, delete, show
 */
object Browser extends Controller {
 
  case class DatasetData(label: String)  
  val datasetForm = Form(
    mapping(
        "label" -> text
    )(DatasetData.apply)(DatasetData.unapply) 
  )
  
  def createDataset() = Action { implicit request =>
    val datasetData = datasetForm.bindFromRequest.get
    Dataset.create(datasetData.label)
    Redirect("/browser")
  }
  
  def deleteDataset(id: Long) = Action {
    Dataset.delete(id)
    Redirect("/browser")
  }
 
  case class RelationData(label: String, data: String, dataset_id: Long)
  val relationForm = Form(
      mapping(
        "label" -> text,
        "data" -> text,
        "dataset_id" -> longNumber
          )(RelationData.apply)(RelationData.unapply)
          
    )
     
  def inputRelation(dataset_id: Long) = Action {
	  Ok(views.html.relationForm(relationForm,dataset_id))
  }
    
   def createRelation() = Action { implicit request => 
    val relationData = relationForm.bindFromRequest.get
    Relation.create(relationData.label, relationData.data, relationData.dataset_id)
    Redirect("/browser")
  }
  
  def showRelation(id: Long) = Action {
    Ok(views.html.relation(Relation.byId(id)))
  }
   
  def deleteRelation(id: Long) = Action {
    Relation.delete(id)
    Redirect("/browser")
  }
  
  def markovLogicRelation(id: Long) = Action {
    val relation = Relation.byId(id)
    Ok(RelationCompiler(relation))
  }
  
} 