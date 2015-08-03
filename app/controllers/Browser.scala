package controllers

import play.api._
import play.api.mvc._
import model.data._
import play.api.data._
import play.api.data.Forms._
import model.markovLogic.RelationCompiler

object Browser extends Controller {
 
  
  def relation(id: Long) = Action {
    Ok(views.html.relation(Relation.byId(id)))
  }
  
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
    
     
  def newRelation(dataset_id: Long) = Action {
	  Ok(views.html.relationForm(relationForm,dataset_id))
  }
    
   def createRelation() = Action { implicit request => 
    val relationData = relationForm.bindFromRequest.get
    Relation.create(relationData.label, relationData.data, relationData.dataset_id)
    Redirect("/browser")
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