package controllers

import play.api._
import play.api.mvc._
import model.data._

object Browser extends Controller {
 
  
  def relation(id: Long) = Action {
    Ok(views.html.relation(Relation.byId(id)))
  }
  
}