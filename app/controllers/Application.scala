package controllers

import play.api._
import play.api.mvc._
import model.data._

object Application extends Controller {

  def index = Action {
    Ok(views.html.index())
  }
  
  def browser = Action {
    Ok(views.html.browser(Dataset.all,Browser.datasetForm))
  }
  
  def ruleGenerator = Action {
    Ok(views.html.ruleGenerator())
  }
}