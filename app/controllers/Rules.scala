package controllers

import play.api._
import play.api.mvc._
import model.data._
import play.api.data._
import play.api.data.Forms._

class Rules extends Controller {
  
  case class RuleData(label: String, rule: String, relation_id: Long)
  val ruleForm = Form(
      mapping(
        "label" -> text,
        "rule" -> text,
        "relation_id" -> longNumber
          )(RuleData.apply)(RuleData.unapply)
    )
  
  def createRule() = Action { implicit request =>
    val ruleData = ruleForm.bindFromRequest.get
    Rule.create(ruleData.label, ruleData.rule, ruleData.relation_id)
    Redirect("/relation/id/" + ruleData.relation_id + "/rules")
  }
  
  def editRule(id: Long) = Action {
    val rule = Rule.byId(id)
    if (rule != null) {
      Ok(views.html.ruleForm(rule))
    } else {
      NotFound("No Rule found")
    }
    
  }
  
  def updateRule(id: Long) = Action { implicit request =>
    val ruleData = ruleForm.bindFromRequest.get
    Rule.update(id, ruleData.label, ruleData.rule)
    Ok(views.html.rules(Relation.byId(ruleDataid)))
  }
  
  def deleteRule(id: Long) = Action {
    val rule = Rule.byId(id)
    Rule.delete(id)
    Redirect(Redirect("/relation/id/" + rule.relation_id + "/rules"))
  }
  
  def rules(relation_id: Long) = Action {
    Ok(views.html.rules(Relation.byId(relation_id)))
  }
  
  
}