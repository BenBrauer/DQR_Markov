package controllers

import play.api._
import play.api.mvc._
import model.data._
import model.markovLogic._
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation._


object Rules extends Controller {
  
  case class RuleData(id: Long, label: String, rule: String, relation_id: Long)
    //TODO: validate to all rule types 
    val ruleValidationConstraint = Constraint[String] {(r: String) => r match {
      case r if CfdRule.validate(r)._1 => Valid
      case _ => Invalid("Rule could not be parsed.")
    }}
  
    val ruleForm = Form(
      mapping(
        "id" -> longNumber,
        "label" -> text(minLength = 0, maxLength = 255),
        "rule" -> text(minLength = 0, maxLength = 500).verifying(ruleValidationConstraint),
        "relation_id" -> longNumber
          )(RuleData.apply)(RuleData.unapply)
      )
    

    def newRule(relation_id: Long) = Action {
      val newRuleForm =ruleForm.fill(new RuleData(0, "","",relation_id))
      val saveAction:Call = routes.Rules.createRule(relation_id)
      Ok(views.html.ruleForm(newRuleForm,"Create new rule", saveAction, relation_id))
    }
    
    def createRule(relation_id: Long) = Action { implicit request =>
      ruleForm.bindFromRequest.fold(
          formWithErrors => {
            val saveAction:Call = routes.Rules.createRule(relation_id)
            BadRequest(views.html.ruleForm(formWithErrors,"Create new rule (Errors)", saveAction, relation_id))
          },
          ruleData => {
            Rule.create(ruleData.label, ruleData.rule, ruleData.relation_id)
            Redirect(routes.Rules.rules(relation_id))
          })
    }
  
    def editRule(id: Long) = Action {
      val rule = Rule.byId(id)
      if (rule != null) {
        val filledRuleForm = ruleForm.fill(new RuleData(id, rule.label, rule.rule, rule.relation_id))
        val saveAction:Call = routes.Rules.updateRule
        Ok(views.html.ruleForm(filledRuleForm,"Edit rule " + rule.label , saveAction, rule.relation_id))
      } else {
        NotFound("No Rule found")
      }
    }
  
    def updateRule() = Action { implicit request =>
      val ruleData = ruleForm.bindFromRequest.get
      Rule.update(ruleData.id, ruleData.label, ruleData.rule)
      Ok(views.html.rules(Relation.byId(ruleData.relation_id)))
    }
  
    def deleteRule(id: Long) = Action {
      val rule = Rule.byId(id)
      Rule.delete(id)
      Redirect(routes.Rules.rules(rule.relation_id))
    }
  
    def rules(relation_id: Long) = Action {
      Ok(views.html.rules(Relation.byId(relation_id)))
    }
  
  
}