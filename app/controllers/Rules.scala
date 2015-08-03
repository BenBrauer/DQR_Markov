package controllers

import play.api._
import play.api.mvc._
import model.data._
import model.markovLogic._
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation._


object Rules extends Controller {
  
  case class RuleData(id: Long, label: String, rule: String, dataset_id: Long) {
    //TODO: validate to all rule types 
    /*val ruleValidationConstraint = Constraint[String] {(r: String) => r match {
      case r if CfdRule.validate(r)._1 => Valid
      case _ => Invalid("Rule could not be parsed.")
    }*/
     }
    
  
    val ruleForm = Form(
      mapping(
        "id" -> longNumber,
        "label" -> text(minLength = 0, maxLength = 255),
        "rule" -> text(minLength = 0, maxLength = 500),//.verifying(ruleValidationConstraint),
        "dataset_id" -> longNumber
          )(RuleData.apply)(RuleData.unapply)
      )
    

    def newRule(dataset_id: Long) = Action {
      val newRuleForm =ruleForm.fill(new RuleData(0, "","",dataset_id))
      val saveAction:Call = routes.Rules.createRule(dataset_id)
      Ok(views.html.ruleForm(newRuleForm,"Create new rule", saveAction, dataset_id))
    }
    
    def createRule(dataset_id: Long) = Action { implicit request =>
      ruleForm.bindFromRequest.fold(
          formWithErrors => {
            val saveAction:Call = routes.Rules.createRule(dataset_id)
            BadRequest(views.html.ruleForm(formWithErrors,"Create new rule (Errors)", saveAction, dataset_id))
          },
          ruleData => {
            Rule.create(ruleData.label, ruleData.rule, ruleData.dataset_id)
            Redirect(routes.Rules.rules(dataset_id))
          })
    }
  
    def editRule(id: Long) = Action {
      val rule = Rule.byId(id)
      if (rule != null) {
        val filledRuleForm = ruleForm.fill(new RuleData(id, rule.label, rule.rule, rule.dataset_id))
        val saveAction:Call = routes.Rules.updateRule
        Ok(views.html.ruleForm(filledRuleForm,"Edit rule " + rule.label , saveAction, rule.dataset_id))
      } else {
        NotFound("No Rule found")
      }
    }
  
    def updateRule() = Action { implicit request =>
      val ruleData = ruleForm.bindFromRequest.get
      Rule.update(ruleData.id, ruleData.label, ruleData.rule)
      Ok(views.html.rules(Dataset.byId(ruleData.dataset_id)))
    }
  
    def deleteRule(id: Long) = Action {
      val rule = Rule.byId(id)
      Rule.delete(id)
      Redirect(routes.Rules.rules(rule.dataset_id))
    }
  
    def rules(dataset_id: Long) = Action {
      Ok(views.html.rules(Dataset.byId(dataset_id)))
    }
    
    def markovLogicRule(id: Long) = Action {
      val rule = Rule.byId(id)
      if (rule != null) {
        Ok(RuleCompiler(rule))
      } else{
        NotFound("Rule does not exist")
      }
    }
    
    def mlFile(id: Long) = Action {
      val dataset = Dataset.byId(id)
      if (dataset != null) {
        val ml = DatasetCompiler(dataset) + "\n\n" + 
           dataset.rules.foldLeft("")(_ + "\n" + RuleCompiler(_))
        Ok(ml).as("application/x-download").withHeaders(
          ("Content-disposition","attachment; filename=" + dataset.label + ".ml")
        ) 
      } else {
        NotFound("Dataset does not exist")
      }
    }
    
    def dbFile(id: Long) = Action {
      val dataset = Dataset.byId(id)
      if (dataset != null) {
        val db = dataset.relations().foldLeft("")(_ + "\n" + RelationCompiler(_))
        Ok(db).as("application/x-download").withHeaders(
          ("Content-disposition","attachment; filename=" + dataset.label + ".db")
        )
      } else {
        NotFound("Dataset does not exist")
      }
    }
  
}