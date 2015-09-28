package controllers

import play.api._
import play.api.mvc._
import model.data._
import model.markovLogic._
import model.parser._
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation._

/**
 * Rules is a Controller for operations on rules like create, read, updadte, delete and generation of markov logic
 */
object Rules extends Controller {
  
  /**
   * Stores all attributes of a Rule
   */
  case class RuleData(id: Long, label: String, rule: String, dataset_id: Long) {
    //TODO: validate to all rule types 
    /*val ruleValidationConstraint = Constraint[String] {(r: String) => r match {
      case r if CfdRule.validate(r)._1 => Valid
      case _ => Invalid("Rule could not be parsed.")
    }*/
     }
    
  /**
   * Form to mmap from input into RuleData instance
   */
  val ruleForm = Form(
      mapping(
        "id" -> longNumber,
        "label" -> text(minLength = 0, maxLength = 255),
        "rule" -> text(minLength = 0, maxLength = 500),
        "dataset_id" -> longNumber
          )(RuleData.apply)(RuleData.unapply)
  )
    
  /**
   * Returns an Action which starts the input of a Rule
   * 
   * @param dataset_id id of the rule's dataset
   */
  def inputRule(dataset_id: Long) = Action {
    val newRuleForm =ruleForm.fill(new RuleData(0, "","",dataset_id))
    val saveAction:Call = routes.Rules.createRule(dataset_id)
    Ok(views.html.ruleForm(newRuleForm,"Create new rule", saveAction, dataset_id))
  }
  
  /**
   * Returns an Action which creates a rule 
   * 
   * @param dataset_id id of the dataset the rule belongs to
   */
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
  
  /**
   * Returns an Action which starts the editable form of a Rule
   * 
   * @param id id of the Rule to be edited
   */
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
  
  /**
   * Returns an Action which updates a Rule
   * 
   */
  def updateRule() = Action { implicit request =>
      val ruleData = ruleForm.bindFromRequest.get
      Rule.update(ruleData.id, ruleData.label, ruleData.rule)
      Ok(views.html.rules(Dataset.byId(ruleData.dataset_id)))
    }
  
  /**
   * Returns an Action which deletes a Rule
   * 
   * @param id of the Rule to deleted
   */
  def deleteRule(id: Long) = Action {
    val rule = Rule.byId(id)
    Rule.delete(id)
    Redirect(routes.Rules.rules(rule.dataset_id))
  }

  /**
   * Returns an Action which shows all rules belonging to the Dataset
   * 
   * @param dataset_id id of the dataset 
   */
  def rules(dataset_id: Long) = Action {
    Ok(views.html.rules(Dataset.byId(dataset_id)))
  }
  
  /**
   * Returns an Action which compiles a rule into Markov Logic
   * 
   * @param id id of the rule to be compiled
   */
  def markovLogicRule(id: Long) = Action {
    val rule = Rule.byId(id)
    if (rule != null) {
      Ok(RuleCompiler(rule))
    } else{
      NotFound("Rule does not exist")
    }
  }
  
   /**
    * Returns an Action which creates an .ml-File from the Dataset
    * 
    * @param id id of the Dataset of which .ml-File should be created
    */
  def mlFile(id: Long) = Action {
    val dataset = Dataset.byId(id)
    if (dataset != null) {
      val ml = DatasetCompiler(dataset) + "\n\n" + 
         dataset.rules.foldLeft("")(_ + "\n" +  RuleCompiler(_))
      Ok(ml).as("application/x-download").withHeaders(
        ("Content-disposition","attachment; filename=" + dataset.label + ".ml")
      ) 
    } else {
      NotFound("Dataset does not exist")
    }
  }
  
  /**
    * Returns an Action which creates a .db-File from the Dataset
    * 
    * @param id id of the Dataset of which .db-File should be created
    */
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