package model.markovLogic

import model.data.Relation

/**
 * Is a compiler to generate markov logic grounded atoms for data of a relation
 */
object RelationCompiler {
  
  /**
   * Generates markov logic grounded atoms for the relation
   * 
   * @param rel the relation for generating grounded atoms
   */
  def apply(rel: Relation): String = {
    val data = rel.parseData()
    val (logic, index) = data.foldLeft(("",1))({
      case ((relationMarkovLogic,rowIndex),row) => {
        val rowMarkovLogic = row.foldLeft("")({
          case (totalLogic,(column, value)) => totalLogic + "\n" + rel.label + "-" + column + 
            "(" + rowIndex.toString() + "," + value +")"
        })
        (relationMarkovLogic  + rowMarkovLogic, rowIndex + 1)
      }
    })
    return logic
  }
}