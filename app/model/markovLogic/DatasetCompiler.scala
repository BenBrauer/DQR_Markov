package model.markovLogic

import model.data.Dataset
import play.api.Logger
import util.{Commons => c}

/**
 * DatasetCompiler is a compiler to generate hidden markov logic predicates for a dataset
 */
object DatasetCompiler {
  def apply(set: Dataset): String = {
    //generate predicates for relation columns 
    val columnPredicates =  set.relations.foldLeft("")((totalLogic, rel) => 
      totalLogic  + c.addNewLine(totalLogic) +  rel.columns.foldLeft("")((totalColumns, column) => 
        totalColumns + c.addNewLine(totalColumns) + 
           rel.label + "-" + column + "(" + rel.label + "id,val" + column + ")"
    ))
    Logger.debug("\ncolumnPredicates: " + columnPredicates)
    //predicates for eq-Function
    val eqPredicates = set.relations.foldLeft("")((totalLogic, rel) => 
      totalLogic  + c.addNewLine(totalLogic) + rel.columns.foldLeft("")((totalColumns, column) => 
        totalColumns + c.addNewLine(totalColumns) + 
          "eq" + rel.label + "-" + column + "(" + rel.label + "id," + rel.label + "id)"
    ))
    //predicates for match-Function
    //generate list of all relation-column pairs
    val allColumns = set.relations().foldLeft(List[(String,String)]())((listRel,rel) => 
      rel.columns.foldLeft(listRel)((listRel,col) => listRel.::(rel.label, col)))
    //generate match-Predicates for all equal column-names
    var allColumnsTail = allColumns.tail
    val matchPredicates = allColumns.foldLeft("")((ml,col)=> ml + c.addNewLine(ml) +
      {
       val colPredicates = allColumnsTail.foldLeft("")((mlcol,tailCol) => mlcol + 
       {
         val (tailColRel, tailColName) = tailCol
         val (colRel, colName) = col
         if (colName == tailColName && tailColRel != colRel)
           c.addNewLine(mlcol) + "match" + colName + "(" + colRel + "id" + "," + tailColRel + "id)"
         else
           ""
       })
       if (allColumnsTail.length > 0)
         allColumnsTail = allColumnsTail.tail
       colPredicates
      })
    return columnPredicates + "\n" + eqPredicates + "\n" + matchPredicates
  }
}