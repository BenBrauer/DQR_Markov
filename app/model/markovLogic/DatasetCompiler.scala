package model.markovLogic

import model.data.Dataset

object DatasetCompiler {
  def apply(set: Dataset): String = {
    //generate predicates for relation columns 
    var columns: List[String] = List()
    val columnPredicates =  set.relations().foldLeft("")((totalLogic, rel) => 
      rel.columns.foldLeft("")((totalColumns, column) => 
        totalColumns + { if (totalColumns.length > 0) "\n"} + 
           column + "-" + rel.label  + "(" + rel.label + "id,val" + column + ")"
    ))
    //predicates for eq-Function
    val eqPredicates = set.relations().foldLeft("")((totalLogic, rel) => 
      rel.columns.foldLeft("")((totalColumns, column) => 
        totalColumns + { if (totalColumns.length > 0) "\n" } + 
          "eq" + rel.label + "-" + column + "(" + rel.label + "id," + rel.label + "id)"
    ))
    //predicates for match-Function
    //generate list of all relation-column pairs
    val allColumns = set.relations().foldLeft(List[(String,String)]())((listRel,rel) => 
      rel.columns.foldLeft(listRel)((listRel,col) => listRel.::(rel.label, col)))
    //generate match-Predicates for all equal column-names
    var allColumnsTail = allColumns.tail
    val matchPredicates = allColumns.foldLeft("")((ml,col)=>
      {
       val colPredicates = allColumnsTail.foldLeft("")((mlcol,tailCol) => {
         val (tailColName, tailColRel) = tailCol
         val (colName, colRel) = col
         if (colName == tailColName && tailColRel != colRel)
           "match" + colName + "(" + colRel + "id" + "," + tailColRel + "id)"
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