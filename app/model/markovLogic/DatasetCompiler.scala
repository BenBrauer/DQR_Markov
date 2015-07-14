package model.markovLogic

import model.data.Dataset

object DatasetCompiler {
  def toMarkovLogic(set: Dataset): String = {
    //generate predicates for relation columns 
    var variableCount = 0
    var columns: List[String] = List()
    val columnPredicates =  set.relations().foldLeft("")((totalLogic, rel) => 
      rel.columns.foldLeft("")((totalColumns, column) => 
        totalColumns + { if (totalColumns.length > 0) "\n" + {
          variableCount =  variableCount + 1
           rel.label + "-" + column+ "(id,val" + variableCount + ")"
        }
      }
    ))
    //predicates for same-Function
    val samePredicates = set.relations().foldLeft("")((totalLogic, rel) => 
      rel.columns.foldLeft("")((totalColumns, column) => 
        totalColumns + { if (totalColumns.length > 0) "\n" + {
          "same" + rel.label + "-" + column + "(id,id)"
        }
      }
    ))
    //predicates for match-Function
    val matchPredicates = set.relations().foldLeft("")((totalLogic, rel) => 
      rel.columns.foldLeft("")((totalColumns, column) => 
        totalColumns + { if (totalColumns.length > 0) "\n" + {
          "match" + rel.label + "-" + column + "(id,id)"
        }
      }
    ))
    return columnPredicates + "\n" + samePredicates + "\n" + matchPredicates
  }
}