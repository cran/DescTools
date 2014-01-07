Desc <-
function(x, ..., wrd=NULL) {
  # main-function for describing an object
  # checks class(x) and chooses sub autonomously
  # we want dichotomous variables described as such and not according to their class:
  if(is.null(wrd)) UseMethod("Desc") else { 
      UseMethod("DescWrd")
  }  
}
