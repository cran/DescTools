AddLm.formula <-
function(formula, data, ...){
  r.lm <- lm(formula, data)
  AddLm(r.lm, ...)  
}
