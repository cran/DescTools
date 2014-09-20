UnitConv <-
function(x, from_unit, to_unit){
  
  if(from_unit == "C") {
    if(to_unit=="F") return(x *1.8+32)
  }
  if(from_unit == "F") {
    if(to_unit=="C") return((x -32) *5/9)
  }
  
  fact <- d.units[d.units$from == from_unit & d.units$to==to_unit, "fact"]
  if(length(fact)==0) fact <- NA 
  
  return(x * fact)
  
}
