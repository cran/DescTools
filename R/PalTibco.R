PalTibco <-
function(){
  col <- apply( mcol <- matrix(c(
      0,91,0, 0,157,69, 253,1,97, 60,120,177,
      156,205,36, 244,198,7, 254,130,1, 
      96,138,138, 178,113,60
  ), ncol=3, byrow=TRUE), 1, function(x) rgb(x[1], x[2], x[3], maxColorValue=255))

  return(col)
}
