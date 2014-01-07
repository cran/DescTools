PlotDesc.data.frame <-
function(x, ..., wrd=NULL){
  for( cx in colnames(x) ){
    PlotDesc( x[,cx], main=cx, ..., wrd=wrd)
  }
}
