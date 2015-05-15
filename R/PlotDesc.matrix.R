PlotDesc.matrix <-
function(x, col1=getOption("col1", hblue), col2=getOption("col2", hred), 
                           horiz = TRUE, main=NA, ..., wrd=NULL){

  # treat matrix as table  
  PlotDesc.table(x, col1=col1, col2=col2, horiz=horiz, main=main, ..., wrd=wrd)
}
