PlotDesc.integer <-
function(x, main = deparse(substitute(x))
  , ord=c("val_asc","val_desc","frq_asc","frq_desc")
  , maxrows=10, ... , wrd=NULL) {

  switch(as.character(cut(length(unique(na.omit(x))), breaks=c(0,2,15,Inf), labels=1:3))
    , "1" = { PlotDesc.logical(x, main=main, ..., wrd=wrd) } 
    , "2" = { PlotDesc.factor(x, main=main, ..., type="dot", ord="none", wrd=wrd)  } 
    , "3" = { PlotDesc.numeric(x, main=main, ..., wrd=wrd) }
  )
  invisible()
  
}
