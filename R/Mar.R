Mar <-
function(bottom=NULL, left=NULL, top=NULL, right=NULL, outer=FALSE){
  
  if(outer){
    if(is.null(bottom)) bottom <- par("oma")[1]
    if(is.null(left)) left <- par("oma")[2]
    if(is.null(top)) top <- par("oma")[3]
    if(is.null(right)) right <- par("oma")[4]
    par(oma=c(bottom, left, top, right))
    
  } else {
    if(is.null(bottom)) bottom <- par("mar")[1]
    if(is.null(left)) left <- par("mar")[2]
    if(is.null(top)) top <- par("mar")[3]
    if(is.null(right)) right <- par("mar")[4]
    par(mar=c(bottom, left, top, right))
    
  }
}
