Canvas <-
function(xlim=NULL, ylim=xlim, xpd=par("xpd"), mar=c(5.1,5.1,5.1,5.1), 
                   asp=1, bg=par("bg"), ...){
  
  if(is.null(xlim)){
    xlim <- c(-1,1)
    ylim <- xlim
  }
  if(length(xlim)==1) { 
    xlim <- c(-xlim,xlim)  
    ylim <- xlim
  }
  
  usr <- par("xpd"=xpd, "mar"=mar, "bg"=bg);  on.exit(par(usr)) 
  
  plot( NA, NA, xlim=xlim, ylim=ylim, asp=asp, type="n", xaxt="n", yaxt="n", 
        xlab="", ylab="", frame.plot = FALSE, ...)
}
