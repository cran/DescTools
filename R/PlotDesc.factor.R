PlotDesc.factor <-
function (x, main = deparse(substitute(x)), 
                ord = c("desc", "level", "name", "asc", "none"), maxrows = 10, lablen = 25, 
                type=c("bar","dot"), col=NULL, border=NULL, ..., wrd=NULL)  {
  
  
  if (nlevels(factor(x)) <= 2) {
    PlotDesc.logical(x, main = main, ..., wrd=wrd)
  }
  else {
    
    oldpar <- par(no.readonly=TRUE);  on.exit( par(oldpar) ) 
    
    # was cex in the dots-args? parse dots.arguments
    cex <- unlist(match.call(expand.dots=FALSE)$...["cex"])
    if(is.null(cex)) cex <- par("cex")
    
    tab <- table(x)
    switch(match.arg(arg = ord, choices = c("desc", "level", "name", "asc", "none")), 
           name = { tab <- tab[order(rownames(tab))]}, 
           asc =  { tab <- sort(tab) }, 
           desc = { tab <- -sort(-tab) })
    
    ptab <- prop.table(tab)
    trunc_fg <- (nrow(tab) > maxrows)
    if (!is.na(maxrows)) {
      tab <- tab[1:min(nrow(tab), maxrows)]
      ptab <- ptab[1:min(nrow(tab), maxrows)]
    }
    
    if(max(nchar(rownames(tab))) > lablen ) rownames(tab) <- StrTrunc(rownames(tab), lablen)
    wtxt <- max(strwidth(rownames(tab), "inch"))
    wplot <- (par("pin")[1] - wtxt) / 2
    layout(matrix(c(1,2), nrow=1), widths=c(wtxt + wplot, wplot) * 2.54 )
    par(mai=c(1.5, max(strwidth(rev(rownames(tab)), "inch"))+.5, 0.2, .3)+.02) 
    if(!is.na(main)) par(oma=c(0,0,3,0))
    
    switch(match.arg(arg = type, choices = c("bar", "dot")), 
           dot = {
             if(is.null(col)) col <- getOption("col1", hblue)
             if(is.null(border)) border <- "black"
             b <- barplot( rev(tab), horiz=TRUE, border=NA, col="white", las=1, 
                           xlim=range(pretty(tab)) + c(-1,1) * diff(range(pretty(tab))) * 0.04, 
                           xpd=FALSE, xlab="frequency", cex.names=cex, cex.axis=cex, cex.lab=cex, tck=-0.04)
             abline(h=b, v=0, col="grey", lty="dotted")
             points( x=rev(tab), y=b, yaxt = "n", col=border, pch=21, bg=col, cex=1.3)
             box()  
             
             par(mai=c(1.5, 0.1, 0.2, .3)+.02) 
             b <- barplot( rev(ptab), horiz=TRUE, border=NA, col="white", las=1, names="", xlim=c(-0.04,1.04), xlab="percent", cex.names=cex, cex.axis=cex, cex.lab=cex, tck=-0.04)
             abline(h=b, v=0, col="grey", lty="dotted")
             points( x=rev(ptab), y=b, col=border, pch=21, bg=col, cex=1.3)
             box()
             
           }, bar = { # type = "bar" 
             if(is.null(col)) col <- "grey80"
             if(is.null(border)) border <- NA
             barplot( rev(tab), horiz=TRUE, col=col, border=border, las=1, xlim=range(pretty(tab)), xpd=FALSE, xlab="frequency", cex.names=cex, cex.axis=cex, cex.lab=cex, tck=-0.04)
             grid(ny=NA)
             
             par(mai=c(1.5, 0.15, 0.2, .3) + .02) 
             barplot( rev(ptab), horiz=TRUE, col=col, border=border, las=1, names="", xlim=c(0,1), xlab="percent", cex.names=cex, cex.axis=cex, cex.lab=cex, tck=-0.04)
             grid(ny=NA)
             
           })

    if(!is.na(main)) title(main=main, outer=TRUE)
    
    if (trunc_fg) 
      text(x = par()$usr[2], y = 0.4, labels = " ...[list output truncated]  ", 
           cex = 0.6, adj = c(1, 0.5))
    
    if(!is.null(wrd)) WrdPlot(width=8, height=pmin(2+3/6*nrow(ptab), 10), dfact=2.7, crop=c(0,0,1,0), wrd=wrd, append.cr=FALSE)
    
  }
  invisible()
  
}
