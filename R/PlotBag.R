PlotBag <-
function(x, y,
                    factor=3, # expanding factor for bag to get the loop
                    na.rm=FALSE, # should 'NAs' values be removed or exchanged
                    approx.limit=300, # limit 
                    show.outlier=TRUE,# if TRUE outlier are shown
                    show.whiskers=TRUE, # if TRUE whiskers are shown
                    show.looppoints=TRUE, # if TRUE points in loop are shown
                    show.bagpoints=TRUE, # if TRUE points in bag are shown
                    show.loophull=TRUE, # if TRUE loop is shown
                    show.baghull=TRUE, # if TRUE bag is shown
                    create.plot=TRUE, # if TRUE a plot is created 
                    add=FALSE, # if TRUE graphical elements are added to actual plot
                    pch=16,cex=.4, # some graphical parameters
                    dkmethod=2, # in 1:2; there are two methods for approximating the bag
                    precision=1, # controls precision of computation
                    verbose=FALSE,debug.plots="no", # tools for debugging
                    col.loophull="#aaccff", # Alternatives: #ccffaa, #ffaacc
                    col.looppoints="#3355ff", # Alternatives: #55ff33, #ff3355
                    col.baghull="#7799ff", # Alternatives: #99ff77, #ff7799
                    col.bagpoints="#000088", # Alternatives: #008800, #880000
                    transparency=FALSE, ... # to define further parameters of plot
){
  if(missing(x)) return(
    "bagplot, version 2012/12/05, peter wolf"
  )
  bo<-compute.bagplot(x=x,y=y,factor=factor,na.rm=na.rm,
                      approx.limit=approx.limit,dkmethod=dkmethod,
                      precision=precision,verbose=verbose,debug.plots=debug.plots)
  if(create.plot){ 
    plot(bo,
         show.outlier=show.outlier,
         show.whiskers=show.whiskers,
         show.looppoints=show.looppoints,
         show.bagpoints=show.bagpoints,
         show.loophull=show.loophull,
         show.baghull=show.baghull,
         add=add,pch=pch,cex=cex,
         verbose=verbose,
         col.loophull=col.loophull,
         col.looppoints=col.looppoints,
         col.baghull=col.baghull,
         col.bagpoints=col.bagpoints,
         transparency=transparency, ...
    )
  }
  invisible(bo)
}
