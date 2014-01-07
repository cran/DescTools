PlotMatrix <-
function(x, y=NULL, data=NULL, panel=l.panel,
         nrows=0, ncols=nrows, save=TRUE, robrange.=FALSE, range.=NULL,
         pch=NULL, col=1, reference=0, ltyref=3,
         log="", xaxs="r", yaxs="r", xaxmar=NULL, yaxmar=NULL, 
         vnames=NULL, main='', cex.points=NA, cex.lab=0.7, cex.text=1.3,
         cex.title=1,
         bty="o", oma=NULL, ...) {
         
# Purpose:    pairs  with different plotting characters, marks and/or colors
#             showing submatrices of the full scatterplot matrix
#             possibly on several pages
# ******************************************************************************
# Author: Werner Stahel, Date: 23 Jul 93; minor bug-fix+comments:
  # M.Maechler

  is.formula <- function(object) length(class(object))>0 && class(object)=="formula"

  
  l.panel <- function(x,y,indx,indy,pch=1,col=1,cex=cex.points,...) {
    if (is.character(pch)) text(x,y,pch,col=col,cex=cex) else
    points(x,y,pch=pch,col=col,cex=cex,...)
  }
  oldpar <- par(c("mfrow","mar","cex","oma","mgp"))
  on.exit(par(oldpar))
# **************** preparations **************
# data
  if (is.formula(x))  {
    if (length(x)==2)
    x <- model.frame(x,data, na.action=NULL)  else {
      ld <- model.frame(x[c(1,3)],data, na.action=NULL)
      ld <- cbind(ld, model.frame(x[1:2],data, na.action=NULL))
      x <- ld
    }
  }
  if (is.data.frame(x)) {
    for (jj in 1:length(x)) x[[jj]] <- as.numeric(x[[jj]])
    x <- as.matrix(x)
  } else x <- cbind(x)
#  stop("!PlotMatrix! first argument must either be a formula or a data.frame or matrix")
  nv1 <- dim(x)[2]
  lv1 <- lv2 <- 0
  if (is.null(y)) {
    ldata <- x
    if (save) { nv1 <- nv1-1; lv2 <- 1 }
    nv2 <- nv1
  } else { # cbind y to data for easier preparations
    save <- FALSE
    if (is.formula(y))  {
      ld <- model.frame(x[c(1,3)],data, na.action=NULL)
    if (length(x)>2)
      ld <- cbind(ld, model.frame(x[1:2],data, na.action=NULL))
    x <- ld
  }
    if (is.formula(y)) {
      if (length(y)==2)
        y <- model.frame(y,data, na.action=NULL)  else {
          ld <- model.frame(y[c(1,3)],data, na.action=NULL)
          ld <- cbind(ld, model.frame(y[1:2],data, na.action=NULL))
          y <- ld
        }
    }
    if (is.data.frame(y)) {
      for (jj in 1:length(y)) y[[jj]] <- as.numeric(y[[jj]])
      y <- as.matrix(y)
    }
    ldata <- cbind(x, as.matrix(y))
    nv2 <- ncol(ldata)-nv1 ; lv2 <- nv1 }
  nvv <- ncol(ldata)
  tnr <- nrow(ldata)
# variable labels
  if (missing(vnames)) vnames <- dimnames(ldata)[[2]]
  if (is.null(vnames)) vnames <- paste("V",1:nvv)
# plotting characters
  if (length(pch)==0) pch <- 1
# range
  rg <- matrix(nrow=2,ncol=nvv,dimnames=list(c("min","max"),vnames))
  if(is.matrix(range.)) {
    if (is.null(colnames(range.))) {
      if (ncol(range)==ncol(rg)) rg[,] <- range.  else
      warning('argument  range.  not suitable. ignored')
    } else {
      lj <- match(colnames(range.),vnames)
      if (any(is.na(lj))) {
        warning('variables', colnames(range.)[is.na(lj)],'not found')
        if (any(!is.na(lj))) rg[,lj[!is.na(lj)]] <- range.[,!is.na(lj)]
      }
    }
  }
  else
    if (length(range.)==2&&is.numeric(range.)) rg[,] <- matrix(range.,2,nvv)

  lna <- apply(is.na(rg),2, any)
  if (any(lna))
    rg[,lna] <- apply(ldata[,lna,drop=FALSE],2,
      if(robrange.) RobRange else range, na.rm=TRUE, finite=TRUE)
  colnames(rg) <- vnames
# reference lines
  tjref <- (length(reference)>0)&&!(is.logical(reference)&&!reference)
  if (tjref) {
    if(length(reference)==1) lref <- rep(reference,length=nvv) else {
      lref <- rep(NA,nvv)
      lref[match(names(reference),vnames)] <- reference
    }
    names(lref) <- vnames
  }
# plot
  jmain <- !is.null(main)&&main!=""
  lpin <- par("pin")
  lnm <- if (lpin[1]>lpin[2]) {
    if (nv1==6 && nv2==6) c(6,6) else c(5,6) } else c(8,5)
  if (is.na(nrows)||nrows<1) nrows <- ceiling(nv1/((nv1-1)%/%lnm[1]+1))
  if (is.na(ncols)||ncols<1) ncols <- ceiling(nv2/((nv2-1)%/%lnm[2]+1))
  if (is.null(xaxmar)) xaxmar <- 1+(nv1*nv2>1)
  if (any(is.na(xaxmar))) xaxmar <- 1+(nv1*nv2>1)
  xaxmar <- ifelse(xaxmar>1,3,1)
  if (is.null(yaxmar)) yaxmar <- 2+(nv1*nv2>1)
  if (any(is.na(yaxmar))) yaxmar <- 2+(nv1*nv2>1)
  yaxmar <- ifelse(yaxmar>2,4,2)
  if (length(oma)!=4)
    oma <- c(2+(xaxmar==1), 2+(yaxmar==2),
             1.5+(xaxmar==3)+cex.title*2*jmain,
             2+(yaxmar==4))
#    oma <- 2 + c(0,0,!is.null(main)&&main!="",1)
  par(mfrow=c(nrows,ncols))
##-   if (!is.na(cex)) par(cex=cex)
##-   cex <- par("cex")
##-   cexl <- cex*cexlab
##-   cext <- cex*cextext
  par(oma=oma*cex.lab, mar=rep(0.2,4), mgp=cex.lab*c(1,0.5,0))
  if (is.na(cex.points)) cex.points <- max(0.2,min(1,1.5-0.2*log(tnr)))
#
  # log
  if (length(grep("x",log))>0) ldata[ldata[,1:nv1]<=0,1:nv1] <- NA
  if (length(grep("y",log))>0) ldata[ldata[,lv2+1:nv2]<=0,lv2+1:nv2] <- NA
  npgr <- ceiling(nv2/nrows)
  npgc <- ceiling(nv1/ncols)
# ******************** plots **********************
  for (ipgr in 1:npgr) {
    lr <- (ipgr-1)*nrows
  for (ipgc in 1:npgc) {
    lc <- (ipgc-1)*ncols
    if (save&&((lr+nrows)<=lc)) break
  for (jr in 1:nrows) { #-- plot row [j]
    jd2 <- lr+jr
    j2 <- lv2 + jd2
    if (jd2<=nv2)  v2 <- ldata[,j2]
    for (jc in 1:ncols) { #-- plot column  [j2-lv2] = 1:nv2
      jd1 <- lc+jc
      j1 <- lv1 + jd1
    if (jd2<=nv2 & jd1<=nv1) {
      v1 <- ldata[,j1]
      plot(v1,v2, type="n", xlab="", ylab="", axes=FALSE,
           xlim <- rg[,j1], ylim <- rg[,j2],
           xaxs=xaxs, yaxs=yaxs, log=log, cex=cex.points)
      usr <- par("usr")
      if (jr==nrows||jd2==nv2) {
        if (xaxmar==1) axis(1) 
        mtext(vnames[j1], side=1, line=(0.5+1.2*(xaxmar==1))*cex.lab,
              cex=cex.lab, at=mean(usr[1:2]))
      }
      if (jc==1) {
        if (yaxmar==2) axis(2) 
        mtext(vnames[j2], side=2, line=(0.5+1.2*(yaxmar==2))*cex.lab,
              cex=cex.lab, at=mean(usr[3:4]))
      }
      if (jr==1&&xaxmar==3) axis(3,xpd=TRUE)
      if (jc==ncols||jd1==nv1) if (yaxmar==4) axis(4,xpd=TRUE)
      box(bty=bty)
      if (any(v1!=v2,na.rm=TRUE)) { # not diagonal
        panel(v1,v2,jd1,jd2, pch, col, ...)
        if (tjref) abline(h=lref[j1],v=lref[j2],lty=ltyref)
      }
      else { uu <- par("usr") # diagonal: print variable name
             text(mean(uu[1:2]),mean(uu[3:4]), vnames[j1], cex=cex.text) }
    }
      else frame()
    }
  }
  if (jmain) mtext(main,3,oma[3]*0.9-2*cex.title,outer=TRUE,cex=cex.title)
##-   stamp(sure=FALSE,line=par("mgp")[1]+0.5)
#  stamp(sure=FALSE,line=oma[4]-1.8) ### ??? why does it need so much space?
  }}
  on.exit(par(oldpar))
  "PlotMatrix: done"
}
