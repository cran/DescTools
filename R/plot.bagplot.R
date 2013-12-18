plot.bagplot <-
function(x,
                       show.outlier=TRUE,# if TRUE outlier are shown
                       show.whiskers=TRUE, # if TRUE whiskers are shown
                       show.looppoints=TRUE, # if TRUE points in loop are shown
                       show.bagpoints=TRUE, # if TRUE points in bag are shown
                       show.loophull=TRUE, # if TRUE loop is shown
                       show.baghull=TRUE, # if TRUE bag is shown
                       add=FALSE, # if TRUE graphical elements are added to actual plot
                       pch=16,cex=.4, # to define further parameters of plot
                       verbose=FALSE, # tools for debugging
                       col.loophull="#aaccff", # Alternatives: #ccffaa, #ffaacc
                       col.looppoints="#3355ff", # Alternatives: #55ff33, #ff3355
                       col.baghull="#7799ff", # Alternatives: #99ff77, #ff7799
                       col.bagpoints="#000088", # Alternatives: #008800, #880000
                       transparency=FALSE,...
){
  if(missing(x)) return(
    "bagplot, version 2012/12/05, peter wolf"
  )
  # transparency flag and color flags have been proposed by wouter 
  if (transparency==TRUE) {
    col.loophull = paste(col.loophull, "99", sep="")
    col.baghull = paste(col.baghull, "99", sep="")
  }    
  
  win<-function(dx,dy){  atan2(y=dy,x=dx) }
  
  cut.z.pg<-function(zx,zy,p1x,p1y,p2x,p2y){
    a2<-(p2y-p1y)/(p2x-p1x); a1<-zy/zx
    sx<-(p1y-a2*p1x)/(a1-a2); sy<-a1*sx
    sxy<-cbind(sx,sy)
    h<-any(is.nan(sxy))||any(is.na(sxy))||any(Inf==abs(sxy))
    if(h){ # print("NAN found"); print(cbind(a1,a2,zx,zy,sxy,p2x-p1x))
      if(!exists("verbose")) verbose<-FALSE
      if(verbose) cat("special")
      # zx is zero ### 121030
      h<-0==zx 
      sx<-ifelse(h,zx,sx); sy<-ifelse(h,p1y-a2*p1x,sy)
      # points on line defined by line segment
      a1 <- ifelse( abs(a1) == Inf, sign(a1)*123456789*1E10, a1) # 121030
      a2 <- ifelse( abs(a2) == Inf, sign(a2)*123456789*1E10, a2)
      # points on line defined by line segment
      h<-0==(a1-a2) & sign(zx)==sign(p1x)
      sx<-ifelse(h,p1x,sx); sy<-ifelse(h,p1y,sy)
      h<-0==(a1-a2) & sign(zx)!=sign(p1x)
      sx<-ifelse(h,p2x,sx); sy<-ifelse(h,p2y,sy)
      # line segment vertical 
      #   & center NOT ON line segment
      h<-p1x==p2x & zx!=p1x & p1x!=0 
      sx<-ifelse(h,p1x,sx); sy<-ifelse(h,zy*p1x/zx,sy)
      #   & center ON line segment
      h<-p1x==p2x & zx!=p1x & p1x==0 
      sx<-ifelse(h,p1x,sx); sy<-ifelse(h,0,sy)
      #   & center NOT ON line segment & point on line     ### 121126
      h<-p1x==p2x & zx==p1x & p1x!=0 # & sign(zy)==sign(p1y)
      sx<-ifelse(h,zx,sx); sy<-ifelse(h,zy,sy)
      #   & center ON line segment & point on line
      h<-p1x==p2x & zx==p1x & p1x==0 & sign(zy)==sign(p1y)
      sx<-ifelse(h,p1x,sx); sy<-ifelse(h,p1y,sy)
      h<-p1x==p2x & zx==p1x & p1x==0 & sign(zy)!=sign(p1y)
      sx<-ifelse(h,p1x,sx); sy<-ifelse(h,p2y,sy)
      #  points identical to end points of line segment
      h<-zx==p1x & zy==p1y; sx<-ifelse(h,p1x,sx); sy<-ifelse(h,p1y,sy)
      h<-zx==p2x & zy==p2y; sx<-ifelse(h,p2x,sx); sy<-ifelse(h,p2y,sy)
      # point of z is center
      h<-zx==0 & zy==0; sx<-ifelse(h,0,sx); sy<-ifelse(h,0,sy)
      sxy<-cbind(sx,sy)
    } # end of special cases
    #if(verbose){ print(rbind(a1,a2));print(cbind(zx,zy,p1x,p1y,p2x,p2y,sxy))}
    if(!exists("debug.plots")) debug.plots<-"no"
    if(debug.plots=="all"){
      segments(sxy[,1],sxy[,2],zx,zy,col="red") 
      segments(0,0,sxy[,1],sxy[,2],col="green",lty=2) ##!!
      points(sxy,col="red")
    }
    return(sxy)
  } 
  
  find.cut.z.pg<-function(z,pg,center=c(0,0),debug.plots="no"){
    if(!is.matrix(z)) z<-rbind(z)
    if(1==nrow(pg)) return(matrix(center,nrow(z),2,TRUE))
    n.pg<-nrow(pg); n.z<-nrow(z)
    z<-cbind(z[,1]-center[1],z[,2]-center[2])
    pgo<-pg; pg<-cbind(pg[,1]-center[1],pg[,2]-center[2])
    if(!exists("debug.plots")) debug.plots<-"no"
    if(debug.plots=="all"){
      plot(rbind(z,pg,0),bty="n"); points(z,pch="p")
      lines(c(pg[,1],pg[1,1]),c(pg[,2],pg[1,2]))}
    # find angles of pg und z
    apg<-win(pg[,1],pg[,2])
    apg[is.nan(apg)]<-0; a<-order(apg); apg<-apg[a]; pg<-pg[a,]
    az<-win(z[,1],z[,2])
    # find line segments
    segm.no<-apply((outer(apg,az,"<")),2,sum)
    segm.no<-ifelse(segm.no==0,n.pg,segm.no)
    next.no<-1+(segm.no %% length(apg))
    # compute cut points
    cuts<-cut.z.pg(z[,1],z[,2],pg[segm.no,1],pg[segm.no,2],
                   pg[next.no,1],pg[next.no,2])
    # rescale 
    cuts<-cbind(cuts[,1]+center[1],cuts[,2]+center[2])
    return(cuts)
  }
  # find.cut.z.pg(EX,  EX1,center=CE)
  
  center<-hull.center<-hull.bag<-hull.loop<-pxy.bag<-pxy.outer<-pxy.outlier<-NULL
  # random.seed <-
  hdepths<-is.one.dim<-prdata<-xy<-xydata<-exp.dk<-exp.dk.1<-hdepth<-NULL
  tphdepth<-tp<-NULL
  #090216
  bagplotobj<-x
  for(i in seq(along=bagplotobj)) 
    eval(parse(text=paste(names(bagplotobj)[i],"<-bagplotobj[[",i,"]]")))
  if(is.one.dim){
    
    if(!verbose) cat("data set one dimensional") # 121202
    ROT<-round(prdata[[2]],digits=5); IROT<-round(solve(ROT),digits=5)
    if(!add){ ## 121008 ## 121130
      plot(xydata,type="n",bty="n",pch=16,cex=1, ...) # xlim=xlim, ylim=ylim, ...) 
    } 
    # find five points for box and whiskers
    usr <- par()$usr; xlim <- usr[1:2]; ylim <- usr[3:4]
    mins <- usr[c(1,3)]; ranges <- usr[c(2,4)] - mins
    if(ROT[1,1]==0){ #  cat("FALL senkrecht") 
      xydata <- cbind( mean(usr[1:2])  ,xydata[,2])
      boxplotres<-boxplot(xydata[,2],plot=FALSE)
      five<-cbind(mean(usr[1:2]),boxplotres$stat)
      dx <- 0.1*(xlim[2]-xlim[1]); dy <- 0
      idx.out <- if(0<length(boxplotres$out)) match(boxplotres$out, xydata[,2] ) else NULL
    }
    if(ROT[1,2]==0){ #  cat("FALL waagerecht") 
      xydata <- cbind( xydata[,1], mean(usr[3:4]))
      boxplotres<-boxplot(xydata[,1],plot=FALSE)
      five<-cbind(boxplotres$stat,mean(usr[3:4]))
      dx <- 0; dy <- 0.1*(ylim[2]-ylim[1]) # 1/5 of del.y
      idx.out <- if(0<length(boxplotres$out)) match(boxplotres$out, xydata[,1] ) else NULL
    }
    if(ROT[1,2]!=0 && ROT[1,1]!=0){
      xytr<-xydata%*%ROT
      boxplotres<-boxplot(xytr[,1],plot=FALSE)
      five<-cbind(boxplotres$stat,xytr[1,2])%*%IROT
      # find small vector for box height
      vec <- five[5,] - five[1,]
      vec.ortho <- c(vec[2],-vec[1]) * ranges / par()$pin 
      xy.delta <- vec.ortho * par()$pin[2:1] * ranges # plot region inches
      xy.delta <- xy.delta / sqrt( sum(xy.delta * xy.delta) ) 
      xy.delta <- xy.delta * .15 / ( sqrt(sum(abs(par()$pin*xy.delta/ranges)^2) ))
      dx <- xy.delta[1]; dy <- xy.delta[2]
      idx.out <- if(0<length(boxplotres$out)) match(boxplotres$out, xytr ) else NULL
    }
    # construct segments
    # whiskers
    segments(five[h<-c(1,5),1],five[h,2],five[h<-c(2,4),1],five[h,2], # col=col.looppoints,
             lwd=2)
    points(five[c(1,5),], cex=1, col=col.looppoints,pch=16)
    # box
    #segments(five[h<-2:4,1] + dx, five[h,2] + dy, five[h,1] - dx, five[h,2] - dy,
    #         col=col.bagpoints,lwd=2)
    #segments(five[2,1] + (h<-c(-1,1))*dx, five[2,2] + h*dy, 
    #         five[4,1] + h*dx, five[4,2] + h*dy,
    #         col=col.bagpoints,lwd=2)
    polygon(five[c(2,4,4,2,2),1] + c(dx,dx,-dx,-dx,dx), 
            five[c(2,4,4,2,2),2] + c(dy,dy,-dy,-dy,dy),
            col=col.baghull,lwd=1)  
    # median
    segments(five[h<-3  ,1] + dx, five[h,2] + dy,
             five[h,1] - dx, five[h,2] - dy,col="red",lwd=3)
    # Outlier
    if(0 < length(idx.out) && !is.na(idx.out[1])){ 
      points(xydata[idx.out,,drop=FALSE], cex=1, pch=16,col="red")
    }
    #  segments(five[3,1],five[3,2],five[3,1]+1*vec.ortho[1],
    #           five[3,2]+100*vec.ortho[2],col="green",lwd=5)
    #  segments(five[3,1],five[3,2],five[3,1]+1*vec1[1],
    #           five[3,2]+1*vec1[2],col="red",lwd=5)
    #  points(five,cex=2,col="green")
    return("one dimensional boxplot plottet")
  } else {
    
    if(!add) plot(xydata,type="n",pch=pch,cex=cex,bty="n",...)
    if(verbose) text(xy[,1],xy[,2],paste(as.character(hdepth))) # cex=2 needs fonts
    # loop: --************
    if(show.loophull){ # fill loop
      h<-rbind(hull.loop,hull.loop[1,]); lines(h[,1],h[,2],lty=1)
      polygon(hull.loop[,1],hull.loop[,2],col=col.loophull)
    }
    if(show.looppoints && 0 < length(pxy.outer)){ # points in loop
      points(pxy.outer[,1],pxy.outer[,2],col=col.looppoints,pch=pch,cex=cex)
    }
    # bag: --*****************
    if(show.baghull && 0 < length(hull.bag)){ # fill bag
      h<-rbind(hull.bag,hull.bag[1,]); lines(h[,1],h[,2],lty=1)
      polygon(hull.bag[,1],hull.bag[,2],col=col.baghull)
    }
    if(show.bagpoints && 0 < length(pxy.bag)){ # points in bag 
      points(pxy.bag[,1],pxy.bag[,2],col=col.bagpoints,pch=pch,cex=cex)
    }
    # whiskers
    if(show.whiskers && 0 < length(pxy.outer)){
      debug.plots<-"not"
      if((n<-length(xy[,1]))<15){
        segments(xy[,1],xy[,2],rep(center[1],n),rep(center[2],n),
                 col="red")
      }else{
        pkt.cut<-find.cut.z.pg(pxy.outer,hull.bag,center=center)
        segments(pxy.outer[,1],pxy.outer[,2],pkt.cut[,1],pkt.cut[,2],
                 col="red")
      }
    }
    # outlier: ---**********************
    if(show.outlier && 0 < length(pxy.outlier)){ # points in loop 
      points(pxy.outlier[,1],pxy.outlier[,2],col="red",pch=pch,cex=cex)
    }
    # center:
    if(exists("hull.center") && 2 < length(hull.center)){
      h<-rbind(hull.center,hull.center[1,]); lines(h[,1],h[,2],lty=1)
      polygon(hull.center[,1],hull.center[,2],col="orange")
    }
    if(!is.one.dim) points(center[1],center[2],pch=8,col="red")
    if(verbose && 0 < length(exp.dk.1) ){
      h<-rbind(exp.dk,exp.dk[1,]); lines(h,col="blue",lty=2)
      h<-rbind(exp.dk.1,exp.dk.1[1,]); lines(h,col="black",lty=2, lwd=3)
      if(exists("tphdepth") && 0<length(tphdepth))
        text(tp[,1],tp[,2],as.character(tphdepth),col="green")
      text(xy[,1],xy[,2],paste(as.character(hdepth)))  # cex=2 needs special fonts
      points(center[1],center[2],pch=8,col="red")
    }
    "bagplot plottet"
  }
}
