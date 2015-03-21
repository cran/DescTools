PalDescTools <-
function(pal, n=100){
  
  palnames <- c("RedToBlack","RedBlackGreen","SteeblueWhite","RedWhiteGreen",
                "RedWhiteBlue0","RedWhiteBlue1","RedWhiteBlue2","RedWhiteBlue3","Helsana","Tibco","RedGreen1",
                "set1","set2","set3","dark2","accent","pastel1","pastel2","big","big2","dark","med","reg","light")
  
  if(is.numeric(pal)){
    pal <- palnames[pal]
  }
  big <- c("#800000", "#C00000", "#FF0000", "#FFC0C0",
          "#008000","#00C000","#00FF00","#C0FFC0",
          "#000080","#0000C0", "#0000FF","#C0C0FF",
          "#808000","#C0C000","#FFFF00","#FFFFC0",
          "#008080","#00C0C0","#00FFFF","#C0FFFF",
          "#800080","#C000C0","#FF00FF","#FFC0FF",
          "#C39004","#FF8000","#FFA858","#FFDCA8")
  
  switch(pal
         , RedToBlack= res <- colorRampPalette(c("red","yellow","green","blue","black"), space = "rgb")(n)
         , RedBlackGreen= res <- colorRampPalette(c("red", "black", "green"), space = "rgb")(n)
         , SteeblueWhite= res <- colorRampPalette(c("steelblue","white"), space = "rgb")(n)
         , RedWhiteGreen= res <- colorRampPalette(c("red", "white", "green"), space = "rgb")(n)
         , RedWhiteBlue0= res <- colorRampPalette(c("red", "white", "blue"))(n)
         , RedWhiteBlue1= res <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7",
                                            "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))(n)
         , RedWhiteBlue2= res <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))(n)
         , RedWhiteBlue3= res <- colorRampPalette(c(hred, "white", hblue))(n)
         , Helsana =  res <- c("rot"="#9A0941", "orange"="#F08100", "gelb"="#FED037"
                       , "ecru"="#CAB790", "hellrot"="#D35186", "hellblau"="#8296C4", "hellgruen"="#B3BA12")
         , Tibco=  res <- apply( mcol <- matrix(c(
                         0,91,0, 0,157,69, 253,1,97, 60,120,177,
                         156,205,36, 244,198,7, 254,130,1, 
                         96,138,138, 178,113,60
                          ), ncol=3, byrow=TRUE), 1, function(x) rgb(x[1], x[2], x[3], maxColorValue=255))
         , RedGreen1=  res <- c(rgb(227,0,11, maxColorValue=255), rgb(227,0,11, maxColorValue=255),
                     rgb(230,56,8, maxColorValue=255), rgb(234,89,1, maxColorValue=255),
                     rgb(236,103,0, maxColorValue=255), rgb(241,132,0, maxColorValue=255),
                     rgb(245,158,0, maxColorValue=255), rgb(251,184,0, maxColorValue=255),
                     rgb(253,195,0, maxColorValue=255), rgb(255,217,0, maxColorValue=255),
                     rgb(203,198,57, maxColorValue=255), rgb(150,172,98, maxColorValue=255),
                     rgb(118,147,108, maxColorValue=255))
         
         , set1 =  res <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3","#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")
         , set2 =  res <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3","#A6D854", "#FFD92F", "#E5C494", "#B3B3B3")
         , set3 =  res <- c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072","#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9","#BC80BD","#CCEBC5")
         , dark2 =  res <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A","#66A61E", "#E6AB02", "#A6761D", "#666666")
         , accent =  res <- c("#7FC97F", "#BEAED4", "#FDC086", "#FFFF99","#386CB0", "#F0027F", "#BF5B17", "#666666")
         , pastel1 =  res <- c("#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4","#FED9A6", "#FFFFCC", "#E5D8BD", "#FDDAEC", "#F2F2F2")
         , pastel2 =  res <- c("#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4","#E6F5C9", "#FFF2AE", "#F1E2CC", "#CCCCCC")
         , big =  res <- big
         , big2 =  res <- big[c(12,16,25,24,
                       2,11,6,15,18,26,23,
                       3,10,7,14,19,27,22,
                       4,8,20,28)]
         , dark =  res <- big[seq(1,28,by=4)]
         , med =  res <- big[seq(2,28,by=4)]
         , reg =  res <- big[seq(3,28,by=4)]
         , light = res <- big[seq(4,28,by=4)]
         
  )
  return(res)
}
