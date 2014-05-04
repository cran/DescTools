PalDescTools <-
function(pal, n=100){
  
  palnames <- c("RedToBlack","RedBlackGreen","SteeblueWhite","RedWhiteGreen",
                "RedWhiteBlue0","RedWhiteBlue1","RedWhiteBlue2","RedWhiteBlue3","Helsana","Tibco")
  
  if(is.numeric(pal)){
    pal <- palnames[pal]
  }
  switch(pal
         , RedToBlack=colorRampPalette(c("red","yellow","green","blue","black"), space = "rgb")(n)
         , RedBlackGreen=colorRampPalette(c("red", "black", "green"), space = "rgb")(n)
         , SteeblueWhite=colorRampPalette(c("steelblue","white"), space = "rgb")(n)
         , RedWhiteGreen=colorRampPalette(c("red", "white", "green"), space = "rgb")(n)
         , RedWhiteBlue0=col <- colorRampPalette(c("red", "white", "blue"))(n)
         , RedWhiteBlue1=colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7",
                                            "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))(n)
         , RedWhiteBlue2=col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))(n)
         , RedWhiteBlue3=col <- colorRampPalette(c(hred, "white", hblue))(n)
         , Helsana = c("rot"="#9A0941", "orange"="#F08100", "gelb"="#FED037"
                       , "ecru"="#CAB790", "hellrot"="#D35186", "hellblau"="#8296C4", "hellgruen"="#B3BA12")
         , Tibco= col <- apply( mcol <- matrix(c(
           0,91,0, 0,157,69, 253,1,97, 60,120,177,
           156,205,36, 244,198,7, 254,130,1, 
           96,138,138, 178,113,60
         ), ncol=3, byrow=TRUE), 1, function(x) rgb(x[1], x[2], x[3], maxColorValue=255))
  ) 
}
