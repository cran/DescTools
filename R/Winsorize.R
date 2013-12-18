Winsorize <-
function(x, minval = quantile(x=x, probs=probs[1], na.rm=na.rm)
                      , maxval = quantile(x=x, probs=probs[2], na.rm=na.rm), probs=c(0.05, 0.95), na.rm = FALSE) {

  # following an idea from Gabor Grothendieck 
  # http://r.789695.n4.nabble.com/how-to-winsorize-data-td930227.html

  # in HuberM things are implemented the same way
  
  # don't eliminate NAs in x, moreover leave them untouched, 
  # just calc quantile without them...
  pmax(pmin(x, maxval), minval) 

  
  # see also Andreas Alfons, KU Leuven
  # roubustHD, Winsorize
  
  # Jim Lemon's rather clumsy implementation:
  
  # #added winsor.var and winsor.sd and winsor.mean (to supplement winsor.means) 
  # #August 28, 2009 following a suggestion by Jim Lemon
  # #corrected January 15, 2009 to use the quantile function rather than sorting.
  # #suggested by Michael Conklin in correspondence with Karl Healey
  # #this preserves the order of the data
  # "wins" <- function(x,trim=.2, na.rm=TRUE) {
    # if ((trim < 0) | (trim>0.5) ) 
        # stop("trimming must be reasonable")
      # qtrim <- quantile(x,c(trim,.5, 1-trim),na.rm = na.rm)
      # xbot <- qtrim[1]
      # xtop <- qtrim[3]
       # if(trim<.5) { 
      # x[x < xbot]  <- xbot
      # x[x > xtop] <- xtop} else {x[!is.na(x)] <- qtrim[2]}
     # return(x) } 

}
