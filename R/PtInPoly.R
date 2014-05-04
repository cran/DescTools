PtInPoly <-
function(pnts, poly.pnts)  
{
  #check if pnts & poly is 2 column matrix or dataframe
  pnts = as.matrix(pnts); poly.pnts = as.matrix(poly.pnts)
  if (!(is.matrix(pnts) & is.matrix(poly.pnts))) stop('pnts & poly.pnts must be a 2 column dataframe or matrix')
  if (!(dim(pnts)[2] == 2 & dim(poly.pnts)[2] == 2)) stop('pnts & poly.pnts must be a 2 column dataframe or matrix')
  
  #ensure first and last polygon points are NOT the same
  if (poly.pnts[1,1] == poly.pnts[nrow(poly.pnts),1] & poly.pnts[1,2] == poly.pnts[nrow(poly.pnts),2]) poly.pnts = poly.pnts[-1,]
  
  #run the point in polygon code
  out = .Call('pip',pnts[,1],pnts[,2],nrow(pnts),poly.pnts[,1],poly.pnts[,2],nrow(poly.pnts))
  
  #return the value
  return(data.frame(pnts,pip=out))
}
