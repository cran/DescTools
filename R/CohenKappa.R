CohenKappa <-
function (x, y = NULL, weights = c("Unweighted", "Equal-Spacing", "Fleiss-Cohen"), conf.level = NA, ...) {

  if(!is.null(y)) {
    # we can not ensure a reliable weighted kappa for 2 factors with different levels
    # so refuse trying it... (unweighted is no problem)
    
    if(weights != "Unweighted") stop("Vector interface for weighted Kappa is not supported. Provide confusion matrix.")
    
    # x and y must have the same levels in order to build a symmetric confusion matrix
    x <- factor(x)
    y <- factor(y)
    lvl <- unique(c(levels(x), levels(y)))
    x <- factor(x, levels=lvl)
    y <- factor(y, levels=lvl)
    x <- table(x, y, ...)
  } else {
    d <- dim(x)
    if (d[1L] != d[2L]) stop("x must be square matrix if provided a confusion matrix")
  }  

  d <- diag(x)
  n <- sum(x)
  nc <- ncol(x)
  colFreqs <- colSums(x)/n
  rowFreqs <- rowSums(x)/n

  kappa <- function(po, pc) (po - pc)/(1 - pc)
  std <- function(po, pc, W = 1) sqrt(sum(W * W * po * (1 - po))/crossprod(1 - pc)/n)
  
  po <- sum(d)/n
  pc <- crossprod(colFreqs, rowFreqs)

  k <- kappa(po, pc)
  s <- std(po, pc)

  if (is.character(weights)) weights <- match.arg(weights)
  
  W <- if (is.matrix(weights)) 
    weights
  else if (weights == "Equal-Spacing") 
    1 - abs(outer(1:nc, 1:nc, "-"))/(nc - 1)
  else # weightx == "Fleiss-Cohen"
    1 - (abs(outer(1:nc, 1:nc, "-"))/(nc - 1))^2

  pow <- sum(W * x)/n
  pcw <- sum(W * colFreqs %o% rowFreqs)

  kw <- kappa(pow, pcw)
  sw <- std(x/n, 1 - pcw, W)

#   structure(list(Unweighted = c(value = k, ASE = s), Weighted = c(value = kw, 
#       ASE = sw), Weights = W), class = "Kappa")

  if (is.na(conf.level)) {
    if(identical(weights, "Unweighted")) 
      res <- as.vector(k)
    else 
      res <- as.vector(kw)
  } else {  
    if(identical(weights, "Unweighted")) {
      ci <- k + c(1,-1) * qnorm((1-conf.level)/2) * s
      res <- c("kappa"=k, lwr.ci=ci[1], upr.ci=ci[2])
    } else {
      ci <- kw + c(1,-1) * qnorm((1-conf.level)/2) * sw
      res <- c("kappa"=kw, lwr.ci=ci[1], upr.ci=ci[2])
    }
  }    
  return(res)
  
}
