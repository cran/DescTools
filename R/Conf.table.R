Conf.table <-
function(x, pos = NULL, ...) {
  
  CollapseConfTab <- function(x, pos = NULL, ...) {
    
    if(nrow(x) > 2) {
      names(attr(x, "dimnames")) <- c("pred", "obs")
      x <- CollapseTable(x, obs=c("neg", pos)[(rownames(x)==pos)+1],
                         pred=c("neg", pos)[(rownames(x)==pos)+1])
    }
    
    # order confusion table so
    # that the positive class is the first and the others keep their position
    ord <- c(pos, rownames(x)[-grep(pos, rownames(x))])
    # the columnnames must be the same as the rownames
    x <- as.table(x[ord, ord])
    return(x)
  }
  
  p <- (d <- dim(x))[1L]
  if(!is.numeric(x) || length(d) != 2L || p != d[2L]) # allow nxn!  || p != 2L)
    stop("'x' is not a nxn numeric matrix")
  
  # observed in columns, predictions in rows
  if(!identical(rownames(x), colnames(x))) 
    stop("rownames(x) and colnames(x) must be identical")
  
  if(is.null(pos)) pos <- rownames(x)[1]
  if(nrow(x)!=2) {
    # ignore pos for nxn tables, pos makes only sense for sensitivity
    # and that is not defined for n-dim tables
    pos <- NULL    

  } else {
    # order 2x2-confusion table so
    # that the positive class is the first and the others keep their position
    ord <- c(pos, rownames(x)[-grep(pos, rownames(x))])
    # the columnnames must be the same as the rownames
    x <- as.table(x[ord, ord])
  }  
  
  # overall statistics first  
  res <- list(
    table   = x,
    pos     = pos,
    diag    = sum(diag(x)),
    n       = sum(x)
  )
  res <- c(res, 
           acc     = BinomCI(x=res$diag, n=res$n),
           sapply(binom.test(x=res$diag, n=res$n,
                             p=max(apply(x, 2, sum) / res$n),
                             alternative = "greater")[c("null.value", "p.value")], unname),
           kappa   = CohenKappa(x),
           mcnemar = mcnemar.test(x)$p.value
  )
  names(res) <- c("table","pos","diag","n","acc","acc.lci","acc.uci",
                  "nri","acc.pval","kappa","mcnemar.pval") 
  
  # byclass 
  lst <- list()
  for(i in 1:nrow(x)){
    
    z <- CollapseConfTab(x=x, pos=rownames(x)[i])
    A <- z[1, 1]; B <- z[1, 2]; C <- z[2, 1]; D <- z[2, 2]
    
    lst[[i]] <- rbind(
      sens    = A / (A + C),                 # sensitivity
      spec    = D / (B + D),                 # specificity
      ppv     = A / (A + B),                 # positive predicted value
      npv     = D / (C + D),                 # negative predicted value
      prev    = (A + C) / (A + B + C + D),   # prevalence
      detrate = A / (A + B + C + D),         # detection rate   
      detprev = (A + C) / (A + B + C + D),   # detection prevalence
      bacc    = mean(c(A / (A + C), D / (B + D)) )  # balanced accuracy
    )
  }
  
  res <- c(res, byclass=list(do.call(cbind, lst)))
  colnames(res[["byclass"]]) <- rownames(x)
  
  if(nrow(x)==2) res[["byclass"]] <- res[["byclass"]][, res[["pos"]], drop=FALSE]
  
  class(res) <- "Conf"
  
  return(res)
  
}
