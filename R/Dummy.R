Dummy <-
function (x, method = c("treatment", "sum", "helmert", "poly", "full"),  base = 1) {
  
  # Alternatives:
  # options(contrasts = c("contr.sum", "contr.poly"))
  # model.matrix(~x.)[, -1]               ### und die dummy-codes
  # or Ripley's brilliant shorty-function:
  #   diag(nlevels(x))[x,]
  
  x <- as.factor(x)
  if(!is.numeric(base)) base <- match(base, levels(x))

  method <- match.arg( arg = method, choices = c("treatment", "sum", "helmert", "poly", "full") )

  switch( method 
    , "treatment" = { res <- contr.treatment(n = nlevels(x), base = base)[x,] }
    , "sum" = { res <- contr.sum(n = nlevels(x))[x,] }
    , "helmert" = { res <- contr.helmert(n = nlevels(x))[x,] }
    , "poly" = { res <- contr.poly(n = nlevels(x))[x,] }
    , "full" = { res <- diag(nlevels(x))[x,] }
  )
  res <- as.matrix(res) # force res to be matrix, avoiding res being a vector if nlevels(x) = 2

  if(method=="full") {
    dimnames(res) <- list(if(is.null(names(x))) 1:length(x) else names(x), levels(x))
    attr(res, "base") <- NA
  } else {
    dimnames(res) <- list(if(is.null(names(x))) 1:length(x) else names(x), levels(x)[-base])
    attr(res, "base") <- levels(x)[base]
  }
  return(res)
}
