OddsRatio <-
function(x, y = NULL, conf.level = NA, method=c("wald", "mle", "midp")
                      , interval = c(0, 1000), ...) {

  if(!is.null(y)) x <- table(x, y, ...)
                      
  p <- (d <- dim(x))[1L]
  if(!is.numeric(x) || length(d) != 2L || p != d[2L] || p != 2L)
    stop("'x' is not a 2x2 numeric matrix")
  
  switch( match.arg( arg = method, choices = c("wald", "mle", "midp") )
          , "wald" = { 
            if(is.na(conf.level)){
              res <- (x[1,1]*x[2,2])/(x[1,2]*x[2,1])
            } else {
              or <- (x[1,1]*x[2,2])/(x[1,2]*x[2,1])
              sigma2lor <- sum(1/x)
              ci <- or * exp(c(1,-1) * qnorm((1-conf.level)/2) * sqrt(sigma2lor))
              res <- c("odds ratio"=or, lwr.ci=ci[1], upr.ci=ci[2])
            }    
          }
          , "mle" = { 
              if(is.na(conf.level)){
                res <- unname(fisher.test(x, conf.int=FALSE)$estimate)
              } else {  
                res <- fisher.test(x, conf.level=conf.level)
                res <- c(res$estimate, lwr.ci=res$conf.int[1], upr.ci=res$conf.int[2])
              }
          }
          , "midp" = {
            
              # based on code from Tomas J. Aragon Developer <aragon at berkeley.edu>
                
              a1 <- x[1,1]; a0 <- x[1,2]; b1 <- x[2,1]; b0 <- x[2,2]; or <- 1

              # median-unbiased estimate function
              mue <- function(a1, a0, b1, b0, or){
                mm <- matrix(c(a1,a0,b1,b0), 2, 2, byrow=TRUE)
                fisher.test(mm, or=or, alternative="l")$p-fisher.test(x=x, or=or, alternative="g")$p
              }
              ##mid-p function
              midp <- function(a1, a0, b1, b0, or = 1){
                mm <- matrix(c(a1,a0,b1,b0),2,2, byrow=TRUE)
                lteqtoa1 <- fisher.test(mm,or=or,alternative="l")$p.val
                gteqtoa1 <- fisher.test(mm,or=or,alternative="g")$p.val
                0.5*(lteqtoa1-gteqtoa1+1)
              }
              
              # root finding
              EST <- uniroot(
                   function(or){ mue(a1, a0, b1, b0, or)}, 
                   interval = interval)$root
               
              if(is.na(conf.level)){
                res <- EST
              } else {  

                alpha <- 1 - conf.level
                LCL <- uniroot(function(or){
                  1-midp(a1, a0, b1, b0, or)-alpha/2
                },  interval = interval)$root
                UCL <- 1/uniroot(function(or){
                  midp(a1, a0, b1, b0, or=1/or)-alpha/2
                },  interval = interval)$root
                
                res <- c("odds ratio" = EST, lwr.ci=LCL, upr.ci= UCL)
              }  
          }
  )
  return(res)
}
