ICC <-
function(ratings, type=c("all", "ICC1","ICC2","ICC3","ICC1k","ICC2k","ICC3k"), conf.level = NA, na.rm = FALSE) {
  
  ratings <- as.matrix(ratings)
  if(na.rm) ratings <- na.omit(ratings)
  
  ns <- nrow(ratings)
  nr <- ncol(ratings)
  
  x.s <- stack(data.frame(ratings))
  x.df <- data.frame(x.s, subs = rep(paste("S", 1:ns, sep = ""), nr)) 
                                     
  s.aov <- summary(aov(values ~ subs + ind, data=x.df))
  stats <- matrix(unlist(s.aov), ncol=3, byrow=TRUE)
  MSB <- stats[3,1]
  MSW <- (stats[2,2] + stats[2,3])/(stats[1,2] + stats[1,3])
  MSJ <- stats[3,2]
  MSE <- stats[3,3]
  
  ICC1 <- (MSB- MSW)/(MSB+ (nr-1)*MSW)
  ICC2 <- (MSB- MSE)/(MSB + (nr-1)*MSE + nr*(MSJ-MSE)/ns)
  ICC3 <- (MSB - MSE)/(MSB+ (nr-1)*MSE)
  ICC12 <- (MSB-MSW)/(MSB)
  ICC22 <- (MSB- MSE)/(MSB +(MSJ-MSE)/ns)
  ICC32 <- (MSB-MSE)/MSB
  
  #find the various F values from Shrout and Fleiss 
  F11 <- MSB/MSW
  df11n <- ns-1
  df11d <- ns*(nr-1)
  p11 <- 1 - pf(F11, df11n, df11d)
  F21 <- MSB/MSE
  df21n <- ns-1
  df21d <- (ns-1)*(nr-1)
  p21 <- 1-pf(F21, df21n, df21d)
  F31 <- F21
  
  
  # results <- t(results)
  
  results <- data.frame(matrix(NA, ncol=8, nrow=6))
  colnames(results ) <- c("type", "est","F-val","df1","df2","p-val","lwr.ci","upr.ci")
  rownames(results) <- c("Single_raters_absolute","Single_random_raters","Single_fixed_raters", "Average_raters_absolute","Average_random_raters","Average_fixed_raters")

  results[,1] = c("ICC1","ICC2","ICC3","ICC1k","ICC2k","ICC3k")
  results[,2] = c(ICC1, ICC2, ICC3, ICC12, ICC22, ICC32)
  results[1,3] <- results[4,3] <- F11  
  results[2,3] <- F21
  results[3,3] <- results[6,3] <- results[5,3] <- F31 <- F21 
  results[5,3] <- F21  
  results[1,4] <- results[4,4] <- df11n
  results[1,5] <- results[4,5] <- df11d
  results[1,6] <- results[4,6] <- p11
  results[2,4] <- results[3,4] <- results[5,4] <- results[6,4] <- df21n
  results[2,5] <- results[3,5] <- results[5,5] <- results[6,5] <- df21d
  results[2,6] <- results[5,6] <- results[3,6] <- results[6,6] <- p21
  
  #now find confidence limits
  #first, the easy ones
  alpha <- 1 - conf.level
  F1L <- F11 / qf(1-alpha/2, df11n, df11d)  
  F1U <- F11 * qf(1-alpha/2, df11d, df11n)
  L1 <- (F1L-1) / (F1L + (nr - 1))
  U1 <- (F1U -1) / (F1U + nr - 1)
  F3L <- F31 / qf(1-alpha/2, df21n, df21d)
  F3U <- F31 * qf(1-alpha/2, df21d, df21n)
  results[1,7] <- L1
  results[1,8] <- U1
  results[3,7] <- (F3L-1)/(F3L+nr-1)
  results[3,8] <- (F3U-1)/(F3U+nr-1)
  results[4,7] <- 1- 1/F1L
  results[4,8] <- 1- 1/F1U
  results[6,7] <- 1- 1/F3L
  results[6,8] <- 1 - 1/F3U
  
  #the hard one is case 2   
  Fj <- MSJ/MSE
  vn <- (nr-1)*(ns-1)* ( (nr*ICC2*Fj+ns*(1+(nr-1)*ICC2) - nr*ICC2))^2
  vd <- (ns-1)*nr^2 * ICC2^2 * Fj^2 + (ns *(1 + (nr-1)*ICC2) - nr*ICC2)^2
  v <- vn/vd
  F3U <- qf(1-alpha/2,ns-1,v) 
  F3L <- qf(1-alpha/2,v,ns-1)
  
  L3 <- ns *(MSB- F3U*MSE)/(F3U*(nr * MSJ + (nr*ns-nr-ns) * MSE)+ ns*MSB)
  results[2, 7] <- L3
  U3 <- ns *(F3L * MSB - MSE)/(nr * MSJ + (nr * ns - nr - ns)*MSE + ns * F3L * MSB)
  results[2, 8] <- U3
  L3k <- L3 * nr/(1+ L3*(nr-1))
  U3k <- U3 * nr/(1+ U3*(nr-1))
  results[5, 7] <- L3k
  results[5, 8] <- U3k
  
  
  #clean up the output
  results[,2:8] <- results[,2:8]
  
  type <- match.arg(type, c("all", "ICC1","ICC2","ICC3","ICC1k","ICC2k","ICC3k"))

  switch(type
         , all={res <- list(results=results, summary=s.aov, stats=stats, MSW=MSW, ns=ns, nr=nr)
                class(res) <- "ICC"
           }
         , ICC1={idx <- 1}
         , ICC2={idx <- 2}
         , ICC3={idx <- 3}
         , ICC1k={idx <- 4}
         , ICC2k={idx <- 5}
         , ICC3k={idx <- 6}
  )
  
  if(type!="all"){
    if(is.na(conf.level)){
      res <- results[idx, c(2)][,]
    } else {
      res <- unlist(results[idx, c(2, 7:8)])
      names(res) <- c(type,"lwr.ci","upr.ci")
    }
  }

  return(res)
    
}
