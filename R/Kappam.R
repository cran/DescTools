Kappam <-
function(x, method = c("Fleiss", "Conger", "Light"), conf.level = NA) {
  
	ratings <- as.matrix(na.omit(x))

	ns <- nrow(ratings)
	nr <- ncol(ratings)

	# Build table
	lev <- levels(as.factor(ratings))

	for (i in 1:ns) {
		frow <- factor(ratings[i,],levels=lev)

		if (i==1)
			ttab <- as.numeric(table(frow))
		else
			ttab <- rbind(ttab, as.numeric(table(frow)))
	}

	ttab <- matrix(ttab, nrow=ns)
	agreeP <- sum((apply(ttab^2, 1, sum)-nr)/(nr*(nr-1))/ns)
  
	switch( match.arg(method, choices= c("Fleiss", "Conger", "Light")) 
    , "Fleiss" = {
      chanceP <- sum(apply(ttab,2,sum)^2)/(ns*nr)^2
      value <- (agreeP - chanceP)/(1 - chanceP)
      
      pj <- apply(ttab,2,sum)/(ns*nr)
      qj <- 1-pj
      
      varkappa <- (2/(sum(pj*qj)^2*(ns*nr*(nr-1))))*(sum(pj*qj)^2-sum(pj*qj*(qj-pj)))
      SEkappa <- sqrt(varkappa)
      
      ci <- value + c(1,-1) * qnorm((1-conf.level)/2) * SEkappa
    } 
    , "Conger" = {
      for (i in 1:nr) {
        rcol <- factor(ratings[,i],levels=lev)
        
        if (i==1)
          rtab <- as.numeric(table(rcol))
        else
          rtab <- rbind(rtab, as.numeric(table(rcol)))
      }
      
      rtab <- rtab/ns
      
      chanceP <- sum(apply(ttab,2,sum)^2)/(ns*nr)^2 - sum(apply(rtab,2,var)*(nr-1)/nr)/(nr-1)
      value <- (agreeP - chanceP)/(1 - chanceP)

      # we have not SE for exact Kappa value      
      ci <- c(NA, NA)
      
    } 
	  , "Light" = {
	    m <- PairApply(ratings, CohenKappa)
	    value <- mean(m[upper.tri(m)])
      
	    levlen <- length(lev)
	    for (nri in 1:(nr - 1)) for (nrj in (nri + 1):nr) {
	      for (i in 1:levlen) for (j in 1:levlen) {
	        if (i != j) {
	          r1i <- sum(ratings[, nri] == lev[i])
	          r2j <- sum(ratings[, nrj] == lev[j])
	          if (!exists("dis")) 
	            dis <- r1i * r2j
	          else dis <- c(dis, r1i * r2j)
	        }
	      }
	      if (!exists("disrater")) 
	        disrater <- sum(dis)
	      else disrater <- c(disrater, sum(dis))
	      rm(dis)
	    }
	    B <- length(disrater) * prod(disrater)
	    chanceP <- 1 - B/ns^(choose(nr, 2) * 2)
	    varkappa <- chanceP/(ns * (1 - chanceP))
	    SEkappa <- sqrt(varkappa)
	    
	    ci <- value + c(1,-1) * qnorm((1-conf.level)/2) * SEkappa
	    
	  } 
	)        

  
	if (is.na(conf.level)) {
    res <- value
	} else {
    res <- c("kappa"=value, lwr.ci=ci[1], upr.ci=ci[2])
	}    
	return(res)
	
}
