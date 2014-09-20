KendallW <-
function(ratings, correct=FALSE, test=FALSE) {
 
  # see also old Jim Lemon function kendall.w
  # other solution: library(irr);  kendall(ratings, correct = TRUE)
  # http://www.real-statistics.com/reliability/kendalls-w/
  
  
  dname <- deparse(substitute(ratings))
    ratings <- as.matrix(na.omit(ratings))
    
    ns <- nrow(ratings)
    nr <- ncol(ratings)

    
    #Without correction for ties
    if (!correct) {
      #Test for ties
      TIES = FALSE
      testties <- apply(ratings, 2, unique)
      if (!is.matrix(testties)) TIES=TRUE
      else { if (length(testties) < length(ratings)) TIES=TRUE }
      
      ratings.rank <- apply(ratings,2,rank)
      
      coeff.name <- "W"
      coeff <- (12*var(apply(ratings.rank,1,sum))*(ns-1))/(nr^2*(ns^3-ns))
    }
    else { #With correction for ties
      ratings <- as.matrix(na.omit(ratings))
      
      ns <- nrow(ratings)
      nr <- ncol(ratings)
      
      ratings.rank <- apply(ratings,2,rank)
      
      Tj <- 0
      for (i in 1:nr) {
        rater <- table(ratings.rank[,i])
        ties  <- rater[rater>1]
        l 	  <- as.numeric(ties)
        Tj	  <- Tj + sum(l^3-l)
      }
      
      coeff.name <- "Wt"
      coeff <- (12*var(apply(ratings.rank,1,sum))*(ns-1))/(nr^2*(ns^3-ns)-nr*Tj)
    }
    
    if(test){
      #test statistics
      Xvalue  <- nr*(ns-1)*coeff
      df1     <- ns-1
      names(df1) <- "df"
      p.value <- pchisq(Xvalue, df1, lower.tail = FALSE)
      method <- paste("Kendall's coefficient of concordance", coeff.name)
      alternative <- paste(coeff.name, "is greater 0")
      names(ns) <- "subjects"
      names(nr) <- "raters"
      names(Xvalue) <- "Kendall chi-squared" 
      names(coeff) <- coeff.name
      rval <- list(#subjects = ns, raters = nr,
                   estimate = coeff, parameter=c(df1, ns, nr),
                   statistic = Xvalue, p.value = p.value,
                   alternative = alternative, method = method, data.name = dname)

      class(rval) <- "htest"
    } else {
      rval <- coeff
    }
  if (!correct && TIES) warning("Coefficient may be incorrect due to ties")
  return(rval)
}
