EtaSq.aovlist <-
function (x, type = 2, anova = FALSE) {
    
    # author:  Daniel Wollschlaeger
    # contact: contact@dwoll.de
    # changed: 13 October 2014
    
    # EtaSq.aovlist() calculates partial eta-squared and generalized eta-squared
    # for aovlists
    
    if (!is(anova, "logical") | length(anova) != 1) {
      stop("\"anova\" must be a single logical value")
    }
    if (!is(type, "numeric") | length(type) != 1) {
      stop("type must be equal to 1, 2 or 3")
    }
    
    ## alternative: check design has balanced cell sizes
    if (type != 1) {
      stop("type must be equal to 1")
    }
    
    details <- aovlDetails(x)
    ss      <- details$Sum.Sq             # effect SS
    ss.res  <- sum(aovlErrorTerms(x)$SS)  # total error SS
    ss.tot  <- sum(ss) + sum(ss.res)
    
    # eta squared
    eta2 <- ss / ss.tot
    
    # partial eta squared
    # cf. Bakeman, R. (2005) Behavior Research Methods. 37(3), 379-384. Tables 1, 2
    eta2p <- ss / (ss + details$SSE)
    
    # generalized eta squared
    # if all factors are manipulated
    # cf. Bakeman, R. (2005) Behavior Research Methods. 37(3), 379-384. Tables 1, 2
    geta2 <- ss / (ss + sum(ss.res))
    
    if (anova == FALSE) {
      E <- cbind(eta2, eta2p, geta2)
      rownames(E) <- details$tt
      colnames(E) <- c("eta.sq", "eta.sq.part", "eta.sq.gen")
    } else {
      E <- data.frame(eta2=eta2,
                      eta2p=eta2p,
                      geta2=geta2,
                      ss=ss,
                      df=details$Df,
                      ms=details$Mean.Sq,
                      sse=details$SSE,
                      dfe=details$dfE,
                      Fval=details$F.value,
                      p=details$Pr..F.)
      colnames(E) <- c("eta.sq", "eta.sq.part", "eta.sq.gen", "SS", "df", "MS", "SSE", "dfE", "F", "p")
      rownames(E) <- details$tt
    }
    return(E)
  }
