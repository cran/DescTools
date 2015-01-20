EtaSq.lm <-
function (x, type = 2, anova = FALSE) {
  
  # file:    etaSquared.R
  # author:  Dan Navarro
  # contact: daniel.navarro@adelaide.edu.au
  # changed: 13 November 2013
  # modified by Daniel Wollschlaeger 17.9.2014
  
  # etaSquared() calculates eta-squared and partial eta-squared for linear models
  # (usually ANOVAs). It takes an lm object as input and computes the effect size
  # for all terms in the model. By default uses Type II sums of squares to calculate
  # the effect size, but Types I and III are also possible. By default the output
  # only displays the effect size, but if requested it will also print out the full
  # ANOVA table.
  
  if (!is(anova, "logical") | length(anova) != 1) {
    stop("\"anova\" must be a single logical value")
  }
  if (!is(type, "numeric") | length(type) != 1) {
    stop("type must be equal to 1, 2 or 3")
  }
  if (type == 1) {
    ss <- anova(x)[, "Sum Sq", drop = FALSE]
    ss.res <- ss[dim(ss)[1], ]
    ss.tot <- sum(ss)
    ss <- ss[-dim(ss)[1], , drop = FALSE]
    ss <- as.matrix(ss)
  }
  else {
    if (type == 2) {
      ss.tot <- sum((x$model[, 1] - mean(x$model[, 1]))^2)
      ss.res <- sum((x$residuals)^2)
      terms <- attr(x$terms, "factors")[-1, , drop = FALSE]
      l <- attr(x$terms, "term.labels")
      ss <- matrix(NA, length(l), 1)
      rownames(ss) <- l
      for (i in seq_along(ss)) {
        vars.this.term <- which(terms[, i] != 0)
        dependent.terms <- which(apply(terms[vars.this.term, , drop = FALSE], 2, prod) > 0)
        m0 <- lm(x$terms[-dependent.terms], x$model)
        if (length(dependent.terms) > 1) {
          m1 <- lm(x$terms[-setdiff(dependent.terms, i)], x$model)
          ss[i] <- anova(m0, m1)$`Sum of Sq`[2]
        }
        else {
          ss[i] <- anova(m0, x)$`Sum of Sq`[2]
        }
      }
    }
    else {
      if (type == 3) {
        ## check if model was fitted with sum-to-zero contrasts
        ## necessary for valid SS type 3 (e.g., contr.sum, contr.helmert)
        IVs <- names(attr(model.matrix(x), "contrasts"))
        ## only relevant for more than one factor
        ## (and for unbalanced cell sizes and interactions, not tested here)
        if(length(IVs) > 1) {
          isSumToZero <- function(IV) {
            ## check if factor has directly associated contrasts
            if(!is.null(attr(x$model[, IV], "contrasts"))) {
              cm <- contrasts(x$model[, IV])
              all(colSums(cm) == 0)
            } else {
              ## check attributes from model matrix
              attr(model.matrix(x), "contrasts")[[IV]] %in% c("contr.sum", "contr.helmert")
            }
          }
          
          valid <- vapply(IVs, isSumToZero, logical(1))
          
          if(!all(valid)) {
            warning(c(ifelse(sum(!valid) > 1, "Factors ", "Factor "),
                      paste(IVs[!valid], collapse=", "),
                      ifelse(sum(!valid) > 1, " are", " is"),
                      " not associated with sum-to-zero contrasts",
                      " necessary for valid SS type III",
                      " when cell sizes are unbalanced",
                      " and interactions are present.",
                      " Consider re-fitting the model after setting",
                      " options(contrasts=c(\"contr.sum\", \"contr.poly\"))"))
          }
        }
        
        mod <- drop1(x, scope = x$terms)
        ss <- mod[-1, "Sum of Sq", drop = FALSE]
        ss.res <- mod[1, "RSS"]
        ss.tot <- sum((x$model[, 1] - mean(x$model[, 1]))^2)
        ss <- as.matrix(ss)
      }
      else {
        stop("type must be equal to 1, 2 or 3")
      }
    }
  }
  if (anova == FALSE) {
    eta2 <- ss/ss.tot
    eta2p <- ss/(ss + ss.res)
    E <- cbind(eta2, eta2p)
    rownames(E) <- rownames(ss)
    colnames(E) <- c("eta.sq", "eta.sq.part")
  }
  else {
    ss <- rbind(ss, ss.res)
    eta2 <- ss/ss.tot
    eta2p <- ss/(ss + ss.res)
    k <- length(ss)
    eta2p[k] <- NA
    df <- anova(x)[, "Df"]
    ms <- ss/df
    Fval <- ms/ms[k]
    p <- 1 - pf(Fval, df, rep.int(df[k], k))
    E <- cbind(eta2, eta2p, ss, df, ms, Fval, p)
    E[k, 6:7] <- NA
    colnames(E) <- c("eta.sq", "eta.sq.part", "SS", "df", "MS", "F", "p")
    rownames(E) <- rownames(ss)
    rownames(E)[k] <- "Residuals"
  }
  return(E)
}
