Strata <-
function (data, stratanames = NULL, size, 
                    method = c("srswor", "srswr", "poisson", "systematic"), 
                    pik, description = FALSE) 
{
  
#  Author: Yves Tille <yves.tille@unine.ch>, Alina Matei <alina.matei@unine.ch>
#  source: library(sampling)  
  
  inclusionprobabilities <- function (a, n) 
  {
    nnull = length(a[a == 0])
    nneg = length(a[a < 0])
    if (nnull > 0) 
      warning("there are zero values in the initial vector a\n")
    if (nneg > 0) {
      warning("there are ", nneg, " negative value(s) shifted to zero\n")
      a[(a < 0)] = 0
    }
    if (identical(a, rep(0, length(a)))) 
      pik1 = a
    else {
      pik1 = n * a/sum(a)
      pik = pik1[pik1 > 0]
      list1 = pik1 > 0
      list = pik >= 1
      l = length(list[list == TRUE])
      if (l > 0) {
        l1 = 0
        while (l != l1) {
          x = pik[!list]
          x = x/sum(x)
          pik[!list] = (n - l) * x
          pik[list] = 1
          l1 = l
          list = (pik >= 1)
          l = length(list[list == TRUE])
        }
        pik1[list1] = pik
      }
    }
    pik1
  }
  
  srswor <- function (n, N) 
  {
    s <- rep(0, times = N)
    s[sample(N, n)] <- 1
    s
  }
  
  srswr <-  function (n, N) 
    as.vector(rmultinom(1, n, rep(n/N, times = N)))
  
  
  UPsystematic <- function (pik, eps = 1e-06) 
  {
    if (any(is.na(pik))) 
      stop("there are missing values in the pik vector")
    list = pik > eps & pik < 1 - eps
    pik1 = pik[list]
    N = length(pik1)
    a = (c(0, cumsum(pik1)) - runif(1, 0, 1))%%1
    s1 = as.integer(a[1:N] > a[2:(N + 1)])
    s = pik
    s[list] = s1
    s
  }
  
  UPpoisson <- function (pik) 
  {
    if (any(is.na(pik))) 
      stop("there are missing values in the pik vector")
    as.numeric(runif(length(pik)) < pik)
  }
  
  
  
  if (missing(method)) {
    warning("the method is not specified; by default, the method is srswor")
    method = "srswor"
  }
  if (!(method %in% c("srswor", "srswr", "poisson", "systematic"))) 
    stop("the name of the method is wrong")
  if (method %in% c("poisson", "systematic") & missing(pik)) 
    stop("the vector of probabilities is missing")
  if (missing(stratanames) | is.null(stratanames)) {
    if (method == "srswor") 
      result = data.frame((1:nrow(data))[srswor(size, nrow(data)) == 
                                           1], rep(size/nrow(data), size))
    if (method == "srswr") {
      s = srswr(size, nrow(data))
      st = s[s != 0]
      l = length(st)
      result = data.frame((1:nrow(data))[s != 0])
      if (size <= nrow(data)) 
        result = cbind.data.frame(result, st, prob = rep(size/nrow(data), 
                                                         l))
      else {
        prob = rep(size/nrow(data), l)/sum(rep(size/nrow(data), 
                                               l))
        result = cbind.data.frame(result, st, prob)
      }
      colnames(result) = c("id", "replicates", "prob")
    }
    if (method == "poisson") {
      pikk = inclusionprobabilities(pik, size)
      s = (UPpoisson(pikk) == 1)
      if (length(s) > 0) 
        result = data.frame((1:nrow(data))[s], pikk[s])
      if (description) 
        cat("\nPopulation total and number of selected units:", 
            nrow(data), sum(s), "\n")
    }
    if (method == "systematic") {
      pikk = inclusionprobabilities(pik, size)
      s = (UPsystematic(pikk) == 1)
      result = data.frame((1:nrow(data))[s], pikk[s])
    }
    if (method != "srswr") 
      colnames(result) = c("id", "prob")
    if (description & method != "poisson") 
      cat("\nPopulation total and number of selected units:", 
          nrow(data), sum(size), "\n")
  }
  else {
    data = data.frame(data)
    index = 1:nrow(data)
    m = match(stratanames, colnames(data))
    if (any(is.na(m))) 
      stop("the names of the strata are wrong")
    data2 = cbind.data.frame(data[, m], index)
    colnames(data2) = c(stratanames, "index")
    x1 = data.frame(unique(data[, m]))
    colnames(x1) = stratanames
    result = NULL
    for (i in 1:nrow(x1)) {
      if (is.vector(x1[i, ])) 
        data3 = data2[data2[, 1] == x1[i, ], ]
      else {
        as = data.frame(x1[i, ])
        names(as) = names(x1)
        data3 = merge(data2, as, by = intersect(names(data2), 
                                                names(as)))
      }
      y = sort(data3$index)
      if (description & method != "poisson") {
        cat("Stratum", i, "\n")
        cat("\nPopulation total and number of selected units:", 
            length(y), size[i], "\n")
      }
      if (method != "srswr" & length(y) < size[i]) {
        stop("not enough obervations in the stratum ", 
             i, "\n")
        st = c(st, NULL)
      }
      else {
        if (method == "srswor") {
          st = y[srswor(size[i], length(y)) == 1]
          r = cbind.data.frame(data2[st, ], rep(size[i]/length(y), 
                                                size[i]))
        }
        if (method == "systematic") {
          pikk = inclusionprobabilities(pik[y], size[i])
          s = (UPsystematic(pikk) == 1)
          st = y[s]
          r = cbind.data.frame(data2[st, ], pikk[s])
        }
        if (method == "srswr") {
          s = srswr(size[i], length(y))
          st = rep(y[s != 0], s[s != 0])
          l = length(st)
          if (size[i] <= length(y)) 
            r = cbind.data.frame(data2[st, ], prob = rep(size[i]/length(y), 
                                                         l))
          else {
            prob = rep(size[i]/length(y), l)/sum(rep(size[i]/length(y), 
                                                     l))
            r = cbind.data.frame(data2[st, ], prob)
          }
        }
        if (method == "poisson") {
          pikk = inclusionprobabilities(pik[y], size[i])
          s = (UPpoisson(pikk) == 1)
          if (any(s)) {
            st = y[s]
            r = cbind.data.frame(data2[st, ], pikk[s])
            if (description) {
              cat("Stratum", i, "\n")
              cat("\nPopulation total and number of selected units:", 
                  length(y), length(st), "\n")
            }
          }
          else {
            if (description) {
              cat("Stratum", i, "\n")
              cat("\nPopulation total and number of selected units:", 
                  length(y), 0, "\n")
            }
            r = NULL
          }
        }
      }
      if (!is.null(r)) {
        r = cbind(r, i)
        result = rbind.data.frame(result, r)
      }
    }
    
# original, seems a bit "over-ifed"
#     if (method == "srswr") 
#          colnames(result) = c(stratanames, "ID_unit", "Prob", "Stratum")
#     else colnames(result) = c(stratanames, "ID_unit", "Prob", "Stratum")

    colnames(result) = c(stratanames, "id", "prob", "stratum")
    
    if (description) {
      cat("Number of strata ", nrow(x1), "\n")
      if (method == "poisson") 
        cat("Total number of selected units", nrow(result), 
            "\n")
      else cat("Total number of selected units", sum(size), 
               "\n")
    }
  }
  result
}
