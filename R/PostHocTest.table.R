PostHocTest.table <-
function(x, method = c("none","fdr","BH","BY","bonferroni","holm","hochberg","hommel"), 
                               conf.level = 0.95, ...) {
  class(x) <- "matrix"
  PostHocTest(x, method=method, conf.level=conf.level, ...)
}
