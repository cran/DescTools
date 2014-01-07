VecRot <-
function(x, n = 1)  {
  # just one shift:    (1:x %% x) + 1
  n <- n %% length(x)
#  rep(x, times=2)[(n+1):(n+length(x))]
  rep(x, times=2)[(length(x) - n+1):(2*length(x)-n)]
}
