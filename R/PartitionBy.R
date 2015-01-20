PartitionBy <-
function(x, by, FUN, ...){
  
  # SQL-OLAP: sum() over (partition by g)
  # (more than 1 grouping variables are enumerated like by=list(g1,g2,g3), 
  # as it is defined in tapply
  
  # see also ave, which only handles arguments otherwise..
  
  if (missing(by)) 
    x[] <- FUN(x, ...)
  else {
    g <- interaction(by)
    split(x, g) <- lapply(split(x, g), FUN, ...)
  }
  x
  
}
