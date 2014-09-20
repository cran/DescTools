LsData <-
function(package){
  # example  lsf("DescTools")
  ls(pos = gettextf("package:%s", package))
  as.vector(unclass(ls.str(gettextf("package:%s", package), mode="list")))
  
}
