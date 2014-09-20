LsObj <-
function(package){
  # example  lsf("DescTools")
  ls(pos = gettextf("package:%s", package))
}
