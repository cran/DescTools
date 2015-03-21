SampleWord <-
function(size, length, x = LETTERS, replace = TRUE){
  sapply(1:size, function(i) paste(sample(x=x, size=length, replace=replace), collapse=""))   
}
