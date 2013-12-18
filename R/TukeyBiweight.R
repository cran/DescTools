TukeyBiweight <-
function(x, const=9) {
    y <- as.double(x[!is.na(x)])
    .C("tbrm", y, as.integer(length(y)), as.double(const),
       result=NaN, NAOK=TRUE, DUP=FALSE)$result
}
