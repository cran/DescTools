RgbToLong <-
function(col) (c(1, 256, 256^2) %*% col)[1,]
