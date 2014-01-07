LeveneTest.formula <-
function(y, data, ...) {
	form <- y
	mf <- if (missing(data)) model.frame(form) else model.frame(form, data)
	if (any(sapply(2:dim(mf)[2], function(j) is.numeric(mf[[j]])))) 
		stop("Levene's test is not appropriate with quantitative explanatory variables.")
	y <- mf[,1]
	if(dim(mf)[2]==2) group <- mf[,2]
	else {
		if (length(grep("\\+ | \\| | \\^ | \\:",form))>0) stop("Model must be completely crossed formula only.")
		group <- interaction(mf[,2:dim(mf)[2]])
	}
	LeveneTest.default(y = y, group=group, ...)
}
