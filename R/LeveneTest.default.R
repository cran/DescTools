LeveneTest.default <-
function (y, group, center=median, ...) { # original levene.test
	if (!is.numeric(y)) 
		stop(deparse(substitute(y)), " is not a numeric variable")
	if (!is.factor(group)) {
		warning(deparse(substitute(group)), " coerced to factor.")
		group <- as.factor(group)
	}
	valid <- complete.cases(y, group)
	meds <- tapply(y[valid], group[valid], center, ...)
	resp <- abs(y - meds[group])
	table <- anova(lm(resp ~ group))[, c(1, 4, 5)]
	rownames(table)[2] <- " "
	dots <- deparse(substitute(...))
	attr(table, "heading") <- paste("Levene's Test for Homogeneity of Variance (center = ", 
			deparse(substitute(center)), if(!(dots == "NULL")) paste(":", dots),  ")", sep="")
	table
}
