LeveneTest.lm <-
function(y, ...) {
	LeveneTest.formula(formula(y), data=model.frame(y), ...)
}
