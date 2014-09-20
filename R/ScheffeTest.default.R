ScheffeTest.default <-
function (x, g = NULL, which = NULL, contrasts = NULL, conf.level = 0.95, ...) {
  ScheffeTest(x=aov(x~g), which=which, contrasts=contrasts, conf.level=conf.level, ...)
}
