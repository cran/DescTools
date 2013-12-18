DrawBand <-
function(x, y, col = SetAlpha("grey", 0.5), border = NA) {
  # adds a band to a plot, normally used for plotting confidence bands
  polygon(x=x, y=y, col = col, border = border)
}
