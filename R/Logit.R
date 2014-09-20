Logit <-
function(x, min=0, max=1) {
  
  # variant in boot:::logit - CHECKME if better ********
    p <- (x-min)/(max-min)
    log(p/(1-p))
}
