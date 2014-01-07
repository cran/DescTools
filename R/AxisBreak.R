AxisBreak <-
function (axis = 1, breakpos = NULL, pos = NA, bgcol = "white", 
          breakcol = "black", style = "slash", brw = 0.02) {
  
  figxy <- par("usr")
  xaxl <- par("xlog")
  yaxl <- par("ylog")
  xw <- (figxy[2] - figxy[1]) * brw
  yw <- (figxy[4] - figxy[3]) * brw
  if (!is.na(pos)) 
    figxy <- rep(pos, 4)
  if (is.null(breakpos)) 
    breakpos <- ifelse(axis%%2, figxy[1] + xw * 2, figxy[3] + 
                         yw * 2)
  if (xaxl && (axis == 1 || axis == 3)) 
    breakpos <- log10(breakpos)
  if (yaxl && (axis == 2 || axis == 4)) 
    breakpos <- log10(breakpos)
  switch(axis, br <- c(breakpos - xw/2, figxy[3] - yw/2, breakpos + 
                         xw/2, figxy[3] + yw/2), br <- c(figxy[1] - xw/2, breakpos - 
                                                           yw/2, figxy[1] + xw/2, breakpos + yw/2), br <- c(breakpos - 
                                                                                                              xw/2, figxy[4] - yw/2, breakpos + xw/2, figxy[4] + yw/2), 
         br <- c(figxy[2] - xw/2, breakpos - yw/2, figxy[2] + 
                   xw/2, breakpos + yw/2), stop("Improper axis specification."))
  old.xpd <- par("xpd")
  par(xpd = TRUE)
  if (xaxl) 
    br[c(1, 3)] <- 10^br[c(1, 3)]
  if (yaxl) 
    br[c(2, 4)] <- 10^br[c(2, 4)]
  if (style == "gap") {
    if (xaxl) {
      figxy[1] <- 10^figxy[1]
      figxy[2] <- 10^figxy[2]
    }
    if (yaxl) {
      figxy[3] <- 10^figxy[3]
      figxy[4] <- 10^figxy[4]
    }
    if (axis == 1 || axis == 3) {
      rect(breakpos, figxy[3], breakpos + xw, figxy[4], 
           col = bgcol, border = bgcol)
      xbegin <- c(breakpos, breakpos + xw)
      ybegin <- c(figxy[3], figxy[3])
      xend <- c(breakpos, breakpos + xw)
      yend <- c(figxy[4], figxy[4])
      if (xaxl) {
        xbegin <- 10^xbegin
        xend <- 10^xend
      }
    }
    else {
      rect(figxy[1], breakpos, figxy[2], breakpos + yw, 
           col = bgcol, border = bgcol)
      xbegin <- c(figxy[1], figxy[1])
      ybegin <- c(breakpos, breakpos + yw)
      xend <- c(figxy[2], figxy[2])
      yend <- c(breakpos, breakpos + yw)
      if (xaxl) {
        xbegin <- 10^xbegin
        xend <- 10^xend
      }
    }
    par(xpd = TRUE)
  }
  else {
    rect(br[1], br[2], br[3], br[4], col = bgcol, border = bgcol)
    if (style == "slash") {
      if (axis == 1 || axis == 3) {
        xbegin <- c(breakpos - xw, breakpos)
        xend <- c(breakpos, breakpos + xw)
        ybegin <- c(br[2], br[2])
        yend <- c(br[4], br[4])
        if (xaxl) {
          xbegin <- 10^xbegin
          xend <- 10^xend
        }
      }
      else {
        xbegin <- c(br[1], br[1])
        xend <- c(br[3], br[3])
        ybegin <- c(breakpos - yw, breakpos)
        yend <- c(breakpos, breakpos + yw)
        if (yaxl) {
          ybegin <- 10^ybegin
          yend <- 10^yend
        }
      }
    }
    else {
      if (axis == 1 || axis == 3) {
        xbegin <- c(breakpos - xw/2, breakpos - xw/4, 
                    breakpos + xw/4)
        xend <- c(breakpos - xw/4, breakpos + xw/4, breakpos + 
                    xw/2)
        ybegin <- c(ifelse(yaxl, 10^figxy[3 + (axis == 
                                                 3)], figxy[3 + (axis == 3)]), br[4], br[2])
        yend <- c(br[4], br[2], ifelse(yaxl, 10^figxy[3 + 
                                                        (axis == 3)], figxy[3 + (axis == 3)]))
        if (xaxl) {
          xbegin <- 10^xbegin
          xend <- 10^xend
        }
      }
      else {
        xbegin <- c(ifelse(xaxl, 10^figxy[1 + (axis == 
                                                 4)], figxy[1 + (axis == 4)]), br[1], br[3])
        xend <- c(br[1], br[3], ifelse(xaxl, 10^figxy[1 + 
                                                        (axis == 4)], figxy[1 + (axis == 4)]))
        ybegin <- c(breakpos - yw/2, breakpos - yw/4, 
                    breakpos + yw/4)
        yend <- c(breakpos - yw/4, breakpos + yw/4, breakpos + 
                    yw/2)
        if (yaxl) {
          ybegin <- 10^ybegin
          yend <- 10^yend
        }
      }
    }
  }
  segments(xbegin, ybegin, xend, yend, col = breakcol, lty = 1)
  par(xpd = FALSE)
}
