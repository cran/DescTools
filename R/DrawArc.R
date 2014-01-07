DrawArc <-
function (x = 0, y = x, radius.x = 1, radius.y = radius.x, angle.beg = 0, 
    angle.end = pi, nv = 100, col = par("col"), lty = par("lty"), lwd = par("lwd"), plot = TRUE) {
    
    # which geom parameter has the highest dimension
    lgp <- list(x = x, y = y, radius.x = radius.x, radius.y = radius.y, 
        angle.beg = angle.beg, angle.end = angle.end, nv = nv)
    maxdim <- max(unlist(lapply(lgp, length)))
    # recycle all params to maxdim
    lgp <- lapply(lgp, rep, length.out = maxdim)

    # recycle shape properties
    if (length(col) < maxdim) {
        col <- rep(col, length.out = maxdim)
    }
    if (length(lwd) < maxdim) {
        lwd <- rep(lwd, length.out = maxdim)
    }
    if (length(lty) < maxdim) {
        lty <- rep(lty, length.out = maxdim)
    }

    lst <- list()
    for (i in 1:maxdim) {
        angdif <- lgp$angle.end[i] - lgp$angle.beg[i]
        theta <- seq(from = 0, to = ifelse(angdif < 0, angdif + 2*pi, angdif), 
            length.out = lgp$nv[i]) + lgp$angle.beg[i] 
        ptx <- (cos(theta) * lgp$radius.x[i] + lgp$x[i])
        pty <- (sin(theta) * lgp$radius.y[i] + lgp$y[i])
        if (plot) {
            lines(ptx, pty, col = col[i], lty = lty[i], lwd = lwd[i])
        }
        lst[[i]] <- list(x = ptx, y = pty)
    }
    invisible(lst)
}
