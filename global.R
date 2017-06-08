Lorentzian <- function (x, x0, FWHM) {
    y <- rep(0, length(x))
    for (i in 1:length(x0)) {
        y <- y + 2/(pi * FWHM[i])/(1 + ((x - x0[i])/(FWHM[i]/2))^2)
    }
    y
}

sumLor <- function(x,params) 
         with (as.list(params), return(
         y0+A1*Lorentzian(x,c1,fwhm1)+A2*Lorentzian(x,c2,fwhm2) ))
         


Pruby <- function (w, laser = 568.189, dw = 2, P = -1, w0 = -1, l = FALSE) {
    A <- 1876
    dA <- 6.7
    B <- 10.71
    dB <- 0.14
    w_laser <- 1/(laser) * 1e+07
    if (w0 > 0) {
        lambda0 <- 1/(w_laser - w0) * 1e+07
    }
    else {
        lambda0 <- 694.24
    }
    if (P < 0) {
        lambda <- 1/(w_laser - w) * 1e+07
        P <- A/B * ((lambda/lambda0)^B - 1)
        dP <- P * (dA/A + dB/B + 2 * B * dw/w)
        P
    }
    else {
        lambda <- lambda0 * (P * B/A + 1)^(1/B)
        w <- w_laser - 1e+07/lambda
        if (l == FALSE) {
            w
        }
        else {
            lambda
        }
    }
}



curvearea <- function (x, y, col = "gray", alpha = 0.2, border = FALSE, ...) {
    colalpha <- add.alpha(col, alpha = alpha)
    xx <- c(x, rev(x))
    yy <- c(y, rev(y - y))
    polygon(xx, yy, col = colalpha, border = border, ...)
}

add.alpha <- function (col, alpha = 1) {
    if (missing(col)) 
        stop("Please provide a vector of colours.")
    apply(sapply(col, col2rgb)/255, 2, function(x) rgb(x[1], 
        x[2], x[3], alpha = alpha))
}