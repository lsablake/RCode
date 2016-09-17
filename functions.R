add2 <- function(x,y) {
        x + y
}

above10 <- function(x) {
        use <- x > 10  ##this will return a logical vector of TRUEs and FALSEs to indicate which elememnt of x is greater than 10
        x[use]
}



above_n <- function(x, n) {
  use <- x > n  ##this will return a logical vector of TRUEs and FALSEs to indicate which elememnt of x is greater than n
  x[use]
}


above_n <- function(x, n=10) { ##specify default value for n
  use <- x > n  ##this will return a logical vector of TRUEs and FALSEs to indicate which elememnt of x is greater than n
  x[use]
}

columnmean <- function(y, removeNA = TRUE) {
        nc <- ncol(y)
        means <- numeric(nc)
        for (i in 1:nc) {
          means[i] <- mean(y[,i], na.rm = removeNA)
        }
        means
}


