rref <- function(x) {
  if (is.matrix(x)) {
    if (is.numeric(x)) {
      m <- nrow(x)
      n <- ncol(x)
      for (j in 1:n) {
        if (j > m) 
          next else
            i <- j
            if (all(x[i:m, j] == 0)) 
              next else
                if (x[i, j] == 0) {
                  below <- x[(i+1):m, j]
                  Nonzero <- which(below != 0)[1] + i
                  x[i, ] <- x[Nonzero, ]
                  }
                for (i in 1:m) {
                  if (i == j) next
                  x[i, j:n] <- x[i, j:n] - x[j, j:n] * x[i, j] / x[j, j]
                }
      }
    } else cat("\nThe type of", " \"", substitute(x), "\" is not numeric.\n", sep="")
  } else cat("\n\"", substitute(x), "\" is not a matrix.\n")
  return(x)
}
