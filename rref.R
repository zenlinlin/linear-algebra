rref <- function(x) {
  if (!is.matrix(x))
    stop("The type of", " \"", substitute(x), "\" is not numeric.", sep="")
  if (!is.numeric(x))
    stop("\"", substitute(x), "\" is not a matrix.")
  m <- nrow(x)
  n <- ncol(x)
  for (j in 1:n) {
    if (j > m)
      next
    if (all(x[j:m, j] == 0))
      next
    if (x[j, j] == 0) {
      below <- x[(j+1):m, j]
      Nonzero <- which(below != 0)[1] + j
      x[j, ] <- x[Nonzero, ]
    }
    x[j, j:n] <- x[j, j:n] / x[j, j]
    for (i in 1:m) {
      if (i == j)
        next
      x[i, j:n] <- x[i, j:n] - x[j, j:n] * x[i, j] / x[j, j]
    }
  }
  x
}
