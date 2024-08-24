delta.sum <- function(theta, v) {
	if (!is.vector(theta)) { stop("In delta.sum: first argument has to be a vector") }
	if (!is.matrix(v)) { stop("In delta.sum: second argument has to be a matrix") }
  tmp1 <- paste0("x", 1:length(theta))
  tmp2 <- paste(tmp1, collapse="+")
  tmp3 <- paste0("~", tmp2)
  std.err <- msm::deltamethod(as.formula(tmp3), theta, v)
  return(std.err)
}

se.sum <- function(fit, m=NULL) {
  if (is.null(m)) {
    m <- 1:length(coefficients(fit))
  }
  theta <- coefficients(fit)[m]
  v <- vcov(fit)[m, m]
  return(delta.sum(theta, v))
}

delta.cumsum <- function(theta, v) {
	if (!is.vector(theta)) { stop("In delta.cumsum: first argument has to be a vector") }
	if (!is.matrix(v)) { stop("In delta.cumsum: second argument has to be a matrix") }
  result <- rep(NA, length(theta))
  for (ii in 1:length(theta)) {
    result[ii] <- delta.sum(theta[1:ii], as.matrix(v[1:ii, 1:ii]))
  }
  return(result)
}

se.cumsum <- function(fit, m=NULL) {
  if (is.null(m)) {
    m <- 1:length(coefficients(fit))
  }
  theta <- coefficients(fit)[m]
  v <- vcov(fit)[m, m]
  return(delta.cumsum(theta, v))
}

