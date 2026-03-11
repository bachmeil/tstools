var.simulation <- function(fmat, start, Sigma, h, K,
                                        seed = NULL,
                                        shocks = c("normal", "bootstrap"),
                                        residmat = NULL,
                                        cumulate = c("rows", "cols", "none"),
                                        intercept = c("use", "ignore")) {
  shocks <- match.arg(shocks)
  cumulate <- match.arg(cumulate)
  intercept <- match.arg(intercept)
  if (!is.null(seed)) set.seed(seed)

  nstate <- nrow(fmat)
  stopifnot(ncol(fmat) == nstate)
  stopifnot(length(start) == nstate)

  # Construct U: h x K
  if (shocks == "normal") {
    G <- t(chol(Sigma))  # KxK so that u = z %*% t(G), z~N(0,I)
    Z <- matrix(rnorm(h * K), nrow = h, ncol = K)
    U <- Z %*% t(G)
  } else {
    if (is.null(residmat)) { stop("bootstrap shocks require residmat (T x K).") }
    stopifnot(ncol(residmat) == K, "Bootstrap version requires the number of residual columns to equal K")
    idx <- sample.int(nrow(residmat), size = h, replace = TRUE)
    U <- residmat[idx, , drop = FALSE]
    G <- NULL
  }

  # Iterate
  # This is the state vector
  x <- start
  path <- switch(cumulate,
    none = NULL,
    rows = matrix(NA_real_, nrow = h + 1, ncol = K),
    cols = matrix(NA_real_, nrow = K, ncol = h + 1))

  record <- function(storage, index, value) {
		value <- as.numeric(value[1:K])
    if (cumulate == "rows") {
			storage[index,] <- value
		} else if (cumulate == "cols") {
			storage[,index] <- value
		}
		return(storage)
  }
  
  if (cumulate != "none") { 
		path <- record(path, 1, x) 
	}

	# Iterate over each forecast horizon
  for (tt in 1:h) {
		# Initializes the residual vector for the current horizon
    e <- numeric(nstate)
    
    # Add a residual for each of the K variables
    # The lagged values are deterministic updates so residual = 0
    e[1:K] <- U[tt, ]
    
    # Update the state vector
    x <- fmat %*% x + e
    
    # Handle the intercept
    if (intercept == "use") {
			x[nstate] <- 1
		} else {
			x[nstate] <- 0
		}
		
		# Save this iteration of the state vector if needed
		if (cumulate != "none") {
			path <- record(path, tt+1, x)
		}
  }

	if (cumulate == "none") {
		return(as.numeric(x[1:K]))
	} else {
		return(path)
	}
}
