# tstools-src.R
# meta
reload.tstools <- function() {
  detach("package:tstools", unload=TRUE)
  library(tstools)
}

# lags
lags.data <- function(x, k) {
  onevar <- function(v) {
    result <- NULL
    for (ii in k) {
      result <- ts.intersect(result, lag(v, -ii))
    }
    return(result)
  }
  
  # Treat the mts and non-mts cases differently
  # Iterate over columns if x is mts and then combine the resulting list into an mts object
  result <- if (inherits(x, "mts")) {
    temp <- lapply(colnames(x), function(name) { onevar(x[,name]) })
    do.call("ts.intersect", temp)
  } else {
    onevar(x)
  }
  return(na.omit(result))
}

lags.group <- function(x, lagvector) {
  result <- NULL
  for (k in lagvector) {
    result <- cbind(result, lag(x, -k))
  }
  return(na.omit(result))
}

lags.names <- function(x, k, varname=NULL) {
  if (length(k) == 1 & !inherits(x, "mts")) {
    return(NULL)
  }
  if (!inherits(x, "mts")) {
    return(paste0(varname, "L", k))
  }
  result <- NULL
  for (n in colnames(x)) {
    result <- c(result, paste0(n, "L", k))
  }
  return(result)
}

lags <- function(x, k, type="byvar") {
  # Thanks for the clutter, lack of static typing!
  if (!inherits(x, "ts")) {
    stop("function lags requires a ts object")
  }
  
  if (type == "byvar") {
    result <- lags.data(x, k)
    
    # Add names unless the result is a vector
    if (inherits(x, "mts")) {
      colnames(result) <- lags.names(x, k)
    }
    if (!inherits(x, "mts") & length(k) > 1) {
      colnames(result) <- lags.names(x, k, deparse(substitute(x)))
    }
    return(result)
  } else if (type == "bylag") {
    result <- lags.group(x, k)
    if (inherits(x, "mts")) {
      colnames(result) <- paste0(rep(colnames(x), length(k)), "L", rep(k, each=ncol(x)))
    }
    if (!inherits(x, "mts") & length(k) > 1) {
      colnames(result) <- paste0(deparse(substitute(x)), "L", k)
    }
    return(result)
  } else {
    stop("argument 'type' to lags has to be byvar or bylag")
  }
}

first_check <- function(x, n) {
  if (inherits(x, "matrix")) {
    stop("You called `first` on a matrix. That's ambiguous. I don't know if you want the first rows, columns, or elements. Use `head` instead.")
  }
  if (n < 1) { 
    stop("In call to `first`: Second argument cannot be less than one.") 
  }
  if (length(x) < n) {
    stop(paste0("In call to `first`: Attempting to take the first ", n, " elements of a vector that has only ", length(x), " elements."))
  }
}	

first <- function(x, n=1) {
  if (inherits(x, "ts")) {
    return(head(x, n))
  } else {
    first_check(x, n)
    return(x[1:n])
  }
}

# Last n values that are not NA
last <- function(x, n=1L) {
  if (n < 1) { stop("Second argument cannot be less than one") }
  if (inherits(x, "ts")) {
		return(tail(x, n))
	} else if (as.integer(n) == 1L) {
    return(na.omit(x)[length(x)])
  } else {
    return(na.omit(x)[(length(x)-n+1):length(x)])
  }
}

rest <- function(x) {
  return(tail(x, -1))
}

drop_first <- function(x, n=1L) {
  if (n < 1) { stop("second argument has to be greater than zero") }
  return(tail(x, -n))
}

drop_last <- function(x, n=1L) {
  if (n < 1) { stop("second argument has to be greater than zero") }	
  return(head(x, -n))
}

# head and tail
head.ts <- function(x, n=6L) {
  dates <- head(as.vector(time(x)), n)
  return(window(x, start=dates[1], end=tail(dates,1)))
}

tail.ts <- function(x, n=6L) {
  dates <- tail(as.vector(time(x)), n)
  return(window(x, start=dates[1], end=tail(dates,1)))
}

head.mts <- head.ts

tail.mts <- tail.ts

# trend
make.trend <- function(x, k=1) {
  trend <- ts(1:length(x), start=start(x), frequency=frequency(x))
  if (k == 1) { return(trend) }
  result <- trend
  for (ii in 2:k) {
    result <- ts.intersect(result, trend^ii)
  }
  colnames(result) <- paste0("trend", 1:k)
  return(result)
}

# To do a mean regression, simplifies the code a bit
meanreg <- function(y, start=NULL, end=NULL) {
  if (!inherits(y, "ts")) { stop("meanreg requires the first argument to a ts object") }
  tsreg(y, ts(1, start=start(y), end=end(y), frequency=frequency(y)), start, end, FALSE)
}

# ts regression
tsreg <- function(y.raw, x.raw, start=NULL, end=NULL, intercept=TRUE) {
  # Allow for an intercept if regressing on a scalar
  if (length(x.raw) == 1) {
    x.raw <- ts(x.raw, start=start(y.raw), end=end(y.raw), frequency=frequency(y.raw))
    intercept <- FALSE
  }
  
  # Thanks for the clutter, lack of static typing!
  if (!inherits(y.raw, "ts")) { stop("tsreg requires the first argument to a ts object") }
  if (!inherits(x.raw, "ts")) { stop("tsreg requires the second argument to a ts object") }
  
  # Step 1
  y <- na.omit(y.raw)
  x <- na.omit(x.raw)
  
  # Step 2
  s <- max(firstDate(y), firstDate(x), toDecimal(start, frequency(y)))
  e <- min(lastDate(y), lastDate(x), toDecimal(end, frequency(y)))
  
  # Step 3
  lhs <- window(y, start=s, end=e)
  rhs <- window(x, start=s, end=e)
  
  # Step 4
  fit <- if (intercept) { lm(lhs~rhs) } else { lm(lhs~rhs-1) }
  
  # Fix the names - R appends rhs to all variable names
  # That's good in general but not helpful at all here
  if (is.matrix(rhs)) {
    k <- length(names(fit$coefficients))
    if (intercept) {
      names(fit$coefficients)[2:k] <- colnames(rhs)
      names(fit$coef)[2:k] <- colnames(rhs)
    } else {
      names(fit$coefficients) <- colnames(rhs)
      names(fit$coef) <- colnames(rhs)
    }
  }
  
  # Step 5
  result <- fit
  result$coef <- coefficients(fit)
  names(result$coef) <- if (inherits(x.raw, "mts")) {
    if (intercept) {
      c("int", colnames(x.raw))
    } else {
      colnames(x.raw)
    }
  }
  result$resids <- ts(residuals(fit), end=e, frequency=frequency(y))
  result$fitted <- ts(fitted(fit), end=e, frequency=frequency(y))
  result$start <- clearDate(s)
  result$end <- clearDate(e)
  result$int <- intercept
  result$nw <- try(nw.correction(fit))
  class(result) <- c("tsreg", "lm")
  return(result)
}

predict.tsreg <- function(obj, newdata=NULL) {
  if (!inherits(obj, "tsreg")) { 
    stop("predict.tsreg requires a tsreg object, which is produced by a call to tsreg") 
  }
  # If newdata is not specified, use the last observation of the estimation data
  # This works well for the h-step ahead projection approach
  if (missing(newdata) || is.null(newdata)) {
    # This approach accounts automatically for the intercept, if present
    return(sum(tail(model.matrix(obj), 1) * obj$coef))
  # If newdata is a vector, assume it's a single observation
  } else if (is.vector(newdata) && !is.list(newdata)) {
    # Add a one for the intercept?
    hasIntercept <- names(coefficients(obj))[1] == "(Intercept)"
    newdata <- c(1.0, newdata)
    if (length(newdata) != length(obj$coef)) {
      stop("You've sent a vector to predict.tsreg. That vector has the wrong number of elements to be an observation in the dataset you used for estimation.")
    }
    return(sum(newdata * obj$coef))
  } else if (is.matrix(newdata)) {
    # Add a one for the intercept?
    hasIntercept <- names(coefficients(obj))[1] == "(Intercept)"
    newdata <- cbind(1.0, newdata)
    if (ncol(newdata) != length(obj$coef)) {
      stop("You've sent a matrix or mts to predict.tsreg. That object has the wrong number of columns to be a set of observations for the dataset you used for estimation.")
    }
    return(newdata %*% matrix(obj$coef, ncol=1))
  } else if (is.data.frame(newdata)) {
    # Treat as a matrix
    newdata <- as.matrix(newdata)
    hasIntercept <- names(coefficients(obj))[1] == "(Intercept)"
    newdata <- cbind(1.0, newdata)
    if (ncol(newdata) != length(obj$coef)) {
      stop("You've sent a data frame to predict.tsreg. That object has the wrong number of variables to be a set of observations for the dataset you used for estimation.")
    }
    return(newdata %*% matrix(obj$coef, ncol=1))
  } else if (is.list(newdata)) {
    # If it's a list, check that all elements have the same length and then convert to a
    # matrix using an intermediate data frame.
    if (length(unique(sapply(z, length))) != 1) {
      stop("You sent a list as the newdata argument to predict.tsreg. Not all elements of that list have the same number of elements, so there is no way to compute predictions.")
    }
    newdata <- as.matrix(as.data.frame(newdata))
    hasIntercept <- names(coefficients(obj))[1] == "(Intercept)"
    newdata <- cbind(1.0, newdata)
    if (ncol(newdata) != length(obj$coef)) {
      stop("You've sent a data frame to predict.tsreg. That object has the wrong number of variables to be a set of observations for the dataset you used for estimation.")
    }
    return(newdata %*% matrix(obj$coef, ncol=1))
  } else {
    stop("The newdata argument to predict.tsreg has to be a vector, matrix, mts, data frame, or list.")
  }
}    

# add names
asString <- function(x, nope="") {
  if (!is.character(x)) {
    return(nope)
  }
  if (length(x) != 1) {
    return(nope)
  }
  return(x)
}

asInteger <- function(x, nope=-999L) {
  if (!is.numeric(x)) {
    return(nope)
  }
  if (length(x) != 1) {
    return(nope)
  }
  return(as.integer(x))
}

addNames.aux <- function(spec, result=NULL) {
  if (length(spec) < 2) {
    return(result)
  } else {
    nextBlock <- if (asString(spec[[2]]) == "reuse") {
      colnames(spec[[1]])
    } else if (asInteger(spec[[2]]) == 0) {
      spec[1]
    } else {
      lag.part <- paste0("L", as.integer(spec[[2]]))
      lag.part[lag.part=="L0"] <- ""
      paste0(spec[[1]], lag.part)
    }
  }
  Recall(spec[-(1:2)], c(result, nextBlock))
}

addNames <- function(...) {
  return(addNames.aux(list(...)))
}

# withts
tsToList <- function(x) {
  result <- if (!is.matrix(x)) {
    temp <- list(x)
    names(temp) <- deparse(substitute(x))
    temp
  } else {
    temp <- lapply(1:ncol(x), function(ii) { x[,ii] })
    names(temp) <- colnames(x)
    temp
  }
  return(result)
}

with.ts <- function(x, expr) {
  with(tsToList(x), expr)
}

# Everything else
# Returns all blocks of length k of a ts or mts object
all.blocks <- function(x, k) {
  dates <- as.numeric(time(x))
  start_dates <- head(dates, -(k-1))
  end_dates <- tail(dates, -(k-1))
  Map(function(s, e) { window(x, s, e) }, start_dates, end_dates)
}

dates <- function(start, end, freq) {
  return(as.numeric(time(ts(start=start, end=end, frequency=freq))))
}

tsobs <- function(x, d, type="ts") {
  if (type=="numeric") {
    return(as.numeric(window(x, start=d, end=d)))
  } else if (type=="ts") {
    return(window(x, start=d, end=d))
  } else {
    stop("Argument type for tsobs has to be 'numeric' or 'ts'.")
  }
}

vec <- function(x) { UseMethod("vec", x) }
vec.list <- function(x) { as.numeric(unlist(x)) }


forecast <- function(fit, ...) { UseMethod("forecast", fit) }
forecast.dynlm <- function(fit, ...) {
  rhs <- c(1, c(...))
  if (length(rhs) != length(coefficients(fit))) { stop("Need the number of predictors to equal the number of estimated coefficients") }
  return(sum(coefficients(fit) * rhs))
}

month <- function(x) {
  return(as.integer(12*time(x)) %% 12 + 1)
}

one.month <- function(x, k) {
  if (frequency(x) != 12) {
    stop("one.month can only be applied to monthly time series")
  }
  d <- as.double(x)[month(x) == k]
  y1 <- as.integer(time(x)[1])
  return(ts(d, start=y1, frequency=1)) 
}

quarter <- function(x) {
  return(((as.integer(12*time(x)) %% 12) %/% 3) + 1)
}

one.quarter <- function(x, k) {
  if (frequency(x) != 4) {
    stop("one.quarter can only be applied to quarterly time series")
  }
  d <- as.double(x)[quarter(x) == k]
  y1 <- as.integer(time(x)[1])
  return(ts(d, start=y1, frequency=1)) 
}

year <- function(x) {
  return(as.integer(time(x)))
}

yq <- function(x) {
  year(x)*100 + quarter(x)
}

make.dates <- function(start, end, frequency, lagged=0) {
  temp <- lag(ts(NA, start=start, end=end, frequency=frequency), lagged)
  return(as.numeric(time(temp)))
}

month.dummy <- function(x, omit=NULL) {
  # Confirm that it's a ts object
  if (!inherits(x, "ts")) {
    stop(paste0("month.dummy requires a ts object. You have sent an object of type ", class(x)))
  }
  
  # Confirm that it's a monthly ts object
  if (frequency(x) != 12) { 
    stop("Cannot call month.dummy unless the frequency is monthly") 
  }
  
  # Valid names for omit
  m <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  
  # Confirm that omit is either a valid month name or a valid number
  if (!(is.null(omit) || omit %in% m || omit %in% 1:12)) {
    stop(paste0("Argument omit in the call to month.dummy has to be one of the following:\n- ", paste0('"', m, '"', collapse=" "), "\n- A number in the range 1:12\n"))
  }
  
  # If omit is a number, change to the month name
  if (!is.null(omit)) {
    if (omit %in% 1:12) {
      omit <- m[omit]
    }
  }
  
  ## Form the dummies
  result1 <- ts(model.matrix(~m[month(x)]), start=start(x), frequency=frequency(x))[,-1]
  colnames(result1) <- substring(colnames(result1), 12)
  
  ## The output of the above call is strange. It loses the ordering of the columns and one of the names
  ## Need some hacks to fix that
  ind <- which(!(m %in% colnames(result1)))
  result2 <- cbind(month(x) == ind, result1)
  colnames(result2) <- c(m[ind], colnames(result1))
  result <- result2[,m]
  
  # Returns all 12 dummies if omit is not specified, or is NULL
  if (is.null(omit)) {
    return(result)
  } else {
    return(result[,-which(colnames(result) == omit)])
  }
}

## For use inside the forecast function
pred_month <- function(dum, month) {
  if (month %in% colnames(dum)) {
    ind <- which(colnames(dum) == month)
    d <- rep(0, 11)
    d[ind] = 1
    return(d)
  } else {
    return(rep(0,11))
  }
}

quarter.dummy <- function(x, omit=1) {
  if (!(omit %in% 1:4)) { stop("Argument 'omit' must be 1, 2, 3 or 4") }
  qtr <- 4*(time(x) - as.integer(time(x))) + 1
  quarters <- (1:4)[-omit]
  result <- NULL
  for (q in quarters) {
    result <- cbind(result, as.integer(qtr == q))
  }
  colnames(result) <- paste("Q", quarters, sep="")
  return(ts(result, start=start(x), frequency=4))
}

dummy.after <- function(x, n, omit=1) {
	tmp <- ts(1:n, start=end(lag(x,-1)), frequency=frequency(x))
	if (frequency(x) == 4) {
		return(quarter.dummy(tmp, omit))
	} else if (frequency(x) == 12) {
		return(month.dummy(tmp, omit))
	} else {
		stop("dummy.after only works with monthly or quarterly series")
	}
}

seasonal.dummy <- function(x, omit=1) {
	if (!inherits(x, "ts")) { stop("In seasonal.dummy, x has to be a ts or mts object") }
	if (frequency(x) == 4) {
		return(quarter.dummy(x, omit))
	} else if (frequency(x) == 12) {
		return(month.dummy(x, omit))
	} else {
		stop("seasonal.dummy only works with monthy or quarterly time series.")
	}
}

trend.after <- function(x, h) {
  if (!inherits(x, "ts")) { stop("In function trend.after, the first argument has to be a ts object") }
  k <- if (is.matrix(x)) { ncol(x) } else { 1 }
  n <- if (is.matrix(x)) { nrow(x)+h } else { length(x)+h }
	tmp <- ts(1:n, start=start(x), frequency=frequency(x))
	return(last(make.trend(tmp, k), h))
}

## Select the best model by a criteria when dropping one of the variables
best.drop <- function(y, x, crit) {
  compare <- function(previous, new) {
    if (previous$crit < new$crit) {
      return(previous)
    } else {
      return(new)
    }
  }
  
  crit <- Map(function(z) {
    regdata <- x[,-z]
    return(list(criteria=crit(forecast::tslm(y ~ regdata)), vars=colnames(x)[-z]))
  }, 1:ncol(x))
  
  out <- Reduce(compare, crit, init=list(crit=Inf))
  cat("For the model with predictors:", out$vars, "\n")
  cat("The criteria is:", out$criteria, "\n")
  cat("----\n\n")
  return(x[,out$vars])
}

stepwise.selection <- function(y, x, crit=BIC) {
  cat("\n\nOutput of downward stepwise selection procedure\n")
  fit <- tsreg(y, x)
  cat("\nCriteria for full model: ", crit(fit), "\n")
  cat("----\n\n")
  next.mts <- function(data, unused) {
    best.drop(y, data, crit)
  }
  result <- Reduce(next.mts, 2:ncol(x), init=x)
  return("Finished Successfully")
}

## Return dynlm output for White corrected standard errors
white.correction <- function(fit) {
  se.corrected <- sqrt(diag(sandwich::vcovHC(fit)))
  coef <- coefficients(fit)
  tstats <- coef/se.corrected
  result <- cbind(coef, se.corrected, tstats)
  rownames(result) <- names(coef)
  colnames(result) <- c("coef", "se", "t-stat")
  return(result)
}

## Return lm output for Newey-West corrected standard errors
nw.correction <- function(fit) {
  se.corrected <- sqrt(diag(sandwich::NeweyWest(fit)))
  coef <- coefficients(fit)
  tstats <- coef/se.corrected
  result <- cbind(coef, se.corrected, tstats)
  rownames(result) <- names(coef)
  colnames(result) <- c("coef", "se", "t-stat")
  return(result)
}

nw.cov <- function(fit) {
  return(sandwich::NeweyWest(fit))
}

## na.omit inserts $na.action attribute, messing up calls to dynlm
trim.na <- function(x) {
  x2 <- na.omit(x)
  result <- ts(as.numeric(x2), start=start(x2), frequency=frequency(x2))
  return(result)
}

BIC.Arima <- function(x) {
  return(AIC(x, k=log(length(residuals(x)))))
}

# Shorthand for ts.combine
`%~%` <- function(x, y) UseMethod("%~%")
`%~%.ts` <- function(x, y) {
  if (is.null(x)) {
    return(y)
  }
  result <- ts.combine(x, y)
  cn <- if (inherits(x, "mts")) {
    colnames(x)
  } else {
    deparse(substitute(x))
  }
  if (inherits(y, "mts")) {
    cn <- c(cn, colnames(y))
  } else {
    cn <- c(cn, deparse(substitute(y)))
  }
  colnames(result) <- cn
  return(result)
}
`%~%.NULL` <- function(x, y) {
  if (inherits(y, "ts")) {
    return(y)
  } else {
    return(NULL)
  }
}

## Allow appending to the end of a ts object
`%+%` <- function(x, y)  UseMethod("%+%")
`%+%.ts` <- function(x, y) {
  ts(c(as.numeric(x), y), start=start(x), frequency=frequency(x))
}
`%+%.default` <- function(x, y) {
  c(x, y)
}

## Drop variables from an mts object
`%drop%` <- function(obj, names) UseMethod("%drop%")
`%drop%.mts` <- function(obj, names) {
  obj[, subset(colnames(obj), !(colnames(obj) %in% names))]
}

## Pull observations from an mts or ts object
## obj: ts or mts object
## include: logical to indicate whether to include an observation or not
## Warning: omits observations not in BOTH obj and include
`%select%` <- function(obj, include) UseMethod("%select%")
`%select%.mts` <- function(obj, include) {
  if (frequency(obj) != frequency(include)) {
    stop("Arguments to %select% need to be the same frequency")
  }
  s <- max(start(obj), start(include))
  e <- min(end(obj), end(include))
  return(subset(window(obj, start=s, end=e), window(include, start=s, end=e)))
}
`%select%.ts` <- function(obj, include) {
  if (frequency(obj) != frequency(include)) {
    stop("Arguments to %select% need to be the same frequency")
  }
  s <- max(start(obj), start(include))
  e <- min(end(obj), end(include))
  return(as.numeric(window(obj, start=s, end=e))[which(window(include, start=s, end=e))])
}

## In case you want to compare two dates that are vectors
max.date <- function(d1, d2) {
  if (length(d1) != length(d2)) {
    stop("Both arguments to max.date need to be the same length")
  }
  if (length(d1) == 2) {
    if (d1[1] > d2[1]) {
      return(d1)
    } else if (d1[1] < d2[1]) {
      return(d2)
    } else {
      if (d1[2] > d2[2]) {
        return(d1)
      } else {
        return(d2)
      }
    }
  } else {
    return(max(d1, d2))
  }
}

min.date <- function(d1, d2) {
  if (length(d1) != length(d2)) {
    stop("Both arguments to max.date need to be the same length")
  }
  if (length(d1) == 2) {
    if (d1[1] > d2[1]) {
      return(d2)
    } else if (d1[1] < d2[1]) {
      return(d1)
    } else {
      if (d1[2] > d2[2]) {
        return(d2)
      } else {
        return(d1)
      }
    }
  } else {
    return(min(d1, d2))
  }
}


listToVector <- function(xs) {
  for (x in xs) { 
    if (!(is.vector(x))) {
      stop("Conversion of list to vector requires each element to be a scalar")
    }
    if (!(length(x) == 1)) {
      stop("Conversion of list to vector requires each element to be a scalar")
    }
  }
  return(unlist(xs))
}

getArimaForecast <- function(fit, n=1) {
  pred <- predict(fit, n)
  return(pred$pred[n])
}

getVarForecast <- function(fit, var=1, n=1) {
  pred <- predict(fit, n.ahead=n)
  return(pred$fcst[[var]][n])
}

getVarForecasts <- function(fit, var=1, n=1, start=NULL, end=NULL) {
  pred <- predict(fit, n.ahead=max(n))
  if (is.null(start) & is.null(end)) {
    return(pred$fcst[[var]][n,1])
  } else if (!is.null(start)) {
    return(ts(pred$fcst[[var]][n,1], start=start, frequency=frequency(fit$y)))
  } else {
    return(ts(pred$fcst[[var]][n,1], end=end, frequency=frequency(fit$y)))
  }
}

var.fcst <- getVarForecasts
var.fcsts <- getVarForecasts

getVecForecasts <- function(fit, var=1, n=1, start=NULL, end=NULL) {
  pred <- predict(vec2var(fit), n.ahead=max(n))
  if (is.null(start) & is.null(end)) {
    return(pred$fcst[[var]][n,1])
  } else if (!is.null(start)) {
    return(ts(pred$fcst[[var]][n,1], start=start, frequency=frequency(fit$y)))
  } else {
    return(ts(pred$fcst[[var]][n,1], end=end, frequency=frequency(fit$y)))
  }
}

getLowerInterval <- function(fit, var=1, n=1, start=NULL, end=NULL, ci=0.95) {
  pred <- predict(fit, n.ahead=max(n), ci=ci)
  if (is.null(start) & is.null(end)) {
    return(pred$fcst[[var]][n,2])
  } else if (!is.null(start)) {
    return(ts(pred$fcst[[var]][n,2], start=start, frequency=frequency(fit$y)))
  } else {
    return(ts(pred$fcst[[var]][n,2], end=end, frequency=frequency(fit$y)))
  }
}

getUpperInterval <- function(fit, var=1, n=1, start=NULL, end=NULL, ci=0.95) {
  pred <- predict(fit, n.ahead=max(n), ci=ci)
  if (is.null(start) & is.null(end)) {
    return(pred$fcst[[var]][n,3])
  } else if (!is.null(start)) {
    return(ts(pred$fcst[[var]][n,3], start=start, frequency=frequency(fit$y)))
  } else {
    return(ts(pred$fcst[[var]][n,3], end=end, frequency=frequency(fit$y)))
  }
}

varForecasts <- function(fit, var=1, n=1, start=NULL, end=NULL, ci=0.95) {
  pred <- predict(fit, n.ahead=max(n), ci=ci)
  if (is.null(start) & is.null(end)) {
    return(pred$fcst[[var]][n,1:3])
  } else if (!is.null(start)) {
    return(ts(pred$fcst[[var]][n,1:3], start=start, frequency=frequency(fit$y)))
  } else {
    return(ts(pred$fcst[[var]][n,1:3], end=end, frequency=frequency(fit$y)))
  }
}

## Might be convenient
`%after%` <- function(x, y) UseMethod("%after%")
`%after%.numeric` <- function(x, y) {
  ts(x, start=end(lag(y,-1)), frequency=frequency(y))
}

if (FALSE) {
  library(dynlm)
  z1 <- ts(rnorm(100), start=c(1990,1), frequency=12)
  z2 <- ts(rnorm(100), start=c(1990,1), frequency=12)
  z <- ts.intersect(z1, z2)
  arx.fcst("z1 ~ L(z1,1) + L(z2,1)", z, c(1997,1), c(1997,3))
  arx.fcst("z1 ~ L(z1,2) + L(z2,2)", z, c(1997,1), c(1997,3), h=2)
}

## form: a dynlm formula, as a string
## data: the forecasting dataset
## start: date of the first forecasted value (not the data at which estimation is done)
## end: date of the last forecasted value
## h: time horizon of the arx forecast
arx.fcst <- function(form, data, start, end, h=1) {
  tmp <- ts(start=start, end=end, frequency=frequency(data))
  dates <- as.numeric(time(lag(tmp, h)))
  fits <- lapply(dates, function(z) {
    fit <- tslm(as.formula(form), data=data, end=z)
    alldata <- fit$model
    alldata[,1] <- 1
    rhs <- ts(alldata, end=z, frequency=frequency(data))
    sum(coefficients(fit)*window(rhs, start=z, end=z))
  })
  return(ts(unlist(fits), end=end, frequency=frequency(data)))
}

firstDate <- function(x) {
  if (!is.ts(x) & !is.null(x)) { stop("This function will only work if the argument is a ts object") }
  if (is.null(x)) {
    return(NULL)
  } else {
    return(time(x)[1])
  }
}

lastDate <- function(x) {
  if (!is.ts(x) & !is.null(x)) { stop("This function will only work if the argument is a ts object") }
  if (is.null(x)) {
    return(NULL)
  } else {
    ii <- if (is.matrix(x)) { nrow(x) } else { length(x) }
    return(time(x)[ii])
  }
}

toDecimal <- function(d, f) {
  if (is.null(d)) { return(NULL) }
  if (!is.numeric(d)) { stop("The argument has to be numeric") }
  if (length(d) < 1 | length(d) > 2) { stop("The argument needs to be a scalar number or a numeric vector with two elements") }
  return(time(ts(start=d, end=d, frequency=f))[1])
}

previousDate <- function(date, f, k=1) {
  temp <- integerDate(date, f)
  return(integerDateUndo(temp-k, f))
}

integerDate <- function(date, f) {
  if (length(date) == 1) {
    return(date*f+1)
  } else {
    return(date[1]*f + date[2])
  }
}

integerDateUndo <- function(date, f) {
  return((date-1)/f)
}

# Clear printing of monthly dates
# d: A decimal date or integer[2]
# output: Set to "vector" for [1972 7] style (programmatic use)
# Returns: A readable date of the form "July 1972" or "[1972 7]"
clearDate <- function(d, output="") {
  m <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  if (output != "vector") {
    if (length(d) == 2) {
      return(paste(m[d[2]], d[1]))
    } else {
      ind <- round(integerDate(d, 12) %% as.integer(d))
      return(paste(m[ind], as.integer(d)))
    }
  } else {
    if (length(d) == 2) {
      return(d)
    } else {
      ind <- round(integerDate(d, 12) %% as.integer(d))
      return(c(as.integer(d), ind))
    }
  }
}

# d: A vector of dates in numeric form
# Designed for output from the dates function
clearDates <- function(d) {
  result <- rep(NA, length(d))
  for (ii in 1:length(d)) {
    result[ii] <- if(is.list(d)) {
      clearDate(d[[ii]])
    } else {
      clearDate(d[ii])
    }
  }
  return(result)
}


dfToVector <- function(x, bycol=TRUE) {
  if (bycol) {
    return(as.vector(as.matrix(x)))
  } else {
    return(as.vector(t(x)))
  }
}

# This is a big mess due to getting names from ...
# When the arguments might not even be named
# This is just a limitation of a dynamic language
# If all else fails, pass the variable names in varnames
ts.combine <- function(..., varnames=NULL) {
  
  # This returns the unevaluated arguments that were passed in
  argnames <- as.character(substitute(c(...)))[-1]
  
  # In case any argument names were specified for ts arguments
  vars <- list(...)
  
  # If no arguments were given names, then use the names of the arguments
  # Otherwise collect those names
  rawnames <- if (is.null(names(vars))) {
    argnames
  } else {
    names(substitute(list(...)))[-1]
  }
  
  # If there were named arguments but some names were not given,
  # use the name of the argument
  rawnames[which(rawnames=="")] = argnames[which(rawnames=="")]
  
  x <- ts.intersect(...)
  
  # Now go through and assign names to the result
  # If an mts object, use those names
  # Otherwise use the assigned name for a ts
  # If no assigned name for a ts, use the name of that argument
  cn <- character(0)
  counter <- 1
  for (item in vars) {
		if (is.null(varnames)) {
			if (inherits(item, "mts")) {
				cn <- c(cn, colnames(item))
			} else {
				cn <- c(cn, rawnames[counter])
			}
			counter <- counter + 1
		} else {
			cn <- varnames
		}
  }
  colnames(x) <- cn
  return(na.omit(x))
}

# Add a way to make a repeating dummy as well, like c("M", "W", "F", "Sat")
# This is now the preferred way to make a seasonal dummy
# Just send a time series of the appropriate frequency
# You'll always have that available
time.dummy <- function(x, dates1, dates2=NULL) {
  if (identical(dates1, "start")) {
    dates1 <- start(x)
  }
  dates <- if (is.null(dates2)) {
    d <- if (is.list(dates1)) { dates1 } else { list(dates1) }
    date.conversion(d, frequency(x))
  } else if (identical(dates2, "end")) {
    date.conversion(time(ts(start=dates1, end=end(x), frequency=frequency(x))), frequency(x))
  } else {
    date.conversion(time(ts(start=dates1, end=dates2, frequency=frequency(x))), frequency(x))
  }
  return(ts(date.conversion(time(x), frequency(x)) %in% dates, start=start(x), frequency=frequency(x))) 
}

date.conversion <- function(dates, f) {
  result <- lapply(dates, function(d) {
    if (length(d) == 2) {
      return(d[1]*f + d[2])
    } else {
      return(as.integer(d*f + 1))
    }
  })
  return(as.integer(unlist(result)))
}

ts.check <- function(x, dates, values) {
  Map(function(d, v) { all.equal(as.numeric(tsobs(x, d)), v) }, dates, values)
}

tsextend <- function(x, y) {
  return(ts(c(x,y), start=start(x), frequency=frequency(x)))
}

pctChange <- function(x, k=1) {
  if (!inherits(x, "ts")) {
    stop("Argument to pctChange has to be a ts object")
  }
  return(diff(x, k)/lag(x,-k))
}

when.max <- function(x) {
  if (!inherits(x, "ts")) {
    stop("Argument to when.max has to be a ts object")
  }
  return(time(x)[which.max(x)])
}

when.min <- function(x) {
  if (!inherits(x, "ts")) {
    stop("Argument to when.max has to be a ts object")
  }
  return(time(x)[which.min(x)])
}

obs.max <- function(x, index) {
  if (!inherits(x, "ts")) {
    stop("Argument to when.max has to be a ts object")
  }
  return(window(x, start=when.max(x[, index]), end=when.max(x[, index])))
}

obs.min <- function(x, index) {
  if (!inherits(x, "ts")) {
    stop("Argument to when.max has to be a ts object")
  }
  return(window(x, start=when.min(x[, index]), end=when.min(x[, index])))
}

lags.direct <- function(y, x, h=1, maxlag=1, criteria=AIC) {
  output <- lapply(1:maxlag, function(k) {
    fit <- tsreg(y, lags(x, (0:(k-1))+h))
    list(lags=k, crit=criteria(fit))
  })
  Reduce(function(v1, v2) { if (v2$crit < v1$crit) { v2 } else { v1 } }, output, init=list(lags=0, crit=Inf))
}

fit.contemporaneous <- function(y, x, cont, maxlag, criteria=AIC) {
  cont.names <- if (inherits(cont, "mts")) {
    colnames(cont)
  } else {
    deparse(substitute(cont))
  }	
  
  output <- lapply(1:maxlag, function(k) {
    rhs.lags <- lags(x, 1:k, type="bylag")
    rhs <- ts.combine(cont, rhs.lags)
    colnames(rhs) <- c(cont.names, colnames(rhs.lags))
    fit <- tsreg(y, rhs)
    list(lags=k, crit=criteria(fit), fit=fit)
  })
  Reduce(function(v1, v2) { if (v2$crit < v1$crit) { v2 } else { v1 } }, output, init=list(lags=0, crit=Inf))
}

var.lrtest <- function(x, lag1, lag2) {
  rf_r <- VAR(x, p=lag1)
  rf_u <- VAR(x, p=lag2)
  lr <- 2*(as.numeric(logLik(rf_u)) - as.numeric(logLik(rf_r)))*
    (nrow(x)-(lag1*ncol(x)+1))/nrow(x)
  return(1-pchisq(lr, (lag2-lag1)*ncol(x)*ncol(x)))
}

toQuarterly <- function(x, fun=mean) {
  if (frequency(x) != 12) {
    stop("toQuarterly currently works only with monthly data")
  }
  result <- as.double(by(x, as.factor(yq(x)), fun))
  return(ts(result, start=c(year(x)[1], quarter(x)[1]), frequency=4))
}

readts <- function(fn, col=2, header=TRUE, ...) {
  tmp <- read.csv(fn, header=header)
  return(ts(tmp[,col], ...))
} 

thrlags <- function(x, k, f) {
  result <- lags(x,k)
  if (is.logical(f)) {
    tmp <- f*result
    if (inherits(result, "mts")) {
      class(tmp) <- "mts"
    }
    return(ts.combine(result, tmp))
  } else if(inherits(result, "mts")) {
    if (length(f) == ncol(result)) {
      tmp <- matrix(rep(f, each=nrow(result)), nrow=nrow(result))
      return(ts.combine(result, result * (result > tmp)))
    } else {
      return(ts.combine(result, result * (result > f)))
    }
  }
}

levelfcst <- function(fit, x) {
  p <- predict(fit, n.ahead=length(x))$pred
  lf <- as.numeric(p) + as.numeric(x)
  return(ts(lf, frequency=frequency(x), start=start(p)))
}

# Cross correlation, wraps the ccf function in base R
# It's hard to work with the output of ccf, or even know what it means
crosscor <- function(y, x, k=6, plot=FALSE) {
  yvar <- deparse(substitute(y))
  xvar <- deparse(substitute(x))
  if (plot) {
    plot(ts(ccf(y, x, lag.max=k, plot=FALSE)$acf[-(1:k)], start=0), xlab="Horizon (k)", 
         ylab=paste0("cor(", yvar, "[t+k], ", xvar, "[t])"),
         main=paste0("Cross correlation of ", yvar, " and ", xvar))
  } else {
    cat(paste0("Cross correlation of ", yvar, " and ", xvar, "\n\n")) 
    result <- cbind(0:k, ccf(y, x, lag.max=k, plot=FALSE)$acf[-(1:k)])
    colnames(result) <- c("k", paste0("cor(", yvar, "[t+k], ", xvar, "[t])"))
    return(result)
  }
}

# Pull out relevant information from vars package irf function when you
# have only one irf
irf.ts <- function(x) {
  return(ts(as.numeric(x$irf[[1]]), start=0))
}

# Replacement for tsp that displays the same info in a readable format
tsinfo <- function(x) {
  if (!inherits(x, "ts")) { stop("Argument to tsinfo needs to be a time series object") }
  tmp <- tsp(x)
  result <- list()
  if (frequency(x) > 1) {
    result$start <- start(x)
    result$end <- end(x)
  } else {
    result$start <- tsp(x)[1]
    result$end <- tsp(x)[2]
  }
  result$frequency <- frequency(x)
  class(result) <- c("tsinfo")
  return(result)
}	

print.tsinfo <- function(obj) {
  cat("Start date: ", obj$start, "\n")
  cat("End date:   ", obj$end, "\n")
  cat("Frequency:  ", obj$frequency, "\n")
}

# Convert the output of ARMA model estimation to something sensible
# by reporting an actual intercept
armaCoef <- function(fit, x) {
  if (!inherits(fit, "Arima")) {
    stop("Argument to armaCoef has to be output of a call to arima")
  }
  inter <- tail(fit$coef, 1)
  coef.ar <- fit$model$phi
  coef.ma <- fit$model$theta
  p <- fit$arma[1]
  q <- fit$arma[2]
  # Sometimes this is needed for some reason
  if (length(coef.ar) > p) { coef.ar <- coef.ar[1:p] }
  if (length(coef.ma) > q) { coef.ma <- coef.ma[1:q] }
  arpart <- if (p < 1) {
    0
  } else {
    ylag <- rev(tail(x, p))
    sum(coef.ar*ylag)
  }
  options(warn=2)
  mapart <- if (q < 1) {
    0
  } else {
    e <- rev(tail(residuals(fit),q))
    output <- sum(coef.ma*e)
  }
  options(warn=0)
  pred1 <- arpart + mapart
  constant <- as.numeric(predict(fit,1)$pred - pred1)
  result <- c(constant, coef(fit)[1:(p+q)])
  names(result)[1] <- "intercept"
  return(result)
}

# Hopefully a more convenient interface
# If auto is TRUE, ar and ma are the maximum lag lengths to try
armafit <- function(x, ar=0, ma=0, auto=FALSE) {
  if (ar < 0) { stop("Cannot set AR lag length to be negative") }
  if (ma < 0) { stop("Cannot set MA lag length to be negative") }
  if (ar+ma < 1) { stop("Need at least one AR or MA lag in an ARMA model") }
  options(warn=2)
  result <- if (auto) {
    try(forecast::auto.arima(x, max.p=ar, max.q=ma, max.P=0, max.Q=0))
  } else {
    try(arima(x, order=c(ar, 0, ma)), silent=TRUE)
  }
  if (inherits(result, "try-error")) {
    return(result)
  }
  result$raw <- result
  if (auto) {
    ar <- result$arma[1]
    ma <- result$arma[2]
  }
  options(warn=0)
  result$fitted <- x - result$residuals
  par <- armaCoef(result, x)
  se <- sqrt(diag(result$var.coef))[-length(par)]
  result$par <- list(intercept=as.numeric(par[1]), coef=rbind(par[-1], se, par[-1]/se))
  arnames <- if (ar < 1) {
    NULL
  } else {
    paste0("AR", 1:ar)
  }
  manames <- if (ma < 1) {
    NULL
  } else {
    paste0("MA", 1:ma)
  }
  if (ar+ma > 0) {
    colnames(result$par$coef) <- c(arnames, manames)
    rownames(result$par$coef) <- c("Coef", "se", "t-stat")
  } else {
    result$par <- result$raw
  }
  result$raw <- NULL
  attr(result$coef, "Warning Message") <- "What is reported as the intercept is actually the mean!!! You probably want $par, not $coef."
  class(result) <- c("arimafit", "Arima")
  return(result)
}

arfit <- function(y, ar=0, auto=FALSE) {
	return(armafit(y, ar, 0, auto))
}

mafit <- function(y, ma=0, auto=FALSE) {
	return(armafit(y, 0, ma, auto))
}

arxfit <- function(y, xreg=NULL, ar=0, trend=0, seasonal=FALSE, auto=FALSE) {
  return(armaxfit(y, xreg, ar, 0, trend, seasonal, auto))
}

armaxfit <- function(y, xreg=NULL, ar=0, ma=0, trend=0, seasonal=FALSE, auto=FALSE) {
	if (auto) {
		stop("auto is not yet implemented for armaxfit")
	}
	xreg.arg <- !is.null(xreg)
	if (trend > 0) {
    xreg <- xreg %~% make.trend(y, trend)
	}
	if (seasonal) {
    xreg <- xreg %~% seasonal.dummy(y)
	}
	if (is.null(xreg)) {
		result <- list(fit=arima(y, order=c(ar, 0, ma)))
	} else {
    result <- list(fit=arima(y, order=c(ar, 0, ma), xreg=xreg))
    # Needed due to weird scope games played by predict.Arima
    # Otherwise it'll try to pull xreg from the parent environment
    # which of course does not contain xreg once we leave this function
    # If you check the source of stats:::predict.Arima
    # xr <- object$call$xreg
    # xreg <- if (!is.null(xr)) 
    #    eval.parent(xr)
    # So if I do this, it evaluates to itself
    result$fit$call$xreg <- xreg
	}
	result$y <- y
	result$ar <- ar
	result$ma <- ma
	result$xreg <- xreg.arg
	result$trend <- trend
	result$seasonal <- seasonal
	class(result) <- "armaxfit"
	return(result)
}

predict.armaxfit <- function(obj, n.ahead, newxreg=NULL, ...) {
	nxreg <- NULL
	if (obj$xreg) {
		nxreg <- newxreg
	}
	if (obj$trend > 0) {
    nxreg <- nxreg %~% trend.after(obj$y, n.ahead)
	}
	if (obj$seasonal) {
    nxreg <- nxreg %~% dummy.after(obj$y, n.ahead)
	}
	if (is.null(nxreg)) {
		return(predict(obj$fit, n.ahead))
	} else {
		return(predict(obj$fit, n.ahead, newxreg=nxreg))
	}
}

prediction <- function(obj, h) {
  return(last(predictions(obj, h)))
}

predictions <- function(...) {
  return(predict(...)$pred)
}

print.arimafit <- function(f) {
  print(f$par)
}

arma.select <- function(x, ar=integer(0), ma=integer(0), crit=AIC, combine=TRUE) {
  lagmat <- if ((length(ar) > 0) && (length(ma) > 0)) {
    if (combine) {
      part1 <- cbind(ar,0)
      colnames(part1) <- c("AR", "MA")
      part2 <- cbind(0,ma)
      colnames(part2) <- c("AR", "MA")
      part3 <- expand.grid(ar, ma)
      colnames(part3) <- c("AR", "MA")
      rbind(part1, part2, part3)
    } else {
      rbind(cbind(ar,0), cbind(0,ma))
    }
  } else if (length(ar) > 0) {
    cbind(ar, 0)
  } else if (length(ma) > 0) {
    cbind(0, ma)
  } else {
    stop("You have not provided either ar or ma as parameters, so I don't know what to do.")
  }
  failed <- matrix(nrow=0, ncol=2)
  critmat <- apply(lagmat, MARGIN=1, function(tmp) {
    fit <- armafit(x, tmp[1], tmp[2])
    if (class(fit)[1] == "try-error") {
      failed <<- rbind(failed, c(tmp[1], tmp[2]))
      return(Inf)
    } else {
      return(crit(fit))
    }
  })
  result <- list()
  result$crit <- cbind(lagmat, critmat)
  colnames(result$crit) <- c("AR", "MA", "Criteria")
  ind <- which.min(result$crit[,3])
  result$best <- c(ar=lagmat[ind,1], ma=lagmat[ind,2])
  result$ar <- ar
  result$ma <- ma
  result$combine <- combine
  result$failed <- failed
  colnames(result$failed) <- c("AR", "MA")
  class(result) <- "armaselect"
  return(result)
}

print.armaselect <- function(x) {
  cat("ARMA model selection\n\nYou tried\n")
  if (length(x$ar) > 0) { cat("AR lags: ", x$ar, "\n") }
  if (length(x$ma) > 0) { cat("MA lags: ", x$ma, "\n") }
  if (length(x$ar) > 0 && length(x$ma) > 0 && x$combine) { cat("And all of their combinations...\n")}
  cat("\n")
  cat("The best ARMA model includes", x$best[1], "AR lags and", x$best[2], "MA lags.\n\n")
  cat("To see full lag length criteria statistics, print out $crit. You probably don't need that information so it is not reported.\n\n")
  if (length(x$failed) > 0) {
    cat("Note that estimation of models with MA terms might fail. There is nothing you can do about it. In that case, the criteria value is recorded as Infinity. These are the models that failed:\n\n")
    print(x$failed)
    cat("\nDo not use those models for forecasting!")
  }
}

# Matrix with row repeated n times
repeat.row <- function(row, n) {
	result <- matrix(nrow=n, ncol=length(row))
	for (col in 1:length(row)) {
		result[,col] <- row[col]
	}
	return(result)
}

# Matrix with col repeated n times
repeat.col <- function(col, n) {
	result <- matrix(nrow=length(col), ncol=n)
	for (row in 1:length(col)) {
		result[row,] <- col[row]
	}
	return(result)
}

# Remove the mean of columns of a matrix
demean <- function(m) {
	return(residuals(lm(m ~ 1)))
}

firstElement <- function(x) {
	if (is.list(x)) {
		return(x[[1]])
	} else if (is.matrix(x)) {
		return(x[1,])
	} else if (is.vector(x)) {
		return(x[1])
	} else {
		stop("firstElement can only be called with a list, matrix, or vector at this time")
	}
}

collect <- function(x, name, transform, output) { UseMethod("collect") }

# output can be "rows", "columns", "list", "3d", or type of vector
# "3d" is for an array of matrices along the third dimension
# name is for a list element, column name, or element name, depending on the type
# A numerical index also works for name
collect.list <- function(x, name=NULL, transform=NULL, output="list") {
  elementByName <- function(obj, name) {
    if (is.list(obj)) {
      return(obj[[name]])
    } else if (is.matrix(obj)) {
      return(obj[, name])
    } else if (is.vector(obj)) {
      return(obj[name])
    } else {
      stop("In collect: You can only call name if the elements of the list are vector, matrix, or list types.")
    }
  }
  
  result <- if (is.null(name) & is.null(transform)) {
    x
  } else if (is.null(name)) {
    lapply(x, function(z) { transform(z) })
  } else if (is.null(transform)) {
    lapply(x, function(z) { elementByName(z, name) })
  } else {
    lapply(x, function(z) { transform(elementByName(z, name)) })
  }
	if (output == "rows") {
		finalResult <- matrix(nrow=length(result), ncol=length(result[[1]]))
    for (ii in 1:length(result)) {
      finalResult[ii, ] <- result[[ii]]
    }
		return(finalResult)
	} else if (output == "cols") {
		finalResult <- matrix(ncol=length(result), nrow=length(result[[1]]))
    for (ii in 1:length(result)) {
      finalResult[, ii] <- result[[ii]]
    }
		return(finalResult)
  } else if (output == "3d") {
    tmp <- result[[1]]
    finalResult <- array(dim=c(nrow(tmp), ncol(tmp), length(result)))
    for (ii in 1:length(result)) {
      finalResult[, , ii] <- result[[ii]]
    }
    return(finalResult)
	} else if (output == "numeric") {
    return(vapply(result, function(z) { z }, numeric(1)))
	} else if (output == "character") {
    return(vapply(result, function(z) { z }, character(1)))
	} else if (output == "logical") {
    return(vapply(result, function(z) { z }, logical(1)))
	} else if (output == "list") {
    return(result)
  }
}

## Basic linear Wald test
## Implementation follows Cameron and Trivedi (2005)
## q is their little r since I don't want both R and r
## V is their N^{-1} C since V is more of a standard notation
## May be a numerically better way to do the final computation but this is good enough
wald.test <- function(R, q, theta, V) {
  if (is.vector(theta)) { theta <- matrix(theta) }
  if (is.vector(q)) { q <- matrix(q) }
  outside <- R %*% theta - q
  inside <- R %*% V %*% t(R)
  teststat <- t(outside) %*% solve(inside) %*% outside
  critval95 <- qchisq(0.95, nrow(R))
  critval90 <- qchisq(0.90, nrow(R))
  pval <- as.double(1 - pchisq(teststat, nrow(R)))
  return(list(test=teststat, critval95=critval95, critval90=critval90, p=pval))
}

wald <- function(fit, R, q, nw=FALSE, white=FALSE) {
	vhat <- if (nw) {
		sandwich::NeweyWest(fit)
	} else if (white) {
		sandwich::vcovHC(fit)
	} else {
		vcov(fit)
	}
	b <- if (inherits(fit, "lm")) {
    coefficients(fit)
	} else {
		coefficients(fit)[,1]
	}
	return(wald.test(R, q, b, vhat))
}	

## Specific type of Wald test for coefficients jointly equal to zero
## fit can be either an lm object or summary.lm
## ind is a vector of the indexes of the coefficients jointly equal to zero
testzero <- function(fit, ind, nw=FALSE, white=FALSE) {
	vhat <- if (nw) {
		sandwich::NeweyWest(fit)
	} else if (white) {
		sandwich::vcovHC(fit)
	} else {
		vcov(fit)
	}
  if (inherits(fit, "lm")) {
    b <- coefficients(fit)
    R <- matrix(0, nrow=length(ind), ncol=length(b))
    for (ii in 1:length(ind)) {
      R[ii, ind[ii]] <- 1
    }
    return(wald.test(R, matrix(0, ncol=1, nrow=length(ind)), b, vhat))
  }
  else if (inherits(fit, "summary.lm")) {
    b <- coefficients(fit)[,1]
    R <- matrix(0, nrow=length(ind), ncol=length(b))
    for (ii in 1:length(ind)) {
      R[ii, ind[ii]] <- 1
    }
    return(wald.test(R, matrix(0, ncol=1, nrow=length(ind)), b, vhat))
  }
  else {
    stop("testzero requires the first argument to be of type lm or summary.lm")
  }
}

maErrors <- function(y, x, f, ma.order, par) {
  result <- rep(NA, length(y)+ma.order)
  result[1:ma.order] <- 0
  for (ii in 1:length(y)) {
    x.ii <- if (is.matrix(x)) { x[ii,] } else { x[ii] }
    result[ii+ma.order] <- f(par, y[ii], x.ii, result[1:(ii+ma.order-1)])
  }
  return(result[-(1:ma.order)])
}

reverseIndex <- function(x, ii) {
  return(x[length(x)+1-ii])
}

# Generate a ts, including a random walk
# n: Number of observations to generate
# f: Function to generate the data for one observation
# init: Initial values of the generated data
# trend: Whether there's a linear time trend in the model
tsgen <- function(n, f, init, trend=FALSE) {
	result <- c(init, rep(NA, n))
	for (ii in (length(init)+1):length(result)) {
		if (trend) {
			result[ii] <- f(result[1:(ii-1)], ii-length(init))
		} else {
			result[ii] <- f(result[1:(ii-1)])
		}
	}
	return(ts(result[-(1:length(init))]))
}

## If k is a double, it will be converted to an integer
matPower <- function(m, k) {
	if (!(length(k) == 1)) { stop("In matPower: Second argument needs to be a scalar") } 
	if (!(all.equal(k, as.integer(k)))) { stop("In matPower: Second argument needs to be an integer") }
	if (k < 1) { stop("In matPower: Second argument needs to be greater than zero") }
	if (as.integer(k) == 1) {
		return(m)
	} else {
		result <- m
		for (ii in 2:as.integer(k)) {
			result <- m %*% result
		}
		return(result)
	}
}

## Add k months to the date
`%m+%` <- function(date, k) {
    return(end(ts(rep(NA, k+1), start=date, frequency=12)))
}

## Subtract k months from the date
`%m-%` <- function(date, k) {
    return(start(ts(rep(NA, k+1), end=date, frequency=12)))
}

## Add k quarters to the date
`%q+%` <- function(date, k) {
    return(end(ts(rep(NA, k+1), start=date, frequency=4)))
}

## Subtract k quarters from the date
`%q-%` <- function(date, k) {
    return(start(ts(rep(NA, k+1), end=date, frequency=4)))
}

## Make out-of-sample forecasts for the specified estimation
## sample end dates
## lhs is the variable being forecast (must be a ts object)
## endDates is a vector of estimation sample end dates
## firstForecast is the date of the first forecast
## return value depends on f
## If f is a list of functions
## actuals plus forecasts and forecast errors for each model
## If a function
## actuals plus forecasts and forecast errors for f
## Assumed that you'll name your functions if f is a list
oosForecasts <- function(lhs, f, endDates, firstForecast) {
	if (is.function(f)) {
		fcsts <- ts(vapply(endDates, f, double(1)), start=firstForecast, frequency=frequency(lhs))
		actual <- window(lhs, start=start(fcsts), end=end(fcsts))
		err <- actual - fcsts
		return(list(fcst=fcsts, actual=actual, err=err))
	}
	else if (is.list(f)) {
		return(lapply(f, oosForecasts, lhs=lhs, endDates=endDates, firstForecast=firstForecast))
	}
	else { 
		stop("In call to oosForecasts: Second argument has to be a list or function. It is neither.")
	}
}

## Convert a varest object into the F matrix used in the first-order form of the VAR model
## varfit: varest object
## constant: Set to TRUE if you want a constant to appear in the F matrix
##  FALSE if you want to drop the constant term. Default value is TRUE.
formF <- function(varfit, constant=TRUE) {
    nvar <- varfit$K
    nlag <- varfit$p
    B <- Bcoef(varfit)
    if ("const" %in% varfit$type & constant) {
        toprows <- B
        middlerows <- cbind(diag((nlag-1)*nvar), matrix(0, nrow=(nlag-1)*nvar, ncol=nvar+1))
        bottomrow <- matrix(c(rep(0.0, nlag*nvar), 1.0), nrow=1)
        return(rbind(toprows, middlerows, bottomrow))
    } else {
        toprows <- B[,-ncol(B)]
        if (nlag < 2) {
            return(toprows)
        }
        leftside <- diag((nlag-1)*nvar)
        rightside <- matrix(0, ncol=nvar, nrow=(nlag-1)*nvar)
        bottomrows <- cbind(leftside, rightside)
        return(rbind(toprows, bottomrows))
    }
}

## fmat: Output of formF or similar user-defined matrix
## start: Vector to use to start the iteration (last elements equal to 1
##   if there's an intercept
## h: The horizon
## cumulate: "none" to not cumulate them, "rows" to do so in rows, and
##   "cols" to do so in columns
iterate <- function(fmat, start, h, cumulate="none") {
  if (cumulate == "none") {
    return(matPower(F, h) %*% start)
  } else if (cumulate == "rows") {
    result <- matrix(start, nrow=1)
    current <- start
    for (ii in 1:h) {
      current <- F %*% current
      result <- rbind(result, matrix(current, nrow=1))
    }
    return(result)
  } else if (cumulate == "cols") {
    result <- matrix(start, ncol=1)
    current <- start
    for (ii in 1:h) {
      current <- F %*% current
      result <- cbind(result, matrix(current, ncol=1))
    }
    return(result)
  } else {
    stop("iterate requires cumulate to be 'none', 'rows', or 'cols'. Try again.")
  }
} 

# Returns a copy of x that matches the dates in y
# x will start at the later of the start dates of x and y
# x will end at the earlier of the end dates of x and y
trim.dates <- function(x,y) {
  return(window(x,
                start=max(tsp(x)[1], tsp(y)[1]),
                end=min(tsp(x)[2], tsp(y)[2])))
}

zip <- function(...) {
  # Problem is that a string is a vector in R
  # Check that it's numeric instead
  vector_check <- function(z, len) {
    result <- FALSE
    if (is.numeric(z)) {
      if (length(z) == len) {
        result <- TRUE
      }
    }
    return(result)
  }
  
  matrix_check <- function(z, len) {
    result <- FALSE
    if (is.matrix(z)) {
      if (nrow(z) == len) {
        result <- TRUE
      }
    }
    return(result)
  }
  
  ts_check <- function(z, len) {
    result <- FALSE
    if (is.ts(z)) {
      if (length(z) == len) {
        result <- TRUE
      }
    }
    return(result)
  }
  
  tmp <- list(...)
  
  # Check that ... is not empty
  if (length(tmp) == 0) {
    stop("zip requires at least one argument")
  }

  # Have to check that each element is a vector, ts, or matrix
  # with the right dimensions
  if (!is.vector(tmp[[1]])) {
    if (!is.matrix(tmp[[1]])) {
      if (!is.ts(tmp[[1]])) {
        stop("zip only works with vectors, matrices, and ts objects")
      }
    }
  }
  rows <- length(tmp[[1]])
  
  for (ii in 2:length(tmp)) {
    if (!vector_check(tmp[[ii]], rows)) {
      if (!matrix_check(tmp[[ii]], rows)) {
        if (!ts_check(tmp[[ii]], rows)) {
          stop(paste0("zip only works with vectors, matrices, and ts objects, and all arguments need to have the same number of rows. Element ", ii, " fails."))
        }
      }
    }
  }
  
  lapply(1:rows, function(ii) {
    result <- NULL
    for (el in tmp) {
      if (is.matrix(el)) {
        result <- c(result, el[ii,])
      } else {
        result <- c(result, el[ii])
      }
    }
    return(result)
  })
}

draw.pair <- function(...) {
  tmp <- list(...)
  if (length(tmp) < 2) {
    stop("draw.pair only makes sense if you have at least two arguments.")
  }
  result <- tmp[[1]]
  for (el in tmp[-1]) {
    result <- na.omit(ts.intersect(result, el))
  }
  if (is.null(result)) {
    stop("The arguments to draw.pair do not have any observations in common.")
  }
  return(as.numeric(tsobs(result, sample(time(result), size=1))))
}

# Estimate an h-step ahead model
# lhs
# rhs (include lhs for AR) ts or mts object
# k: number of lags
# h: number of time periods between the information on the left and right
# Standard VAR estimation has h=1
# TODO: Allow automated model selection
# TODO: Allow k for each rhs variable
hstep <- function(lhs, rhs, k=1, h=1, ect=NULL) {
  y <- if (is.null(ect)) {
    lags(rhs, h:(k+h-1))
  } else {
    ts.combine(lags(rhs, h:(k+h-1)), lags(ect, h))
  }
  result <- list(fit=tsreg(lhs, y), rhs=rhs, y=y, k=k, h=h)
  class(result) <- "hstep"
  return(result)
}

# obj: hstep class output
predict.hstep <- function(obj) {
	newrhs <- if (inherits(obj$y, "mts")) {
    c(1, obj$y[nrow(obj$y),])
	} else {
		c(1, last(obj$y))
	}
  fcst <- sum(obj$fit$coefficients * newrhs)
  tmp <- ts(c(rep(0.0, obj$h), fcst), start=end(obj$fit$resids), frequency=frequency(obj$y))
	return(last(tmp))
}

# x: The time series
# every: Keep every nth date (six to keep every sixth, etc.)
# first: In case you want to start other than the first
# clear: Set to true for clear date labels rather than floating point
graph.dates <- function(x, every, first=1, clear=TRUE) {
  ind <- seq(first, length(x), by=every)
  labels <- if (clear) {
    if (frequency(x) == 12) {
      clearDates(time(x))
    } else if (frequency(x) == 4) {
      paste0(year(x), "Q", quarter(x))
    } else if (frequency(x) == 1) {
      time(x)
    }
  } else {
    time(x)
  }
  return(list(at=as.numeric(time(x))[ind], 
              labels=labels[ind]))
}
