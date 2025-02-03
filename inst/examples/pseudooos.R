library(tstools)
cpi <- import.fred("cpi.csv")
inf <- pctChange(cpi, 12)

fcst <- function(e) {
	data <- window(inf, end=e)
	fit <- arfit(data, ar=4)
	return(predictions(fit)[1])
}

print(pseudo.oos(fcst, 12, end.from=c(1999,12), end.to=c(2024,9)))

#~ recursive.forecast2 <- function(data, f, pct=NULL, first.date=NULL, P=NULL, h=1, window=NULL) {
#~   freq <- frequency(data)
#~   nobs <- length(time(data))
#~   `first forecast date` <- NULL
#~   `last forecast date` <- last(time(data))
#~   if (!is.null(pct)) {
#~     npred <- floor(pct*nobs/100)
#~     `first forecast date` <- time(data)[nobs-npred+1]
#~   }
#~   else if (!is.null(first.date)) {
#~     `first forecast date` <- first.date
#~   }
#~   else if (!is.null(P)) {
#~     `first forecast date` <- time(data)[nobs-P+1]
#~   } 
#~   else { 
#~     stop("In call to recursive.forecast: You have not specified pct, first.date, or P.")
#~   }
#~   `first estimation date` <- start(ts(rep(NA, h+1), end=`first forecast date`, frequency=freq))
#~   `last estimation date` <- start(ts(rep(NA, h+1), end=`last forecast date`, frequency=freq))
#~   `estimation dates` <- dates(`first estimation date`, `last estimation date`, freq)
#~   fs <- if (is.null(window)) {
#~     vapply(`estimation dates`, function(z) { f(window(data, end=z)) }, FUN.VALUE=double(1))
#~   } else {
#~     `window start date` <- start(ts(rep(NA, window+1), end=z, frequency=freq))
#~     vapply(`estimation dates`, function(z) { f(window(data, start=`window start date`, end=z)) }, FUN.VALUE=double(1))
#~   }
#~   return(ts(fs, end=`last forecast date`, frequency=freq))
#~ }
    
fcst2 <- function(ds) {
  fit <- tsreg(ds, lags(ds, 2))
  b <- fit$coef
  return(b[1] + b[2]*last(ds))
}

print(recursive.forecast(cpi, fcst2, pct=50, h=2))
