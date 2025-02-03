#~ f: Function to make a forecast at one sample end date
#~ freq: The frequency of the data
#~ end.dates (optional): If provided, a vector of dates to make forecasts
#~ end.from, end.to (optional): If end.dates is not provided, but end.from and end.to are,
#~   the estimation end dates from end0 to end1 will be used. end.dates or
#~   both end.from and end.to have to be provided. Otherwise the program will stop
#~   with an error message.
#~ forecast1: If provided, the date of the first forecast. If not provided,
#~   it is assumed that you're making one-step ahead forecasts, so the first
#~   forecast date will be set to one time period after end.from.
pseudo.oos <- function(f, freq, end.dates=NULL, end.from=NULL, end.to=NULL, forecast1=NULL) {
	if (is.null(end.dates)) {
		if (!is.null(end.from) & !is.null(end.to)) {
			end.dates <- dates(end.from, end.to, freq)
		} else {
			stop("In call to pseudo.oos: You have to specify either end.dates or both end.from and end.to")
		}
	}
	fs <- vapply(end.dates, f, FUN.VALUE=double(1))
	if(is.null(forecast1)) {
		f1 <- end(ts(rep(NA, 2), start=end.dates[1], frequency=freq))
		return(ts(fs, start=f1, frequency=freq))
	} else {
		return(ts(fs, start=forecast1, frequency=freq))
	}
}
			
recursive.forecast <- function(data, f, pct=NULL, first.date=NULL, P=NULL, h=1, window=NULL) {
  freq <- frequency(data)
  nobs <- length(time(data))
  `first forecast date` <- NULL
  `last forecast date` <- last(time(data))
  if (!is.null(pct)) {
    npred <- floor(pct*nobs/100)
    `first forecast date` <- time(data)[nobs-npred+1]
  }
  else if (!is.null(first.date)) {
    `first forecast date` <- first.date
  }
  else if (!is.null(P)) {
    `first forecast date` <- time(data)[nobs-P+1]
  } 
  else { 
    stop("In call to recursive.forecast: You have not specified pct, first.date, or P.")
  }
  `first estimation date` <- start(ts(rep(NA, h+1), end=`first forecast date`, frequency=freq))
  `last estimation date` <- start(ts(rep(NA, h+1), end=`last forecast date`, frequency=freq))
  `estimation dates` <- dates(`first estimation date`, `last estimation date`, freq)
  fs <- if (is.null(window)) {
    vapply(`estimation dates`, function(z) { f(window(data, end=z)) }, FUN.VALUE=double(1))
  } else {
    `window start date` <- start(ts(rep(NA, window+1), end=z, frequency=freq))
    vapply(`estimation dates`, function(z) { f(window(data, start=`window start date`, end=z)) }, FUN.VALUE=double(1))
  }
  return(ts(fs, end=`last forecast date`, frequency=freq))
}
    
