library(tstools)
cpi <- import.fred("cpi.csv")
inf <- pctChange(cpi, 12)

fcst <- function(e) {
	data <- window(inf, end=e)
	fit <- arfit(data, ar=4)
	return(predictions(fit)[1])
}

print(pseudo.oos(fcst, 12, end.from=c(1999,12), end.to=c(2024,9)))
