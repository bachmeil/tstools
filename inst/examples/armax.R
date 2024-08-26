library(tstools)
y <- ts(rnorm(100), frequency=4, start=c(2000,1))
fit <- armaxfit(y, ar=1, ma=1, trend=1, seasonal=TRUE)
print(predict(fit, 3))
print(predictions(fit, 3))
plot(predictions(fit, 3))

fit2 <- arxfit(y, ar=2, trend=2)
print(predictions(fit2, 12))
