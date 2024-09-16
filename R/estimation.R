# Simplified version of glmnet for students working with ts data 
tslasso <- function(y, x) {
  # I don't want to add a dependency on glmnet just for this
  if (length(find.package("glmnet")) == 0) {
    stop("You need to install glmnet before you can call the tslasso function.")
  }
  fullDataset <- ts.combine(y, x)
  yvariable <- as.matrix(fullDataset[,1])
  xvariables <- as.matrix(fullDataset[,-1])
  start <- start(fullDataset)
  end <- end(fullDataset)
  cv <- glmnet::cv.glmnet(xvariables, yvariable)
  fit <- glmnet::glmnet(xvariables, yvariable)
  fitvalue <- function() { return(fit) }
  svalue <- function() { return(cv$lambda.min) }
  predfn <- function(newx) {
    return(ts(cbind(1, newx) %*% coef(fit, s=cv$lambda.min)[,1], 
      start=start(newx), frequency=frequency(fullDataset)))
  }
  result <- list(fit=fit, lambda.min=cv$lambda.min, coef=coef(fit, s=cv$lambda.min),
    predict=predfn, start=start, end=end)
  return(result)
}
