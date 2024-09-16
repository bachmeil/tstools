# Simplified version of glmnet for students working with ts data 
tslasso <- function(y, x) {
  # I don't want to add a dependency on glmnet just for this
  if (length(find.package("glmnet")) == 0) {
    stop("You need to install glmnet before you can call the tslasso function.")
  }
  fullDataset <- ts.combine(y, x)
  yvariable <- as.matrix(fullDataset[,1])
  xvariables <- as.matrix(fullDataset[,-1])
  cv <- glmnet::cv.glmnet(xvariables, yvariable)
  fit <- glmnet::glmnet(xvariables, yvariable)
  predfn <- function(new.x) {
    return(predict(fit, newx = new.x, s=cv$lambda.min))
  }
  return(list(fit=fit, lambda.min=cv$lambda.min, coef=coef(fit, s=cv$lambda.min),
    predict=predfn))
}
