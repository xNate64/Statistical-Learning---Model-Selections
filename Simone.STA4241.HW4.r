### 8(a) ###
#Generate both x and error vectors using rnorm()
set.seed(1)

x <- rnorm(100)
e <- rnorm(100)

#-----------------------------------------------------------------
### 8(b) ###
#Generate y using x and e

y = 10 + 4*x - 2*(x^2) + (x^3) + e

#Beta0 = 10, Beta1 = 4, Beta2 = -2, Beta3 = 1

#-----------------------------------------------------------------
### 8(c) ###
#The variable df will be used to capture a data frame of both x and y
df <- data.frame(x, y)
library(leaps)

#Function for plotting with Adj r2, Cp, and BIC as graphing criteria
plots <- function(fit){
  par(mfrow = c(2,2))
  ## Adj r^2 ##
  plot(fit$adjr2, xlab = "Number of Variables",
       ylab = "Adj r^2")
  print(which.max(fit$adjr2))
  
  ## Cp ##
  plot(fit$cp, xlab = "Number of Variables",
       ylab = "Cp")
  print(which.min(fit$cp))
  
  ## BIC ##
  plot(fit$bic, xlab = "Number of Variables",
       ylab = "BIC")
  print(which.min(fit$bic))
  par(mfrow = c(1,1))
}

regfit.full = regsubsets(y ~ poly(x, 10, raw = TRUE), data = df, nvmax=10)
summary(regfit.full)
regfit.summary = summary(regfit.full)
plots(regfit.summary)

#Show coefficients for models with 4 and 3 terms
coefficients(regfit.full, id = 4)
coefficients(regfit.full, id = 3)

#-----------------------------------------------------------------
### 8(d) ###
#Forward Selection
regfit.fwd =  regsubsets(y ~ poly(x, 10, raw = TRUE), data = df, nvmax=10,
                         method = "forward")
summary(regfit.fwd)
regfit.summary = summary(regfit.fwd)
#The plots for Cp, BIC, and adj r^2 is shown below.
plots(regfit.summary)
coefficients(regfit.fwd, id = 4)
coefficients(regfit.fwd, id = 3)

#________________________
#Backwards Selection
regfit.bwd =  regsubsets(y ~ poly(x, 10, raw = TRUE), data = df, nvmax=10,
                         method = "backward")
summary(regfit.bwd)
regfit.summary = summary(regfit.bwd)
#The plots are shown below.
plots(regfit.summary)
coefficients(regfit.bwd, id = 4)
coefficients(regfit.bwd, id = 3)

#-----------------------------------------------------------------
### 8(e) ###
library(glmnet)
set.seed(1)
#All possible values of lambda
grid = 10^seq(10, -2, length = 100)
X = model.matrix(df$y ~ poly(df$x, 10, raw = TRUE))

cv.out=cv.glmnet(X,df$y,alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min; bestlam

#Using the optimized lambda value in lasso regression
out = glmnet(X, df$y, alpha = 1, lambda = grid)
lasso.coef = predict(out, type = "coefficients", s = bestlam)
lasso.coef

#-----------------------------------------------------------------
### 8(f) ###

y2 = 10 + 2*(x^7) + e
df2 = data.frame(x, y2)

#Best subset
regfit.full = regsubsets(y2 ~ poly(x, 10, raw = TRUE), data = df2, nvmax=10)
regfit.sum = summary(regfit.full)
plots(regfit.sum)

coefficients(regfit.full, id = 4)
coefficients(regfit.full, id = 2)
coefficients(regfit.full, id = 1)

#Lasso
set.seed(1)
X = model.matrix(df2$y ~ poly(df2$x, 10, raw = TRUE))

cv.out=cv.glmnet(X,df2$y,alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min; bestlam

out = glmnet(X, df2$y, alpha = 1, lambda = grid)
lasso.coef = predict(out, type = "coefficients", s = bestlam)
lasso.coef

