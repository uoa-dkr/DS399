library(tidyverse)

dfs <- read_csv('GlobalTemperatureAnomaly.csv') 
df <- read_csv('Wombats.csv')

fit1 <- dfs %>% 
  lm(GlobalTempAnomaly.hC~.,data=.)

fit2 <- df %>% 
  glm(nWombats~nBurrows,data = .,family = poisson)

fit3 <- df %>% 
  glm(nWombats~nBurrows,data = .,family = quasipoisson)

fit4 <- df %>% 
  glm(cbind(males,females)~nBurrows,data=.,family = binomial)


# task 1 (least square)
leastSquares.func <- function(dat, xvar="x", yvar="y", startvec=c(0, 0)){
  dat.x <- dat[,xvar]
  dat.y <- dat[,yvar]
  objective.func <- function(pars){
    a <- pars[1]
    b <- pars[2]
    fittedValues <- a + b*dat.x
    sumSquares <- sum((dat.y - fittedValues)^2) # slide 25
    if(is.na(sumSquares) | is.infinite(sumSquares))
      sumSquares <- (abs(dat.x) + abs(dat.y)) * 1e+10 # slide 44
    return(
      sumSquares
    )
  }
  leastSquares.fit <- nlm(f=objective.func, p=startvec)
  return(leastSquares.fit)
}
leastSquares.func(dfs,names(dfs)[1], names(dfs)[2])[[2]];fit1 %>% coef(.)

# task 1(least square bootstrap)
leastSquaresBootstrap.func <- function(dat, xvar="x", yvar="y", startvec=c(0, 0), nboot=10000){
  ndat <- nrow(dat)
  boot.res <- data.frame(bootrep=1:nboot, LS.a=rep(NA, nboot), LS.b=rep(NA, nboot))
  for(i in 1:nboot){
    resampleRows <- sample(1:ndat, size = ndat, replace = TRUE) # slide 56
    dat.boot <- dat[resampleRows, ]
    LSfit.boot <- leastSquares.func(dat.boot, xvar = xvar, yvar = yvar, startvec) # slide 56
    boot.res$LS.a[i] <- LSfit.boot$estimate[1]
    boot.res$LS.b[i] <- LSfit.boot$estimate[2]
  }
  se.a.LS <- sd(boot.res$LS.a)
  se.b.LS <- sd(boot.res$LS.b) # line before
  CI.a.LS <- quantile(boot.res$LS.a, probs=c(0.025, 0.975))
  CI.b.LS <- quantile(boot.res$LS.b, probs=c(0.025, 0.975)) # line before and slide 56
  return(list(stderror=c(LS.a=se.a.LS, LS.b=se.b.LS), CI.a.LS=CI.a.LS, CI.b.LS=CI.b.LS))
}
leastSquaresBootstrap.func(dfs,names(dfs)[1], names(dfs)[2]);fit1 %>% summary(.) %>% coef(.); confint(fit1)



# task 1 (maximum likelihood)
ML.func <- function(dat, xvar="x", yvar="y", startvec=c(1,1,10)){
  dat.x <- dat[,xvar]
  dat.y <- dat[,yvar]
  objective.func <- function(pars){
    a <- pars[1]
    b <- pars[2]
    sigma <- pars[3]
    fittedValues <- a + b*dat.x
    sumlikelihood <- -sum(dnorm(dat.y[[1]],fittedValues[[1]],sigma,log = T))
    if(is.na(sumlikelihood) | is.infinite(sumlikelihood) | is.nan(sumlikelihood))
      sumlikelihood <- (abs(a) + abs(b) + abs(sigma)) * 1e+10
    return(
      sumlikelihood
    )
  }
  maximumlikelihood.fit <- nlm(f=objective.func, p=startvec)
  return(maximumlikelihood.fit)
}
ML.func(dfs, names(dfs)[1], names(dfs)[2])[[2]];fit1$coefficients;sigma(fit1)

# task 1 (maximum likelihood bootstrap)
MLBootstrap.func <- function(dat, xvar="x", yvar="y", startvec=c(1, 1, 10), nboot=10000){
  n <- nrow(dat)
  boot.res <- data.frame(bootrep=1:nboot, ML.a=rep(NA, nboot), ML.b=rep(NA, nboot), ML.sigma=rep(NA, nboot))
  for(i in 1:nboot){
    resampleRows <- sample(1:n, size=n, replace=T)
    dat.boot <- dat[resampleRows, ]
    MLfit.boot <- ML.func(dat.boot, startvec=startvec, xvar=xvar, yvar=yvar)
    boot.res$ML.a[i] <- MLfit.boot$estimate[1]
    boot.res$ML.b[i] <- MLfit.boot$estimate[2]
  }
  se.a.ML <- sd(boot.res$ML.a)
  se.b.ML <- sd(boot.res$ML.b)
  CI.a.ML <- quantile(boot.res$ML.a, probs=c(0.025, 0.975))
  CI.b.ML <- quantile(boot.res$ML.b, probs=c(0.025, 0.975))
  return(list(stderror=c(ML.a=se.a.ML, ML.b=se.b.ML), CI.a.ML=CI.a.ML, CI.b.ML=CI.b.ML))
}
MLBootstrap.func(dfs, names(dfs)[1], names(dfs)[2]);fit1 %>% summary(.) %>% coef(.); confint(fit1)


# task 2 (maximum likelihood)
MLglm.func <- function(dat, xvar="x", yvar="y", startvec=c(1e-5,1e-5)){
  dat.x <- dat[,xvar]
  dat.y <- dat[,yvar]
  objective.func <- function(pars){
    a <- pars[1]
    b <- pars[2]
    fittedValues <- exp(a + b*dat.x)
    sumlikelihood <- -sum(dpois(dat.y[[1]],fittedValues[[1]],log = T))
    if(is.na(sumlikelihood) | is.infinite(sumlikelihood))
      sumlikelihood <- (abs(a) + abs(b)) * 1e+10 # slide 44
    return(
      sumlikelihood
    )
  }
  maximumlikelihood.fit <- nlm(f=objective.func, p=startvec)
  return(maximumlikelihood.fit)
}
MLglm.func(df, 'nBurrows', 'nWombats')[[2]];fit2$coefficients

# task 2 (maximum likelihood bootstrap)
MLglmBootstrap.func <- function(dat, xvar="x", yvar="y", startvec=c(1e-5,1e-5), nboot=10000){
  n <- nrow(dat)
  boot.res <- data.frame(bootrep=1:nboot, ML.a=rep(NA, nboot), ML.b=rep(NA, nboot), ML.sigma=rep(NA, nboot))
  for(i in 1:nboot){
    resampleRows <- sample(1:n, size=n, replace=T)
    dat.boot <- dat[resampleRows, ]
    MLfit.boot <- MLglm.func(dat.boot, startvec=startvec, xvar=xvar, yvar=yvar)
    boot.res$ML.a[i] <- MLfit.boot$estimate[1]
    boot.res$ML.b[i] <- MLfit.boot$estimate[2]
  }
  se.a.ML <- sd(boot.res$ML.a)
  se.b.ML <- sd(boot.res$ML.b)
  CI.a.ML <- quantile(boot.res$ML.a, probs=c(0.025, 0.975))
  CI.b.ML <- quantile(boot.res$ML.b, probs=c(0.025, 0.975))
  return(list(stderror=c(ML.a=se.a.ML, ML.b=se.b.ML), CI.a.ML=CI.a.ML, CI.b.ML=CI.b.ML))
}
MLglmBootstrap.func(df, 'nBurrows', 'nWombats');fit2 %>% summary(.) %>% coef(.);fit3 %>% summary(.) %>% coef(.);confint(fit2)


# task 3 (maximum likelihood)
MLbin.func <- function(dat, xvar, yvar, p, startvec=c(0,0)){
  dat.x <- dat[,xvar]
  dat.y <- dat[,yvar]
  dat.p <- dat[,p]
  objective.func <- function(pars){
    a <- pars[1]
    b <- pars[2]
    fittedValues <- 1 / (1+exp(-b*dat.x-a))
    sumlikelihood <- -sum(dbinom(dat.p[[1]],dat.y[[1]],fittedValues[[1]],log = T))
    if(is.na(sumlikelihood) | is.infinite(sumlikelihood))
      sumlikelihood <- (abs(a) + abs(b)) * 1e+10 # slide 44
    return(
      sumlikelihood
    )
  }
  maximumlikelihood.fit <- nlm(f=objective.func, p=startvec)
  return(maximumlikelihood.fit)
}
MLbin.func(df, 'nBurrows', 'nWombats', 'males')[[2]];fit4 %>% coef(.)

# task 3 (maximum likelihood bootstrap)
MLbinBootstrap.func <- function(dat, xvar, yvar, p, startvec=c(0,0), nboot=10000){
  n <- nrow(dat)
  boot.res <- data.frame(bootrep=1:nboot, ML.a=rep(NA, nboot), ML.b=rep(NA, nboot), ML.sigma=rep(NA, nboot))
  for(i in 1:nboot){
    resampleRows <- sample(1:n, size=n, replace=T)
    dat.boot <- dat[resampleRows, ]
    MLfit.boot <- MLbin.func(dat.boot, startvec=startvec, xvar=xvar, yvar=yvar,p=p)
    boot.res$ML.a[i] <- MLfit.boot$estimate[1]
    boot.res$ML.b[i] <- MLfit.boot$estimate[2]
  }
  se.a.ML <- sd(boot.res$ML.a)
  se.b.ML <- sd(boot.res$ML.b)
  CI.a.ML <- quantile(boot.res$ML.a, probs=c(0.025, 0.975))
  CI.b.ML <- quantile(boot.res$ML.b, probs=c(0.025, 0.975))
  return(list(stderror=c(ML.a=se.a.ML, ML.b=se.b.ML), CI.a.ML=CI.a.ML, CI.b.ML=CI.b.ML))
}
MLbinBootstrap.func(df, 'nBurrows', 'nWombats', 'males');fit4 %>% summary(.) %>% coef(.); confint(fit4)


