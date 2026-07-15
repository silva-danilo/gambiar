# packs
library(mgcv)

# link mu
gmu <- function(mu) log(mu/(1-mu))

# link phi
gphi <- function(phi) log(phi)

# inverse link mu
inv_gmu <- function(eta){ 
  # return
  thresh <- -log(.Machine$double.eps)
  eta <- pmin(thresh, pmax(eta, -thresh))
  1/(exp(-eta)+1)
}

# inverse link phi
inv_gphi <- function(eta){ 
  # return
  thresh <- -log(.Machine$double.eps)
  eta <- pmin(thresh, pmax(eta, -thresh))
  exp(eta)
}

# dmu/deta
dmu_deta <- function(mu) mu*(1-mu)

# dphi/deta
dphi_deta <- function(phi) phi

# loglik
loglik <- function(fit){
  # prep
  mu <- fit$mu
  phi <- fit$phi
  y <- fit$y

  # return
  loglik <- lgamma(phi) - lgamma(mu*phi) - lgamma((1-mu)*phi)
  loglik <- loglik + (mu*phi-1)*log(y) + ((1-mu)*phi-1)*log(1-y)
  sum(loglik)
}

# score (wrt eta_mu)
d_mu <- function(fit){
  # prep
  mu <- fit$mu
  phi <- fit$phi
  y_star <- gmu(fit$y)
  mu_star <- digamma(mu*phi) - digamma((1-mu)*phi)

  # return
  d <- phi*(y_star - mu_star)*dmu_deta(mu)
  d
}

# score (wrt eta_phi)
d_phi <- function(fit){
  # prep
  mu <- fit$mu
  phi <- fit$phi
  y_star <- gmu(fit$y)
  mu_star <- digamma(mu*phi) - digamma((1-mu)*phi)
  c_star <- digamma(phi) - digamma((1-mu)*phi) + log(1-y)
  
  # return
  d <- (mu*(y_star - mu_star) + c_star)*dphi_deta(phi)
  d
}

# prep
set.seed(13)
n <- 600
beta_mu <- c(2,-8,0,4)/10
beta_phi <- 1/2 # phi constant for mgcv run

# data
dat <- list()
dat$X_mu <- list(x1=rep(1,n), x2=runif(n,0,1), x3=runif(n,0,1), x4=runif(n,0,1)) 
dat$X_phi <- list(z1=rep(1,n)) 
dat$f_mu <- list(f1=dat$X_mu$x1 * beta_mu[1],
                 f2=dat$X_mu$x2 * beta_mu[2],
                 f3=dat$X_mu$x3 * beta_mu[3],
                 f4=dat$X_mu$x4 * beta_mu[4])
dat$f_phi <- list(f1=dat$X_phi$z1 * beta_phi)
mu <- inv_gmu(dat$f_mu$f1 + dat$f_mu$f2 + dat$f_mu$f3 + dat$f_mu$f4) 
phi <- inv_gphi(dat$f_phi$f1) 
y <- rbeta(n, mu*phi, phi*(1-mu)) 
y[y == 1] <- 1 - .Machine$double.eps
y[y == 0] <- 0 + .Machine$double.eps
dat$y <- y

# plot
par(mar=c(4.5,5.5,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7, pch=19)
plot(mu~1)
plot(phi~1)
plot(y~1)

# model
dat_df <- data.frame(x1=dat$X_mu$x1,
                     x2=dat$X_mu$x2,
                     x3=dat$X_mu$x3,
                     x4=dat$X_mu$x4,
                     y=dat$y)
b1 <- gam(y~x1+x2+x3+x4-1, family=betar, data=dat_df)
summary(b1)

# plot (naive visual check)
par(mar=c(4.5,5.5,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7, pch=19)
plot(predict(b1, type="terms")[,2]~dat$X_mu$x2, type="l")
lines(dat$f_mu$f2~dat$X_mu$x2, col=2)
plot(predict(b1, type="terms")[,3]~dat$X_mu$x3, type="l")
lines(dat$f_mu$f3~dat$X_mu$x3, col=2)
plot(predict(b1, type="terms")[,4]~dat$X_mu$x4, type="l")
lines(dat$f_mu$f4~dat$X_mu$x4, col=2)

# initial fit
fit_initial <- function(y, X_mu, X_phi){
  # prep
  n <- length(y)
  m_mu <- length(X_mu)
  m_phi <- length(X_phi)

  # fit
  fit <- list()
  fit$n <- n
  fit$m_mu <- m_mu
  fit$m_phi <- m_phi
  fit$y <- y
  fit$X_mu <- X_mu
  fit$X_phi <- X_phi
  fit$f_mu <- list()
  for(j in 1:m_mu) fit$f_mu[[j]] <- rep(0, n)
  fit$f_phi <- list()
  for(j in 1:m_phi) fit$f_phi[[j]] <- rep(0, n)
  
  # # f1 start
  # y_m <- mean(y)
  # y_v <- var(y)
  # fit$f_mu[[1]] <- fit$f_mu[[1]] + gmu(y_m)
  # fit$f_phi[[1]] <- fit$f_phi[[1]] + gphi(max(y_m*(1-y_m)/y_v-1, 0.1))
  
  # mu and phi
  eta_mu <- colSums(do.call(rbind, fit$f_mu))
  eta_phi <- colSums(do.call(rbind, fit$f_phi))
  fit$mu <- inv_gmu(eta_mu)
  fit$phi <- inv_gphi(eta_phi)
  fit$metric <- 10
  
  # return
  names(fit$f_mu) <- paste0("f", 1:m_mu)
  names(fit$f_phi) <- paste0("f", 1:m_phi)
  fit
} 

# update fit
fit_update <- function(fit){
  # prep
  nu <- 0.1
  loglik1 <- loglik(fit)
  terms_mu <- list()
  terms_phi <- list()
  d1 <- d_mu(fit)
  d2 <- d_phi(fit)
  
  # compute each term in mu
  for(j in 1:fit$m_mu){
    # prep
    Xj <- fit$X_mu[[j]]
    Lj <- qr(crossprod(Xj))
    fitj <- fit
    
    # proj
    stepj <- nu*Xj %*% qr.solve(Lj, t(Xj) %*% d1) # casual version
    stepj <- c(stepj)
    fitj$mu <- inv_gmu(gmu(fit$mu) + stepj)
    terms_mu[[j]] <- list(loglik=loglik(fitj), step=stepj)
  } 
  
  # compute each term in phi
  for(j in 1:fit$m_phi){
    # prep
    Xj <- fit$X_phi[[j]]
    Lj <- qr(crossprod(Xj))
    fitj <- fit
    
    # proj
    stepj <- nu*Xj %*% qr.solve(Lj, t(Xj) %*% d2) # casual version
    stepj <- c(stepj)
    fitj$phi <- inv_gphi(gphi(fit$phi) + stepj)
    terms_phi[[j]] <- list(loglik=loglik(fitj), step=stepj)
  }
  
  # choose the best for mu
  j_mu <- which.max(sapply(terms_mu, "[[", "loglik"))

  # choose the best for phi
  j_phi <- which.max(sapply(terms_phi, "[[", "loglik"))

  # # update the best for all
  # if(terms_mu[[j_mu]]$loglik > terms_phi[[j_phi]]$loglik){
  #   # update mu
  #   fit$f_mu[[j_mu]] <- fit$f_mu[[j_mu]] + terms_mu[[j_mu]]$step
  # }else{
  #   # update phi
  #   fit$f_phi[[j_phi]] <- fit$f_phi[[j_phi]] + terms_phi[[j_phi]]$step
  # }
  
  # update mu if better that ficar de boas
  if(terms_mu[[j_mu]]$loglik > loglik1){
    # up
    fit$f_mu[[j_mu]] <- fit$f_mu[[j_mu]] + terms_mu[[j_mu]]$step
  }

  # update phi if better that ficar de boas
  if(terms_phi[[j_phi]]$loglik > loglik1){
    # up
    fit$f_phi[[j_phi]] <- fit$f_phi[[j_phi]] + terms_phi[[j_phi]]$step
  }
  
  # # update both
  # fit$f_mu[[j_mu]] <- fit$f_mu[[j_mu]] + terms_mu[[j_mu]]$step
  # fit$f_phi[[j_phi]] <- fit$f_phi[[j_phi]] + terms_phi[[j_phi]]$step
  
  # return
  eta_mu <- colSums(do.call(rbind, fit$f_mu))
  eta_phi <- colSums(do.call(rbind, fit$f_phi))
  fit$mu <- inv_gmu(eta_mu)
  fit$phi <- inv_gphi(eta_phi)
  loglik2 <- loglik(fit)
  #fit$metric <- mean(abs(c(d_mu(fit), d_phi(fit))))
  fit$metric <- abs(loglik2-loglik1)/(abs(loglik1) + 1e-4)
  fit
}

# boosting for babies
boost_bb <- function(y, X_mu, X_phi){
  # initial fit
  fit <- fit_initial(y, X_mu, X_phi)
  
  # loop
  tot_ite <- 0
  while(fit$metric > 1e-5 & tot_ite < 1999){
    # update
    fit <- fit_update(fit)
    
    # cat
    tot_ite <- tot_ite + 1
    msg <- paste("\r", "Iteration total:", tot_ite)
    cat(msg, "- The metric:", fit$metric, strrep(" ", 20))
  }
  
  # return
  fit
}

# model
b2 <- boost_bb(dat$y, dat$X_mu, dat$X_phi)

# plot
par(mar=c(4.5,5.5,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7, pch=19)
plot(b2$f_mu$f2~dat$X_mu$x2, type="l")
lines(dat$f_mu$f2~dat$X_mu$x2, col=2)
plot(b2$f_mu$f3~dat$X_mu$x3, type="l")
lines(dat$f_mu$f3~dat$X_mu$x3, col=2)
plot(b2$f_mu$f4~dat$X_mu$x4, type="l")
lines(dat$f_mu$f4~dat$X_mu$x4, col=2)

# add: stepj com gaussian gam (dinamical precomputed for fit_update() fast)
# add: update ploglik and pscores 
