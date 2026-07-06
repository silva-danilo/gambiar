
# packs
library(mgcv)
library(gamlss)

# prep
dat <- data.frame(y=1:3)

# lm 
b1 <- lm(y~1, data=dat)
coef(b1)
residuals(b1)

# lm object mod
b1$model$y <- dat$y + 2
coef(b1)
residuals(b1)

# gamlss 
b2 <- gamlss(y~1, data=dat)
coef(b2)
residuals(b2)

# gamlss object mod
b2$y <- dat$y + 2
coef(b2)
residuals(b2)

# mgcv
b3 <- gam(y~1, data=dat)
coef(b3)
residuals(b3)

# mgcv object mod (qq.gam trick: object$y <- yr)
b3$y <- dat$y + 2
coef(b3)
residuals(b3)
