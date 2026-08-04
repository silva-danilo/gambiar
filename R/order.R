# packs
library(mgcv) 

# data 
dat <- data.frame(y=1, x1=c(1,1,2,2,3,3), x2=c(1,2)) 
dat$x1 <- factor(dat$x1) 
dat$x2 <- factor(dat$x2) 

# model 
b1 <- gam(y~x1+x2, data=dat) 
b2 <- gam(y~x2+x1, data=dat) 

# check (order does not matter)
m1 <- model.matrix(b1) 
m1 <- lapply(data.frame(m1), unname)
m2 <- model.matrix(b2) 
m2 <- lapply(data.frame(m2), unname)
setequal(m1, m2) # same coef interp

# model 
b3 <- gam(y~x1+x2-1, data=dat) 
b4 <- gam(y~x2+x1-1, data=dat) 

# check (order matters)
m3 <- model.matrix(b3) 
m3 <- lapply(data.frame(m3), unname)
m4 <- model.matrix(b4) 
m4 <- lapply(data.frame(m4), unname)
setequal(m3, m4) # diff coef interp
