# packs
library(mgcv)

# data
dat <- mtcars

# model
b <- gam(mpg~s(hp, bs="ps"), data=dat)

# knots
b$smooth[[1]]$knots

# knots explain
m <- 2
k <- 9
nknots <- (k + 1) + (m + 2)
min <- min(dat$hp)
max <- max(dat$hp)
delta <- max - min
min <- min - delta*0.001
max <- max + delta*0.001
h <- (max - min)/(nknots - 2*(m + 1) - 1)
knots <- seq(min-3*h, max+3*h, length.out=nknots)
knots

# check
knots-b$smooth[[1]]$knots
