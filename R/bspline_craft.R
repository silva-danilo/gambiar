# packs
library(splines)

# prep
n <- 600

# simu
set.seed(13)
x1 <- runif(n, 1, 2)
u1 <- runif(n, -5, 5)
f1 <- sin(u1) + (u1/4)^3
f1 <- f1 - mean(f1)
mu <- 8 - 2*x1 + f1
y <- rnorm(n, mu, sd=1)
dat <- data.frame(y, x1, u1, f1, mu)

# prep
par(mar=c(4.5,5.5,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7, pch=19)

# plot
plot(dat$f1~dat$u1)
plot(dat$y~dat$u1)

# wood code (with repeated knots)
bspline <- function(x, knots, i, m=2){ 
  # base
  if(m==-1){
    res <- as.numeric(x<knots[i+1] & x>=knots[i])
  } else{
    # recursion
    d0 <- (knots[i+m+1]-knots[i])
    d1 <- (knots[i+m+2]-knots[i+1])
    z0 <- (x-knots[i])/d0
    z1 <- (knots[i+m+2]-x)/d1
    ids1 <- d0==0
    ids2 <- d1==0
    z0[ids1] <- 0
    z1[ids2] <- 0
    res <- z0*bspline(x, knots, i, m-1) + z1*bspline(x, knots, i+1, m-1)
  }
  res
}

# knot
m <- 2
k <- 9
nknots <- (k + 1) + (m + 2)
min <- min(dat$u1)
max <- max(dat$u1)
delta <- max - min
min <- min - delta*0.001
max <- max + delta*0.001
h <- (max - min)/(nknots - 2*(m + 1) - 1)
knots <- seq(min-3*h, max+3*h, length.out=nknots)
knots[knots %in% head(knots, m+1)] <- min # crime
knots[knots %in% tail(knots, m+1)] <- max # crime

# craft
B_craft <- matrix(NA, nrow(dat), (k + 1))
for(i in 1:(k + 1)) B_craft[,i] <- bspline(dat$u1, knots, i)

# true
B_true <- splineDesign(dat$u1, knots=knots, ord=4, outer.ok=T)

# check
round(colSums(B_craft-B_true), 4)
