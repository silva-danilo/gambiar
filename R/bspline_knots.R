# packs
library(splines)

# data
dat <- mtcars

# wood bspline (with repeated knots)
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

# knots
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
# knots[knots %in% head(knots, m+1)] <- min # crime
# knots[knots %in% tail(knots, m+1)] <- max # crime

# craft
B_craft <- matrix(NA, nrow(dat), (k + 1))
for(i in 1:(k + 1)) B_craft[,i] <- bspline(dat$hp, knots, i)

# true
B_true <- splineDesign(dat$hp, knots=knots, ord=4, outer.ok=T)

# check
round(colSums(B_craft-B_true), 4)
