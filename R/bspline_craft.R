# packs
library(mgcv)
library(Matrix)

# data
dat <- mtcars

# model
b1 <- gam(mpg~s(hp, bs="ps"), data=dat, family=Gamma)

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
knots_craft <- seq(min-3*h, max+3*h, length.out=nknots)

# check
knots_true <- b1$smooth[[1]]$knots
round(abs(knots_craft - knots_true), 8)

# Wood bspline with repeated knots
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

# X
X_craft <- matrix(NA, nrow(dat), (k + 1))
for(i in 1:(k + 1)) X_craft[,i] <- bspline(dat$hp, knots_craft, i)

# check
X_true <- splines::splineDesign(dat$hp, knots=knots_true, ord=4, outer.ok=T)
round(colSums(abs(X_craft - X_true)), 8)

# # crime
# knots_crime <- knots_craft
# knots_crime[knots_crime %in% head(knots_crime, m+1)] <- min
# knots_crime[knots_crime %in% tail(knots_crime, m+1)] <- max
# X_crime_craft <- matrix(NA, nrow(dat), (k + 1))
# for(i in 1:(k + 1)) X_crime_craft[,i] <- bspline(dat$hp, knots_crime, i)
# 
# # check
# X_crime_true <- splines::splineDesign(dat$hp, knots=knots_crime, outer.ok=T)
# round(colSums(abs(X_crime_true - X_crime_craft)), 8)

# XZ via Householder reflection
u <- colSums(X_craft)
u[1] <- u[1] + sign(u[1])*sqrt(sum(u^2)) # norm(as.matrix(u), type="e")
XH_craft <- X_craft - (2/sum(u^2))*(X_craft %*% u) %*% t(u)
XZ_craft <- XH_craft[,-1]

# check
XZ_true <- model.matrix(b1)[,-1]
round(colSums(abs(XZ_craft - XZ_true)), 8)

# Wood scale for pspline
penalty_order <- 2
scale_craft <- 2^(2*penalty_order)
# max_XtX <- norm(X_craft, type="I")^2
# scale_craft <- norm(crossprod(D_craft)) / max_XtX

# check
scale_true <- b1$smooth[[1]]$S.scale
round(scale_true - scale_craft, 8)

# ZtSZ 
D_craft <- diff(diag(k+1), differences=penalty_order) 
DH_craft <- D_craft - (2/sum(u^2))*(D_craft %*% u) %*% t(u)
DZ_craft <- DH_craft[,-1]
ZtSZ_craft <- crossprod(DZ_craft)/scale_craft

# check
ZtSZ_true <- b1$smooth[[1]]$S[[1]]
round(colSums(abs(ZtSZ_craft - ZtSZ_true)), 8)

# # edf
# sp <- b1$sp
# w <- b1$weights
# XZtWXZ <- crossprod(sqrt(w) * XZ_craft)
# F_craft <- chol2inv(chol(XZtWXZ + sp*ZtSZ_craft)) %*% XZtWXZ
# edf_craft <- diag(F_craft)
# 
# # check
# edf_true <- b1$edf[-1]
# round(edf_craft - edf_true, 8)

# edf
sp <- b1$sp
w <- b1$weights
XZtWXZ <- crossprod(sqrt(w) * cbind(1, XZ_craft))
F_craft <- chol2inv(chol(XZtWXZ + sp*bdiag(0,ZtSZ_craft))) %*% XZtWXZ
edf_craft <- diag(F_craft)[-1]

# check
edf_true <- b1$edf[-1]
round(edf_craft - edf_true, 8)
