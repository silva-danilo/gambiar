# packs
library(mgcv)

# prep
n <- 600

# simu
set.seed(13)
x1 <- runif(n, 1, 2)
u1 <- runif(n, -5, 5)
f1 <- sin(u1) + (u1/4)^3
f1 <- f1 - mean(f1)
df1 <- cos(u1) + (3/4)*(u1/4)^2
# df1 <- df1 - mean(df1)
mu <- 8 - 2*x1 + f1
y <- rnorm(n, mu, sd=1)
dat <- data.frame(y, x1, u1, f1, df1, mu)

# prep
par(mar=c(4.5,5.5,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7, pch=19)

# plot
plot(dat$f1~dat$u1)
plot(dat$df1~dat$u1)

# model
b1 <- gam(y~x1 + s(u1, bs="ps"), data=dat)
summary(b1)
# ini <- b1$smooth[[1]]$first.para
# fin <- b1$smooth[[1]]$last.para
# B <- model.matrix(b1)[,ini:fin]
# f1_coef <- coef(b1)[ini:fin]
# f1_pred <- B%*%f1_coef
f1_pred <- predict(b1, type="terms")[,2]

# prep
par(mar=c(4.5,5.5,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7, pch=19)

# plot
plot(f1_pred~dat$u1)
plot(dat$f1~dat$u1)

# df
db1 <- b1
db1$smooth[[1]]$deriv <- 1
# dB <- model.matrix(db1)[,ini:fin]
# df1_pred <- dB%*%f1_coef
df1_pred <- predict(db1, type="terms")[,2]

# prep
par(mar=c(4.5,5.5,1,1), mfrow=c(1,3), cex.lab=2, cex.axis=1.7, pch=19)

# plot
ylim <- c(-0.6,1.6)
plot(df1_pred~dat$u1, ylim=ylim)
plot(dat$df1~dat$u1, ylim=ylim)
