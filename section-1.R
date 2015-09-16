## Section 1 Assignment: Regression and Simulation
# STATS 216
# September 15th, 2015
# written by GKFragiadakis

## Problem 1
# generate model
X1 <- rnorm(100, mean = 20, sd = 3)
B0 <- 25
B1 <- 2
e <- rnorm(100, mean = 0, sd = 15)

Y <- B0 + X1*B1 + e

# histogram of X1
hist(X1)

# plot model
plot(X1, Y)
abline(B0, B1, col = "red")

## Problem 2
# fit a linear model, add it to the plot
fit <- lm(Y~X1)
summary(fit)
coef(fit)
abline(fit, col = "blue")

# can keep reproducible results with set.seed()

## Problem 3
#estimate the standard deviation of the estimated slope coefficient (beta 1 hat)

X1 <- rnorm(100, mean = 20, sd = 3)
B0 <- 25
B1 <- 2
coefs = c()

for (i in 1:1000){
  e <- rnorm(100, mean = 0, sd = 15)
  Yi <- B0 + X1*B1 + e
  fit <- lm(Yi~X1)
  c1 <- as.numeric(fit$coefficients[2])
  coefs <- c(coefs, c1)
}

mean_fixedX <- mean(coefs)
sd_fixedX <- sd(coefs)

## Problem 4
# Now regenerate X1 each time 

B0 <- 25
B1 <- 2
coefs = c()

for (i in 1:1000){
  X1 <- rnorm(100, mean = 20, sd = 3)
  e <- rnorm(100, mean = 0, sd = 15)
  Yi <- B0 + X1*B1 + e
  fit <- lm(Yi~X1)
  c1 <- as.numeric(fit$coefficients[2])
  coefs <- c(coefs, c1)
}

mean_newX <- mean(coefs)
sd_newX <- sd(coefs) 

# very close even though we are regenerating X1!  Doesn't seem to effect the distribution
# of the estimated coefficient

## Problem 5

B0 <- 25
B1 <- 2
B2 <- 1
X1 <- rnorm(100, mean = 20, sd = 3)
X2 <- rnorm(100, mean = 20, sd = 3)
e <- rnorm(100, mean = 0, sd = 15)

Y <- B0 + X1*B1 X2*B2 + e


fit <- lm(Y ~ X1 + X2)
summary(fit)
cor(X1, X2)

# for correlation of .6
X2 <- X1 + e*.3
cor(X1, X2)
fit <- lm(Y ~ X1 + X2)
summary(fit)

# for correlation of .8
X2 <- X1 + e*.15
cor(X1, X2)
fit <- lm(Y ~ X1 + X2)
summary(fit)

# coefficient variation increases due to correlation 




