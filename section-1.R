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

