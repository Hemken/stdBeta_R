library(stdBeta)

x1 <- runif(n=50, min=0, max=5)
x2 <- rbinom(n=50, size=1, prob=0.5)
mean(x2); sd(x2)

e <- rnorm(n=50)

y <- 0 + x1 + x2 + e
fit <- lm(y ~ x1 + x2)
summary(fit)
summary(stdBeta(fit))

x3 <- rbinom(n=50, size=1, prob=0.05)
mean(x3); sd(x3)
y <- 0 + x1 + x3 + e
fit2 <- lm(y ~ x1 + x3)
summary(fit2)
summary(stdBeta(fit2))
