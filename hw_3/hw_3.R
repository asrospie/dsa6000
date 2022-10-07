library(MASS)

boston_df <- Boston

head(boston_df)
print(colnames(boston_df))

# Total Sum of Squares of medv
tss <- sum(boston_df$medv^2)
sprintf('TSS: %f', tss)

# Create Linear Model
lm.fit <- lm(medv ~ lstat + age, data = boston_df)
summary(lm.fit)

# Residual Sum of Squares
rss <- deviance(lm.fit)
sprintf('RSS: %f', rss)

names(lm.fit)
summary(lm.fit)

anova(lm.fit)
boston_df$age[1:5]

sapply(boston_df, mode)
lm.fit.full <- lm(medv ~ ., data = boston_df)
summary(lm.fit.full)
num_predictors <- length(lm.fit.full$coefficients) - 1
print(num_predictors)

library(car)
vif(lm.fit.full)

lm.fit.all.but.age <- lm(medv ~ . - age, data = boston_df)
summary(lm.fit.all.but.age)
mse <- function(m) {
  return(mean(m$residuals^2))
}

mse(lm.fit.full)
mse(lm.fit.all.but.age)
AIC(lm.fit.full)
BIC(lm.fit.full)
AIC(lm.fit.all.but.age)
BIC(lm.fit.all.but.age)

library(leaps)
lm.bwd <- regsubsets(medv ~ ., data = boston_df, method = 'backward', nvmax = 19)
summary(lm.bwd)
names(summary(lm.bwd))
plot(summary(lm.bwd)$bic, xlab = "# of Variables", ylab = "BIC", type = "l")
plot(summary(lm.bwd)$cp, xlab = "# of Variables", ylab = "AIC", type = "l")
plot(summary(lm.bwd)$adjr2, xlab = "# of Variables", ylab = "Adj. R2", type = "l")
title('Adjusted R^2 for Best Models of Different Variables')
names(data.frame(summary(lm.bwd)['which'])[11,])

best.lm <- lm(medv ~ . - age - indus, data = boston_df)
summary(best.lm)
