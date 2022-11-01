library(ISLR)
set.seed(2022)
trainset <- sample(1:nrow(Default), 0.5*nrow(Default))
validset <- setdiff(1:nrow(Default), trainset)
m1 <- glm(default ~ student, data = Default, subset = trainset, family = binomial)
pred1 <- predict(m1, newdata = Default[validset, ], type = 'response')
m2 <- glm(default ~ student + income, data = Default, subset = trainset, family = binomial)
pred2 <- predict(m2, newdata = Default[validset, ], type = 'response')
m3 <- glm(default ~ student + income + balance, data = Default, subset = trainset, family = binomial)
pred3 <- predict(m3, newdata = Default[validset, ], type = 'response')
m4 <- glm(default ~ balance, data = Default, subset = trainset, family = binomial)
pred4 <- predict(m4, newdata = Default[validset, ], type = 'response')


pred0 <- runif(length(validset))
source("ROC_func.R")
df <- data.frame(label = ifelse(Default[validset, 'default'] == "Yes", 1, 0),
                 m1 = pred1,
                 m2 = pred2,
                 m3 = pred3,
                 m0 = pred0, 
                 m4 = pred4)
ROC_func(df, 1, 2, add_on = F, color = "black")
ROC_func(df, 1, 5, add_on = T, color = "gray")
ROC_func(df, 1, 3, add_on = T, color = "blue")
ROC_func(df, 1, 4, add_on = T, color = "red")
ROC_func(df, 1, 6, add_on = T, color = "green")

score_threshold = 0.1
pred4a <- ifelse(pred4 > score_threshold, "Yes", "No")
table(pred4a, Default[validset, 'default'], dnn = c("Predicted","Actual"))
sum(pred4a == Default[validset, 'default'])/length(validset)

func1 <- function(newdata){
  score <- exp((-10.764012886 + 0.005553562*newdata$balance))/(1+exp((-10.764012886 + 0.005553562*newdata$balance)))
  return(score)
}
pred4b <- func1(newdata = Default[validset, ])
sum(abs(pred4b - pred4) < 0.0001)

library(MASS)
m_lda <- lda(default ~ ., data = Default, subset = trainset)
str(m_lda)
pred_lda <- predict(m_lda, newdata = Default[validset, ])
str(pred_lda)
head(pred_lda$posterior)
score_lda <- (pred_lda$posterior)[,'Yes']
ROC_func(data.frame(label = ifelse(Default[validset, 'default'] == "Yes", 1, 0),
         score <- score_lda), 1, 2, add_on = T, color='chocolate')
