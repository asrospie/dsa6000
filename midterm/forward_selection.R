library(MASS)
set.seed(42)
load("~/development/dsa6000/midterm/winequality-red.RData")
# MODIFY the line below: use the last two digits of you access ID, e.g., gn0061 gives 61
set.seed(42)  # <-- MODIFY this number
# DO NOT MODIFY the next four lines
wine$quality <- ifelse(wine$quality >= 6, 1, 0)  # 1 means good quality, 0 means bad quality
trainset <- sample(1:nrow(wine), 80)  # DO NOT CHANGE: you must sample 80 data points for training
validset <- setdiff(1:nrow(wine), trainset)  # The remaining is used for validation
source("~/development/dsa6000/midterm/ROC_func.R")  # source in the ROC_func.R (presumably located in your current directory)

for (v in varpool){
  this_formula_string <- paste0('medv ~ lstat + rm + ptratio + ', v)
  this_formula <- as.formula(this_formula_string)
  sm <- summary(lm(this_formula, data = Boston, subset = trainset))
  cat(paste0(this_formula_string, ": ", format(sm$r.squared, digits = 4), "\n"))
}

library(leaps)
mforward <- regsubsets(medv ~., data = Boston, subset = trainset, method = 'forward', nvmax = 13)
smforward <- summary(mforward)
smforward$adjr2


