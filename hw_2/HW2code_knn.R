mk <- read.csv(file="~/development/dsa6000/hw_2/Advertising.csv", header=T)
mk[c(1,3,4)] <- NULL  # Remove the unwanted columns, keep only TV and sales column
head(mk)  # mk should now be a data frame with two columns

# Write an R function to predict the sales given a TV advertising budget value,
# by implementing the nearest neighbor averaging method.
# Specifically, the function should return a numeric value, i.e., the predicted sales given the newTV value
# The predicted sales should be the average of the k sales quantities in the training data whose corresponding
# TV budget values are closest to the newTV value
# Complete the function body
newsales <- function(newTV, train = mk, k = 3){
  # Your code here:
  train$dist <- abs(newTV - train$TV)
  return(mean(train[order(train$dist), ]$sales[1:k]))
}

# Test code
newsales(200)  # The result should be 13.43
newsales(200, k=5)  # The result should be 15.14

# Report your prediction results for the following cases (keep two digits after the decimal point)
newsales(newTV = 180, k = 1)
newsales(newTV = 180, k = 3)
newsales(newTV = 180, k = 5)
newsales(newTV = 180, k = 7)

