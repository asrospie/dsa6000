load("~/development/dsa6000/midterm/winequality-red.RData")
# MODIFY the line below: use the last two digits of you access ID, e.g., gn0061 gives 61
set.seed(36)  # <-- MODIFY this number
# DO NOT MODIFY the next four lines
wine$quality <- ifelse(wine$quality >= 6, 1, 0)  # 1 means good quality, 0 means bad quality
trainset <- sample(1:nrow(wine), 80)  # DO NOT CHANGE: you must sample 80 data points for training
validset <- setdiff(1:nrow(wine), trainset)  # The remaining is used for validation
source("~/development/dsa6000/midterm/ROC_func.R")  # source in the ROC_func.R (presumably located in your current directory)

# Task: carry out the forward selection procedure to find the best logistic regression model. 
# The target variable is quality (binary) and all the other 11 variables are potential candidates in the variable selection process.
# Use the validset AUC as both the variable selection criterion and the stopping criterion.
# Specifically, in each round, evaluate the addition of one candidate variable. Add the variable whose addition would 
# result in the greatest AUC on the validset and when this (greatest) AUC is also greater than the AUC from the best model 
# in the last round (i.e., the model without adding this variable). 
# If the greatest AUC is not greater than the best-model AUC from the last round, stop the procedure. The best model from
# the last round will be the final model. 
# Report the following items: (1) the final model formula string; (2) number of independent variables in the final model; 
# (3) the validset AUC value of the final model. 
# For example: if you final model is "quality ~ alcohol + total.sulfur.dioxide + sulphates", report this formula string for (1),
# report 3 for (2), and report the validset AUC value of this model for (3). 
# Note: use the original variable names in the data set, do not change the names. 
# Submit your R code for partial credit in case your solution is incorrect. 

# Your work code starts here. Good luck!
forward_selection <- function(df, target_col_idx) {

    used_cols <- c()
    unused_cols <- 1:(ncol(df) - 1)
    best_auc <- -1
    mdl_str <- paste(colnames(df)[target_col_idx], '~')
    for (j in seq_along(colnames(df))) {
        best_auc_of_round <- -1
        best_auc_idx <- -1
        for (i in seq_along(unused_cols)) {
            cur_col <- colnames(df)[unused_cols[i]]
            cur_mdl_str <- mdl_str
            # Add good columns to current model
            for (k in seq_along(used_cols)) {
                cur_mdl_str <- paste0(cur_mdl_str, ' ', colnames(df)[used_cols[k]], ' +')
            } 
            cur_mdl_str <- paste0(cur_mdl_str, ' ', colnames(df)[unused_cols[i]])

            # create data frame to be used in ROC function
            mdl_test_df <- data.frame(label = df[validset, 'quality'])

            # build model and get predictions
            test_mdl <- glm(as.formula(cur_mdl_str), family = 'binomial', data = df, subset = trainset)
            test_preds <- predict(test_mdl, newdata = df[validset, ], type = 'response')
            mdl_test_df$pred <- test_preds

            # check if AUC value is better than previous run
            test_auc <- ROC_func(mdl_test_df, 1, 2, add_on = F, color = 'black')
            if (test_auc > best_auc_of_round) {
                best_auc_of_round <- test_auc
                best_auc_idx <- i
            }
            print(cur_mdl_str)
            print(paste('Round', j, cur_col, 'AUC =', test_auc))
        }

        # display best run
        print(paste('Best AUC of Round', best_auc_of_round))
        print(paste('Best AUC column:', colnames(df)[unused_cols[best_auc_idx]]))

        # break if the AUC value falls below best so far
        if (best_auc_of_round <= best_auc) {
            break
        } else {
            used_cols <- append(used_cols, unused_cols[best_auc_idx])
            unused_cols <- unused_cols[-best_auc_idx]
            best_auc <- best_auc_of_round
        }
    }

    # build final string for return
    final_mdl_str <- 'quality ~'
    for (i in seq_along(used_cols)) {
        final_mdl_str <- paste0(final_mdl_str, ' ', colnames(df)[used_cols[i]])
        if (i != length(used_cols)) {
            final_mdl_str <- paste0(final_mdl_str, ' +')
        }
    }
    return(c(mdl_str = final_mdl_str, num_preds = length(used_cols), auc = best_auc))
}
# mdl_str, num_preds, auc <- forward_selection(wine, 12)
res <- forward_selection(wine, 12)
print(paste('Model Formula ::', res['mdl_str']))
print(paste('# of Predictors ::', res['num_preds']))
print(paste('Best AUC ::', res['auc']))
