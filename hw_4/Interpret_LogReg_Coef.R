# Logistic Regression: HOW DO I INTERPRET LOGISTIC REGRESSION MODELS?
honor <- read.table(file = "development/dsa6000/hw_4/honor.csv", header = T, sep = ",")
head(honor)
str(honor)
# The data set has 200 observations and the outcome variable used will be hon, indicating if a student is in an honors class or not.  So our p = prob(hon=1).  We will focus on the meaning of the regression coefficients, and ignore all the significance tests. 

# 0. Logistic regression with no predictor variables. 
m0 <- glm(hon ~ 1, data = honor, family = binomial)  # The model is logit(p) := log(p/(1-p)) = beta0. 
summary(m0)
# The intercept beta0 is -1.1255. This means that log(p/(1-p)) = -1.12546.  What is p here?  It turns out that p is the overall probability of being in honors class (hon = 1).  Let's take a look at the frequency table for hon.
table(honor$hon)  
prop.table(table(honor$hon))  # p = 49/200 = 0.245
# The odds are .245/(1-.245) = .3245 and the log of the odds (logit) is log(.3245) = -1.12546.  In other words, the intercept from the model with no predictor variables is the estimated log odds of being in honors class for the whole population of interest.  We can also transform the log of the odds back to a probability: p = exp(-1.12546)/(1+exp(-1.12546)) = .245, if we like.

# 1. Logistic regression with a single dichotomous predictor variables
# Now let's go one step further by adding a binary predictor variable, female, to the model.
m1 <- glm(hon ~ female, data = honor, family = binomial)  # The model is logit(p) = β0 + β1*female. 
summary(m1)
# Before trying to interpret the two parameters estimated above, let's take a look at the crosstab of the variable hon with female.
table(honor[,c('hon','female')])
# In the dataset, what are the odds of a male being in the honors class and what are the odds of a female being in the honors class?  We can manually calculate these odds from the table: for males, the odds of being in the honors class are (17/91)/(74/91) = 17/74 = .23; and for females, the odds of being in the honors class are (32/109)/(77/109) = 32/77 = .42.  The ratio of the odds for female to the odds for male is (32/77)/(17/74) = (32*74)/(77*17) = 1.809.  So the odds for males are 17 to 74, the odds for females are 32 to 77, and the odds for female are about 81% higher than the odds for males.
coefficients(m1)
# Now we can relate the odds for males and females and the output from the logistic regression.  The intercept of 1.471 is the log odds for males since male is the reference group (female = 0).  Using the odds we calculated above for males, we can confirm this: log(.23) = -1.47.  The coefficient for female is the log of odds ratio between the female group and male group: log(1.809) = .593.  So we can get the odds ratio by exponentiating the coefficient for female, exp(.593) = 1.809.

# 2. Logistic regression with a single continuous predictor variable
# Now let's consider a model with a single continuous predictor variable, math. 
m2 <- glm(hon ~ math, data = honor, family = binomial)  # The model is logit(p) = beta0 + beta1*math. 
summary(m2)
# In this case, the estimated coefficient for the intercept is the log odds of a student with a math score of zero being in an honors class.  In other words, the odds of being in an honors class when the math score is zero is exp(-9.79394) = .00005579.  These odds are very low, but if we look at the distribution of the variable math, summary(honor$math), we will see that no one in the sample has math score lower than 30.  In fact, all the test scores in the dataset were standardized around mean of 50 and standard deviation of 10.  So the intercept in this model corresponds to the log odds of being in an honors class when math is at the hypothetical value of zero.
coefficients(m2)
# How do we interpret the coefficient for math?  The coefficient and intercept estimates give us the following equation:
# log(p/(1-p)) = logit(p) = -9.793942  + 0.1563404*math
# Let's fix math at some value. We will use 54.  Then the conditional logit of being in an honors class when the math score is held at 54 is log(p/(1-p))(math=54) = -9.793942 + .1563404*54 = -1.35156.
# Let's examine the effect of a one-unit increase in math score.  When the math score is held at 55, the conditional logit of being in an honors class is log(p/(1-p))(math=55) = -9.793942 + .1563404*55 = -1.19522.
# Taking the difference of the two equations, we have the following:
# log(p/(1-p))(math=55)  - log(p/(1-p))(math = 54) = -1.19522 - (-1.35156) = 0.15634.
# We can say now that the coefficient for math is the difference in the log odds.  In other words, for a one-unit increase in the math score, the expected change in log odds is .1563404. 
# Can we translate this change in log odds to the change in odds? Indeed, we can.  Recall that logarithm converts multiplication and division to addition and subtraction. Its inverse, the exponentiation converts addition and subtraction back to multiplication and division.  If we exponentiate both sides of our last equation, we have the following: exp[log(p/(1-p))(math=55)  - log(p/(1-p))(math = 54)] = exp(log(p/(1-p))(math=55)) / exp(log(p/(1-p))(math = 54)) = odds(math=55)/odds(math=54) = exp(.1563404) = 1.1692241.
# So we can say for a one-unit increase in math score, we expect to see about 17% increase in the odds of being in an honors class.  This 17% of increase does not depend on the value that math is held at.

# 3. Logistic regression with multiple predictor variables and no interaction terms
# Let's build this model: logit(p) = log(p/(1-p))= beta0 + bet1*math + beta1*female + beta3*read
m3 <- glm(hon ~ math + female + read, data = honor, family = binomial)
summary(m3)
# This fitted model says that, holding math and reading at a fixed value, the odds of getting into an honors class for females (female = 1)over the odds of getting into an honors class for males (female = 0) is exp(.979948) = 2.66.  In terms of percent change, we can say that the odds for females are 166% higher than the odds for males.  The coefficient for math says that, holding female and reading at a fixed value, we will see 13% increase in the odds of getting into an honors class for a one-unit increase in math score since exp(.1229589) = 1.13.

# 4. Logistic regression with an interaction term of two predictor variables
# In all the previous examples, we have said that the regression coefficient of a variable corresponds to the change in log odds and its exponentiated form corresponds to the odds ratio.  This is only true when our model does not have any interaction terms.  When a model has interaction term(s) of two predictor variables, it attempts to describe how the effect of a predictor variable depends on the level/value of another predictor variable.  The interpretation of the regression coefficients become more involved.
# Let's build this model: logit(p) = log(p/(1-p)) = beta0 + beta1*female + beta2*math + beta3*female*math
m4 <- glm(hon ~ female + math + female*math, data = honor, family = binomial)
summary(m4)
# In this simple example where we examine the interaction of a binary variable and a continuous variable, we can think that we actually have two equations: one for males and one for females.  
# For males (female=0), the equation is logit(p) = log(p/(1-p))= beta0 + beta1*math.
# For females, the equation is logit(p) = log(p/(1-p))= (beta0 + beta1) + (beta2 + beta3)*math.
# Now we can map the logistic regression output to these two equations. So we can say that the coefficient for math is the effect of math when female = 0.  More explicitly, we can say that for male students, a one-unit increase in math score yields a change in log odds of 0.13.  On the other hand, for the female students, a one-unit increase in math score yields a change in log odds of (.13 + .067) = 0.197.  In terms of odds ratios, we can say that for male students, the odds ratio is exp(.13)  = 1.14 for a one-unit increase in math score and the odds ratio for female students is exp(.197) = 1.22 for a one-unit increase in math score.  The ratio of these two odds ratios (female over male) turns out to be the exponentiated coefficient for the interaction term of female by math: 1.22/1.14 = exp(.067) = 1.07.

# The above tutorial is adopted (with modification) from https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/

# Exercises:
# E1. Build a logistic regression model to predict the probability that a student will be in the honors class, based on information we know about the student: male, math = 65, reading = 70. What is the probability?
m5 <- glm(hon ~ female + math + read, data = honor, family = binomial)
summary(m5)
p <- 1 - predict(m5, data.frame(female = 0, read = 70, math = 65), type = 'response')
print(p)
# E2. There are two students, A and B. A's math score is 10 points higher than B's. Build an appropriate model to answer: The odds of A getting in the honors class is _____ times the odds of B getting in the honors class. 
m6 <- glm(hon ~ math, data = honor, family = binomial)
summary(m6)
logit_a <- -9.79394 + .15634 * 60
logit_b <- -9.79394 + .15634 * 70
diff_logits <- logit_b - logit_a
exp(diff_logits)
# A. 2.775
# B. 3.775
# C. 4.775 - CORRECT
# D. Insufficient information to answer.

# E3. There are two students, Mary and John (note: gender is implied by name). 
# (a) If they have the same math score and we have no information about their reading and writing scores, who do you think has a higher chance of being in the honors class? 
# I think that Mary would have a higher chance to be an honor student. Looking at the table discussed earlier, it can be seen that in general, females have a higher chance of being in honors class. 
# (b) Suppose John's math score is 7 points higher than Mary's, which statement is right about their odds of being in the honors class?
m7 <- glm(hon ~ female + math, data = honor, family = binomial)
summary(m7)
logit_john <- -10.80595 + 0.96531 * 0 + 0.16422 * 70
logit_mary <- -10.80595 + 0.96531 * 1 + 0.16422 * 63
print(honors_p)
predict(m7, data.frame(female = 0, math = 70), type = 'response')
predict(m7, data.frame(female = 1, math = 63, type = 'response'))
# A. The odds of John is about 16% higher than the odds of Mary. - CORRECT
# B. The odds of Mary is about 16% higher than the odds of John.
# C. There is insufficient information to tell. 

