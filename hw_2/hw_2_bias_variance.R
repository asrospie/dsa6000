library(splines)
# Verify the Bias-Variance Tradeoff Equation (Section 2.2.2 of ISLR):
# For a given input x_0, E[(y0 - fhat(x0))^2] = Var(fhat(x0)) + [Bias(fhat(x0))]^2 + Var(eps)
# Population and ground truth:
noisysine <- function(x){
  y <- 5 + 8*sin(0.1*x + 2) + rnorm(length(x), 0, 2)
  return(y)
}
cleansine <- function(x){
  y <- 5 + 8*sin(0.1*x + 2)
  return(y)
}

# visualize a sample
pdf("~/Downloads/noisy_sine_plot.pdf")
par(mfrow = c(1,1), mar = c(4.5,4.5,4,4))
x <- sort(runif(50, 25, 70))
y <- noisysine(x)
plot(x, y, ylab = 'y')
points(x, cleansine(x), type = 'l', col = 'red', lty=2)
legend("bottomright", legend = c("A random sample", "The true f"), pch = c(1,NA), lty = c(NA,
                                                                                        2), col = c("black","red"))
dev.off()

m1 <- lm(y ~ x)
abline(m1)

pdf("~/Downloads/bias_variance_models.pdf")
par(mfrow = c(2,2), mar=c(3,3,2,1))
for(i in 1:4){
  x <- runif(50, 25, 70)
  y <- noisysine(x)
  plot(x, y, xlab='', ylab='', main = paste0("Sample ", i))
  abline(lm(y~x))
  mtext(c("x","y"), side = c(1,2), line = 2)
}

for(i in 1:4){
  x <- sort(runif(50, 25, 70))
  y <- noisysine(x)
  plot(x, y, xlab='', ylab='', main = paste0("Sample ", i))
  md <- lm(y~ns(x, df = 10))
  points(x, predict(md), type = 'l')
  mtext(c("x","y"), side = c(1,2), line = 2)
}
dev.off()

x0 <- 25 # Pick a test input point x0
sampsize <- 300 # training sample size
itermax <- 200 # Number of repetitions
SqErr = numeric(itermax)
fhat_x0 = numeric(itermax)

for(i in 1:itermax){
  x <- runif(sampsize, 25, 70) # Take a sample
  y <- noisysine(x) # from the real world
  fhat <- lm(y ~ x) # Fit a model
  y0 <- noisysine(x0)
  fhat_x0[i] <- predict(fhat, newdata=data.frame(x = x0))
  SqErr[i] <- (y0 - fhat_x0[i])^2
}

E_SqErr <- mean(SqErr)
E_fhat_x0 <- mean(fhat_x0)
Var_fhat_x0 <- mean((fhat_x0 - E_fhat_x0)^2)
Bias <- E_fhat_x0 - cleansine(x0)
Var_Eps <- 4
# We expect the following two quantities to be close.
cat(paste("E[(y0 - fhat(x0))^2] = ", round(E_SqErr,3), "\n",
             "Var(fhat(x0)) + [Bias(fhat(x0))]^2 + Var(Eps) = ",
             round(Var_fhat_x0,3), " + ", round(Bias^2,3), " + ", round(Var_Eps,3), " = ",
             round(Var_fhat_x0 + Bias^2 + Var_Eps,3), "\n"))

# Exercise 1: Change the sampsize from 100 to 300, re-run, observe the difference in the
# output. Explain.
# Exercise 2: Increase the flexilility of the model, e.g., using y~ns(x, ...), try df=8, 20
# and , observe the difference in the output. Explain.
# Exercise 3: Does picking a different x0 affect the observations?
# Exercise 4: Generate a plot similar to Figure 2.12 to showcase the Bias-Variance tradeoff.
# Try to make the plot self-contained and self-explanatory by spelling out the context and
# parameters used in the experiments.

#pdf("simple_to_complex_model_fits.pdf")
x <- runif(100, 25, 70)
y <- noisysine(x)
plot(x,y) # sample data
test_x <- seq(10,80,length=200)
true_y <- cleansine(test_x)
points(test_x,true_y,type='l',lwd=1, lty = 2) # true f

simple_model <- lm(y~x) # A linear model
pred_simple <- predict(simple_model, newdata=data.frame(x = test_x))
points(test_x,pred_simple,type='l',col='orange',lwd=1)

moderate_model <- lm(y~ns(x, df=8)) # natural spline with df 8
pred_moderate <- predict(moderate_model, newdata=data.frame(x = test_x))
points(test_x,pred_moderate,type='l',col='blue',lwd=1)

flexible_model <- lm(y~ns(x, df=30)) # natural spline with df 30
pred_flexible <- predict(flexible_model, newdata=data.frame(x = test_x))
points(test_x,pred_flexible,type='l',col='red',lwd=1)

legend("topleft", legend=c('data','ground truth','simple model fit', 'moderate model fit',
                               'flexible model fit'),
           col=c('black','black','orange','blue','red'),
           pch=c(1, rep(NA,4)), lwd=c(NA,rep(1,4)), lty = c(NA, 2, rep(1,3)))
#dev.off()

# Calculate the training MSE
(trainMSE_simple <- mean(residuals(simple_model)^2))
(trainMSE_moderate <- mean(residuals(moderate_model)^2))
(trainMSE_flexible <- mean(residuals(flexible_model)^2))

# Calculate the test MSE on a test sample
xtest <- runif(100, 25, 70)
ytest <- noisysine(xtest)
pred_simple <- predict(simple_model, newdata=data.frame(x = xtest))
pred_moderate <- predict(moderate_model, newdata=data.frame(x = xtest))
pred_flexible <- predict(flexible_model, newdata=data.frame(x = xtest))
(testMSE_simple <- mean((ytest - pred_simple)^2))
(testMSE_moderate <- mean((ytest - pred_moderate)^2))
(testMSE_flexible <- mean((ytest - pred_flexible)^2))

#pdf("simple_flexible_MSE_compare.pdf", width=10, height=8)
degf <- c(2, 8, 30)
trainMSEs <- c(trainMSE_simple, trainMSE_moderate, trainMSE_flexible)
colors <- c('orange','blue','red')
testMSEs <- c(testMSE_simple, testMSE_moderate, testMSE_flexible)
plot(degf, trainMSEs, type='p', col=colors, lwd=2, cex=1.5, cex.lab=1.5, cex.axis=1.5,
     pch=15, xlab='Flexibility', ylab='MSE', ylim=c(min(c(trainMSEs, testMSEs)), max(c(trainMSEs,
                                                                                       testMSEs))))
points(degf, trainMSEs, type='l', col='black', lwd=2, cex=1.5)
points(degf, testMSEs, type='p', col=colors, lwd=2, cex=1.5, pch=22)
points(degf, testMSEs, type='l', col='green', lwd=2, cex=1.5)
abline(h=4, lty=2, lwd=2, col='gray') # Do you know why 4?
legend('topright', legend=c('TrainMSE','TestMSE','Var(eps)'), cex=1.5, lwd=2, pch =
         c(15,22,NA), lty=c(1,1,2), col=c('black','green','gray'))
#dev.off()

