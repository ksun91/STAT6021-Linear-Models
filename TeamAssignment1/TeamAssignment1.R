###########################
#                         #
#   Team Assignment 1     #
#                         #
###########################

## Please submit one set of answers per team.            ##
## Your answers may be submitted as an annotated R file. ##
###########################################################

set.seed(7)

## Start with given x-values
x <- read.table("teamassign01data.txt")[,1]
x

## Generate corresponding y-values according to the model y ~ 25 + 4x + e, where e~N(0,var=12^2)
y <- 25 + 4*x + rnorm(100, mean=0, sd = 12)

## Plot the relationship
plot(x,y, pch=20, cex=0.3)

#################
## Question 1: ##
#################

# Using the (x,y) from above, generate a linear model. 
model <- lm(y~x)

#   (a) Report the coefficients hat(beta_0) and hat(beta_1).
coef(model) # hat(beta_1) = 3.9913, hat(beta_0) = 27.0130

#   (b) Report the predicted value of y for x=18.
y_18 <- predict.lm(model, data.frame(x=18))
y_18 # Predicted value of y for x=18 is 98.8559

#   (c) Report MS_Res.
MSE <- sum(residuals(model)^2)/(length(x)-2)
MSE # MSE is 133.7221

#################
## Question 2: ##
#################

# Generate the linear model requested in Question 1 1000 times. Create a new vector of y-values 
# for each repetition.

y_vec <- data.frame(matrix(0, nrow=1000, ncol=100))

for (i in 1:1000){
  y_run <- 25 + 4*x + rnorm(100, mean=0, sd = 12)
  y_vec[i,] <- y_run
}  
  
#   (a) Determine and report the mean and variance of the generated coefficients.
b0 <- c()
b1 <- c()

for (i in 1:1000){
  model <- lm(unlist(y_vec[i,])~x)
  b0 <- c(b0, model$coefficients[1])
  b1 <- c(b1, model$coefficients[2])
}

### Answer (2a):
mean(b0) # The mean of the hat(beta_0) coefficients is 24.89696.
mean(b1) # The mean of the hat(beta_1) coefficients is 4.00237.

sd(b0)^2 # The variance of the hat(beta_0) coefficients is 20.72103.
sd(b1)^2 # The variance of the hat(beta_1) coefficients is 0.1190849.

#   (b) Based on theoretical considerations, what should the mean and variance of the  
#       generated coefficients be? Explain your answer.
sigma <- 12 # This is the standard deviation of our error term in the true equation.
s_xx <- sum((x - mean(x))^2)
b1_var <- sigma^2/s_xx
b0_var <- sigma^2 * (1/length(x) + mean(x)^2/s_xx)

### Answer (2b): The mean of hat(beta_0) should approach 25 and the mean of hat(beta_1) should 
###              approach 4 since the expected value of beta_0 and beta_1 are the coefficients
###              of y = 25 + 4*x. The variance of beta_1 is sigma^2/s_xx and variance of beta_0 
###              is sigma^2(1/n + x_bar^2/s_xx),where s_xx = sum((x - mean(x))^2), and we know 
###              that the value of sigma is 12 in our actual model because the standard deviation 
###              is 12. Therefore, the variance of hat(beta_0) should approach 20.315779 and the 
###              variance of hat(beta_1) should approach 0.01182988.

#   (c) Find a 95% confidence interval centered at each coefficient. Determine and report  
#       the percentage of intervals that contain the true value of the coefficient. 
#       What should the percentage be?

n_b1 <- 0
n_b0 <- 0
for (i in 1:1000){
  y <- unlist(y_vec[i,])
  model <- lm(y~x)
  coefficients <- confint(model, level=0.95)
  
  if (coefficients[1,1] < 25 & coefficients[1,2] > 25){
    n_b0 = n_b0 + 1
  }
  
  if (coefficients[2,1] < 4 & coefficients[2,2] > 4){
    n_b1 = n_b1 + 1
  }
}

(n_b0/1000) * 100 # = 94.7%
(n_b1/1000) * 100 # = 94.5%
### Answer (2c): 94.7% of intervals contain the true value of beta_0, and 94.5% of
###              of intervals contain the true value of beta_1. We would expect these
###              percentages to approach 95% since that is our confidence level.

#   (d) Carry out the hypothesis test H0: beta_1 = 4 vs H1: beta_1 not= 4 at a 5% significance level. 
#       Determine and report the proportion of times that the null hypothesis is rejected, 
#       implying that beta_1 not= 4.

reject <- 0
s_xx <- sum((x - mean(x))^2)
n <- length(x)
t <- qt(.975, df=98)

for (i in 1:1000) {
  y <- unlist(y_vec[i,])
  model <- lm(y~x)
  b1_hat <- coef(model)[2]
  mse <- sum(model$residuals^2)/(n - 2)
  t_0 <- (b1_hat - 4)/sqrt(mse/s_xx)
  
  if (abs(t_0) > t) {
    reject <- reject + 1
  }
}
(reject/1000) * 100

### Answer (2d): The proportion of times that the null hypothesis is rejected, implying that 
###              beta_1 not=4, is 5.5%.

#   (e) For each set of coefficients, find a 95% confidence interval for the mean
#       response associated with x = 18. Determine and report the percentage of your
#       intervals that contain the true value. What should the percentage be?

count <- 0
y_0 <- 25 + 4*18 
for (i in 1:1000){
  y <- unlist(y_vec[i,])
  model <- lm(y~x)
  interval <- predict(model, data.frame(x = 18), interval = "confidence", level = 0.95)
  if (interval[2] < y_0 & interval[3] > y_0){
    count = count + 1
  }
}
(count/1000) * 100 # = 94.8%

### Answer (2e): The percentage of confidence intervals containing the true value of y when x=18
###              is 94.8%. The percentage of intervals should approach 95% since that is our 
###              confidence level.

#   (f) For each estimated mean response from part (e), find a corresponding
#       95% prediction interval for the response y. Generate one random response y based 
#       on the true model. Determine and report the percentage of intervals that contain the response.
#       What should the percentage be?

count <- 0
y_0 <- (25 + 4*18 + rnorm(1, mean=0, sd = 12))
for (i in 1:1000){
  y <- unlist(y_vec[i,])
  model <- lm(y~x)
  interval <- predict(model, data.frame(x = 18), interval = "predict", level = 0.95)
  if (interval[2] < y_0 & interval[3] > y_0){
    count = count + 1
  }
}
(count/1000)*100
### Answer (2f): Our percentage of intervals that contain the response is 100%. If we were to run the
###              test many, many more times, we would expect the mean of the percentages to approach 95%,
###              as that is our confidence level.

#   (g) Find and report a 95% confidence interval for sigma^2 by finding the 2.5th and 97.5th 
#       percentiles of the generated values of MS_Res to give the lower and upper confidence limits.

sigma <- c()
for (i in 1:1000){
  y <- unlist(y_vec[i,])
  model <- lm(y~x)
  sigma <- c(sigma, anova(model)$`Mean Sq`[2])
}
quantile(sigma, c(0.025, 0.975)) # = (108.6264, 187.4937)

### Answer (2g): The 95% CI for sigma^2 is (108.6264, 187.4937).
