###########################
#                         #
#   Team Assignment 3     #
#                         #
###########################

## Please submit one set of answers per team.                  ##
## Your answers may be submitted as an annotated R file.       ##
## Please submit your plots in a PDF as a separate attachment. ##
#################################################################
library(car)
library(gdata)
library(MASS)
#################
## Question 1: ##
#################

# For this problem you will use the files "teamassign03data01.csv" and "teamassign03data02.csv" to 
# demonstrate through simulation the effects of multicollinearity on the variance of the regression 
# coefficients and how they influence the accuracy of predictions.
#
#   (a) Repeat the following 1000 times:
#       (1) Select a random sample of 100 observations from data01.
#       (2) Fit a linear model to the 100 observations using all four variables. Save the values
#           of the estimated coefficients in separate vectors.
#       (3) Use your linear model to predict the y-values given in data02 then compute the MSE
#           using these residuals. Save this value in a vector.
#       (4) Compute the standard deviation for the vectors containing the coefficients and compute
#           the mean of the vector containing the MSEs. Record these values.
coef <- data.frame(matrix(0, nrow = 0, ncol = 5))
MSE <- c()
data1 <- read.csv("teamassign03data01.csv")
data2 <- read.csv("teamassign03data01.csv")
for (i in 1:1000){
  sample <- data1[sample(1:nrow(data1), 100, replace=TRUE),]
  model <- lm(y~., data=sample)
  coef[i,] <- summary(model)$coefficients[,1]
  data2$predict <- predict(model, data2)
  MSE <- c(MSE, sum((data2$predict-data2$y)^2) / (nrow(data2)-5))
}

sd(coef[,1]) # sd(b0) = 8.444842
sd(coef[,2]) # sd(b1) = 3.62919
sd(coef[,3]) # sd(b2) = 3.631205
sd(coef[,4]) # sd(b3) = 3.635176
sd(coef[,5]) # sd(b4) = 0.1404312
mean(MSE) # MSres = 426.4999

#   (b) Choose a suitable variable to remove from the model. Repeat (1)-(4) given in part (a)
#       1000 times using this model.
Anova(model) 
anova(model)
summary(model)
plot(sample) # simple scatter plot matrix
# Plot name: Q1-partB-plot.pdf
vif(model)

# From the summary and anova table, it's difficult to tell which variable to remove.
# Then we ran the simple scatter plot, to see if there is any multicollinearity 
# From the scatter plot, it seems that x3 is highly correlated with x1 and x2, 
# but x1 and x2 don't seem to be correlated. Then we calcualted the VIF for x1-x4 for our model
# in part a, and found high VIFs for x1, x2 and x3. We then ran a different model without x3,
# and calculated the VIFs for x1, x2, and x4, and the VIFs for those regressors were about 1. 
# (Code shown below)
# From this analysis, we decided to take x3 out.

coef_b <- data.frame(matrix(0, nrow = 0, ncol = 4))
MSE_b <- c()
for (i in 1:1000){
  sample <- data1[sample(1:nrow(data1), 100, replace=TRUE),]
  model <- lm(y~x1+x2+x4, data=sample)
  coef_b[i,] <- summary(model)$coefficients[,1]
  data2$predict <- predict(model, data2)
  MSE_b <- c(MSE_b, sum((data2$predict-data2$y)^2) / (nrow(data2)-5))
}

sd(coef_b[,1]) # sd(b0) = 8.705252
sd(coef_b[,2]) # sd(b1) = 0.1390358
sd(coef_b[,3]) # sd(b2) = 0.1371147
sd(coef_b[,4]) # sd(b4) = 0.1459811
mean(MSE_b) # MSres = 423.3984
vif(model)

#   (c) How do the results from parts (a)(4) and (b)(4) compare? Explain what you observe.
#  Standard deviation for b1 and b2 decrease significantly. Standard deviation for b4 is about 
#  the same because x4 did not exhibit collinearity with the removed regressor x3. Although the MSE
#  decreased slightly less, the change was not very significant. The MSE is about the same for both models.
#  From these findings, we prefer the simpler model (with x3 removed as a regressor). 


#################
## Question 2: ##
#################

# For this problem you will use the file "data-table-B2.XLS".
#
#   (a) Fit the model using all explanatory variables. Iteratively remove insignificant variables
#       one-by-one until the all remaining variables are significant. Which variables remain in your model?
dataQ2 <- read.xls("data-table-B2.xls")
model.full <- lm(y~., data = dataQ2)
summary(model.full) # x5 is insignificant

model2 <- lm(y~x1+x2+x3+x4, data = dataQ2)
summary(model2) # x1 is insignificant

model3 <- lm(y~x2+x3+x4, data = dataQ2)
summary(model3) # x2 is insignificant

model4 <- lm(y~x3+x4, data = dataQ2)
summary(model4)
# all variables are significant
# x3 and x4 remain in the model.


#   (b) Compute each of the five types of residuals discussed in the textbook:
#       Residuals; Standardized residuals; Studentized residuals; PRESS residuals; R-student residuals.
#       You may use R functions.
# residuals
residuals <- resid(model4)
# Standardized residuals
standardized.res <- residuals /sqrt(anova(model4)$`Mean Sq`[3])
# Studentized residuals
studentized.res <- rstandard(model4)
# PRESS residuals
press.res <- residuals / (1 - lm.influence(model4)$hat)
# R-student residuals
r.student <- rstudent(model4)

#   (c) Use the results from part(a) to decide if there appear to be any outliers and/or high 
#       influence points.

#Detecting potential outliers by looking at residuals
abs(residuals) > (mean(abs(residuals)-3*sd(abs(residuals)))) & abs(residuals) < (mean(abs(residuals)) + 3*sd(abs(residuals)))
abs(residuals)
#Data point 22 seems to be a potential outlier because it is more than 3 standard deviations away from
#the mean residuals. 

#Detecing potential outliers by looking at standardized residuals
abs(standardized.res) > 3
abs(standardized.res)
#There doesn't seem to be a potential outlier when we consider outliers to be 3 or more standard deviations
#away. However, by looking at point 22, it is 2.64 standard deviations away so the data point being an outlier
#is not out of the realm of possibility. Using 3 standard deviations away is just a convention. 

#Detecing potential outliers by looking at PRESS residuals
press_dif <- press.res - residuals
abs(press_dif ) > (mean(abs(press_dif )-3*sd(abs(press_dif )))) & abs(press_dif ) < (mean(abs(press_dif )) + 3*sd(abs(press_dif )))
abs(press.res - residuals)
#Point 24 seems to be a potential influential point because the difference between the PRESS residual
#and the residual for point 24 is large. 

#Detecting potential outliers by R-student residuals 
abs(r.student) > abs(qt(.975/(2*nrow(dataQ2)), df = nrow(dataQ2)-3))
abs(r.student) > (mean(abs(r.student)-3*sd(abs(r.student)))) & abs(r.student) < (mean(abs(r.student)) + 3*sd(abs(r.student)))
abs(r.student)

#Point 22 seems to be a potential outlier based on the Bonferroni-type approach and crude cutoff approach
#when looking at the r-student residuals

# From these different residual analyses, we conclude that 22 is most likely a potential outlier/influential point
# and 24 is a possible influential point. We tried to confirm our analysis by creating a model without
# point 22 and another model without point 24, and comparing those models with the original model with those
# points. These models supported our results because the coefficient of x4 changed by about a standard error
# when we removed point 24, and the Adjusted R-squared increased when we removed point 22 by about 5 percentage points
# and the F-statistic increased.
model5 <- lm(y~x3+x4, data = dataQ2[c(1:21,23:29),]) #Removing point 22
summary(model5)

model6 <- lm(y~x3+x4, data = dataQ2[c(1:23,25:29),]) #Removing point 24
summary(model6)

summary(model4)

#   (d) Produce a normal probability plot of the R-student residuals and evaluate the plot for 
#       signs of departures from normality.
qqnorm(r.student)
qqline(r.student)
# Plot name: Q2partd.pdf
# The qqplot shows that we obtained a light-tail normal probability plot

#   (e) Produce plots of the R-student residuals vs. 
#       (1) the predicted values, and
#       (2) each explanatory variable.
#       What assumptions may not be satisfied? Explain.
dataQ2$predict = predict(model4, dataQ2)
plot(x=dataQ2$predict, y = r.student)
# Plot name: Q2parte1.pdf
# From the plot of r.student vs fitted values, we can see the variance gets 
# smaller as fitted value increases. Therefore, the assumption of constant 
# variance of the error is not satisfied. 

plot(x=dataQ2$x3, y = r.student)
# Plot name: Q2parte2-x3.pdf
# The plot shows no problematic pattern. All assumptions seems satisfied.

plot(x=dataQ2$x4, y = r.student)
# Plot name: Q2parte2-x4.pdf
# There is a increasing trend of the r-student residuals as x4 increases. Also,
# the variance gets larger as x4 increases.
# The assumptions of constant variance and uncorrelated residuals are not satisfied.
# We might need to apply transformation to x4 before including it as a regressor.
