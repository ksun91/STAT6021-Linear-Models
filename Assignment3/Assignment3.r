install.packages("gdata")
library ("gdata")
install.packages("dplyr")
library("dplyr")
install.packages("car")
library("car")

#Problem 3.1 
NFL <- read.xls("data-table-B1.xls")
names(NFL)
#3.1 part a
lm.nfl <- lm(y~x2+x7+x8, data=NFL)
summary(lm.nfl)
#Linear model is y = -1.808372 + 0.003598x2 + 0.193960x7 -0.004816x8

#3.1 part b
anova(lm.nfl)
summary(lm.nfl)

# Analysis of Variance Table
# 
# Response: y
#           Df  Sum Sq Mean Sq F value    Pr(>F)    
# x2         1  76.193  76.193  26.172 3.100e-05 ***
# x7         1 139.501 139.501  47.918 3.698e-07 ***
# x8         1  41.400  41.400  14.221 0.0009378 ***
# Residuals 24  69.870   2.911             

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.808372   7.900859  -0.229 0.820899    
# x2           0.003598   0.000695   5.177 2.66e-05 ***
# x7           0.193960   0.088233   2.198 0.037815 *  
# x8          -0.004816   0.001277  -3.771 0.000938 ***
#   ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.706 on 24 degrees of freedom
# Multiple R-squared:  0.7863,	Adjusted R-squared:  0.7596 
# F-statistic: 29.44 on 3 and 24 DF,  p-value: 3.273e-08

#The regression is significant since p value is 3.273e-0.8 < 0.05. 
#That means at least one of the betas is non-zero.

#3.1 part c

summary(lm.nfl)

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.808372   7.900859  -0.229 0.820899    
# x2           0.003598   0.000695   5.177 2.66e-05 ***
# x7           0.193960   0.088233   2.198 0.037815 *  
# x8          -0.004816   0.001277  -3.771 0.000938 ***
#   ---

#All three regressors (x2, x7, and x8) are significant since their p values are < 0.05
#The conclusion to be drawn is that when all three regressors are in the model, than each of them
#are significant in predicting the number of games won. Team passing yardage and percentage rush plays
#have a positive effect on winning games, and opponents' yards rushing has a negative effect on winning games

#3.1 part d
summary(lm.nfl)

# Residual standard error: 1.706 on 24 degrees of freedom
# Multiple R-squared:  0.7863,	Adjusted R-squared:  0.7596 
# F-statistic: 29.44 on 3 and 24 DF,  p-value: 3.273e-08

#The R-squared is 0.7863 and the adjusted R-squared is 0.7596

#3.1 part e
lm.reducedNFL <- lm(y~x2 + x8, data = NFL)

anova(lm.reducedNFL, lm.nfl)

# Analysis of Variance Table
# 
# Model 1: y ~ x2 + x8
# Model 2: y ~ x2 + x7 + x8
#       Res.Df    RSS Df Sum of Sq      F  Pr(>F)  
# 1     25 83.938                              
# 2     24 69.870  1        14.068 4.8324 0.03782 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Since the p-value < 0.05, we reject the null hypothesis that x7 = 0 after considering x2 and x8,
#and conclude that x7 is still a significant factor even after accounting for x2 and x8. 
anova(lm.reducedNFL, lm.nfl)$F[2] ^ 0.5
#The F-value for x7 is 4.8324 which is the square of the T-value for x7 found in part c (2.19826)
#Source: http://www.stat.columbia.edu/~martin/W2024/R6.pdf

#Question 3.3
#3.3 part a
confint(lm.nfl, level=0.95)

#                 2.5 %       97.5 %
# (Intercept) -18.114944410 14.498200293
# x2            0.002163664  0.005032477
# x7            0.011855322  0.376065098
# x8           -0.007451027 -0.002179961

#The confidence interval for beta_7 is (0.011855322, 0.376065098)

#3.3 part b
predict(lm.nfl, data.frame(x2=2300, x7=56.0, x8=2100), interval = "confidence")

# fit      lwr      upr
# 1 7.216424 6.436203 7.996645
#The confidence interval of number of games one in this scenario is (6.436203, 7.996645)


#Question 3.4
#3.4 part a
lm2.nfl <- lm(y~x7+x8, data = NFL)
summary(lm2.nfl)

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)   
# (Intercept) 17.944319   9.862484   1.819  0.08084 . 
# x7           0.048371   0.119219   0.406  0.68839   
# x8          -0.006537   0.001758  -3.719  0.00102 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.432 on 25 degrees of freedom
# Multiple R-squared:  0.5477,	Adjusted R-squared:  0.5115 
# F-statistic: 15.13 on 2 and 25 DF,  p-value: 4.935e-05

#The p-value for this regression is 4.935e-05, which is less than 0.05, which means
#that the regression is significant (it's not that case that coefficients of x7 = x8 = 0)

#3.4 part b
summary(lm2.nfl)

# Residual standard error: 2.432 on 25 degrees of freedom
# Multiple R-squared:  0.5477,	Adjusted R-squared:  0.5115 
# F-statistic: 15.13 on 2 and 25 DF,  p-value: 4.935e-05

#the r-squared for this one is 0.5477, which decreased from the one found in 3.1. This is reasonable
#since we lost one regressor. The adjusted r-squared for 3.4 part b is 0.5115, which is also lower
#than the adjusted r-squared for 3.1, which was 0.7596. This can be a sign that x2 is actually a variable
#that influences number of games won

#3.4 part c
confint(lm2.nfl, level=0.95)

#                 2.5 %       97.5 %
# (Intercept) -2.36784828 38.256485319
# x7          -0.19716429  0.293906022
# x8          -0.01015637 -0.002916818
#The confidence interval for x7 in this model is (-0.19716429, .293906022)
#since 0 is contained in this interval, x7 is not significant in this model.
#Comparing this to the CI in 3.3, x7 had a CI that did not contain 0 and was significant

predict(lm2.nfl, data.frame(x7=56.0, x8=2100), interval = "confidence")

#   fit      lwr      upr
# 1 6.926243 5.828643 8.023842

#the confidence interval for number of predicted wins is (5.828643, 8.023842). This CI
#is much wider than the CI interval we got for 3.3 which was (6.436203, 7.996645)

#3.4 part d
#based on these comparisons, we can conclude that by omitting an important variable,
#our model weakens. Not only does the R^2 decreases, but the Adjusted-R^2 descreases.
#Our confidence intervals also becomes wider. In addition, the significance of other variables changes
#which may lead to inaccurate conclusions of what factors are important.


#Problem 3.5
#3.5 part a 
Gas <- read.xls("data-table-B3.xls")
View(Gas)
lm.gas <- lm(y~x1+x6, data = Gas)
summary(lm.gas)

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) 32.884551   1.535408  21.417  < 2e-16 ***
#   x1          -0.053148   0.006137  -8.660 1.55e-09 ***
#   x6           0.959223   0.670277   1.431    0.163    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# y = 32.884551 - 0.053148x1 + 0.959223x6

#3.5 part b
anova(lm.gas)

# Analysis of Variance Table
# 
# Response: y
#           Df Sum Sq Mean Sq F value    Pr(>F)    
# x1         1 955.72  955.72 105.290 3.666e-11 ***
# x6         1  18.59   18.59   2.048    0.1631    
# Residuals 29 263.23    9.08                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(lm.gas)

# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) 32.884551   1.535408  21.417  < 2e-16 ***
#   x1          -0.053148   0.006137  -8.660 1.55e-09 ***
#   x6           0.959223   0.670277   1.431    0.163    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.013 on 29 degrees of freedom
# Multiple R-squared:  0.7873,	Adjusted R-squared:  0.7726 
# F-statistic: 53.67 on 2 and 29 DF,  p-value: 1.79e-10

#Because the p-value for this model is 1.79e-10 which is < 0.05, the model is significant
#this means that it is NOT the case that the coefficients of x1 = x6 = 0

#3.5 part c
summary(lm.gas)

# Residual standard error: 3.013 on 29 degrees of freedom
# Multiple R-squared:  0.7873,	Adjusted R-squared:  0.7726 
# F-statistic: 53.67 on 2 and 29 DF,  p-value: 1.79e-10

lm.simple <- lm(y ~ x1, data = Gas)
summary(lm.simple)

# Residual standard error: 3.065 on 30 degrees of freedom
# Multiple R-squared:  0.7723,	Adjusted R-squared:  0.7647 
# F-statistic: 101.7 on 1 and 30 DF,  p-value: 3.743e-11

#Both the R-squared and adjusted r-squared inceased when you add x8 (number of carburetor barrels) 
#into the model than when you only have x1 (engine displacement). The R-squared increased from
#0.7723 to 0.7873 and the adjusted r-squared increased from 0.7647 to 0.7726. 
#It makes sense that r-squared increased because adding another variable will always increase r-squared.
#It is promising that the adjusted r-squared also increased, though only a tiny bit. 

#3.5 part d
confint(lm.gas)
#                 2.5 %      97.5 %
# (Intercept) 29.74428901 36.02481266
# x1          -0.06569892 -0.04059641
# x6          -0.41164739  2.33009349

#the confidence interval for coefficient of x1 is (-.06569892, -0.04059641)

#3.5 part e
summary(lm.gas)

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) 32.884551   1.535408  21.417  < 2e-16 ***
#   x1          -0.053148   0.006137  -8.660 1.55e-09 ***
#   x6           0.959223   0.670277   1.431    0.163    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#The t-value for x1 is -8.660 and for x6 is 1.431. x1 shows signifiance but x6 does not.
#We conclude that in this model that contains both x1 and x6, 
#engine displacement is significant in inferring gasoline mileage
#but number of carburetor barrels is not significant.

#3.5 part f
predict(lm.gas, data.frame(x1=275, x6=2), interval = "confidence")
#     fit      lwr      upr
# 1 20.18739 18.87221 21.50257

#The confidence interval is (18.87221, 21.50257)

#3.5 part g
predict(lm.gas, data.frame(x1=275, x6=2), interval = "prediction")
#     fit     lwr      upr
# 1 20.18739 13.8867 26.48808

#The prediction interval is (13.8867, 26.48808)

#Problem 3.6
lm.simple.gas <- lm(y ~ x1, data=Gas)

#confidence interval for 2.4
predict(lm.simple.gas, data.frame(x1=275), interval = "confidence")

#     fit      lwr      upr
# 1 20.69879 19.58807 21.80952

#prediction interval for 2.4
predict(lm.simple.gas, data.frame(x1=275), interval = "prediction")

#     fit      lwr      upr
# 1 20.69879 14.34147 27.05611

#testing differences in confidence and prediction intervals of the two models
(21.80952 - 19.58807) < (21.50257 - 18.87221)
(27.05611 - 14.34147) < (26.48808 - 13.8867)

#The confidence interval of the simple model is tighter than the confidence interval of the more complicated model
#The prediction interval of the simple model is a bit looser than the prediction interval of the more complicated model
#since the differences are so tiny, and that the mean of these intervals are not that much different,
#we can say that there is not much difference between the two models. So the benefit of adding x6
#is not that great. It is only adding more complexity with not much gain in predicting gasoline mileage


#Problem 3.8
#3.8 part a
chem <- read.xls("data-table-B5.xls")
lm.chem <- lm(y~x6+x7, data = chem)
summary(lm.chem)

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 2.526460   3.610055   0.700   0.4908    
# x6          0.018522   0.002747   6.742 5.66e-07 ***
# x7          2.185753   0.972696   2.247   0.0341 *  

# y = 2.526460 + 0.018522x6 + 2.185753x7

#3.8 part b
summary(lm.chem)

# Residual standard error: 9.924 on 24 degrees of freedom
# Multiple R-squared:  0.6996,	Adjusted R-squared:  0.6746 
# F-statistic: 27.95 on 2 and 24 DF,  p-value: 5.391e-07

#The R-squared is 0.6996 and the Adjusted R-squared is 0.6746
#The p-value is 5.391e-07 which is < 0.05 which means the model is significant.
#This means that it is NOT the case that x6=x7=0

#3.8 part c
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 2.526460   3.610055   0.700   0.4908    
# x6          0.018522   0.002747   6.742 5.66e-07 ***
# x7          2.185753   0.972696   2.247   0.0341 *  

#The p-value for x6 is 5.66e-07 and for x7 is 0.0341. Both of these are less than 0.05
#which means both of these variables are significant

#3.8 part d
confint(lm.chem)

#                 2.5 %     97.5 %
# (Intercept) -4.92432697 9.97724714
# x6           0.01285196 0.02419204
# x7           0.17820756 4.19329833

#The CI for x6 is (0.01285196, 0.02419204)
#The CI for x7 is (0.17820756, 4.19329833)

#3.8 part e
lm.simple.chem <- lm(y~x6, data = chem)
summary(lm.simple.chem)

# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 6.144181   3.483064   1.764   0.0899 .  
# x6          0.019395   0.002932   6.616 6.24e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 10.7 on 25 degrees of freedom
# Multiple R-squared:  0.6365,	Adjusted R-squared:  0.6219 
# F-statistic: 43.77 on 1 and 25 DF,  p-value: 6.238e-07

#x6 is significant. R-squared is 0.6365 and the adjusted-r-squared is 0.6219.
#Comparing this to the more complex model, adjusted r-squared did decrease, but not that much.
#It seems that x6 is a good enough predictor on it's own, so one can be satisfied with this model

#3.8 part f 
confint(lm.simple.chem)
#                 2.5 %      97.5 %
# (Intercept) -1.02932458 13.31768586
# x6           0.01335688  0.02543261

#The confidence interval for x6 is (0.1335688, 0.02543261). For the more complex model
#The CI for x6 is (0.01285196, 0.02419204). The tightness of the CIs are about the same,
#this shows that the contribution of x7 is not that significant to the model 

#3.8 part g
mse.complex <- mean(lm.chem$residuals^2)
mse.simple <- mean(lm.simple.chem$residuals^2)
mse.complex
mse.simple
#The mse of the complex model is 87.54945 and the mse of the simple model is 105.9695.
#This doesn't say anything about the contribution of x7 because when a new variable is added,
#MSE will always decrease 

#3.11 part a
peanut <- read.xls("data-table-B7.xls")
lm.peanut <- lm(y~., data = peanut)
summary(lm.peanut)

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  5.208e+01  1.889e+01   2.757 0.020218 *  
# x1           5.556e-02  2.987e-02   1.860 0.092544 .  
# x2           2.821e-01  5.761e-02   4.897 0.000625 ***
# x3           1.250e-01  4.033e-01   0.310 0.762949    
# x4           1.776e-16  2.016e-01   0.000 1.000000    
# x5          -1.606e+01  1.456e+00 -11.035  6.4e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#y = 5.208e+01 + 5.556e-02*x1 + 2.821e-01 * x2 + 1.250e-01*x3 + 1.776e-16 * x4 - 1.606e01 * x5

#3.11 part b 
summary(lm.peanut)

# Residual standard error: 8.065 on 10 degrees of freedom
# Multiple R-squared:  0.9372,	Adjusted R-squared:  0.9058 
# F-statistic: 29.86 on 5 and 10 DF,  p-value: 1.055e-05

#because the p-value is so small 1.055e-05, which is less than 0.05, the model is significant
#this means that not all of the coefficients of the variables are 0.

#3.11 part c
summary(lm.peanut)

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  5.208e+01  1.889e+01   2.757 0.020218 *  
# x1           5.556e-02  2.987e-02   1.860 0.092544 .  
# x2           2.821e-01  5.761e-02   4.897 0.000625 ***
# x3           1.250e-01  4.033e-01   0.310 0.762949    
# x4           1.776e-16  2.016e-01   0.000 1.000000    
# x5          -1.606e+01  1.456e+00 -11.035  6.4e-07 ***
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#The t-values for each coefficient is listed in the 3rd column above (such as x1 = 1.860, etc.)
#This shows that x2 and x5 are significant at the 99.9% level. X1 is significant at the 90% level.
#X3 and X4 are not significant in this model.

#3.11 part d
summary(lm.peanut)

# Residual standard error: 8.065 on 10 degrees of freedom
# Multiple R-squared:  0.9372,	Adjusted R-squared:  0.9058 
# F-statistic: 29.86 on 5 and 10 DF,  p-value: 1.055e-05

lm.peanut.simpler <- lm(y ~ x2 + x5, data = peanut)
summary(lm.peanut.simpler)

# Residual standard error: 8.236 on 13 degrees of freedom
# Multiple R-squared:  0.9149,	Adjusted R-squared:  0.9018 
# F-statistic: 69.89 on 2 and 13 DF,  p-value: 1.107e-07

#As you can see from the R-squared and adjusted r-squared presented above, there wasn't that much change
#in the adjusted r-squared when comparing the simpler model to the complex model (0.9018 vs 0.9058). 
#This means that the simpler model may be a more preferred model because it is simpler

#3.11 part e
confint(lm.peanut)
#                 2.5 %      97.5 %
# (Intercept)   9.99688896  94.1612109
# x1           -0.01100273   0.1221138
# x2            0.15378045   0.4105053
# x3           -0.77353688   1.0235369
# x4           -0.44926844   0.4492684
# x5          -19.30879739 -12.8211665

#the confidence interval for temperature for the complex model is (0.15378045, 0.4105053)

confint(lm.peanut.simpler)

#               2.5 %      97.5 %
# (Intercept)  67.8389462  92.4302647
# x2            0.1550559   0.4092298
# x5          -19.2765650 -12.8533989

#the confidence interval for temperature for the simpler model is (0.1550559, 0.4092298)
#The difference in the confidence interval is negligible. The intervals are almost the same.
#This is another indicator that perhaps the simpler model is a more preferred model.


#3.16 part a
life <- read.xls("data-table-B16.xls")

lm.overall <- lm(LifeExp~People.per.TV + People.per.Dr, data = life)
lm.male <- lm(LifeExpMale~People.per.TV + People.per.Dr, data = life)
lm.female <- lm(LifeExpFemale~People.per.TV + People.per.Dr, data = life)

summary(lm.overall)

# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)   70.2362645  1.0925483  64.287   <2e-16 ***
#   People.per.TV -0.0226074  0.0096005  -2.355   0.0243 *  
#   People.per.Dr -0.0004470  0.0002016  -2.217   0.0332 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.032 on 35 degrees of freedom
# Multiple R-squared:  0.4347,	Adjusted R-squared:  0.4024 
# F-statistic: 13.46 on 2 and 35 DF,  p-value: 4.623e-05

#overall_life = 70.2362645 - 0.022607People.per.TV - 0.0004470People.per.Dr

summary(lm.male)

# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   73.0919445  1.2505753  58.447   <2e-16 ***
# People.per.TV -0.0256825  0.0109891  -2.337   0.0253 *  
# People.per.Dr -0.0004785  0.0002308  -2.074   0.0455 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.904 on 35 degrees of freedom
# Multiple R-squared:  0.4173,	Adjusted R-squared:  0.384 
# F-statistic: 12.53 on 2 and 35 DF,  p-value: 7.863e-05

#male_life = 73.0919445 - 0.0256825People.per.TV - 0.0004785People.per.Dr

summary(lm.female)

# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   67.4297595  0.9569733  70.461   <2e-16 ***
# People.per.TV -0.0198637  0.0084091  -2.362   0.0239 *  
# People.per.Dr -0.0004086  0.0001766  -2.314   0.0267 *  
#   ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5.283 on 35 degrees of freedom
# Multiple R-squared:  0.4457,	Adjusted R-squared:  0.414 
# F-statistic: 14.07 on 2 and 35 DF,  p-value: 3.279e-05

#female_life = 67.4297595 - 0.0198637People.per.TV - 0.0004086People.per.Dr

#Question 3.16 part b
#from the summary tables shown in 3.16 part a, all models are significant. 
#For overall_life, the p-value was 4.623e-05. For male_life the p-value was 7.863e-05.
#For Famel_life, the p-value was 3.279e-05. 

#3.16 part c
#From the summary tables shown in part 3.16 part a
#all of the regressors (People.per.TV and People.per.Dr) were significant at the 95% level.
# For overall_life, t-values for People.per.TV and People.per.DR were respectively (-2.355, -2.217)
# For male_life, t-values for People.per.TV and People.per.DR were respectively (-2.337, -2.074)
# For female_life, t-values for People.per.TV and People.per.DR were respectively (-2.362, -2.314)

#3.16 part d
#From the summary tables shown in part 3.16 part a
#For overall_life, the r-squared and adjusted_r-squared were respectively (0.4347, 0.4024)
#For male_life, the r-squared and adjusted_r-squared were respectively (0.4173, 0.384)
#For female_life, the r-squared and adjusted_r-squared were respectively (0.4457, 0.414)

#3.16 part e
confint(lm.overall)
#In the overall model, the CI for People.per.Dr was (-0.0008563196, -3.777668e-05)

confint(lm.male)
#In the male_life model, the CI for People.per.Dr was (-0.0009470177, -1.008023e-05)

confint(lm.female)
# In the female_life model, the CI for People.per.Dr was (-0.0007670492, -5.007977e-05)
