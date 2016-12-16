#11.1
install.packages("gdata")
library("gdata")

nfl <- read.xls("data-table-B1.xls")
View(nfl)

mod.nfl <- lm(y ~ x2+x7+x8, data = nfl)

#11.1a calculate the PRESS statistic
r <- resid(mod.nfl)
pr <- resid(mod.nfl)/(1-lm.influence(mod.nfl)$hat)
sum(pr^2)
#PRESS statistic is 87.46123

summary(mod.nfl)

y_hat = predict(mod.nfl, newdata=nfl)
SSt = sum((nfl$y-y_hat)^2) + sum((y_hat-mean(nfl$y))^2)

r2_pred = 1- sum(pr^2)/SSt
r2_pred
#0.7325052
#R^2 for prediction is 73.25% which is pretty good
train.size <- nrow(nfl)/2
train <- sample(1:nrow(nfl), train.size)
test = -train
nfl.train <- nfl[train, ]
nfl.test <- nfl[test, ]

mod2.nfl <- lm(y ~ x2+x7+x8, data = nfl.train)

summary(mod.nfl)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) -1.808372   7.900859  -0.229 0.820899    
#   x2           0.003598   0.000695   5.177 2.66e-05 ***
#   x7           0.193960   0.088233   2.198 0.037815 *  
#   x8          -0.004816   0.001277  -3.771 0.000938 ***

summary(mod2.nfl)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)  
#   (Intercept) 11.427874  14.348842   0.796   0.4443  
#   x2           0.003051   0.001015   3.005   0.0132 *
#   x7           0.035876   0.154379   0.232   0.8209  
#   x8          -0.006271   0.002135  -2.937   0.0148 *
#The coefficients for x7, and the intercept is very noticible. The coefficient for x8 also changed.
#the coefficient for x2 has a very slight change. 

yhat_test <- predict(mod2.nfl, newdata=nfl.test)

nfl.test$y
# 10 11 13 10  7 10  9  9  6  5  5  4 10  0
test.resid <-  nfl.test$y - yhat_test
test.resid 
# 1          3          4          7         11         12         13         14         15         17         18         20 
# 4.2007083  3.5008084  2.4388076 -1.1347062  1.0759086  1.8558588 -0.2957310 -0.5383555 -2.1606123 -1.5364174  0.6277030 -0.9794374 
# 24         28 
# 0.1899622 -1.8800397 

#The residuals are pretty big, given that the number of wins in the test set goes from 0 to 13. 
#so the model does not predict the number of games well. 

#11.1c
nfl.delete <- nfl[c(-7,-8,-9,-11,-17,-26), ]
mod3.nfl <- lm(y~x2+x7+x8, data=nfl.delete)
nfl.teams <- nfl[c(7,8,9,11,17,26), ]
yhat_team <- predict(mod3.nfl, newdata=nfl.teams)
nfl.teams$y
# 10 11  4  7  5  8
teams.resid <- nfl.teams$y - yhat_team
teams.resid
    # 7           8           9          11          17          26 
# -2.34232552  0.08825961  2.61104123  0.06372995 -0.32060258 -0.22270528 
#The residuals are much better. It seems that the model predicts these teams very well

#11.2
#I will use the training and test set produced in 11.1
plot(nfl.train)
mod.train <- lm(y~x2+x7+x8, data = nfl.train)
yhat_train <- predict(mod.train, newdata=nfl.train)
yhat_test <- predict(mod.train, newdata = nfl.test)
train.resid <- nfl.train$y - yhat_train
test.resid <- nfl.test$y - yhat_test
plot(y=train.resid, x=yhat_train, main = "residual vs. fitted for training set")
plot(y=test.resid, x=yhat_test, main = "residual vs fitted for testing set")
#By looking at the residual vs fitted, one can see that the model created for the training set
#does pretty well in predicting the training set, but does poorly for the testing set 
mean(sum(train.resid^2))
#the MSE for the training set is 26.44

mean(sum(test.resid^2))
#The MSE for the testing set is 54.068

#by looking at the MSE, the training model does not do well on the testing set.

#11.3
r <- resid(mod.train)
pr <- resid(mod.train)/(1-lm.influence(mod.train)$hat)
sum(pr^2)
#The PRESS statistic is 49
SSt = sum((nfl.test$y-yhat_test)^2) + sum((yhat_test-mean(nfl.test$y))^2)

r2_pred = 1- sum(pr^2)/SSt
r2_pred
#0.6646988
#The indication of the predictive performance of the model is only 66.47% on the testing set.

#calculating r^2
sum((yhat_test-mean(nfl.test$y))^2)/sum((nfl.test$y-mean(nfl.test$y))^2)
# 0.5966409
#The r^2 is about 0.6, or 60%. This is close to the indication of the predictive performance of the model of 66.47%

#11.11
summary(mod.train)
#               Estimate Std. Error t value Pr(>|t|)  
#   (Intercept) 11.427874  14.348842   0.796   0.4443  
#   x2           0.003051   0.001015   3.005   0.0132 *
#   x7           0.035876   0.154379   0.232   0.8209  
#   x8          -0.006271   0.002135  -2.937   0.0148 *

summary(mod.nfl)
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) -1.808372   7.900859  -0.229 0.820899    
#   x2           0.003598   0.000695   5.177 2.66e-05 ***
#   x7           0.193960   0.088233   2.198 0.037815 *  
#   x8          -0.004816   0.001277  -3.771 0.000938 ***

#The standard errors of the regression coefficients for the model developed from the estimation data
#are bigger than the standard errors for the model developed using all the data

#11.12 part a
mod.test <- lm(y~x2+x7+x8, data=nfl.test)
summary(mod.test)
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)  
#   (Intercept) -7.215777  10.640046  -0.678   0.5130  
#   x2           0.003539   0.001174   3.015   0.0130 *
#   x7           0.274206   0.124732   2.198   0.0526 .
#   x8          -0.004332   0.001921  -2.255   0.0478 *

#comparing the standard errors above to the one shown in 11.11 on the estimation set, 
#we can see that they are about the same. 

#11.12 part b
yhat.test.on.train <- predict(mod.test, newdata=nfl.train)

#calculating r^2
sum((yhat.test.on.train-mean(nfl.train$y))^2)/sum((nfl.train$y-mean(nfl.train$y))^2)
#The r^2 is 0.9751952. The model does very well in predicting the data in the prediction set

#13.1 a
missile <- read.xls("data-prob-13-1.XLS")
mod.missle <- glm(y~., data=missile, family=binomial)
summary(mod.missle)

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)   
#   (Intercept)  6.070884   2.108996   2.879  0.00399 **
#   x           -0.017705   0.006076  -2.914  0.00357 **

#y_hat = exp(6.070884 - 0.0177x)/(1 + exp(6.070884 - 0.0177x))

#13.1b
summary(mod.missle)
# Null deviance: 34.617  on 24  degrees of freedom
# Residual deviance: 20.364  on 23  degrees of freedom
20.364/23
# 0.8853913 #Because the residual deviance / (df) is less than 1, the model is adequate

#13.1c
exp(-0.0177) 
# 0.9824557
1-0.9824557
#0.0175443
#for every unit increase of knot speed of the missle, the odds of hitting the target
#is decreased by 1.754%

#13.1d
missile$x2 <- (missile$x)^2
mod2.missile <- glm(y~., data =missile, family = binomial)
summary(mod2.missile)

# Null deviance: 34.617  on 24  degrees of freedom
# Residual deviance: 20.363  on 22  degrees of freedom

#The residual deviance didn't change much, which means that this quadratic term is unncessary

#13.2
home <- read.xls("data-prob-13-2.XLS")
View(home)
#13.2a
mod.home <- glm(y~., data=home, family=binomial)
summary(mod.home)
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)  
#   (Intercept) -8.7395139  4.4394326  -1.969   0.0490 *
#   x            0.0002009  0.0001006   1.998   0.0458 *

#y_hat = exp(-8.74 - 0.0002x)/(1 + exp(-8.74 - 0.0002x))

#13.2b
summary(mod.home)
# Null deviance: 27.526  on 19  degrees of freedom
# Residual deviance: 22.435  on 18  degrees of freedom

#Since residual deviance > df, it shows the model is inadequate

#13.2c
exp(0.0002009)
# 1.000201
#As income increases by $1 the odds of owning a home increases by 0.0201%

#13.2d
home$x2 <- (home$x)^2
mod2.home <- glm(y~x2, data=home, family=binomial)
summary(mod2.home)

# Null deviance: 27.526  on 19  degrees of freedom
# Residual deviance: 22.610  on 18  degrees of freedom

#Because the residual deviance didn't change much, it shows the quadratic term is not required

#13.5 a
auto <- read.xls("data-prob-13-5.XLS")
mod.auto<- glm(y~., data=auto, family=binomial)
summary(mod.auto)

# Coefficients:
#              Estimate Std. Error z value Pr(>|z|)  
# (Intercept) -7.047e+00  4.674e+00  -1.508    0.132  
# x1           7.382e-05  6.371e-05   1.159    0.247  
# x2           9.879e-01  5.274e-01   1.873    0.061 .

#13.5b
# Null deviance: 27.726  on 19  degrees of freedom
# Residual deviance: 21.082  on 17  degrees of freedom
#Because residual deviance > degress of freedome, the model is inadequate

#13.5 c
exp(7.382e-05)
# 1.000074
#as income increases by $1, the odds of buying a car increases by 0.0074%

exp(9.879e-01)
# 2.685589
#As the age of a car increases, the odds of buying a new car increases by 168%

#13.5d
exp(-7.047e+00 +(7.382e-05)*45000 + 9.879e-01*(5))/(1+exp(-7.047e+00 +(7.382e-05)*45000 + 9.879e-01*(5)))
# 0.7710766 #Probrability is 77%

#13.5e
auto$x3 <- auto$x1*auto$x2
mod2.auto <- glm(y~x3, data=auto, family=binomial)
summary(mod2.auto)
# Null deviance: 27.726  on 19  degrees of freedom
# Residual deviance: 18.611  on 18  degrees of freedom

#The residual deviance decreased by 3, which shows that the interaction term is worth including

#13.5f
summary(mod.auto)
Coefficients:
#               Estimate Std. Error z value Pr(>|z|)  
# (Intercept) -7.047e+00  4.674e+00  -1.508    0.132  
# x1           7.382e-05  6.371e-05   1.159    0.247  
# x2           9.879e-01  5.274e-01   1.873    0.061 .
  
#Based on the p values of x1 and x2, they are not significant at the 95% confidence level
  
#13.5g
exp(confint(mod.auto))
#                 2.5 %   97.5 %
# (Intercept) 1.440855e-08 2.794192
# x1          9.999564e-01 1.000218
# x2          1.166984e+00 9.847452

#Confidence interval for Beta1 is (9.999564e-01, 1.000218)
#Confidence interval for Beta2 is (1.166984e+00, 9.847452)

#13.10
dev_res <-residuals(mod.auto, c="deviance")
plot(mod.auto$fitted.values, dev_res, main="deviance residuals vs fitted")
#No, based on the plot, there seems to be a pattern

#13.25
shuttle <- read.xls("data-prob-13-25.xls")
View(shuttle)
names(shuttle)
mod.shuttle <- glm(`At.Least.One.O.ring.Failure`~., data=shuttle, family = binomial)
summary(mod.shuttle)
# Coefficients:
#                        Estimate Std. Error z value Pr(>|z|)  
# (Intercept)           10.87535    5.70291   1.907   0.0565 .
# Temperature.at.Launch -0.17132    0.08344  -2.053   0.0400 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 28.975  on 23  degrees of freedom
# Residual deviance: 23.030  on 22  degrees of freedom

plot(x=shuttle$Temperature.at.Launch, y = shuttle$At.Least.One.O.ring.Failure, main = "temperature vs O-ring failure")
curve(predict(mod.shuttle,data.frame(`Temperature.at.Launch`=x),type="resp"),add=TRUE)
#the model fits the data pretty well

#13.25b
exp(-0.17132)
# 0.8425519
#This means an increase in temperature by 1 degree decreases the odds of o-ring failure by 16%

#13.25c
exp(-0.17132*(50) + 10.87535)/(1 + exp(-0.17132*(50) + 10.87535))
#0.9096484 which is 90.96%

#13.25d
exp(-0.17132*(75) + 10.87535)/(1 + exp(-0.17132*(75) + 10.87535))
#0.1219974 which is 12.20%

#13.25e
exp(-0.17132*(31) + 10.87535)/(1 + exp(-0.17132*(31) + 10.87535))
#0.9961829 which is 99.6%
#I will only launch a space shuttle if it's temperature is relatively high, maybe beyond 75 degrees farenheight

#13.25f
dev_res <-residuals(mod.shuttle, c="deviance")
plot(mod.shuttle$fitted.values, dev_res, main="deviance residuals vs probability")
#there seems to be a pattern in the plot, so there is so there are problems with the model

#13.25g
shuttle$t2 <- (shuttle$Temperature.at.Launch)^2
mod2.shuttle <-glm(At.Least.One.O.ring.Failure~t2, data=shuttle, family=binomial)
summary(mod2.shuttle)
# Coefficients:
#                Estimate Std. Error z value Pr(>|z|)  
# (Intercept)  5.1003840  2.9250224   1.744   0.0812 .
# t2          -0.0012599  0.0006203  -2.031   0.0423 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 28.975  on 23  degrees of freedom
# Residual deviance: 23.228  on 22  degrees of freedom

#The residual deviance didn't change much, suggesting that the term did not improve the model
