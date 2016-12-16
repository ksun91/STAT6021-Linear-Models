install.packages('gdata')
library(gdata)
install.packages('car')
library(car)
install.packages('glmnet')
library(glmnet)
install.packages('emdbook')
library(emdbook)
install.packages("MASS")
library(MASS)
install.packages("leaps")
library(leaps)
#9.7
gas <- read.xls('data-table-B3.xls')

#9.7 part a
gas.no.na<- gas[which(!is.na(gas$x3)),] #removing na values
cor(gas.no.na) #looking at correlation matrix

          # y         x1         x2         x3          x4         x5          x6         x7         x8         x9        x10        x11
# y    1.0000000 -0.8721701 -0.7968304 -0.8495915  0.42237247  0.6347500 -0.47180548  0.7077682 -0.7528208 -0.7629952 -0.8528801 -0.7212809
# x1  -0.8721701  1.0000000  0.9408473  0.9891628 -0.34697246 -0.6720903  0.64279836 -0.7719151  0.8623681  0.7974811  0.9515520  0.8244446
# x2  -0.7968304  0.9408473  1.0000000  0.9643592 -0.28989951 -0.5509642  0.76141897 -0.6259445  0.8027387  0.7105117  0.8878810  0.7086735
# x3  -0.8495915  0.9891628  0.9643592  1.0000000 -0.32599915 -0.6728661  0.65312630 -0.7461800  0.8641224  0.7881284  0.9434871  0.8012765
# x4   0.4223725 -0.3469725 -0.2898995 -0.3259992  1.00000000  0.4137808  0.03748643  0.5582357 -0.3041503 -0.3781736 -0.3584588 -0.4405457
# x5   0.6347500 -0.6720903 -0.5509642 -0.6728661  0.41378081  1.0000000 -0.21952829  0.8717662 -0.5613315 -0.4534470 -0.5798617 -0.7546650
# x6  -0.4718055  0.6427984  0.7614190  0.6531263  0.03748643 -0.2195283  1.00000000 -0.2756386  0.4220680  0.3003862  0.5203669  0.3954893
# x7   0.7077682 -0.7719151 -0.6259445 -0.7461800  0.55823570  0.8717662 -0.27563863  1.0000000 -0.6552065 -0.6551300 -0.7058126 -0.8506963
# x8  -0.7528208  0.8623681  0.8027387  0.8641224 -0.30415026 -0.5613315  0.42206800 -0.6552065  1.0000000  0.8831512  0.9554541  0.6824919
# x9  -0.7629952  0.7974811  0.7105117  0.7881284 -0.37817358 -0.4534470  0.30038618 -0.6551300  0.8831512  1.0000000  0.8994711  0.6326677
# x10 -0.8528801  0.9515520  0.8878810  0.9434871 -0.35845879 -0.5798617  0.52036693 -0.7058126  0.9554541  0.8994711  1.0000000  0.7530353
# x11 -0.7212809  0.8244446  0.7086735  0.8012765 -0.44054570 -0.7546650  0.39548928 -0.8506963  0.6824919  0.6326677  0.7530353  1.0000000

#yes, x1 is highly correlated with x2, x3, and x10. X2 is highly correlatd with x3. X8 is highly correlated with x10. X9 is correlated with x10

#9.7 part b
lm.gas <- lm(y~., data=gas)
vif(lm.gas)

# x1         x2         x3         x4         x5         x6         x7         x8         x9        x10        x11 
# 119.487804  42.800811 149.234409   2.060036   7.729187   5.324730  11.761341  20.917632   9.397108  85.744344   5.145052 
#yes there is multicollinearity with x1, x2, x3, x7, x8, and x10 because VIF is greater than 10

lm.gas.no.na <- lm(y~., data =gas.no.na)
vif(lm.gas.no.na) #checking multicollinearity if we remove the data points where x3 is na

# x1         x2         x3         x4         x5         x6         x7         x8         x9        x10        x11 
# 119.487804  42.800811 149.234409   2.060036   7.729187   5.324730  11.761341  20.917632   9.397108  85.744344   5.145052 
#We still get multicollinearityy for the same variables


#9.13
fuel <- read.xls('data-table-B18.xls')
cor(fuel) #looking at correlation matrix

#         y.         x1          x2         x3          x4          x5         x6          x7          x8
# y.  1.00000000 -0.2337999 -0.06983142  0.1623350 -0.04383325 -0.59099039  0.5878101 -0.50651485  0.50527943
# x1 -0.23379987  1.0000000  0.00000000  0.0000000  0.00000000  0.00000000  0.0000000  0.00000000  0.00000000
# x2 -0.06983142  0.0000000  1.00000000 -0.4686644 -0.17832047 -0.08191878 -0.1993188 -0.03162179 -0.46152267
# x3  0.16233496  0.0000000 -0.46866445  1.0000000  0.81618511  0.20253917  0.3954947  0.34513629  0.58008819
# x4 -0.04383325  0.0000000 -0.17832047  0.8161851  1.00000000  0.35408199  0.2511725  0.54093995  0.08638917
# x5 -0.59099039  0.0000000 -0.08191878  0.2025392  0.35408199  1.00000000 -0.7560775  0.91836954 -0.46129094
# x6  0.58781008  0.0000000 -0.19931880  0.3954947  0.25117255 -0.75607750  1.0000000 -0.66171734  0.61679607
# x7 -0.50651485  0.0000000 -0.03162179  0.3451363  0.54093995  0.91836954 -0.6617173  1.00000000 -0.38218608
# x8  0.50527943  0.0000000 -0.46152267  0.5800882  0.08638917 -0.46129094  0.6167961 -0.38218608  1.00000000

#x5 seems to have high multicollinearity with x7

lm.fuel <- lm(y.~., data=fuel)
vif(lm.fuel)
# x1         x2         x3         x4         x5         x6         x7         x8 
# 1.000000   1.900541 168.467420  43.103776  60.791320 275.472571 185.707184  44.363364

#there is multicollinearity with x3, x4, x5, x6, x7, and x8 due to vif's being greater than 10.

#9.14
wine <- read.xls('data-table-B19.xls')
cor(wine)

#         y           x1          x2         x3           x4           x5          x6          x7           x8         x9         x10
# y    1.0000000 -0.169878902  0.27747066 -0.3758899  0.701838522  0.707654712  0.65117305  0.68129132 -0.168180167  0.6170341  0.68129132
# x1  -0.1698789  1.000000000 -0.08880617  0.1150501 -0.008509266  0.012577406 -0.16389528  0.14334447  0.050018542  0.1637408  0.14334447
# x2   0.2774707 -0.088806174  1.00000000 -0.5820282  0.213164035  0.152141791  0.22044010  0.08631043  0.095509243 -0.0489386  0.08631043
# x3  -0.3758899  0.115050136 -0.58202821  1.0000000 -0.391465151 -0.370944023 -0.32542171 -0.36902794  0.404545811 -0.4959928 -0.36902794
# x4   0.7018385 -0.008509266  0.21316403 -0.3914652  1.000000000  0.995666429  0.94541703  0.93671923  0.015519116  0.7968608  0.93671923
# x5   0.7076547  0.012577406  0.15214179 -0.3709440  0.995666429  1.000000000  0.92529036  0.95892682  0.003091977  0.8260006  0.95892682
# x6   0.6511730 -0.163895282  0.22044010 -0.3254217  0.945417026  0.925290358  1.00000000  0.77970743 -0.042853322  0.6905468  0.77970743
# x7   0.6812913  0.143344473  0.08631043 -0.3690279  0.936719226  0.958926821  0.77970743  1.00000000  0.037155356  0.8472281  1.00000000
# x8  -0.1681802  0.050018542  0.09550924  0.4045458  0.015519116  0.003091977 -0.04285332  0.03715536  1.000000000 -0.4558050  0.03715536
# x9   0.6170341  0.163740803 -0.04893860 -0.4959928  0.796860805  0.826000635  0.69054681  0.84722807 -0.455804998  1.0000000  0.84722807
# x10  0.6812913  0.143344473  0.08631043 -0.3690279  0.936719226  0.958926821  0.77970743  1.00000000  0.037155356  0.8472281  1.00000000

#It seems that there is multicollinearity on x4 with x5, x6, x7, x10. X5 seem to be multicollinear with x6, x7, and x10. X7 is perfectly collinear
#with x10.

lm.wine <- lm(y~. - x10 - x7, data = wine) #removing x7 and x10
vif(lm.wine)
# x1         x2         x3         x4         x5         x6         x8         x9 
# 1.970605   4.092086   4.513202 603.518791 511.870261  33.319560   7.930630  36.170717

#Even after removing x7 and x10, it seems that there is still multicollinearity with x4, x5, x6, and x9

#9.15
meth <- read.xls('data-table-B20.xls')
cor(meth)

#       X.y         x1         x2          x3         x4          x5
# X.y  1.0000000  0.5089305  0.8394717 -0.74451745 -0.3733987  0.11544765
# x1   0.5089305  1.0000000  0.2410010 -0.22352540 -0.4005925 -0.12043517
# x2   0.8394717  0.2410010  1.0000000 -0.97718300 -0.3671877  0.11266889
# x3  -0.7445174 -0.2235254 -0.9771830  1.00000000  0.3580068 -0.04768291
# x4  -0.3733987 -0.4005925 -0.3671877  0.35800678  1.0000000 -0.50579161
# x5   0.1154477 -0.1204352  0.1126689 -0.04768291 -0.5057916  1.00000000

#seems like x2 and x3 has some collinearity

lm.meth <- lm(X.y~., data = meth)
vif(lm.meth)

# x1        x2        x3        x4        x5 
# 1.519064 26.283999 26.447032  2.202201  1.922689 

#yes, in fact x2 and x3 are collinear


#9.19 part a
#putting gas dataframe into a matrix
gas.x = model.matrix(y~., data=gas.no.na)[,-1]
gas.y = gas.no.na$y

range <- lseq(0.001,1, length = 25)
ridge.gas<-glmnet(gas.x,gas.y,alpha=0, lambda = range) 

ridge.gas$lambda


plot(ridge.gas,xvar="lambda",label=TRUE)

#the residual plot shows that the coefficients stabilizes at around log(lambda) = -1.5
exp(-1.5) #0.2231302
#The closest lambda that is modeled is 0.237137 which is our k
#No, this is not an adequate model based on the results of 9.19 part b and 9.19 part c

ridge.gas$lambda[6]
coef(ridge.gas)[,6]

#9.19 part b
#calculating RSS 
ridge.pred.y <- coef(ridge.gas)[,6][1] + gas.no.na$x1*coef(ridge.gas)[,6][2]+ gas.no.na$x2*coef(ridge.gas)[,6][3] + gas.no.na$x2*coef(ridge.gas)[,6][3]
+ gas.no.na$x3*coef(ridge.gas)[,6][4] + gas.no.na$x4*coef(ridge.gas)[,6][5] + gas.no.na$x5*coef(ridge.gas)[,6][6] + gas.no.na$x6*coef(ridge.gas)[,6][7]
+ gas.no.na$x7*coef(ridge.gas)[,6][8] + gas.no.na$x8*coef(ridge.gas)[,6][9] + gas.no.na$x9*coef(ridge.gas)[,6][10] + gas.no.na$x10*coef(ridge.gas)[,6][11]
+ gas.no.na$x11*coef(ridge.gas)[,6][12]

#RSS for the ridge regression
sum((ridge.pred.y-gas.no.na$y)^2) #4266.353

#RSS for the OLS regression
sum((predict(lm.gas.no.na, newdata=gas.no.na)-gas.no.na$y)^2) #187.4007

4266.353/187.4007 #22.76594

#The RSS has inflated by about 22 times when comparing the RSS from the ridge regression to the OLS regression

#9.19 part C
summary(lm.gas.no.na) # Multiple R-squared:  0.8355

mss.ridge <- sum((ridge.pred.y-mean(gas.no.na$y))^2)
ess.ridge <- sum((ridge.pred.y - gas.no.na$y)^2)

tss.ridge <- mss.ridge+ess.ridge

r.square.ridge <- 1-(4266.353/tss.ridge)
r.square.ridge #0.4907369

(0.8355-.4907369)/0.8355 #a 41.26% reduction in R^2 due to the ridge regression


#9.20
beta.transpose.m <- t(as.matrix(lm.gas$coefficients))
beta.m <- as.matrix(lm.gas$coefficients)
p <- ncol(gas.no.na)-1
sigma_square <- (summary(lm.gas.no.na)$sigma)**2

k <- (p*sigma_square)/(beta.transpose.m%*%beta.m)

#           [,1]
# [1,] 0.3290422

#Ideal k is 0.3290422

ridge.gas.mod2<-glmnet(gas.x,gas.y,alpha=0, lambda = 0.3290422) 

#Predicting y's
ridge.mod2.pred.y <- coef(ridge.gas.mod2)[1] + gas.no.na$x1*coef(ridge.gas.mod2)[2]+ gas.no.na$x2*coef(ridge.gas.mod2)[3] + gas.no.na$x2*coef(ridge.gas.mod2)[3]
+ gas.no.na$x3*coef(ridge.gas.mod2)[4] + gas.no.na$x4*coef(ridge.gas.mod2)[5] + gas.no.na$x5*coef(ridge.gas.mod2)[6] + gas.no.na$x6*coef(ridge.gas.mod2)[7]
+ gas.no.na$x7*coef(ridge.gas.mod2)[8] + gas.no.na$x8*coef(ridge.gas.mod2)[9] + gas.no.na$x9*coef(ridge.gas.mod2)[10] + gas.no.na$x10*coef(ridge.gas.mod2)[11]
+ gas.no.na$x11*coef(ridge.gas.mod2)[12]

#RSS for the ridge regression
sum((ridge.mod2.pred.y-gas.no.na$y)^2) #3479.988

#R^2
mss.ridge.mod2 <- sum((ridge.mod2.pred.y-mean(gas.no.na$y))^2)
ess.ridge.mod2 <- sum((ridge.mod2.pred.y - gas.no.na$y)^2)

tss.ridge.mod2 <- mss.ridge.mod2+ess.ridge.mod2

r.square.ridge.mod2 <- 1-(4266.353/tss.ridge.mod2)
r.square.ridge.mod2 #0.36814188

#comparing this model to the one found in 9.19, both the RSS and the R^2 decreased. I would conclude that this model does worse

#10.1 part a
football <- read.xls('data-table-B1.xls')

## Begin by defining the models with no variables (null) and all variables (full)
s.null <- lm(y~1, data=football)
s.full <- lm(y~., data=football)

## Forward selection
step(s.null, scope=list(lower=s.null, upper=s.full), direction="forward")

# Call:
#   lm(formula = y ~ x8 + x2 + x7 + x9, data = football)
# 
# Coefficients:
#   (Intercept)           x8           x2           x7           x9  
#   -1.821703    -0.004015     0.003819     0.216894    -0.001635 

lm.forward.football <- lm(formula = y ~ x8 + x2 + x7 + x9, data = football)
summary(lm.forward.football)

# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) -1.8217034  7.7847061  -0.234  0.81705    
#   x8          -0.0040149  0.0013983  -2.871  0.00863 ** 
#   x2           0.0038186  0.0007051   5.416 1.67e-05 ***
#   x7           0.2168941  0.0886759   2.446  0.02252 *  
#   x9          -0.0016349  0.0012460  -1.312  0.20244    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.681 on 23 degrees of freedom
# Multiple R-squared:  0.8012,	Adjusted R-squared:  0.7666 
# F-statistic: 23.17 on 4 and 23 DF,  p-value: 8.735e-08

#10.1 part b backward subsetting
step(s.full, scope=list(lower=s.null, upper=s.full), direction="backward")

# Call:
#   lm(formula = y ~ x2 + x7 + x8 + x9, data = football)
# 
# Coefficients:
#   (Intercept)           x2           x7           x8           x9  
#      -1.821703     0.003819     0.216894    -0.004015    -0.001635  

#The backward subset model is the same as the forward subset model

#10.1 part c stepwise regression model
step(s.null, scope=list(lower=s.null, upper=s.full), direction="both")

# Call:
#   lm(formula = y ~ x8 + x2 + x7 + x9, data = football)
# 
# Coefficients:
#   (Intercept)           x8           x2           x7           x9  
#     -1.821703    -0.004015     0.003819     0.216894    -0.001635  

#the stepwise subset selection method results in the same model as the forward and backward subset selection method

#10.2 

bestmod.football <- regsubsets(y~.-x3-x5-x6, data=football)
summary(bestmod.football)

#           x1  x2  x4  x7  x8  x9 
# 1  ( 1 ) " " " " " " " " "*" " "
# 2  ( 1 ) " " "*" " " " " "*" " "
# 3  ( 1 ) " " "*" " " "*" "*" " "
# 4  ( 1 ) " " "*" " " "*" "*" "*"
# 5  ( 1 ) "*" "*" " " "*" "*" "*"
# 6  ( 1 ) "*" "*" "*" "*" "*" "*"
football.cp <- summary(bestmod.football)$cp
football.r_square <- summary(bestmod.football)$adjr2
football.mse <- summary(bestmod.football)$rss/nrow(football)
ncol(football)

which.min(football.cp) #model 3
which.max(football.r_square) #model 4
which.min(football.mse) #model 6

#checking model 3
#cp for model 3
summary(bestmod.football)$cp[3]
3.688101
#adjusted r_square for model 3
summary(bestmod.football)$adjr[3]
0.7595953
#mse for model 3
summary(bestmod.football)$rss[3]/nrow(football)
2.495357

#checking model 4
#cp for model 4
summary(bestmod.football)$cp[4]
4.038492
#adjusted r_square for model 4
summary(bestmod.football)$adjr[4]
0.7666123
#mse for model 4
summary(bestmod.football)$rss[4]/nrow(football)
2.321584

#Checking model 6
#cp for model 6
summary(bestmod.football)$cp[6]
7
#adjusted r_square for model 6
summary(bestmod.football)$adjr[6]
0.7564299
#mse for model 6
summary(bestmod.football)$rss[6]/nrow(football)
2.212187

#conclude that model 4 is the best which is:
#y ~ x2 + x7 + x8 + x9, due to low cp compared to number of predictors, highest adjusted r_squre and pretty low mse

#10.14 part a
wine <- read.xls("data-table-B11.xls")

#generating indicator variables
for (i in unique(wine$Region)){
  temp <- as.character(i)
  print(i)
  wine[, temp] <- 0
}

for (i in 1:nrow(wine)){
  temp <- as.character(wine$Region[i])
  wine[i, temp] <- 1
}

x.wine = wine[,c(1:5,9:10)]
y.wine = wine[,6]
wine.best.set <- leaps(x=x.wine, y=y.wine, method="Cp")

#finding the model with the lowest cp
min(wine.best.set$Cp) #lowest CP is 2.240659
which.min(wine.best.set$Cp) #28
wine.best.set$which[28,]
#   1     2     3     4     5     6     7 
# FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE 

#This is the best model with the lowest cp
lm.wine.cp <- lm(Quality~Flavor+Oakiness+`3`+`2`, data=wine)

#10.14b
#finding the second model based on best cp
sort(wine.best.set$Cp)[2] #2.473672, this is model number 18

wine.best.set$which[18,]
#   1     2     3     4     5     6     7 
# FALSE FALSE FALSE  TRUE FALSE  TRUE  TRUE 

lm.2.wine.cp <- lm(Quality ~ Flavor+`3`+`2`, data = wine)

#Residual plots
par(mfrow=c(1,2))
lm.wine.cp.1.resid <- rstandard(lm.wine.cp)
lm.wine.cp.2.resid <- rstandard(lm.2.wine.cp)

lm.wine.cp.1.pred <-predict(lm.wine.cp, newdata=wine)
lm.wine.cp.2.pred <-predict(lm.2.wine.cp, newdata=wine)
plot(lm.wine.cp.1.resid  ~ lm.wine.cp.1.pred)
plot(lm.wine.cp.2.resid  ~ lm.wine.cp.2.pred)

#Based on the residual plots, it's hard to tell which model is better

#10.14 part c
#press statistic
#press residual for lm1
lm.wine.cp.1.press.resid <- resid(lm.wine.cp)
pr <- lm.wine.cp.1.press.resid/(1-lm.influence(lm.wine.cp)$hat)
sum(pr^2) ##33.08173

lm.wine.cp.2.press.resid <- resid(lm.2.wine.cp)
pr <- lm.wine.cp.2.press.resid/(1-lm.influence(lm.2.wine.cp)$hat)
sum(pr^2) ##33.99346

#No, there is basically no difference in the PRESS statistics

#10.15
s.null <- lm(Quality~1, data=wine)
s.full <- lm(Quality~., data=wine)

#stepwise
step(s.null, scope=list(lower=s.null, upper=s.full), direction="both")

# Call:
#   lm(formula = Quality ~ Flavor + `2` + `3` + Oakiness, data = wine)
# 
#   Coefficients:
#   (Intercept)       Flavor          `2`          `3`     Oakiness  
#     8.1208       1.1920      -1.5155       1.0935      -0.3183  

#This model is the same model that I found in 10.14 part a

#10.16 part a
wine2<-wine[,1:6]

x.wine2 = wine2[,c(1:5)]
y.wine2 = wine2[,6]

wine2.best.set <- leaps(x=x.wine2, y=y.wine2, method="Cp")
min(wine2.best.set$Cp) #3.92779
which.min(wine2.best.set$Cp) #index 16
wine2.best.set$which[16,]
#   1     2     3     4     5 
# FALSE  TRUE FALSE  TRUE  TRUE 

lm.wine.2 <- lm(Quality~Aroma+Flavor+Oakiness, data=wine2)
summary(lm.wine.2) #Adjusted r-square = 0.6776
summary(lm.wine.cp) #Adjusted r-square = 0.8164

#It seems that region does substantially improve the model, since the adjusted r-square decreased significantly without it
#Also the cp increased by a lot percentage when the region variables were taken out.

#10.16 part b
mod1.lower <- mean(predict(lm.wine.cp, newdata=wine, interval="confidence")[,2])
mod1.upper <- mean(predict(lm.wine.cp, newdata=wine, interval="confidence")[,3])
#confidence interval for the model with region is (11.79928, 13.07441)

mod2.lower <- mean(predict(lm.wine.2, newdata=wine2, interval = "confidence")[,2])
mod2.upper <- mean(predict(lm.wine.2, newdata=wine2, interval = "confidence")[,3])
#confidence interval for the model without region is (11.69338, 13.18031)

#I prefer the first model that includes the region between the range of the confidence interval is smaller. 
