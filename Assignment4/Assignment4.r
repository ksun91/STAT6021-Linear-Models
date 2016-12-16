install.packages('gdata')
library('gdata')
install.packages('car')
library('car')
install.packages('MASS')
library('MASS')

football <- read.xls('data-table-B1.XLS')
#Question 4.2 part a
lm1 <- lm(y~x2+x7+x8, data = football)
qqnorm(lm1$residuals, main = '4.2a qqplot')
qqline(lm1$residuals)
#yes, there seems to be a problem with the normality assumptions because the residuals do not line up with
#the 45 degree line. One can make an argument that the distribution is slightly light-tailed.

#Question 4.2 part b
plot(y=lm1$residuals, x=predict(lm1), main = '4.2b')
#The plot looks fine since there is not a noticeable pattern

#Question 4.2 part c
plot(y=lm1$residuals, x=football$x2, main = '4.2c x2 vs residuals')
#The plot of x2 vs. residuals show a weak inner-opening funnel so heteroscedasticity
# (non-constant variance) may exist, though it may not be strong. This regressor may not be correctly specified

plot(y=lm1$residuals, x = football$x7, main = '4.2c x7 vs residuals')
#The plot of x7 vs. residuals show a strong outward-opening funnel so heteroscedasticity does exist.
#This regressor is not correctly specified. 

plot(y=lm1$residuals, x = football$x8, main = '4.2c x8 vs residuals')
#The plot of x8 vs. residuals look satisfactory. This regressor is correctly specified

#Question 4.2 part d
avPlots(lm1, main = '4.2d partial regression plots')
#source: http://math.furman.edu/~dcs/courses/math47/R/library/car/html/av.plots.html
#These plots provide information about xi's affect given that the other regressors are already in the model
#x2 and x8 look okay. However, x7 seem to have a upward curvature so it may not be linear. This conforms
#with our findings from part 4.2c in which we identified that x7 showed heteroscedasticity. 

#Question 4.2 part e
rstandard(lm1) #computing studentized residuals
# 1            2            3            4            5            6            7            8            9 
# 2.231851618  1.225616368  1.702625305  1.029767789  0.006124483 -0.418876221 -1.206836995  0.299328499  1.338032316 
# 10           11           12           13           14           15           16           17           18 
# -1.441760607 -0.036468456  1.251090093 -0.083851688 -0.160668820 -1.335367350  0.644990078 -0.196937383 -0.365011749 
# 19           20           21           22           23           24           25           26           27 
# -0.078998342 -0.206464327 -1.869940122  0.817274105 -0.551056514 -0.276544687 -1.018586104 -0.094055761 -0.262130195 
# 28 
# -1.048746774 

rstudent(lm1) #computing R-student residuals
# 1            2            3            4            5            6            7            8            9 
# 2.454354223  1.239218310  1.777586702  1.031123075  0.005995537 -0.411563960 -1.218993620  0.293574644  1.361631132 
# 10           11           12           13           14           15           16           17           18 
# -1.476806719 -0.035701602  1.266752172 -0.082098218 -0.157370596 -1.358701256  0.636954384 -0.192946834 -0.358322410 
# 19           20           21           22           23           24           25           26           27 
# -0.077345090 -0.202296957 -1.980521136  0.811437522 -0.542899513 -0.271154408 -1.019417881 -0.092092392 -0.256979177 
# 28 
# -1.051031132 

#The studentized residuals and r-student residuals are rescaled residuals in which one can easily
#use these numbers to detect outliers. Since these residuals should have constant variance of 1 when 
#the form of the model is correct, anything that deviates from this greatly should be an outlier
#such a point is the first point with studentized residual of 2.23 and r-student residual of 2.454.

#Question 4.4 part a
gas <-read.xls('data-table-B3.XLS')
View(gas)
lm.gas <- lm(y~x1+x6, data=gas)
qqnorm(lm.gas$residuals, main = '4.4a qqplot')
qqline(lm.gas$residuals)
#The qqplot shows that for the most part, normality can be assumed. There may be a few outliers, but 
#for the most part, the normality assumption is satisfied

#Question 4.4 part b
plot(y=lm.gas$residuals, x=predict(lm.gas), main = '4.4b')
#The seems to be a nonlinear pattern since one can see a 'U' shape in the plot. 

#Question 4.4 part c
avPlots(lm.gas, main = '4.4c partial regression plots')
#the partial regression plots show that x1 has a linear relationship but x6 has a very weak relationship
#and may not be appropriate to include in the model. Partial regression plots of Xi's show the marginal relationship
#of Xi to Y, assuming the other regressors are in the model

#Question 4.4 part d
rstandard(lm.gas) #computing studentized residuals
abs(rstandard(lm.gas))>2
# 1          2          3          4          5          6          7          8          9         10 
# 0.2717882 -0.3900557 -0.1980105  0.7313729 -0.6404817 -0.7469016 -0.1376161  0.2007271  1.6672464  0.2656477 
# 11         12         13         14         15         16         17         18         19         20 
# -0.5642251  2.2343937 -1.4445983 -0.1539895 -2.4499447 -0.3253417  1.5186561  0.5431086 -0.1113846 -0.5121263 
# 21         22         23         24         25         26         27         28         29         30 
# 0.3438369  2.0277182  0.3917877  0.7990680  0.7068771  0.3504291 -1.3231450  0.6274283 -0.7822915 -1.0015118 
# 31         32 
# -1.3281597 -0.3785506

rstudent(lm.gas) #computing R-student residuals
abs(rstudent(lm.gas))>2
# 1          2          3          4          5          6          7          8          9         10 
# 0.2674019 -0.3842810 -0.1946983  0.7253733 -0.6338410 -0.7410735 -0.1352667  0.1973731  1.7229000  0.2613455 
# 11         12         13         14         15         16         17         18         19         20 
# -0.5574801  2.4130447 -1.4734791 -0.1513731 -2.7032887 -0.3202682  1.5553718  0.5363974 -0.1094707 -0.5055101 
# 21         22         23         24         25         26         27         28         29         30 
# 0.3385474  2.1507428  0.3859964  0.7939593  0.7006450  0.3450656 -1.3412473  0.6207433 -0.7769267 -1.0015660 
# 31         32 
# -1.3466593 -0.3728890 

#From these scaled residuals we can see that 12 and 15 may be outliers.

#Question 4.8 part a
plant <- read.xls('data-prob-2-12.XLS')
lm.plant <- lm(usage ~ temp, data=plant)
qqnorm(lm.plant$residuals, main = '4.8a qqplot')
qqline(lm.plant$residuals)
#The qq plot seem to show that there are no obvious problems with the normality assumption

#4.8 part b
plot(y=lm.plant$residuals, x=predict(lm.plant), main = '4.8b')
#There seem to be an outlier at the point where the residual is about 4. Also, there seem to be a 
#pattern in which for the predicted values (200-400) there were negative residuals, but for the 
#predicted values (450-650), there were positive residuals. 

#4.8 part c
plot(y=lm.plant$residuals, x = 1:nrow(plant), main = "4.8c residual vs time", xlab = "time")
#There seems to be positive autocorrelation since when momentum is positive, the next residual tend to be 
#positive and when momentum is negative, the next residual tend to be more negative.

#4.13 
chem <- read.xls('data-table-B5.XLS')
lm1.chem <- lm(y ~ x6+x7, data = chem)
lm2.chem <- lm(y ~x6, data = chem)

###residual analysis for lm1.chem
#qq plots 
qqnorm(lm1.chem$residuals, main = '4.13 qqplot')
qqline(lm1.chem$residuals)
#qq plot shows that the normality assumption may be violated

#residuals vs predicted
plot(y=lm1.chem$residuals, x=predict(lm1.chem), main = '4.13 residuals vs predicted')
#outward-opening funnel. Heteroskedasticity exists

#press residual for lm1
lm1.res <- resid(lm1.chem)
pr <- lm1.res/(1-lm.influence(lm1.chem)$hat)
sum(pr^2)
#Source: https://stevencarlislewalker.wordpress.com/2013/06/18/calculating-the-press-statistic-in-r/
#PRESS statistic is 3388.604

###residual analysis for lm2.chem 
#qq plots
qqnorm(lm2.chem$residuals, main = '4.13 lm2 qqplot')
qqline(lm2.chem$residuals)
#Although the qqplot is better than lm1.chem, there still may be some concern with normality due to the 
#edge points in the qqplot

#residuals vs predicted
plot(y=lm2.chem$residuals, x=predict(lm2.chem), main = '4.13 lm2 residuals vs predicted')
#There is an outward-opening funnel. Heteroskedasticity exists

#PRESS statistic
lm2.res <- resid(lm2.chem)
pr <- lm2.res/(1-lm.influence(lm2.chem)$hat)
sum(pr^2)
#The PRESS statistic is 3692.881

#Due to the Heteroskedasticity shown in both models, the large PRESS statistic, and the fact the QQ plots
#did not line up relatively well with the 45 degree line, both models do not do well at all. There really
#is not much insight regarding the best choice of the model. The best choice might be a different model
#outside of the choice of these two options. 

#Question 4.25 part a
life <- read.xls('data-table-B-16.xls')

lm.overall <- lm(LifeExp~People.per.TV + People.per.Dr, data = life)
lm.male <- lm(LifeExpMale~People.per.TV + People.per.Dr, data = life)
lm.female <- lm(LifeExpFemale~People.per.TV + People.per.Dr, data = life)

#qq plot for overall life
qqnorm(lm.overall$residuals, main = '4.25a overall qqplot')
qqline(lm.overall$residuals)
#The qq plot shows the normality assumption is violated. It is possible there is a light-tail distribution

#qq plot for male life
qqnorm(lm.male$residuals, main = '4.25a male qqplot')
qqline(lm.male$residuals)
#The qq plot shows the normality assumption is violated. It is possible there is a light-tail distribution

#qq plot for female life 
qqnorm(lm.female$residuals, main = '4.25a female qqplot')
qqline(lm.female$residuals)
#The qq plot shows the normality assumption is violated. It is possible there is a light-tail distribution

#4.25 part b
#residuals vs fitted for overall
plot(y=lm.overall$residuals, x=predict(lm.overall), main = '4.25 overall residuals vs predicted')
#The plot shows that a linear relationship may not be appropriate since there doesn't seem to be a random distribution
#in a band.

#residuals vs fitted for male
plot(y=lm.male$residuals, x=predict(lm.male), main = '4.25 male residuals vs predicted')
#The plot shows that a linear relationship may not be appropriate since there doesn't seem to be a random distribution
#in a band.

#residuals vs fitted for female
plot(y=lm.female$residuals, x=predict(lm.female), main = '4.25 female residuals vs predicted')
#The plot shows that a linear relationship may not be appropriate since there doesn't seem to be a random distribution
#in a band.

#4.29
meth <- read.xls('data-table-B20.xls')
lm.meth <- lm(X.y ~ ., data = meth)

#qq plot 
qqnorm(lm.meth$residuals, main = '4.29 qqplot')
qqline(lm.meth$residuals)
#qq plot shows normality

#residuals vs fitted 
plot(y=lm.meth$residuals, x=predict(lm.meth), main = '4.29 residuals vs predicted')
#the residuals vs predicted shows that the relationship may be non linear because there doesn't seem to be a 
#random distribution of points in a band

#studentized residuals
rstandard(lm.meth) 
 abs(rstandard(lm.meth))>2
# 1          2          3          4          5          6          7          8          9         10 
# 1.0863284 -0.4169188  1.6323275 -1.6276758 -0.3723259 -0.1559185  0.1090424 -1.1970871  0.8419806  1.2339046 
# 11         12         13         14         15         16         17         18 
# -0.3557897 -0.2793779  0.2536121 -0.5859054 -0.5993820  1.9069181  0.1001415 -1.1810223 

#r-student residuals
rstudent(lm.meth) 
abs(rstudent(lm.meth))>2
# 1           2           3           4           5           6           7           8           9 
# 1.09533265 -0.40209215  1.77188149 -1.76539884 -0.35855198 -0.14943206  0.10445189 -1.22136820  0.83105719 
# 10          11          12          13          14          15          16          17          18 
# 1.26429824 -0.34245369 -0.26835812  0.24346847 -0.56916185 -0.58265266  2.18690605  0.09591829 -1.20280522

#There seems to be no issues with studentized and r-student residuals either. With all of this analysis,
#it seems that the normality assumption holds. The residuals vs predicted plot may indict that there might
#be a better model than a linear model that fits the data better

#Problem 5.2 a
vapor <- read.xls('data-prob-5-2.XLS')
plot(vapor, main = "5.2 temp vs. vapor")
#No, a straigt-line model will not be adequate

#5.2 b
lm.vapor <- lm(vapor ~ temp, data = vapor)
summary(lm.vapor)

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1956.258    363.807  -5.377 0.000446 ***
# temp            6.686      1.121   5.964 0.000212 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 117.6 on 9 degrees of freedom
# Multiple R-squared:  0.7981,	Adjusted R-squared:  0.7756 
# F-statistic: 35.57 on 1 and 9 DF,  p-value: 0.0002117

#qq plot
qqnorm(lm.vapor$residuals, main = '5.2 qqplot')
qqline(lm.vapor$residuals)
#The qq plot seems to show that the normality assumption is violated

#residuals vs fitted
plot(y=lm.vapor$residuals, x=predict(lm.vapor), main = '5.2 residuals vs predicted')
#the plot shows that a linear relationship is not appropriate. Perhaps a quadradic equation is better

vapor$lnvapor <- log(vapor$vapor)
vapor$invtemp <- 1/vapor$temp
lm2.vapor <- lm(lnvapor ~ invtemp, data = vapor)
summary(lm2.vapor)

# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)  2.061e+01  6.325e-02   325.8   <2e-16 ***
#   invtemp     -5.201e+03  2.014e+01  -258.3   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.02067 on 9 degrees of freedom
# Multiple R-squared:  0.9999,	Adjusted R-squared:  0.9999 
# F-statistic: 6.672e+04 on 1 and 9 DF,  p-value: < 2.2e-16

#The adjusted r-squared is much higher, the F-statistic is higher and p-value is lower
#seems like so far the model is a better fit 

#linear scatterplot check 
plot(y=vapor$lnvapor, x = vapor$invtemp, main = 'log(vapor) vs 1/temp')
#The plot looks linear, seems like the transformed model is adequate 

#r-student residuals vs fitted
plot(y=rstudent(lm2.vapor), x=predict(lm2.vapor), main = '5.2c r-student residuals vs predicted')
#There seems to be a pattern in this plot so perhaps the transformation is not the most accurate

#overall, this model does better than the original model, although looking at r-student residuals vs. fitted,
#it is possible that this transformation is not the most ideal one. 

#5.5 part a
glass <- read.xls('data-prob-5-5.XLS')
lm.glass <- lm(defects ~ weeks, data = glass)
summary(lm.glass)

# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) -31.6982     9.7758  -3.243  0.00705 ** 
#   weeks         7.2767     0.8692   8.372 2.35e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 13.11 on 12 degrees of freedom
# Multiple R-squared:  0.8538,	Adjusted R-squared:  0.8416 
# F-statistic: 70.09 on 1 and 12 DF,  p-value: 2.354e-06

plot(x=glass$weeks, y = glass$defects)
#seem to have a curved relationship

##qq plots
qqnorm(lm.glass$residuals, main = '5.5a qqplot')
qqline(lm.glass$residuals)
#The qq plot suggest that normality assumption may not be satisfied. May suggest a quadratic relationship 
#may be appropriate

#residuals vs fitted
plot(y=lm.glass$residuals, x=predict(lm.glass), main = '5.5 residuals vs predicted')
#definitely a linear relationship is not appropriate here

#5.5 part b
glass$weeks2 <- glass$weeks^2
mod2.glass <- lm(defects ~ weeks2, data = glass)
summary(mod2.glass)

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.31704    4.40891  -0.072    0.944    
# weeks2       0.35592    0.02884  12.343 3.53e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 9.265 on 12 degrees of freedom
# Multiple R-squared:  0.927,	Adjusted R-squared:  0.9209 
# F-statistic: 152.3 on 1 and 12 DF,  p-value: 3.531e-08

#The Adjusted R-squared is much higher than the linear version, the F-statistic is higher,
#and the p-value is lower. Seems liek the transformation does a better job than the linear model so far.

#linear scatterplot check 
plot(y=glass$defects, x = glass$weeks2, main = '5.5b defects vs weeks^2')
#The plot looks more linear but there is still some curvature 

#r-student residuals vs fitted
plot(y=rstudent(mod2.glass), x=predict(mod2.glass), main = '5.5b r-student residuals vs predicted')
#The plot looks pretty random and centered around 0. Seems like the model is adequate

#From all the analysis above, seems like this transformation was successful and fits the data better

#5.7
#checking multicollinearity
mod1.meth <- lm(X.y ~.,  data = meth)
summary(mod1.meth)
vif(mod1.meth)

#Checking which variables to keep
mod2.meth <- lm(X.y ~. - X.x_3., data = meth)
summary(mod2.meth)
vif(mod2.meth)

mod3.meth <- lm(X.y ~ x_1. + X.x_2., data = meth)
summary(mod3.meth)

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) -595.3723    94.9180  -6.272 1.49e-05 ***
#   x_1.          23.4369     8.4686   2.768   0.0144 *  
#   X.x_2.         1.2477     0.1928   6.470 1.06e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 16.44 on 15 degrees of freedom
# Multiple R-squared:  0.8045,	Adjusted R-squared:  0.7785 
# F-statistic: 30.87 on 2 and 15 DF,  p-value: 4.822e-06

#plotting each regressor to the response variable
plot(x = meth$x_1., y=meth$X.y)
#This is a binary variable

plot(x=meth$X.x_2., y = meth$X.y)
#quadratic seems like a better fit 

#qq plots
qqnorm(mod3.meth$residuals, main = '5.7 linear qqplot')
qqline(mod3.meth$residuals)
#Normality assumption is not appropriate

#r-student residuals vs fitted
plot(y=rstudent(mod3.meth), x=predict(mod3.meth), main = '5.7 r-student residuals vs predicted')
#at the predicted = 20, there seems to be a clump of r-student residuals there. Perhaps a linear model is not the best

#5.7 
meth$x_1. <- as.factor(meth$x_1.)
meth$x.x_2.squared <- meth$X.x_2.^2

mod4.meth <- lm(meth$X.y ~ meth$x_1. + meth$x.x_2.squared)
summary(mod4.meth)

# Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)        -2.931e+02  4.632e+01  -6.329 1.35e-05 ***
#   meth$x_1.1          2.250e+01  8.235e+00   2.732   0.0154 *  
#   meth$x.x_2.squared  1.286e-03  1.905e-04   6.750 6.52e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 15.93 on 15 degrees of freedom
# Multiple R-squared:  0.8165,	Adjusted R-squared:  0.792 
# F-statistic: 33.37 on 2 and 15 DF,  p-value: 3.005e-06

#The Adjusted R-squared increased, F-statistic increased and p-value decreased. Though not by much for 
#any of these metrics

qqnorm(mod4.meth$residuals, main = '5.7 transformed qqplot')
qqline(mod4.meth$residuals)
#Normality assumption is not appropriate

#r-student residuals vs fitted
plot(y=rstudent(mod4.meth), x=predict(mod4.meth), main = '5.7 transformed r-student residuals vs predicted')
#at the predicted = 20, there seems to be a clump of r-student residuals there. Perhaps a linear model is not the best

#second transformation
meth$logy <- log(meth$X.y)

mod5.meth <- lm(meth$logy ~ meth$x_1. + meth$X.x_2.)
summary(mod5.meth)

# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) -31.22481    2.48579 -12.561 2.31e-09 ***
#   meth$x_1.1   -0.11260    0.22178  -0.508    0.619    
#   meth$X.x_2.   0.06838    0.00505  13.539 8.17e-10 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4305 on 15 degrees of freedom
# Multiple R-squared:  0.9273,	Adjusted R-squared:  0.9176 
# F-statistic: 95.68 on 2 and 15 DF,  p-value: 2.891e-09

#x_1.1 is not significant anymore so taking that out

mod6.meth <- lm(meth$logy ~ meth$X.x_2.)
summary(mod6.meth)

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) -30.956378   2.371909  -13.05 6.04e-10 ***
#   meth$X.x_2.   0.067758   0.004786   14.16 1.81e-10 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4204 on 16 degrees of freedom
# Multiple R-squared:  0.9261,	Adjusted R-squared:  0.9214 
# F-statistic: 200.4 on 1 and 16 DF,  p-value: 1.815e-10

#Adjusted R-squared is much higher, F-statistic is higher, p-value is much lower. This model seems to be the best

qqnorm(mod6.meth$residuals, main = '5.7 log_transform qqplot')
qqline(mod6.meth$residuals)
#Although the normality assumption is still not that strong, it's the strongest among all the other models tested

#r-student residuals vs fitted
plot(y=rstudent(mod6.meth), x=predict(mod6.meth), main = '5.7 log_transformed r-student residuals vs predicted')
#There still seems to be a visible outlier and perhaps this still isn't the best model. But it's the best model so far
#given the data


#Question 5.9 
clath <- read.xls('data-table-B8.XLS')
mod1.clath <- lm(clath$y ~ clath$x1 + clath$x2)
summary(mod1.clath)

# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) 1.109e+01  1.669e+00   6.642 1.48e-07 ***
#   clath$x1    3.501e+02  3.968e+01   8.823 3.38e-10 ***
#   clath$x2    1.089e-01  9.983e-03  10.912 1.74e-12 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4.782 on 33 degrees of freedom
# Multiple R-squared:  0.8415,	Adjusted R-squared:  0.8319 
# F-statistic:  87.6 on 2 and 33 DF,  p-value: 6.316e-14

qqnorm(mod1.clath$residuals, main = '5.9 qqplot')
qqline(mod1.clath$residuals)
#The normality assumption does not hold. There seems to exist a light-tailed distribution 

#r-student residuals vs fitted
plot(y=rstudent(mod1.clath), x=predict(mod1.clath), main = '5.9 r-student residuals vs predicted')
#The residuals look okay 

#5.9 part b
clath$logy <- log(clath$y)
mod2.clath <- lm(clath$logy ~ clath$x1 + clath$x2)
summary(mod2.clath)
#Statistical metrics got worse

clath$ysquare <- clath$y^2
mod3.clath <- lm(clath$ysquare ~ clath$x1 + clath$x2)
summary(mod3.clath)
#Statistical metrics still got worse

plot(x = clath$x1, y = clath$y)
plot(x = clath$x2, y = clath$y)
#Based on these plots, it seems like we can try transforming x2 to logx2

clath$logx2 <- log(clath$x2)
mod4.clath <- lm(clath$y ~ clath$x1 + clath$logx2)
summary(mod4.clath)

# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) -17.5537     3.4365  -5.108 1.34e-05 ***
#   clath$x1    308.4722    34.4038   8.966 2.31e-10 ***
#   clath$logx2   9.5313     0.7393  12.892 1.96e-14 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4.178 on 33 degrees of freedom
#  Multiple R-squared:  0.879,	Adjusted R-squared:  0.8717 
#  F-statistic: 119.9 on 2 and 33 DF,  p-value: 7.334e-16

#The adjusted r-squared increased, F-statistic increased, and p-value decreased
#this seems to be an adequate model.

qqnorm(mod4.clath$residuals, main = '5.9b qqplot')
qqline(mod4.clath$residuals)
#The qq plot looks much better. The normality assumption is stronger 

#r-student residuals vs fitted
plot(y=rstudent(mod4.clath), x=predict(mod4.clath), main = '5.9b r-student residuals vs predicted')
#The residual vs fitted plot does look a bit off so perhaps this model is still not ideal. However,
#it does better than the linear model so we'll keep this transformation for now.

#Question 5.10
pressure <- read.xls('data-table-B10.XLS')
mod1.pressure <- lm(y~., data = pressure)
summary(mod1.pressure)

# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)  0.679439   0.143532   4.734 3.20e-05 ***
#   x1           1.407331   0.196925   7.147 1.81e-08 ***
#   x2          -0.015629   0.001428 -10.948 3.67e-13 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2593 on 37 degrees of freedom
# Multiple R-squared:  0.822,	Adjusted R-squared:  0.8124 
# F-statistic: 85.46 on 2 and 37 DF,  p-value: 1.351e-14

qqnorm(mod1.pressure$residuals, main = '5.10 qqplot')
qqline(mod1.pressure$residuals)
#The normality assumption is not valid at all. 

#r-student residuals vs fitted
plot(y=rstudent(mod1.pressure), x=predict(mod1.pressure), main = '5.10 r-student residuals vs predicted')
#the r-student residuals vs predicted plot strongly suggest that the relationship is non-linear

#5.10 part b
plot(x=pressure$x1, y = pressure$y)
plot(x = pressure$x2, y = pressure$y)
#We can see that x2 is definitely not linear, but it has negative values too

pressure$logy <- log(pressure$y)
mod2.pressure <- lm(pressure$logy ~ pressure$x1 + pressure$x2)
summary(mod2.pressure)

# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) -0.4008015  0.0429805  -9.325 2.97e-11 ***
#   pressure$x1  1.2573291  0.0589691  21.322  < 2e-16 ***
#   pressure$x2 -0.0142882  0.0004275 -33.423  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.07766 on 37 degrees of freedom
# Multiple R-squared:  0.977,	Adjusted R-squared:  0.9758 
# F-statistic: 785.9 on 2 and 37 DF,  p-value: < 2.2e-16

#The adjusted r-squared and F-statistic are a lot higher. The p-value is smaller

qqnorm(mod2.pressure$residuals, main = '5.10 transformed qqplot')
qqline(mod2.pressure$residuals)
#The normality assumption is still not valid at all. 

#r-student residuals vs fitted
plot(y=rstudent(mod2.pressure), x=predict(mod2.pressure), main = '5.10 transformed r-student residuals vs predicted')
#the transformed r-student residuals vs predicted is still not good.

#This concludes that there is still problems with model form. However, based on the summary statistics
#we can conclude that this transformation is better than the linear one. 
