install.packages("gdata")
library ("gdata")

#6.12
wine <- read.xls("data-table-B11.xls")

lm.wine <- lm(Quality ~ Clarity+Aroma+Body+Flavor+Oakiness, data = wine)

## A summary of potential leverage and/or influential points
summary(influence.measures(lm.wine))

#     dfb.1_ dfb.Clrt dfb.Arom dfb.Body dfb.Flvr dfb.Okns dffit   cov.r   cook.d hat  
# 12  0.83  -0.97    -0.40     0.23    -0.01    -0.04     1.38_*  1.16    0.30   0.39
# 14 -0.03   0.11    -0.16    -0.05     0.05     0.08    -0.28    1.84_*  0.01   0.36
# 20 -0.04  -0.38     0.46    -1.06_*   0.65     0.60    -1.54_*  0.30_*  0.31   0.20
# 32 -0.04   0.04    -0.03     0.02     0.04    -0.03    -0.07    1.57_*  0.00   0.23
# 37 -0.06  -0.15     0.04     0.28    -0.37     0.39     0.61    1.72_*  0.06   0.38

#None of Cook's distance is greater than 1, thus none of the observations are influential

#6.13
gears <- read.xls("data-table-B12.xls")
lm.gears <- lm(pitch~., data = gears)

## A summary of potential leverage and/or influential points
summary(influence.measures(lm.gears))

#     dfb.1_  dfb.temp dfb.sktm dfb.skpc dfb.dfft dfb.dffp dffit   cov.r   cook.d  hat    
# 5   0.18   -0.23     0.18     0.03    -0.06     0.02     0.37    1.85_*  0.02    0.35  
# 28 -1.16_*  1.24_*  -0.38     0.50    -0.02     0.04     1.51_*  0.90    0.34    0.38  
# 29  1.33_* -1.31_*  -0.11    -0.44     0.25    -1.45_*   2.06_*  0.08_*  0.45    0.21  
# 31  0.02    0.04     0.07    -0.17     0.03     0.04     0.46    1.75_*  0.04    0.34  
# 32  0.00    0.11    -1.78_*  -0.32     0.65     0.03    -2.40_*  3.14_*  0.93_*  0.74_*

#Observation 32 is influential because cook's distance is statistically significant

#6.14
jet <- read.xls("data-table-B13.xls")
lm.jet <- lm(y~., data = jet)

## A summary of potential leverage and/or influential points
summary(influence.measures(lm.jet))

#     dfb.1_  dfb.x1  dfb.x2  dfb.x3  dfb.x4  dfb.x5  dfb.x6  dffit   cov.r   cook.d  hat    
# 6   0.02   -0.07    0.08   -0.04    0.07    0.01    0.09   -0.20    1.74_*  0.01    0.30  
# 9   0.01   -0.07    0.00   -0.01    0.07    0.06   -0.05    0.19    1.65_*  0.01    0.27  
# 10 -0.06   -0.01    0.05    0.05   -0.01   -0.11    0.10   -0.32    1.72_*  0.02    0.31  
# 11  0.86    1.45_*  0.21   -0.75   -1.15_*  0.08    0.48    1.65_*  1.39    0.37    0.50  
# 20 -1.51_* -0.88   -4.41_*  1.76_*  1.63_*  1.41_* -1.18_* -5.13_*  0.82    3.01_*  0.74_*

#Because the cook's distance is way greater than 1 for point 20, it's clear that point 20 is influential
#In addition DFBETAs and DFFITs show that it is influential too

#6.15
elect <- read.xls("data-table-B14.xls")
lm.elect <- lm(y ~. - x5, data =elect)

## A summary of potential leverage and/or influential points
summary(influence.measures(lm.elect))

#     dfb.1_  dfb.x1 dfb.x2  dfb.x3  dfb.x4  dffit   cov.r   cook.d  hat  
# 2   1.67_*  0.21  -3.77_*  0.05   -1.07_* -4.67_*  0.04_*  1.98_*  0.47
# 4  -0.59    0.20  -0.57   -0.94    2.34_*  2.50_*  0.91    1.04_*  0.56
# 8   0.41   -0.60   1.46_* -0.54   -0.29    1.57_*  0.62    0.41    0.34
# 9  -0.13   -0.09  -0.11    1.05_* -0.51    1.15    1.38    0.26    0.41
# 10  0.19   -0.28   0.07   -0.19    0.02   -0.33    1.98_*  0.02    0.38

#Points 2 and 4 are influential since the cook's distance is statistically significant and above 1

#Problem 8.5
auto <- read.xls("data-table-B3.xls")

#part a
lm.auto <- lm(y ~ x10+x11, data=auto)
summary(lm.auto)

#   Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 39.1919052  2.5570509  15.327 1.92e-15 ***
# x10         -0.0047484  0.0009544  -4.975 2.72e-05 ***
# x11         -2.6958431  1.9805597  -1.361    0.184    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#No, the type of transmission does not significantally affect the mileage performance at alpha = 10%

#part b
auto$weightTrans <- auto$x10*auto$x11
auto.lm2 <- lm(y ~x10+x11+weightTrans, data = auto)
summary(auto.lm2)

# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)  58.108420   5.077985  11.443 4.53e-12 ***
#   x10          -0.012517   0.002055  -6.090 1.44e-06 ***
#   x11         -26.724910   6.107349  -4.376 0.000152 ***
#   weightTrans   0.009035   0.002217   4.076 0.000342 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.58 on 28 degrees of freedom
# Multiple R-squared:  0.8494,	Adjusted R-squared:  0.8332 
# F-statistic: 52.63 on 3 and 28 DF,  p-value: 1.244e-11

#shows that there is significant interactions between weight and transmission.
#y = 58.1 - 0.0125x10 - 26.73x11 + 0.009x10*x11
# if the engine is automatic then x11 = 1. So y = 31.37 - 0.0035x10. Which means that on average for each one pound increase
# the miles/gallon decreases by 0.0035
# if the engine is manual then x11 = 0. So y = 58.1 - 0.0125x10. Which means for each one pound increase on average
#the miles/gallon decreases by 0.0125

#Question 8.6
football <- read.xls('data-table-B1.xls')
#tranforming x5 into indicator variables 0 -1 and 1.

x5n <- as.numeric(football$x5 <0)
x5p <- as.numeric(football$x5 >0)
football2 <- cbind(football, x5n, x5p)
View(football2)

football.lm <- lm(y ~ x8+x7+x5n + x5p, data = football2)
summary(football.lm)

# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)   
# (Intercept) 19.353055   9.525127   2.032  0.05388 . 
# x8          -0.006337   0.001719  -3.685  0.00122 **
# x7          -0.006825   0.118841  -0.057  0.95470   
# x5n          0.460504   2.466185   0.187  0.85351   
# x5p          2.333031   2.483813   0.939  0.35734   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.337 on 23 degrees of freedom
# Multiple R-squared:  0.6158,	Adjusted R-squared:  0.549 
# F-statistic: 9.216 on 4 and 23 DF,  p-value: 0.0001349

#Turnovers is not significant in association to the number of games won. 
