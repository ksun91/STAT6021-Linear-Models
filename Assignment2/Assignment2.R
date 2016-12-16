##########
## 2.20 ##
##########
#install.packages('gdata')
library(gdata)
p1 <- read.xls("data-table-B18.XLS")
summary(lm(p1$y. ~ p1$X.x_5.))

# y = 410.7232 - 0.2638*x, F = 7.514 with p = 0.0159, R-square = 34.93%       #
# There is a relationship between initial boiling point and fuel consumption. #
# However, there is still a lot of unexplained variation in this model.       #


##########
## 2.21 ##
##########
p2 <- read.xls("data-table-B19.XLS")
summary(lm(p2$y ~ p2$X.x_3.))

# y = 16.56403 - 0.01276*x, F = 4.936 with p = 0.034, R-squared = 14.13%      #
# There is a negative relationship between sulfur content and taste.          #
# However, there is still a lot of unexplained variation in this model.       #


##########
## 2.22 ##
##########
p3 <- read.xls("data-table-B20.XLS")
summary(lm(p3$X.y ~ p3$X.x_5.))

# y = 21.25 + 7.80*x, F = 0.2161 with p = 0.648, R-squared = 1.333%           #
# There is no relationship between ratio of inlet oxygen to the inlet methanol#
# and the conversion process.                                                 #


##########
## 2.30 ##
##########
p4 <- read.xls("data-prob-2-12.XLS")

# a. 
reg.p4 <- lm(usage ~ temp, data = p4) # positive correlation 
r <- sqrt(summary(reg.p4)$r.squared) # correlation = square-root of r-squared 
r
# correlation between usage and temperature is 0.9999326

# b. 
rho <- 0
(atanh(r)-atanh(rho))*sqrt(nrow(p4)-3) # z-value
# Null hypothesis is rejected. 

# c. 
rho <- 0.5
(atanh(r)-atanh(rho))*sqrt(nrow(p4)-3) # z-value
# Null hypothesis is rejected. 

#d. 
z <- qnorm(0.995)
c(tanh(atanh(r)-z/sqrt(nrow(p4)-3)), tanh(atanh(r)+z/sqrt(nrow(p4)-3)))
# 99% CI is (0.9996244, 0.9999879)
