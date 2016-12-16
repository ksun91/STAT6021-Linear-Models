### problem 2.1a
football <- data.frame(table.b1)
football.lm1 <- lm(y~x8, data=football)
summary(football.lm1)
# as shown below, the output model is y = 21.7882 - 0.007*x8
# Coefficients:
# Estimate   
# (Intercept) 21.788251
#   x8        -0.007025

### problem 2.1b
anova(football.lm1)
# looking at the p-value shown in the ANOVA table (7.381e-06) we can compare this to our alpha level of 0.05,
# which leads us to believe that the x8 variable is significant in predicting the number of games won by a team

### problem 2.1c
confint(football.lm1, level=0.95)
# the 95% confidence interval for the slope is (-0.0096, -0.0044)


### problem 2.1d
summary(football.lm1)$r.squared
# the R^2 of this model is 0.5447, which allows us to say that 54.47% of the variability in y is explained by x8

### problem 2.1e
football2 <- data.frame(x8 = 2000)
predict(football.lm1, football2, interval="confidence")
# the 95% confidence interval is (6.7657, 8.7103)




### problem 2.2
football3 <- data.frame(x8 = 1800)
predict(football.lm1, football3, interval="prediction", level=0.90)
# the point estimate is 9.1431 games if they can hold their oponents to 1800 yards
# the 90% prediction interval is (4.9364, 13.3497)



### problem 2.4a
cars <- data.frame(table.b3)
cars.lm1 <- lm(y~x1, data=cars)
summary(cars.lm1)
# as shown below, the output model is y = 33.7227 - 0.0474*x1
# Coefficients:
# Estimate
# (Intercept) 33.722677
#    x1       -0.047360

### problem 2.4b
anova(cars.lm1)
# looking at the p-value shown in the ANOVA table (3.743e-11) we can compare this to our alpha level of 0.05,
# which leads us to believe that the x1 variable is significant in predicting the mpg of a car

### problem 2.4c
summary(cars.lm1)$r.squared
# the R^2 of this model is 0.7723, which allows us to say that 72.23% of the variability in y is explained by x1

### problem 2.4d
cars2 <- data.frame(x1 = 275)
predict(cars.lm1, cars2, interval="confidence")
# the 95% confidence interval of a car with a 275in engine displacement is (19.5881, 21.8095)

### problem 2.4e
predict(cars.lm1, cars2, interval="prediction", level=0.95)
# the point estimate for mileage (of cars with engine displacements of 275in) is 20.6988
# the 90% prediction interval for cars with engine displacements of 275in is (14.3415, 27.0561)

### problem 2.4f
# the 95% prediction interval produces a wider interval than the 95% confidence interval due to two primary reasons
# reason number 1 is that the standard error estimate for the prediction interval includes a '1+' in the calculations
# reason number 2 is that the prediction interval is attemtping to predict a potential range of future values, while
# the confidence interval is attempting to predict the mean value for y. another way to put this is that the prediction
# interval is attempting to predict the RANGE of values for y, while the confidence interval is attempting to predict
# the range of values for E(y|x)



### problem 2.5
cars.lm2 <- lm(y~x10, data=cars)
summary(cars.lm2)
# as shown below, the output model is y = 40.8524 - 0.0057*x10
# Coefficients:
# Estimate
# (Intercept) 40.8524315
#   x10       -0.0057516

anova(cars.lm2)
# looking at the p-value shown in the ANOVA table (2.121e-10) we can compare this to our alpha level of 0.05,
# which leads us to believe that the x10 variable is significant in predicting the mpg of a car

summary(cars.lm2)$r.squared
# the R^2 of this model is 0.7446, which allows us to say that 74.46% of the variability in y is explained by x10

# if we compare the R^2 of the two models we can note that the R^2 for the model involving x1 is slightly higher
# than the R^2 for the model involving x10...this is also supported by the fact that the p-value for the model
# involving x1 is also smaller



### problem 2.12a
plant <- data.frame(p2.12)
plant.lm1 <- lm(usage~temp, data=plant)
summary(plant.lm1)
# as shown below, the output model is usage = -6.3321 + 9.2085*temp
# Coefficients:
# Estimate 
# (Intercept) -6.33209
#   temp       9.20847

### problem 2.12b
anova(plant.lm1)
# looking at the p-value shown in the ANOVA table (2.2e-16) we can compare this to our alpha level of 0.05,
# which leads us to believe that temperature is significant in predicting the steam usage at a plant

### problem 2.12c
confint(plant.lm1, level=0.95)
# a 95% confidence interval for the slope produces an interval of (9.1331, 9.2838)
# this interval does not include the value 10, leading us to believe that the data does not support the statement
# made by management about the relationship between temperature and usage

### problem 2.12d
plant2 <- data.frame(temp=58)
predict(plant.lm1, plant2, interval="prediction", level=0.99)
# the 99% prediction interval on steam usage with an average temperature of 58 degrees is (521.2237, 534.2944)
