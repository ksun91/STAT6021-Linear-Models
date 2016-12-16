###########################
#                         #
#   Team Assignment 4     #
#                         #
###########################

## Please submit one set of answers per team.                    ##
## Your answers may be submitted as an annotated R file.         ##
## Please submit your plots in one PDF as a separate attachment. ##
###################################################################


#################
## Question 1: ##
#################

# For each part of this problem, you are to create (or find) a data set with about 30 observations 
# that is suitable for simple linear regression and satisfies the given specifications. If it is 
# not possible, explain why it is not possible. For each part, include a plot that shows the 
# interesting points.
#

## Use x values from Team Assignment 1
x <- read.table("teamassign01data.txt")[,1]

#   (a) The data set has a point that is clearly visible for all four types of residuals 
#       discussed -- standardized, studentized, PRESS, R-student. - Kevin

set.seed(7)
x.1a <- x[1:30]

## Generate corresponding y-values 
y.1a <- 25 + 6*x.1a + rnorm(30, mean=0, sd=20)

# Plotting the points 
plot(y.1a~x.1a)

## Add a point to the set that is not on the regression line
x.1a.add <- 64
x.1a <- c(x.1a, x.1a.add)
y.1a.add <- 150
y.1a <- c(y.1a, y.1a.add)

## Plot the new points and the regression line
lm.1a <- lm(y.1a~x.1a)
plot(y.1a~x.1a)
abline(lm.1a)

# Standardized residuals
standardized.res.1a <- resid(lm.1a)/sqrt(anova(lm.1a)$`Mean Sq`[2])

plot(standardized.res.1a, pch=16, cex=1, 
     main="Question 1a: Standardized Residuals", ylab="Standardized Residuals")

#Point 31 stands out 

# Studentized residuals
studentized.res.1a <- rstandard(lm.1a)
plot(studentized.res.1a, pch=16, cex=1, 
     main="Question 1a: Studentized Residuals", ylab="Studentized Residuals")

#Point 31 stands out 

# PRESS residuals
press.res.1a <- resid(lm.1a) / (1 - lm.influence(lm.1a)$hat)
plot(press.res.1a, pch=16, cex=1, 
     main="Question 1a: PRESS Residuals", ylab="PRESS Residuals")

#Point 31 stands out 

#R-student residuals 
r.student.1a <- rstudent(lm.1a)
plot(r.student.1a, pch=16, cex=1, 
     main="Question 1a: r-student Residuals", ylab="r-student Residuals")

#Point 31 stands out 

#   (b) The data set has a point that stands out when viewing studentized residuals but not 
#       when viewing standardized residuals. - Kevin
set.seed(7)
x.1b <- x[1:30]

## Generate corresponding y-values 
y.1b <- 25 + 6*x.1b + rnorm(30, mean=0, sd=2)

# Plotting the points 
plot(y.1b~x.1b)

x.1b.add <- 100
x.1b <- c(x.1b, x.1b.add)
y.1b.add <- 635
y.1b <- c(y.1b, y.1b.add)

## Plot the new points and the regression line
lm.1b <- lm(y.1b~x.1b)
plot(y.1b~x.1b)
abline(lm.1b)

#Studentized residuals 
studentized.res.1b <- rstandard(lm.1b)
plot(studentized.res.1b, pch=16, cex=1, 
     main="Question 1b: Studentized Residuals", ylab="Studentized Residuals")

#Standardized residuals
standardized.res.1b <- resid(lm.1b)/sqrt(anova(lm.1b)$`Mean Sq`[2])

plot(standardized.res.1b, pch=16, cex=1, 
     main="Question 1b: Standardized Residuals", ylab="Standardized Residuals")

#Point 31 stands out for studentized, but not for standardized. 

#   (c) The data set has a point that stands out when viewing PRESS residuals but not when 
#       viewing standardized residuals. - Kaley
set.seed(7)
x.1c <- x[1:30]
y.1c <- 25 + 6*x.1c + rnorm(30, mean=0, sd=40) # Generate corresponding y-values 
plot(x.1c,y.1c)
# Change point for residual exercise
x.1c[12] <- 100
y.1c[12]<- 450
lm.1c <- lm(y.1c~x.1c)
plot(x.1c,y.1c)
abline(lm.1c)

# Standardized residuals
standardized.1c <- resid(lm.1c)/sqrt(anova(lm.1c)$`Mean Sq`[2])
plot(standardized.1c, pch=16, cex=1, main="Question 1c: Standardized Residuals", ylab="Standardized Residuals")
# The 12th point does not stand out when looking at standardized residuals, it is within abs(2). In fact,
# there are a number of points tha have greater standardized residuals (in absolute value).
# PRESS residuals
press.1c <- resid(lm.1c) / (1 - lm.influence(lm.1c)$hat)
plot(press.1c, pch=16, cex=1, main="Question 1c: PRESS Residuals", ylab="PRESS Residuals")
# The 12th point very much stands out when looking at press residuals. It is greater than abs(150) from the mean.

#   (d) The data set has a point that stands out when viewing R-student residuals but not when 
#       viewing standardized residuals. - Kaley
set.seed(7)
x.1d <- x[1:30]
y.1d <- 25 + 6*x.1d + rnorm(30, mean=0, sd=10) # Generate corresponding y-values
plot(x.1d, y.1d)
# Change point for residual exercise
x.1d[12] <- 150
y.1d[12]<- 830
lm.1d <- lm(y.1d~x.1d)
plot(x.1d, y.1d)
abline(lm.1d)

# R-student residuals
rstudent.1d <- rstudent(lm.1d)
plot(rstudent.1d, pch=16, cex=1, main="Question 1d: R-Student Residuals", ylab="R-Student Residuals")
# The 12th point stands out when looking at R-student residuals, it is greater than abs(4).
# Standardized residuals
standardized.1d <- resid(lm.1d)/sqrt(anova(lm.1d)$`Mean Sq`[2])
plot(standardized.1d, pch=16, cex=1, main="Question 1d: Standardized Residuals", ylab="Standardized Residuals")
# The 12th point does NOT stand out when looking at the standardized residuals, it is within abs(2).


#   (e) The data set has a point that stands out when viewing PRESS residuals but not when 
#       viewing studentized residuals. - Chris

set.seed(7)

## Use x values from Team Assignment 1
x.1e <- read.table("teamassign01data.txt")[,1]
x.1e <- x.1e[1:30]

## Generate corresponding y-values 
y.1e <- 25 + 6*x.1e + rnorm(30, mean=10, sd=12)

## Add a point to the set that is not on the regression line
x.1e.add <- 100.63
x.1e <- c(x.1e, x.1e.add)
y.1e.add <- 553.75
y.1e <- c(y.1e, y.1e.add)

# Create the simple linear model
lm.1e <- lm(y.1e~x.1e)

## Plot the points and the regression line
plot(y.1e~x.1e)
abline(lm.1e)

# Find points that have an absolute value of the studentized residual greater than 3
plot(rstandard(lm.1e), pch=16, cex=3, 
     main="Question 1e: Studentized Residuals", ylab="Studentized Residuals")
# Point 31 stands out in this model.

# Find the PRESS residuals
press.1e <- resid(lm.1e) / (1 - lm.influence(lm.1e)$hat)

plot(press.1e, pch=16, cex=3,  
     main="Question 1e: PRESS Residuals", ylab="PRESS Residuals")
# Point 31 stands out in this model.

# ANSWER 1e:
# It is not possible for a point to stand out when viewing PRESS residuals but not when 
# viewing studentized residuals, because the standardized PRESS residual is the same as 
# studentized residuals.

#   (f) The data set has a point that stands out when viewing R-student residuals but not when 
#       viewing PRESS residuals. - Qi
set.seed(7)
x.1f <- x[1:30]
## Generate corresponding y-values 
y.1f <- 25 + 6*x.1f + rnorm(30, mean=10, sd=12)
plot(y.1f~x.1f)
## Add a point to the set that is not on the regression line
x.1f.add <- 60
x.1f <- c(x.1f, x.1f.add)
y.1f.add <- 450
y.1f <- c(y.1f, y.1f.add)

# Create the simple linear model
lm.1f <- lm(y.1f ~x.1f)

## Plot the points and the regression line
plot(y.1f~x.1f)
abline(lm.1f)

# Find points that have an absolute value of the r-student residual greater than 3
plot(rstudent(lm.1f), main="Question 1f: R-student Residuals", ylab="Studentized Residuals")
# Point 31 stands out in this model.

# Find the PRESS residuals
press.1f <- resid(lm.1f) / (1 - lm.influence(lm.1f)$hat)

plot(press.1f, main="Question 1f: PRESS Residuals", ylab="PRESS Residuals")
# Point 31 stands out in this model.

# ANSWER 1f:
# It is not possible for a point to stand out in r-student residual but not in PRESS residual.
# R-student residual is proportional to PRESS residual, therefore if a r-student residual stands
# out, the PRESS residual must stand out. 

#################
## Question 2: ##
#################

# For each part of this problem, you are to create (or find) a data set with about 30 observations 
# that is suitable for simple linear regression and satisfies the given specifications on the variance
# of the residuals. For each part, include a plot of your residuals that shows the required characteristic.
#
#   (a) The residuals have constant variance. - Kevin

## Use x values from Team Assignment 1 (used above)
set.seed(53)
x.2a <- x[1:30]

#Creating y-values with constant variance
y.2a <- 25 + 125.2*x.2a + rnorm(30, mean=0, sd=50)

plot(y.2a~x.2a)

# Create a linear model
lm.2a <- lm(y.2a~x.2a)

# Plot the residuals
plot(rstudent(lm.2a), pch=16, cex=3, main="Question 2a: R-student Residuals for Model with a 
     constant variance", ylab="R-student Residuals")

#   (b) The residuals have variance proportional to E(y). - Kaley
set.seed(3)
x.2b <- seq(1,30,1)
y.2b <- 5 + 10*x.2b + rnorm(30, mean=0, sd=.2*(5 + 10*x.2b)) # SD of y-values proportional to E(y)
lm.2b <- lm(y.2b~x.2b)
plot(lm.2b$fit, lm.2b$residuals, pch=16, cex=1, main="Question 2b: Residual Variance Proportional to E(y)", ylab="Residuals")
# The plot of residuals show their variance proportional to E(y)

#   (c) The residuals have variance proportional to E(y)^2. - Chris

set.seed(19) #202

## Use x values from Team Assignment 1 (used above)
x.2c <- x.1e[1:30]

# Create y values with variance of E(y)^2 (by making SD = E(y))
y.2c <- 25 + 125.2*x.2c + rnorm(30, mean=0, sd=96*(25 + 125.2*x.2c))

plot(y.2c~x.2c)

# Create a linear model
lm.2c <- lm(y.2c~x.2c)

# Plot the residuals
plot(rstudent(lm.2c), pch=16, cex=2, main="Question 2c: R-student Residuals for Model with Variance
     Proportional to E(y)^2", ylab="R-student Residuals")

#   (d) The residuals have variance proportional to 1/E(y). - Chris

set.seed(202)

## Use x values from Team Assignment 1 (used above)
x.2d <- x.2c

# Create y values with variance of 1/E(y) (by making SD = square root of 1/E(y))
y.2d <- 364.189 + 84.76*x.2d + rnorm(30, mean=0, sd=103*sqrt(1/(364.189 + 84.76*x.2d)))

plot(y.2d~x.2d)

# Create a linear model
lm.2d <- lm(y.2d~x.2d)

# Plot the residuals
plot(rstudent(lm.2d), pch=16, cex=2, main="Question 2d: R-student Residuals for Model with Variance
     Proportional to 1/E(y)", ylab="R-student Residuals")

#   (e) The residuals have variance proportional to C-E(y) for some constant C. - Qi
set.seed(202)

## Use x values from Team Assignment 1 (used above)
x.2e <- x.2a

# Create y values with variance of C-E(y) (by making SD = square root of C-E(y))
E.2e <- 2 + 2*x.2e
y.2e <- 2 + 2*x.2e + rnorm(30, mean=0, sd=0.2*sqrt(130-E.2e))

plot(y.2e~x.2e)

# Create a linear model
lm.2e <- lm(y.2e~x.2e)

# Plot the residuals
plot(rstudent(lm.2e), main="Question 2e: R-student Residuals for Model with Variance
     Proportional to C-E(y)", ylab="R-student Residuals")

#   (f) The residuals have variance proportional to E(y)(C-E(y)) for some constant C. - Qi
set.seed(40)

## Use x values from Team Assignment 1 (used above)
x.2f <- x.2a

# Create y values with variance of E(y)(C-E(y)) (by making SD = square root of E(y)(C-E(y)))
E <- - 1 + 0.1*x.2f
y.2f <- - 1 + 0.1*x.2f + rnorm(30, mean=0, sd=1/5*sqrt(E*(6-E)))

#plot(y.2f~x.2f)

# Create a linear model
lm.2f <- lm(y.2f~x.2f)

# Plot the residuals
plot(rstudent(lm.2f), main="Question 2f: R-student Residuals for Model with Variance
     Proportional to E(y)(C-E(y))", ylab="R-student Residuals")
