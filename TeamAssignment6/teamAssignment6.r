 For this team assignment you will use the file "teamassign06train.csv" to develop
# a linear model using whatever methods you consider appropriate. You will then use
# the model that you have developed to predict the values of the response variable
# corresponding to the explanatory variable values given in the file
# "teamassign06test.csv". 
#
# These data are from Portugal and record students' grade along with 30 explanatory 
# variables describing the student and family. The list of variables is included
# at the end of this assignment.
#
# Once you have predicted the values of the response variable for the testing set,
# you should save them to a vector called predvect and write them into a .csv file 
# using the following code:

#
# Your annotated R code should explain the reasoning behind your choices in 
# model selection and should be neatly organized.
#
# Your grade on this team assignment will be based on how well your model predicts
# the observed values relative to the other teams.
#
#
# List of variables:
# 1 school - student's school (binary: 'GP' or 'MS')
# 2 sex - student's sex (binary: 'F' - female or 'M' - male)
# 3 age - student's age (numeric: from 15 to 22)
# 4 address - student's home address type (binary: 'U' - urban or 'R' - rural)
# 5 famsize - family size (binary: 'LE3' - less or equal to 3 or 'GT3' - greater than 3)
# 6 Pstatus - parent's cohabitation status (binary: 'T' - living together or 'A' - apart)
# 7 Medu - mother's education (numeric: 0 - none, 1 - primary education (4th grade), 
#                               2 - 5th to 9th grade, 3 - secondary education,
#                               or 4 - higher education)
# 8 Fedu - father's education (numeric: 0 - none, 1 - primary education (4th grade),
#                               2 - 5th to 9th grade, 3 - secondary education, 
#                               or 4 - higher education)
# 9 Mjob - mother's job (nominal: 'teacher', 'health' care related, civil 'services'
#                        (e.g. administrative or police), 'at_home' or 'other')
# 10 Fjob - father's job (nominal: 'teacher', 'health' care related, civil 'services'
#                         (e.g. administrative or police), 'at_home' or 'other')
# 11 reason - reason to choose this school (nominal: close to 'home', school 'reputation',
#                                           'course' preference or 'other')
# 12 guardian - student's guardian (nominal: 'mother', 'father' or 'other')
# 13 traveltime - home to school travel time (numeric: 1 - <15 min., 2 - 15 to 30 min.,
#                                             3 - 30 min. to 1 hour, or 4 - >1 hour)
# 14 studytime - weekly study time (numeric: 1 - <2 hours, 2 - 2 to 5 hours,
#                                   3 - 5 to 10 hours, or 4 - >10 hours)
# 15 failures - number of past class failures (numeric: n if 1<=n<3, else 4)
# 16 schoolsup - extra educational support (binary: yes or no)
# 17 famsup - family educational support (binary: yes or no)
# 18 paid - extra paid classes within the course subject (binary: yes or no)
# 19 activities - extra-curricular activities (binary: yes or no)
# 20 nursery - attended nursery school (binary: yes or no)
# 21 higher - wants to take higher education (binary: yes or no)
# 22 internet - Internet access at home (binary: yes or no)
# 23 romantic - with a romantic relationship (binary: yes or no)
# 24 famrel - quality of family relationships (numeric: from 1 - very bad to 5 - excellent)
# 25 freetime - free time after school (numeric: from 1 - very low to 5 - very high)
# 26 goout - going out with friends (numeric: from 1 - very low to 5 - very high)
# 27 Dalc - workday alcohol consumption (numeric: from 1 - very low to 5 - very high)
# 28 Walc - weekend alcohol consumption (numeric: from 1 - very low to 5 - very high)
# 29 health - current health status (numeric: from 1 - very bad to 5 - very good)
# 30 absences - number of school absences (numeric: from 0 to 93)
# 31 Grade (numeric: from 0 to 20)

library(readr)
library(car)
library(MASS)
library("DAAG")
library(car)

train <- read.csv('teamassign06train.csv')
test <- read.csv('teamassign06test.csv')

#Changing categorical and binary variables into factors for test and train data set
binary <- c("school", "sex", "address", "famsize", "schoolsup", "famsup", "paid", "activities", "nursery",
            "higher", "internet")
levels <- c("Pstatus", "Medu", "Fedu", "Fjob", "Mjob", "reason", "guardian", "traveltime", "studytime", "failures", 
            "romantic", "famrel", "freetime", "goout", "Dalc", "Walc", "health")

train[binary] <- lapply(train[binary], function(x) as.factor(x))
train[levels] <- lapply(train[levels], function(x) as.factor(x))
test[binary] <- lapply(test[binary], function(x) as.factor(x))
test[levels] <- lapply(test[levels], function(x) as.factor(x))

#Model 1, all the variables
train.lm <- lm(Grade~., data=train)

# Plot R-student residuals for the model with all predictors. Label points with an absolute value
# of the residual greater than 2, and color in red any observations where the Grade=0
# to determine how poorly this model predicts for students with a grade of zero.
par(mfrow=c(1,1))
plot(rstudent(train.lm))
for (i in 1:300) {
  if (abs(resid(train.lm))[i] > 0.00001 && (abs(rstudent(train.lm)[i])>2)) {
    text(i, rstudent(train.lm)[i], labels=i, cex= 0.7, pos=3)
  }
}  
for (i in 1:300) {
  if (train$Grade[i] == 0) {
    points(i, rstudent(train.lm)[i], col= "red", pch = 16)
  }
}

#From this insight, we will do a logistic regression first to see if we can accurately predict which grades are 0
log.train <- train

#Making grade as 0 or 1 to represent positive or 0 grade values
log.train$bi.Grade <- train$Grade
log.train$bi.Grade[log.train$bi.Grade>0] <-1

#model selection
log.full.mod <- glm(bi.Grade~. - Grade, data=log.train, family = binomial)
log.null.mod <- glm(bi.Grade~1, data=log.train, family = binomial)

#Backward
step(log.full.mod, scope=list(lower=log.null.mod, upper=log.full.mod), direction="backward")

log.backward <- glm(formula = bi.Grade ~ school + sex + age + address + famsize + 
                      failures + famsup + activities + higher + internet + absences, 
                    family = binomial, data = log.train)

summary(log.backward) 
# Null deviance: 7.3774e+01  on 299  degrees of freedom
# Residual deviance: 2.3771e-06  on 286  degrees of freedom
# AIC: 28

#hybrid
step(log.null.mod, scope=list(lower=log.null.mod, upper=log.full.mod), direction="both")

log.hybrid <- glm(formula = bi.Grade ~ absences + famrel + age + school + studytime + 
                    goout + paid, family = binomial, data = log.train)

summary(log.hybrid) 
# Null deviance: 7.3774e+01  on 299  degrees of freedom
# Residual deviance: 1.7935e-07  on 284  degrees of freedom
# AIC: 32

#forward
step(log.null.mod, scope=list(lower=log.null.mod, upper=log.full.mod), direction="forward")

log.forward <- glm(formula = bi.Grade ~ absences + famrel + age + school + studytime + 
                     goout + paid, family = binomial, data = log.train)

summary(log.forward)

# Null deviance: 7.3774e+01  on 299  degrees of freedom
# Residual deviance: 1.7935e-07  on 284  degrees of freedom
# AIC: 32

#Because backward subset selection had lower AIC and is computationally better, we will go with backward
#Predicting 0 values for the training set
options(scipen=999)
predict(log.backward, type="response") #seems like model is very confident which ones are 1's and which ones are 0's
log.train$ghat<-1-as.numeric(predict(log.backward, type="response")<0.99)
#check to see how accurate we get on our training set
View(log.train)
#We got 100% accuracy on our training set

#Predicting Testing Set
test$zero <- ifelse(predict.glm(log.backward, newdata=test, type = "response") >0.5, 1, 0)
View(test)

#Construct a linear model for the datapoint with grade not equal to 0
train.pos <- train[which(train$Grade>0),]
train.pos[binary] <- lapply(train.pos[binary], function(x) as.factor(x))
train.pos[levels] <- lapply(train.pos[levels], function(x) as.factor(x))

#Forward/backward/stepwise subsetting
pos.null <-  lm(Grade~1, data=train.pos)
pos.full <- lm(Grade~., data=train.pos)

#Forward
step(pos.null, scope=list(lower=pos.null, upper=pos.full), direction="forward")
lm.pos.forward <- lm(formula = Grade ~ failures + higher + sex + Medu + schoolsup + 
     absences + age + reason + internet + goout, data = train.pos)
summary(lm.pos.forward) #adj-R^2 is 0.3573
cv.lm(data=train.pos, form.lm=lm.pos.forward, m=10, plotit=F) #ms = 5.08

#Backward
step(pos.full, scope=list(lower=pos.null, upper=pos.full), direction="backward")
lm.pos.backward <- lm(formula = Grade ~ school + sex + age + Medu + failures + schoolsup + 
                        higher + internet + absences, data = train.pos)
summary(lm.pos.backward) #adjusted r-squared is 0.339
cv.lm(data=train.pos, form.lm=lm.pos.backward, m=10, plotit=F) #ms = 5.19

#stepwise
step(pos.null, scope=list(lower=pos.null, upper=pos.full), direction="both")
lm.pos.step <- lm(formula = Grade ~ failures + higher + sex + Medu + schoolsup + 
                    absences + age + reason + internet + goout, data = train.pos)
summary(lm.pos.step) #Adjusted r-square is 0.3573
cv.lm(data=train.pos, form.lm=lm.pos.step, m=10, plotit=F) #ms = 5.08

#We will use the stepwise regression due to low CV mse 
test$poshat <- predict(lm.pos.step, newdata=test)
test$predvect <- test$poshat*test$zero
predvect <- test$predvect

write.table(predvect, file="teamassign06preds.csv", row.names=F, col.names=F, sep=",")
