# Name: Anas Ahmad Siddiqui
# Reg. NO.: 20BPS1094
# Course Code: CSE3506
# Course Title: Essentials of Data Analytics
# LAB ASSIGNMENT 06

options(prompt ="Anas Ahmad Siddiqui 20BPS1094>",continue=" ")

# 1. This question should be answered using the Weekly data set, which is part of the ISLR package. This data is similar in nature to the Smarket data from this chapter’s lab, except that it contains 1,089 weekly returns for 21 years, from the beginning of 1990 to the end of 2010. 
require(ISLR)
require(MASS)
require(class)

# a). Produce some numerical and graphical summaries of the Weekly data. Do there appear to be any patterns?
summary(Weekly)

plot(Today~Lag1, col="darkred", data=Weekly)
simplelm = lm(Today~Lag1, data=Weekly)
abline(simplelm, lwd= 3, col= "darkgreen")

pairs(Weekly)

# b) Use the full data set to perform a logistic regression with Direction as the response and the five lag variables plus Volume as predictors. Use the summary function to print the results. Do any of the predictors appear to be statistically significant? If so, which ones?
logmod = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,family = "binomial", data=Weekly)
summary(logmod)

# c) Compute the confusion matrix and overall fraction of correct predictions. Explain what the confusion matrix is telling you about the types of mistakes made by logistic regression.
probs = predict(logmod, type="response")
preds = rep("Down", 1089)
preds[probs > 0.5] = "Up"
table(preds, Weekly$Direction)

hist(probs, breaks= 100, col= "darkred")
abline(v = mean(probs), lwd = 2)

plot(probs, col= ifelse(Weekly$Direction=="Down", "red","green"), pch=16)
abline(h = 0.5, lwd= 3)


# d) Now fit the logistic regression model using a training data period from 1990 to 2008, with Lag2 as the only predictor. Compute the confusion matrix and the overall fraction of correct predictions for the held out data (that is, the data from 2009 and 2010).

training.data = Weekly[Weekly$Year<2009,]
test.data = Weekly[Weekly$Year>2008,]
simpglm = glm(Direction~Lag2, data= training.data, family = "binomial")
summary(simpglm)

testprobs = predict(simpglm, type="response", newdata = test.data)
testdirs = Weekly$Direction[Weekly$Year>2008]
plot(testprobs, col= ifelse(Weekly$Direction[Weekly$Year>2008]=="Down", "red","green"), pch=16)
abline(h = 0.5, lwd= 3)

testpreds = rep("Down", 104)
testpreds[testprobs>0.5] = "Up"
mean(probs)

table(testpreds, testdirs)


# 2. In this problem, you will develop a model to predict whether a given car gets high or low gas mileage based on the Auto data set.
library(ISLR)

# a). Create a binary variable, =pg01, that contains a 1 if pg contains a value above its median, and a 0 if mpg contains a value below its median. You can compute the median using the median() function. Note you may find it helpful to use the data.frane() function to create a single data set containing both mpg01 and the other Auto variables.
data("Auto")
mpg01 <- rep(0, length(Auto$mpg))
mpg01[Auto$mpg > median(Auto$mpg)] <- 1
Auto <- data.frame(Auto, mpg01)
summary(Auto)

# b). Explore the data graphically in order to investigate the association between mpg0i and the other features. Which of the other features seem most likely to be useful in predicting mpg01? Scatterplots and boxplots may be useful tools to answer this question. Describe your findings.
cor(Auto[, -9])

library(corrplot)

corrplot::corrplot.mixed(cor(Auto[, -9]), upper="circle")

pairs(Auto[, -9])

par(mfrow=c(2,3))
boxplot(cylinders ~ mpg01, data = Auto, main = "Cylinders vs mpg01")
boxplot(displacement ~ mpg01, data = Auto, main = "Displacement vs mpg01")
boxplot(horsepower ~ mpg01, data = Auto, main = "Horsepower vs mpg01")
boxplot(weight ~ mpg01, data = Auto, main = "Weight vs mpg01")
boxplot(acceleration ~ mpg01, data = Auto, main = "Acceleration vs mpg01")
boxplot(year ~ mpg01, data = Auto, main = "Year vs mpg01")


# c). Perform logistic regression on the training data in order to predict mpg01 using the variables that seemed most associated with mpg01 in (b). What is the test error of the model obtained?
library(MASS)

set.seed(123)

train <- sample(1:dim(Auto)[1], dim(Auto)[1]*.7, rep=FALSE)
test <- -train
training_data<- Auto[train, ]
testing_data= Auto[test, ]
mpg01.test <- mpg01[test]

lda_model <- lda(mpg01 ~ cylinders + weight + displacement + horsepower, data = training_data)
lda_model

lda_pred = predict(lda_model, testing_data)
names(lda_pred)

pred.lda <- predict(lda_model, testing_data)
table(pred.lda$class, mpg01.test)

mean(pred.lda$class != mpg01.test)


