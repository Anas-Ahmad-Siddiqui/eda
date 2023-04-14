library(ISLR)
library(lattice)

###################################################
options(prompt="Anas 20BPS1094 > ", continue = " ")
###################################################

data("Auto")

#(a)
splom(Auto)

#(b)
cor(subset(Auto, select = -name))

#(c) Use the lm() function to perform a multiple linear regression with mpg as the response and all other variables except name as
#the predictors. Use the summary() function to print the results. Comment on the output. For instance:
model <- lm(mpg~.-name, data = Auto)
model

#i. Is there a relationship between the predictors and the response?
#Yes, there is a relatioship between the predictors and the response by testing the null hypothesis of whether all the regression coefficients are zero. The F-statistic is far from 1 (with a small p-value), indicating evidence against the null hypothesis.

#ii. Which predictors appear to have a statistically significant relationship to the response?
#Looking at the p-values associated with each predictors t-statistic, we see that displacement, weight, year, and origin have a statistically significant relationship, while cylinders, horsepower, and acceleration do not.


#iii. What does the coefficient for the `year` variable suggest?
#The regression coefficient for year, 0.7507727, suggests that for every one year, mpg increases by the coefficient. In other words, cars become more fuel efficient every year by almost 1 mpg / year.


#(d)Use the plot() function to produce diagnostic plots of the linear regression fit. Comment on any problems you see with the fit. Do the residual plots suggest any unusually large outliers? Does the leverage plot identify any observations with unusually high leverage?
par(mfrow = c(2, 2))
plot(model)

plot(predict(model), rstudent(model))

#(e) Use the * and : symbols to fit linear regression models withinteraction effects. Do any interactions appear to be statistically significant?
model1 <-  lm(mpg ~ cylinders * displacement + displacement * weight, data = Auto)
summary(model1)

#Try a few different transformations of the variables, such as log(X), √X, X2. Comment on your findings.
model2 <- lm(data = Auto[,1:8], mpg ~ log(horsepower)*log(weight)*log(displacement))
summary(model2)
