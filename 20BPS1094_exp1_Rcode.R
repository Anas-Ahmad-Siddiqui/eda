#Name: Anas Ahmad Siddiqui
#Reg No: 20BPS1094
#CSE3506 L57-L58

# Experiment 1

options(prompt = "ANAS 20BPS1094 >", continue = " ")

library(ISLR)
data(Auto)


# a. Is there a relationship between the predictor and the response?
lm.fit <- lm(mpg ~ horsepower, data = Auto)
summary(lm.fit)
# Null hypothesis:  all regression coefficients equal to zero
# Alternative hypothesis: regression coefficients not equal to zero
# Since the F-statistic (599.7) is far larger than 1 and the p-value (2.2e-16) of the F-statistic is close to zero we can reject the null hypothesis 
# Yes, there is a relationship between horsepower and mpg as determined by testing the null hypothesis of all regression coefficients equal to zero. 

# b. How strong is the relationship between the predictor and the response?
(4.906/mean(Auto$mpg))*100

cor(Auto$mpg, Auto$horsepower, method = "pearson")

# c. Is the relationship between the predictor and the response positive or negative?
cor(Auto$mpg, Auto$horsepower, method = "pearson")
# The relationship between mpg and horsepower is negative as the Pierson’s Correlation Coefficient is negative. The more horsepower an automobile has the less mpg fuel efficiency the automobile will have.

# d. What is the predicted mpg associated with a horsepower of 98? What are the associated 95% confidence and prediction intervals?
predict(lm.fit, data.frame(horsepower = 98), interval = "confidence") 

predict(lm.fit, data.frame(horsepower = 98), interval = "prediction") 

# The predicted mpg associated with a horsepower of 98 is about 24.467.
# We are 95% confident that the average mpg of a car with horsepower of 98 is between 23.97 to 24.96.
# We are 95% confident that the mpg of a car with horsepower of 98 is between 14.81 to 34.12.

# e. Plot the response and the predictor. Display the least squares regression line.
plot(Auto$horsepower, Auto$mpg, main = "Scatterplot of mpg vs. horsepower", xlab = "horsepower", ylab = "mpg", col = "blue")
abline(lm.fit, lwd = 3, col = "magenta")

# f. Use the plot() function to produce diagnostic plots of the least squares regression fit. Comment on any problems you see with the fit.
par(mfrow = c(2,2))
plot(lm.fit)
# The plot of residuals versus fitted values indicates that there is non linearity present in the data. 
# The plot of standardized residuals versus leverage indicates the presence of a few outliers.