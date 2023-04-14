# Name: Anas Ahmad Siddiqui
# Reg. NO.: 20BPS1094
#C ourse Code: CSE3506
# Course Title: Essentials of Data Analytics
# LAB ASSIGNMENT 02

###################################################
options(prompt="Anas 20BPS1094 > ", continue = " ")
###################################################

#Q2: This question should be answered using the Carseats data set.

data("Carseats", package = "ISLR")

#(a) Fit a multiple regression model to predict Sales using Price, Urban, and US.

lm.fit <-  lm(Sales ~ Price + Urban + US, data = Carseats)
summary(lm.fit)

#(b) Provide an interpretation of each coefficient in the model. Be careful—some of the variables in the model are qualitative!

# Price: suggests a relationship between price and sales given the low p-value of the t-statistic. The coefficient states a negative relationship between Price and Sales: as Price increases, Sales decreases. When price increases by $1000 and other predictors are held constant, sales decrease by 54.459 unit sales. In otherwords, when price increases by $1000, the number of carseats sold decrease by 54,459.
# Urban Yes: The linear regression suggests that there is not enough evidence for a relationship between the location of the store and the number of sales based. A store’s sale is not affected by whether or not it is in a Urban area.
# USYes: Suggests there is a relationship between whether the store is in the US or not and the amount of sales. A positive relationship between USYes and Sales: if the store is in the US, the sales will increase by approximately 1201 units.


#(c) Write out the model in equation form, being careful to handle the qualitative variables properly.
# Sales = 13.04 + (-0.05) * Price + (-0.02) * UrbanYes + (1.20) * US.Yes

#(d) For which of the predictors can you reject the null hypothesis H0 : βj = 0?

# Price and USYes, based on the p-values, F-statistic, and p-value of the F-statistic.

#(e) On the basis of your response to the previous question, fit a smaller model that only uses the predictors for which there is evidence of association with the outcome.

model <-  lm(Sales ~ Price + US, data = Carseats)
summary(model)


#(f) How well do the models in (a) and (e) fit the data?

# Based on the RSE and R^2 of the linear regressions, they both fit the data similarly, with linear regression from (e) fitting the data slightly better.

#(g) Using the model from (e), obtain 95 % confidence intervals for the coefficient(s).
confint(model, level=0.95)

#(h) Is there evidence of outliers or high leverage observations in the model from (e)?
par(mfrow = c(1, 1))
plot(predict(model), rstudent(model))

# All studentized residuals appear to be bounded by -3 to 3, so no potential outliers are suggested from the linear regression.

par(mfrow = c(2, 2))
plot(model)

# There are a few observations that greatly exceed (p+1)/n (0.0075567) on the leverage-statistic plot that suggest that the corresponding points have high leverage.
