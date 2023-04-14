#Name: Anas Ahmad Siddiqui
#Reg. NO.: 20BPS1094
#Course Code: CSE3506
#Course Title: Essentials of Data Analytics


options(prompt ="Anas 20BPS1094 >",continue=" ")
setwd('/home/ex1/Documents/20BPS1094/LAB 3')

mammals = read.csv('mammals.csv')
View(mammals)
mammals = na.omit(mammals)

# A. Plot brain size against body size. Is the relationship linear?
ggplot(df, aes(x = body_mass_kg, y = brain_mass_g)) + geom_point() + ggtitle("Relation between body and brain mass") + xlab("Body mass") + ylab("Brain mass")+ theme_classic()

# B. Find a transformation (for either or both variables) that makes the relationship between these two variables linear.
plot(y = mammals$body_mass_kg, x = (mammals$brain_mass_g)^4.5, data = mammals, type = "l", col = "blue")

# C. Is there statistical evidence that brain size is correlated with body size? Assume that the species data are independent.
cor(df$body_mass_kg, df$brain_mass_g)

# D. What line best predicts (transformed) brain size from (transformed) body size?
model <- lm(brain_mass_g ~ body_mass_kg, data = df)
summary(model)
coef(model)
predictions <- predict(model, df)
ggplot(df, aes(brain_mass_g, body_mass_kg)) + geom_point() + geom_smooth(method = "lm")

# E. Based on your answer in (d), what is the predicted change in log-brain size accompanying an increase of 3 units of log- body size?
lr_bothlog = lm(log(body_mass_kg) ~ log(brain_mass_g), data=mammals)
summary(lr_bothlog)
lr_bothlog

# F. Make a residual plot using the regression fitted to the transformed variables. Do the data look like they match the assumptions of linear regression?
model <- lm(brain_mass_g ~ body_mass_kg, data = df)
df$residuals <- residuals(model)
library(ggplot2)
ggplot(df, aes(body_mass_kg, residuals)) + geom_point() + geom_hline(yintercept = 0)

# G Which species has the highest brain size relative to that predicted by its body size? Which species has the smallest brain relative to that predicted by its body size? 
# smallest
model <- lm(body_mass_kg ~ brain_mass_g, data = df)
predicted_values <- predict(model, df)
df$residuals <- df$brain_mass_g - predicted_values
min_row <- which.min(df$residuals)
df[min_row,]

# largest
model <- lm(body_mass_kg ~ brain_mass_g, data = df)
predicted_values <- predict(model, df)
df$residuals <- df$brain_mass_g - predicted_values
max_row <- which.max(df$residuals)
df[max_row,]

