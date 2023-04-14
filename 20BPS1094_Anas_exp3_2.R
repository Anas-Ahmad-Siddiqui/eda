
###################################################
options(prompt="Anas 20BPS1094 > ", continue = " ")
###################################################
setwd("C:/Users/anass/Desktop/sem 6/CSE3506 EDA/LAB/LAB 3")

# a. Create a scatter plot showing the relationship between father and offspring telomere length.
telomere_data <- read.csv("telomere inheritance.csv")

plot(telomere_data$father_telomere_length, telomere_data$offspring_telomere_length,
     main = "Father and Offspring Telomere Length",
     xlab = "Father Telomere Length",
     ylab = "Offspring Telomere Length",
     pch = 20)

# b. Do the data require any transformation before analysis using linear regression?
model <- lm(offspring_telomere_length ~ father_telomere_length, data = telomere_data)
# create a histogram of the residuals
hist(residuals(model), main = "Residuals Histogram")
# Check for outliers create a box plot of the data
boxplot(telomere_data$father_telomere_length, telomere_data$offspring_telomere_length,
        xlab = "Father Telomere Length",
        ylab = "Offspring Telomere Length")


# c. Estimate an equation that predicts the offspring telomere length from its father’s. Is there evidence that the father’s telomere length predicts his offspring’s value?
model <- lm(offspring_telomere_length ~ father_telomere_length, data = telomere_data)
summary(model)
