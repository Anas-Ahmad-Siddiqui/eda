# Name: Anas Ahmad Siddiqui
# Reg. NO.: 20BPS1094
# Course Code: CSE3506
# Course Title: Essentials of Data Analytics
# LAB ASSIGNMENT 05

###################################################
options(prompt="Anas 20BPS1094 > ", continue = " ")
###################################################

# 1.
days = c(82, 92, 83, 97, 131)
sale = c(15, 25, 17, 28, 41)

cor(days, sale)

model = lm(sale~days)
# sale = -23.6027 + 0.5031 * days

day = data.frame(days = 150)
predict(model, newdata = day)

# 2.
months <- 1:8
S <- c(140,145,150,156,162,167,175,182)
cor(S, months)
model <- lm(S~months)
model
# equation: y = 132.786 * x + 5.964

# 3.
drug <- c('A','A','C','C','A','B','B','B','C','C','B','B','C','A','A')
res <- c(75,95,85,45,60,70,45,75,65,75,55,65,75,65,75)

res.aov <- aov(res~drug)
res.aov
summary(res.aov)

