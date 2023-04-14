# Name: Anas Ahmad Siddiqui
# Reg. NO.: 20BPS1094
#C ourse Code: CSE3506
# Course Title: Essentials of Data Analytics
# LAB ASSIGNMENT 04

###################################################
options(prompt="Anas 20BPS1094 > ", continue = " ")
###################################################

x <- c(4.4, 6.7, 10.5, 9.6, 12.4, 5.5, 11.1, 8.6, 14.0, 10.1, 7.2, 7.9)
y <- c(586, 565, 515, 532, 478, 560, 493, 533, 575, 490, 530, 515)

# library function
cor(x, y)
cor.test(x, y)

# user defined function
numerator = (sum((x - mean(x)) * (y - mean(y))))
denominator = (sqrt(sum((x - mean(x))^2) * sum((y - mean(y))^2)))
correlation_coeff = numerator / denominator
correlation_coeff