# Author: Anas Ahmad Siddiqui
# Branch: CSE CPS
# Registration Number: 20BPS1094
# Group: G1

###################################################
options(prompt="Anas 20BPS1094 > ", continue = " ")
###################################################

# Q1.

# a)
plot(NA, NA, type = "n", xlim = c(-4, 2), ylim = c(-1, 5), asp = 1, xlab = "X1", ylab = "X2")
symbols(c(-1), c(2), circles = c(2), add = TRUE, inches = FALSE)

# b)
plot(NA, NA, type = "n", xlim = c(-4, 2), ylim = c(-1, 5), asp = 1, xlab = "X1", ylab = "X2")
symbols(c(-1), c(2), circles = c(2), add = TRUE, inches = FALSE)
text(c(-1), c(2), "< 4")
text(c(-4), c(2), "> 4")

# c)
plot(c(0, -1, 2, 3), c(0, 1, 2, 8), col = c("blue", "red", "blue", "blue"), type = "p", asp = 1, xlab = "X1", ylab = "X2")
symbols(c(-1), c(2), circles = c(2), add = TRUE, inches = FALSE)


# Q2.

# a)
x1 = c(3, 2, 4, 1, 2, 4, 4, 5, 3, 6, 7, 2, 4, 5, 8, 3, 4, 2, 5, 4, 7, 3, 4, 6, 5, 7, 9, 5, 3, 8)
x2 = c(4, 2, 4, 4, 1, 3, 1, 2, 6, 7, 8, 9, 5, 9, 5, 3, 7, 4, 8, 5, 3, 4, 8, 9, 3, 6, 4, 9, 4, 1)
colors = c("red", "red", "red", "red", "blue", "blue", "blue", "red", "red", "red", "red", "blue", "blue", "blue", "red", "red", "red", "red", "blue", "blue", "blue", "red", "red", "red", "red", "blue", "blue", "blue", "red", "blue")
plot(x1, x2, col = colors, xlim = c(0, 10), ylim = c(0, 10))

# b) 
plot(x1, x2, col = colors, xlim = c(0, 10), ylim = c(0, 10))
abline(-0.5, 1)

# d)
plot(x1, x2, col = colors, xlim = c(0, 10), ylim = c(0, 10))
abline(-0.5, 1)
abline(-1, 1, lty = 2)
abline(0, 1, lty = 2)

# g)
plot(x1, x2, col = colors, xlim = c(0, 10), ylim = c(0, 10))
abline(-0.3, 1)

# h)
plot(x1, x2, col = colors, xlim = c(0, 10), ylim = c(0, 10))
points(c(3), c(1), col = c("red"))

