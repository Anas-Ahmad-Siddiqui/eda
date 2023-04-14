# Author: Anas Ahmad Siddiqui
# Branch: CSE CPS
# Registration Number: 20BPS1094
# Group: G1

###################################################
options(prompt="Anas 20BPS1094 > ", continue = " ")
###################################################

# Q1. In this problem, you will perform K-means clustering, with K = 2, on a small example with n = 6 observations and p = 2 features. The observations are as follows.

# a. Plot the observations. 
x <- cbind(c(1, 1, 0, 5, 6, 4), c(4, 3, 4, 1, 2, 0))
plot(x[,1], x[,2])

# b. Randomly assign a cluster label to each observation. You can use the sample() command in R to do this. Report the cluster labels for each observation.
set.seed(1)
labels <- sample(2, nrow(x), replace = T)
labels

plot(x[, 1], x[, 2], col = (labels + 1), pch = 20, cex = 2)

# c. Compute the centroid for each cluster
centroid1 <- c(mean(x[labels == 1, 1]), mean(x[labels == 1, 2]))
centroid2 <- c(mean(x[labels == 2, 1]), mean(x[labels == 2, 2]))
plot(x[,1], x[,2], col=(labels + 1), pch = 20, cex = 2)
points(centroid1[1], centroid1[2], col = 2, pch = 4)
points(centroid2[1], centroid2[2], col = 3, pch = 4)

# d. Assign each observation to the centroid to which it is closest, in terms of Euclidean distance. Report the cluster labels for each observation.
labels <- c(1, 1, 1, 2, 2, 2)
plot(x[, 1], x[, 2], col = (labels + 1), pch = 20, cex = 2)
points(centroid1[1], centroid1[2], col = 2, pch = 4)
points(centroid2[1], centroid2[2], col = 3, pch = 4)

# e. Repeat (c) and (d) until the answers obtained stop changing.
centroid1 <- c(mean(x[labels == 1, 1]), mean(x[labels == 1, 2]))
centroid2 <- c(mean(x[labels == 2, 1]), mean(x[labels == 2, 2]))
plot(x[,1], x[,2], col=(labels + 1), pch = 20, cex = 2)
points(centroid1[1], centroid1[2], col = 2, pch = 4)
points(centroid2[1], centroid2[2], col = 3, pch = 4)

# f. In your plot from (a), color the observations according to the cluster labels obtained.
plot(x[, 1], x[, 2], col=(labels + 1), pch = 20, cex = 2)


# Q2.Consider the USArrests data. We will now perform hierarchical clustering on the states.

# a. Using hierarchical clustering with complete linkage and Euclidean distance, cluster the states.
set.seed(2)
hc.complete <- hclust(dist(USArrests), method = "complete")
plot(hc.complete)

# b. Cut the dendrogram at a height that results in three distinct clusters. Which states belong to which clusters?
cutree(hc.complete, 3)

# c. Hierarchically cluster the states using complete linkage and Euclidean distance, after scaling the variables to have standard deviation one.
sd.data <- scale(USArrests)
hc.complete.sd <- hclust(dist(sd.data), method = "complete")
plot(hc.complete.sd)

# d. What effect does scaling the variables have on the hierarchical clustering obtained? In your opinion, should the variables be scaled before the interobservation dissimilarities are computed?
cutree(hc.complete.sd, 3)
table(cutree(hc.complete, 3), cutree(hc.complete.sd, 3))


# Q3. In this problem, you will generate simulated data, and then perform K-means clustering on the data.

# a. Generate a simulated data set with 20 observations in each of three classes (i.e. 60 observations total), and 50 variables.
set.seed(2)
x <- matrix(rnorm(20 * 3 * 50, mean = 0, sd = 0.001), ncol = 50)
x[1:20, 2] <- 1
x[21:40, 1] <- 2
x[21:40, 2] <- 2
x[41:60, 1] <- 1
true.labels <- c(rep(1, 20), rep(2, 20), rep(3, 20))

# b. Perform K-means clustering of the observations with K = 3. How well do the clusters that you obtained in K-means clustering compare to the true class labels?
km.out <- kmeans(x, 3, nstart = 20)
table(true.labels, km.out$cluster)

# c. Perform K-means clustering with K = 2. Describe your results.
km.out <- kmeans(x, 2, nstart = 20)
table(true.labels, km.out$cluster)

# d. Now perform K-means clustering with K = 4, and describe your results.
km.out <- kmeans(x, 4, nstart = 20)
table(true.labels, km.out$cluster)

