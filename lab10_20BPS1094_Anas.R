
# 1. You have been given a gene expression data set (Ch10Ex11.csv) that consists of 40 tissue samples with measurements on 1,000 genes. The first 20 samples are from healthy patients, while the second 20 are from a diseased group.

# (a) Load in the data using read.csv(). You will need to select header=F.

df=read.csv("C:/Users/anass/Desktop/sem 6/CSE3506 EDA/LAB/lab 10/Ch10Ex11.csv",header=F)
head(df)

# (b) Apply hierarchical clustering to the samples using correlationbased distance, and plot the dendrogram. Do the genes separate the samples into the two groups? Do your results depend on the type of linkage used?
  
dists <- dist(cor(df))
methods <- c('centroid', 'average', 'single', 'complete')
for (method in methods) {
  clusts <- hclust(dists, method = method)
  plot(clusts, 
       col = "#487AA1", col.main = "#45ADA8",
       col.lab = "#7C8071", col.axis = "#F38630",
       sub = "", hang = -1,
       axes = FALSE,
       main = paste0('Cluster Dendrogram using ', method, ' metric'))
}

# (c) Your collaborator wants to know which genes differ the most across the two groups. Suggest a way to answer this question,and apply it here.

require(ape); require(RColorBrewer)
plot(as.phylo(clusts), type = 'fan', 
     tip.color = brewer.pal(2, 'Accent')[cutree(clusts, 2)],
     edge.color = 'steelblue', edge.lty = 2,
     cex = 1.8,
     main = 'Polar Dendrogram of Three Clusters')


# 2. The Wage data set contains a number of other features, such as marital status (maritl), job class (jobclass),and others. Explore the relationships between some of these other predictors and wage, and use non-linear fitting techniques in order to fit flexible models to the data. Create plots of the results obtained, and write a summary of your findings.

require(ISLR) 
attach(Wage)

summary(maritl)

plot(maritl, wage)
title("Marital Status vs Wage")

plot(jobclass, wage)
title("Jobclass vs Wage")

require(gam)

gam.1 <- gam(wage ~ year + ns(age, df=4), data=Wage)
gam.2 <- gam(wage ~ year + ns(age, df=4) + maritl, data=Wage)
gam.3 <- gam(wage ~ year + ns(age, df=4) + jobclass, data=Wage)
gam.4 <- gam(wage ~ year + ns(age, df=4) + maritl + jobclass, data=Wage)

anova(gam.1, gam.2, gam.4, test="F")

anova(gam.1, gam.3, gam.4, test="F")

par(mfrow=c(2,2))
plot(gam.4, se=TRUE, col='blue')



# 3. Fit some of the non-linear models to the Auto data set. Is there evidence for non-linear relationships in this data set? Create some informative plots to justify your answer.

require(ISLR)
data(Auto)
attach(Auto)

pairs(Auto)

require(boot)
set.seed(1)
cv.error <- rep(0,5)

for (i in 1:5){
  glm.fit <- glm(mpg ~ poly(weight,i),data=Auto)
  cv.error[i]<- cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error

plot(cv.error, type="b", xlab="Degree", ylab="Test MSE")
points(which.min(cv.error), cv.error[2], col="red", pch=20, cex=2)

fit.1 <- lm(mpg ~ poly(weight, 5), data=Auto)
summary(fit.1)

set.seed(1)
cv.error <- rep(0,10)

for (i in 1:10){
  glm.fit <- glm(horsepower ~ poly(acceleration,i),data=Auto)
  cv.error[i]<- cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error

plot(cv.error, type="b", xlab="Degree", ylab="Test MSE")
points(which.min(cv.error), cv.error[9], col="red", pch=20, cex=2)

fit.2 <- lm(horsepower ~ poly(acceleration, 10), data=Auto)
summary(fit.2)
