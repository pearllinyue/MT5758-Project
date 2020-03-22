#Facebook Metrics Analysis

#setwd("~/Desktop/MT5758/projects/Facebook_metrics")

#import the dataset
facebook <- read.csv("dataset_Facebook.csv", header = TRUE)
head(facebook)
length(facebook)

#data cleaning
facebook <- na.omit(facebook)

#enable non-numerical variables as factors
facebook$Type <- as.factor(facebook$Type)
facebook$Category <- as.factor(facebook$Category)
facebook$Paid <- as.factor(facebook$Paid)


#reorder the dataset & drop Post.Hour, Post.Month and Post.Weekday
#install.packages("tidyverse")
library(tidyverse)
facebook <- facebook %>% select(Type, Category, Paid, Page.total.likes, Lifetime.Post.Total.Reach, 
                                Lifetime.Post.Total.Impressions, Lifetime.Engaged.Users, Lifetime.Post.Consumers,
                                Lifetime.Post.Consumptions, Lifetime.Post.Impressions.by.people.who.have.liked.your.Page,
                                Lifetime.Post.reach.by.people.who.like.your.Page, Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post,
                                Total.Interactions)

#change the colnames for easy works
colnames(facebook) <- c("Type", "Category", "Paid", "PTL", "LPTR", "LPTI", "LEU","LPConsumers", "LPConsumptions", "LPIbpwhlyP", "LPrbpwlyP", "LPwhlyPaewyp", "TI")
head(facebook)
length(facebook)

#dataset description
#Category: 1-Action; 2-Product; 3-Inspiration
#Paid: 0-not paid promotion; 1-paid promotion
#PTL - Page Total Likes
#LPTR - Lifetime Post Total Reach
#LPTI - Lifetime Post Total Impressions
#LEU - Lifetime Engaged Users
#LPConsumers - Lifetime Post Consumers
#LPConsumptions - Lifetime Post Comsumptions
#LPIbpwhlyP - Lifetime Post Impressions by people who have liked your Page
#LPrbpwlyP - Lifetime Post Reach by people who like your Page
#LPwhlyPaewyp - Lifetime People who have liked your Page and engaged with your post
#TI - Total Interactions

#scale the data
scale(facebook[,4:13])

#exploration of data
summary(facebook)
pairs(facebook)



#1 'Type' as response variable
#fit multinomial logistic regression model
#install.packages("nnet")
library(nnet)
Type.multinom <- multinom(Type ~ PTL + LPTR + LPTI + LEU + LPConsumers + LPConsumptions + LPIbpwhlyP + LPrbpwlyP + LPwhlyPaewyp + TI, data = facebook)

#Anova to drop some insignificant variables
library(car)
Anova(Type.multinom, type = "III")
#retain 'PTL', 'LEU', 'LPConsumers', 'LPConsumptions', 'LPwhlyPaewyp', 'TI'

#form new dataset for Type.multinom
Type.data <- facebook[,c(4,7,8,9,12,13)]
scale(Type.data)
head(Type.data)

#pca on correlation matrix
Type.pca <- princomp(Type.data, scores = T, cor = T)
Type.pca

#list new axes in descending order
summary(Type.pca)
#PC2 77.7% PC3 90.28%

#find the components to retain
#screeplot
screeplot(Type.pca, type = "lines", main = "Screeplot of Facebook Metrics Analysis - Type")

##Kaiser's Criterion 
#calculate eigenvalues
Type.eigenvalue <- Type.pca$sdev^2
Type.eigenvalue
which(Type.eigenvalue > 1)
#first 2 components are important should be retain

#find the loadings
Type.pca.loadings <- loadings(Type.pca)

#find the scores
Type.pca.scores1 <- Type.pca$scores[,1]
Type.pca.scores2 <- Type.pca$scores[,2]
par(pty = "s")

#plots of the scores
plot(Type.pca.scores1, Type.pca.scores2)
#label the scores according to the response variable - #1 Type
plot(Type.pca.scores1, Type.pca.scores2, ylim = range(Type.pca.scores1), xlab = "PC1", ylab = "PC2", type = "n", lwd = 2)
text(Type.pca.scores1, Type.pca.scores2, labels = abbreviate(facebook[,1]), cex = 0.7, lwd = 2)


#Exploring Important Variables
##Equilibrium Contribution
Type.equil <- 1/sqrt(ncol(Type.data))
length(which(abs(Type.pca.loadings[,1]) > Type.equil))
#Suggest 3 Variables

##Mardia's Criterion
Type.mardi <- 0.7 * max(Type.pca.loadings[,1])
length(which(abs(Type.pca.loadings[,1]) > Type.mardi))
#Suggest 6 Variables


#multinom using pca
Type.predict <- predict(Type.pca)
facebook$z1 <- Type.predict[,1]
facebook$z2 <- Type.predict[,2]
new.Type.multinom <- multinom(Type ~ z1 + z2, data = facebook)
Anova(new.Type.multinom, type = "III")

#transform relationship with pca with covariates
Type.beta <- coef(new.Type.multinom)
Type.x.bar <- Type.pca$center
Type.x.sd <- Type.pca$scale
Type.coef <- (Type.beta[2]*Type.pca.loadings[,1]+Type.beta[,3]*Type.pca.loadings[,2]/Type.x.sd)
Type.beta0 <- Type.beta[1] - sum(Type.x.bar*Type.coef)
c(Type.beta0, Type.coef)


#Plots
##Biplot
biplot(Type.pca, xlabs = rep(".", nrow(facebook)),
       choices = c(1,2), cex = 0.5, main = "Biplot of PC1 and PC2 - Type")

##Autoplot
#install.packages("ggfortify")
library(ggfortify)
autoplot(Type.pca, colour= "Type", data = facebook)

##Network graph
library(corrplot)
corrplot(cor(Type.data), method = "ellipse")
require(qgraph)
qgraph(cor(Type.data))
#all positive correlated?





#2 'Category' as response variable
#fit multinomial logistic regression model
library(nnet)
Category.multinom <- multinom(Category ~ PTL + LPTR + LPTI + LEU + LPConsumers + LPConsumptions + LPIbpwhlyP + LPrbpwlyP + LPwhlyPaewyp + TI, data = facebook)

#Anova to drop some insignificant variables
library(car)
Anova(Category.multinom, type = "III")
#retain 'PTL', 'LPTR','LPConsumptions', 'LPrbpwlyP', 'LPwhlyPaewyp', 'TI'

#form new dataset for Type.multinom
Category.data <- facebook[,c(4,5,9,11,12,13)]
scale(Category.data)
head(Category.data)

#pca on correlation matrix
Category.pca <- princomp(Category.data, scores = T, cor = T)
Category.pca

#list new axes in descending order
summary(Category.pca)
#PC2 67.72% PC4 90.75%

#find the components to retain
#screeplot
screeplot(Category.pca, type = "lines", main = "Screeplot of Facebook Metrics Analysis - Category")

##Kaiser's Criterion 
#calculate eigenvalues
Category.eigenvalue <- Category.pca$sdev^2
Category.eigenvalue
which(Category.eigenvalue > 1)
#first 2 components are important should be retain

#find the loadings
Category.pca.loadings <- loadings(Category.pca)

#find the scores
Category.pca.scores1 <- Category.pca$scores[,1]
Category.pca.scores2 <- Category.pca$scores[,2]
par(pty = "s")

#plots of the scores
plot(Category.pca.scores1, Category.pca.scores2)
#label the scores according to the response variable - #2 Category
plot(Category.pca.scores1, Category.pca.scores2, ylim = range(Category.pca.scores1), xlab = "PC1", ylab = "PC2", type = "n", lwd = 2)
text(Category.pca.scores1, Category.pca.scores2, labels = abbreviate(facebook[,2]), cex = 0.7, lwd = 2)


#Exploring Important Variables
##Equilibrium Contribution
Category.equil <- 1/sqrt(ncol(Category.data))
length(which(abs(Category.pca.loadings[,1]) > Equil))
#Suggest 4 Variables

##Mardia's Criterion
Category.mardi <- 0.7 * max(Category.pca.loadings[,1])
length(which(abs(Category.pca.loadings[,1]) > Category.mardi))
#Suggest 6 Variables

#multinom using pca
Category.predict <- predict(Category.pca)
facebook$z3 <- Category.predict[,1]
facebook$z4 <- Category.predict[,2]
new.Category.multinom <- multinom(Type ~ z3 + z4, data = facebook)
Anova(new.Category.multinom, type = "III")

#transform relationship with pca with covariates
Category.beta <- coef(new.Category.multinom)
Category.x.bar <- Category.pca$center
Category.x.sd <- Category.pca$scale
Category.coef <- (Category.beta[2]*Category.pca.loadings[,1]+Category.beta[,3]*Category.pca.loadings[,2]/Category.x.sd)
Category.beta0 <- Category.beta[1] - sum(Category.x.bar*Category.coef)
c(Category.beta0, Category.coef)


#Plots
##Biplot
biplot(Category.pca, xlabs = rep(".", nrow(facebook)),
       choices = c(1,2), cex = 0.5, main = "Biplot of PC1 and PC2 - Category")

##Autoplot
library(ggfortify)
autoplot(Category.pca, colour= "Type", data = facebook)

##Network graph
library(corrplot)
corrplot(cor(Category.data), method = "ellipse")
require(qgraph)
qgraph(cor(Category.data))
#all positive correlated?




#3 'Paid' as response variable
#fit binomial glm
Paid.glm <- glm(Paid ~ PTL + LPTR + LPTI + LEU + LPConsumers + LPConsumptions + LPIbpwhlyP + LPrbpwlyP + LPwhlyPaewyp + TI, data = facebook, family = "binomial")

#drop some insignificant variables
summary(Paid.glm)
#retain 'LPwhlyPaewyP', the dataset will no longer be multivariate
