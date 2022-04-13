library(ggplot2)
library(tidyverse)
library(ggdendro)
library(cluster)
library(corrplot)
library(SciViews)
library(MASS)
library(klaR)

homevalue <- read.csv("C:/Users/moharahman/Downloads/homevalue.csv")
View(homevalue)
summary(homevalue)

#A
colMeans(homevalue[1:6])

#B
cor_home <- round(cor(homevalue[1:6]), digits = 3)
cor_home
corrplot(cor_home, method = "color")

#C
t_home <- t(homevalue[1:6])
head(t_home)

d_t_home <- dist(t_home)
d_t_home

ave_hclust_home <- hclust(d_t_home, method="average") 
ggdendrogram(ave_hclust_home, rotate = FALSE, size = 2) 
 
com_hclust_home <- hclust(d_t_home, method="complete") 
ggdendrogram(com_hclust_home, rotate = FALSE, size = 2) 


#D

set.seed(532)
km_home <- kmeans(homevalue[1:6], 4, iter.max=40, algorithm="MacQueen")
summary(km_home)

km_home

k_clus <- km_home$cluster
slt <- silhouette(k_clus,dist(k_clus))
plot(slt)


#E
str(homevalue)
plot(homevalue)
attach(homevalue)

lin_home <- lm(home~pop+pro+job+gov+crime)
summary(lin_home)
plot(lin_home)

#F 

homevalue$pol <- as.factor(homevalue$pol)
str(homevalue)

lda_home <- lda(pol~pop+pro+job+gov+crime+home)
lda_home

class_home <- predict(lda_home, method="plug-in")$class

table(pol, class_home)


aper <- (8+11)/(19+11+8+23)
aper

new_person <- data.frame(pop=5.5, pro=7.4, job=69, gov=20, crime=44, home=2.99)
predict(lda_home, new_person)




  