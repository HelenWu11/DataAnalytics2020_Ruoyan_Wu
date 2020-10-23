titanic <- read.csv('C:/Users/rwu/Desktop/Fall2020/6962-DataAnalytics/DataAnalytics2020_Ruoyan_Wu/lab5/Titanic_train.csv')
attach(titanic)
View(titanic)

#rpart
titanic <- na.omit(titanic)
str(titanic)
titanic$Pclass <- factor(titanic$Pclass, order=TRUE, levels = c(3,2,1))
library(rpart)
titanic_rpart <- rpart(Survived~  Sex+Pclass+Age, , method='class',data=titanic)
library(rpart.plot)
rpart.plot(titanic_rpart)

#ctree
require(party)
titanic$Sex <- factor(titanic$Sex)
titanic_ctree <- ctree(Survived~  Sex+Pclass+Age, data=titanic)
plot(titanic_ctree)

#hclust
titanic_hclust <- dist(as.matrix(titanic))
hc <- hclust(titanic_hclust)
plot(hc)
