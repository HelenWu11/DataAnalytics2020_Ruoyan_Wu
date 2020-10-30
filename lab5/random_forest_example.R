install.packages("randomForest")
library(randomForest)
#Loading the dataset
data1 <- read.csv(file.choose(), head=TRUE)
head(data1)
# Adding the column names:
colnames(data1) <- c("BuyingPrice","Maintenance","NumDoors","NumPersons","BootSpace","Safety","Condition")
head(data1)
str(data1)
data1[complete.cases(data1), ]
data1$Condition <- as.factor(data1$Condition)
data1$BuyingPrice <- as.factor(data1$BuyingPrice)
data1$Maintenance <- as.factor(data1$Maintenance)
data1$BootSpace <- as.factor(data1$BootSpace)
data1$Safety <- as.factor(data1$Safety)
data1$NumDoors <- as.numeric(data1$NumDoors)
data1$NumPersons <- as.numeric(data1$NumPersons)

# Let's take a look at the Levels of the Condition column.
# Conditions has 4 levels: "acc", "good", "unacc", "vgood"
levels(data1$Condition)
summary(data1)
## Creating the "training dataset" and "Validation dataset"
# we will randomly choose 70% (0.7) of the data points for training and 30% (0.3) for validation
# First we need to set the seed.
set.seed(100)
train <- sample(nrow(data1), 0.7*nrow(data1), replace=FALSE)
TrainSet <- data1[train,]
ValidSet <- data1[-train,]
summary(TrainSet)
summary(ValidSet)

# Random Forest Model with default parameters
model1 <- randomForest(Condition ~ ., data=TrainSet,importance=TRUE)
model1
# By default, number of trees is 500 and number of variables tried at each split is 2 in this case.

# Fine tuning the parameters of the RandomForest model
# we have increased the mrtry to 6 from 2
# mtry = Number of variables randomly sampled as candidates at each split.
# Noe that the default values are different from
# classification (sqrt(p) where p is number of variables in x) and regression
model2 <- randomForest(Condition ~ ., data = TrainSet, ntree = 500, mtry = 6, importance = TRUE)
model2

# Now, we hvae seen the implementation of Random Foest and understood the importance of the model.
# Let's compare this model with decision tree and see how decision trees fare in comparison to random forest.
library(rpart)
library(caret)
library(e1071)
# we will compare model1 of Random Forest with Decision Tree model
model_dt <- train(Condition ~., data=TrainSet, method="rpart")
model_dt_1 = predict(model_dt, data=TrainSet)
table(model_dt_1, TrainSet$Condition)
mean(model_dt_1 == TrainSet$Condition)

# Running on Validation set.
model_dt_vs = predict(model_dt, newdata=ValidSet)
table(model_dt_vs, ValidSet$Condition)
mean(model_dt_vs == ValidSet$Condition)