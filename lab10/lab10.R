require(ggplot2)        # or load package first
data(diamonds)
head(diamonds)          # look at the data!
#
ggplot(diamonds, aes(clarity, fill=cut)) + geom_bar()
ggplot(diamonds, aes(clarity)) + geom_bar() + facet_wrap(~ cut)
ggplot(diamonds) + geom_histogram(aes(x=price)) + geom_vline(xintercept=12000)

ggplot(
  data = diamonds,
  mapping = aes(color = cut_number(carat, 5), x = price)
) +
  geom_freqpoly() +
  labs(x = "Price", y = "Count", color = "Carat")

ggplot(diamonds, aes(x = cut_number(price, 10), y = carat)) +
  geom_boxplot() +
  coord_flip() +
  xlab("Price")

ggplot(diamonds, aes(x = cut_number(carat, 5), y = price, colour = cut)) +
  geom_boxplot()


# Work on the Textbook (Intro to Statistical Learning with R - 7th Edition) Chapter 4.6.3 on 
# Linear Discriminant Analysis Lab example 
library(MASS)
library(ISLR)
# Introduction to Statistical Learning with R 7th Edition – Chapter 4 Lab: 4.6.1
# We will perform LDA on the Smarket dataset
# data set consists of percentage returns for the S&P 500 stock index over 1, 250 days, from the
# beginning of 2001 until the end of 2005

# Read the lda() function documentation on RStudio
help(lda)
# now we will use the lda() function to fit the model
# Notice that the
# lda() syntax for the lda() function is identical to that of lm(), and to that of
# glm() except for the absence of the family option.

data("Smarket")
attach(Smarket)
head(Smarket)
# data set consists of percentage returns for the S&P 500 stock index over 1, 250 days, from the
# beginning of 2001 until the end of 2005
names(Smarket)
str(Smarket)
dim(Smarket)
# For each date, we have recorded the percentage returns for each of the five previous
# trading days, "Lag1" through "Lag5".
# We have also recorded "Volume" (the number of shares traded on the previous day, in billions of dollars.
# "Today" is the percentage return on the date
# "Direction" (whether the market was Up or Down on this date)

# Next, we will first create a vector corresponding
# to the observations from 2001 through 2004. We will then use this vector
# to create a held out data set of observations from 2005.
train <- (Year < 2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005 <- Direction[!train]

# We fit the model using only the observations before 2005.

lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.fit
# The LDA output indicates that ˆπ1 = 0.492 and ˆπ2 = 0.508; in other words,
# 49.2% of the training observations correspond to days during which the
# market went down

# The predict() function returns a list with three elements. The first element,
# class, contains LDA’s predictions about the movement of the market.
# The second element, posterior, is a matrix whose kth column contains the
# posterior probability that the corresponding observation belongs to the kth class,
# x contains the linear discriminants

lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class <- lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class == Direction.2005)

