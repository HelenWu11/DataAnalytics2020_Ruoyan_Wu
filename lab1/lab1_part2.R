multivariate <- read.csv("C:/Users/rwu/Desktop/Fall2020/6962-DataAnalytics/labs/lab3/multivariate.csv")
head(multivariate)
attach(multivariate)
names(multivariate)
multivariate

# Create some Scatterplots
plot(Income, Immigrant, main="Scatterplot")
plot(Immigrant, Homeowners)

# Flitting Linear Models using "lm" function
help(lm)
mm <- lm(Homeowners~Immigrant)
mm
# The output above shows the estimate of the regression beta coefficients (column Estimate) and 
# their significance levels (column Pr(>|t|).
# The intercept is 107494.898 and the coefficient of Immigrant variable is -6656.839.
# The estimated regression equation can be written as follow:
# Homeowners = 107494.898 + (-6656.839)*Immigrant 
# We can rewrite it as: 
# Homeowners = 107494.898 - 6656.839*Immigrant.
summary(mm)$coef

plot(Immigrant, Homeowners)
help(abline)
abline(mm)
abline(mm, col=2, lwd=3)
# Using this formula, for each new value in Immigrant, you can predict the value for Homeowners.
# As an example:
# For Immigrant value = 0, we will get: Homeowners = 107494.898 - 6656.839*0 = 107494.898
# for Immigrant value = 20, we will get: Homeowners = 107494.898 - 6656.839*20 = -25641.88
# Predictions can be easily made using the R function predict().
# In the following example, we predict Homeowners for two Immigrant values: 0 and 20.
# you can pass the 0 and 20 values as a concatenated list for Immigrants as follows:
newImmigrantdata <- data.frame(Immigrant=c(0,20))
newp <- predict(mm, newdata = newImmigrantdata)
newp

abline(mm)
abline(mm, col=3, lwd=3) #line color=green, line width=3
attributes(mm)
mm$coefficients


# ggplot examples
plot(mtcars$wt, mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt, mtcars$mpg)
qplot(wt, mpg, data=mtcars)
ggplot(mtcars, aes(x=wt,y=mpg)) + geom_point()
plot(pressure$temperature,pressure$pressure,type="l")
points(pressure$temperature,pressure$pressure)

lines(pressure$temperature,pressure$pressure/2, col="red")
points(pressure$temperature,pressure$pressure/2, col="blue")
library(ggplot2)
qplot(pressure$temperature, pressure$pressure, geom="line")
qplot(temperature, pressure, data=pressure, geom="line")
ggplot(pressure, aes(x=temperature, y=pressure)) + geom_line() + geom_point()
ggplot(pressure, aes(x=temperature, y=pressure)) + geom_line() + geom_point()

# Creating Bar graphs
barplot(BOD$demand, names.arg = BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl)) # generate a table of counts.
qplot(mtcars$cyl) # cyl is continuous here
qplot(factor(mtcars$cyl)) #treat cyl as discrete
# Bar graph of counts
qplot(factor(cyl), data=mtcars)
ggplot(mtcars, aes(x=factor(cyl))) + geom_bar()

# Creating Histogram
# View the distribution of one-dimentional data with a histogram
hist(mtcars$mpg)
hist(mtcars$mpg, breaks = 10) # specify apprioximate number of bins with breaks
hist(mtcars$mpg, breaks = 5)
hist(mtcars$mpg, breaks = 12)
qplot(mpg, data = mtcars, binwidth=4)
ggplot(mtcars,aes(x=mpg)) + geom_histogram(binwidth = 4)
ggplot(mtcars,aes(x=mpg)) + geom_histogram(binwidth = 5)

# Creating Box-plot
plot(ToothGrowth$supp, ToothGrowth$len) # using plot() function and pass it a factor of x-values and a vector of y-values
# Formula Syntax
boxplot(len~supp, data=ToothGrowth) # if the two vectos are in the same dataframe, you can use the formula syntax, with
# this syntax you can combine two variables on the x-axis.
# put interation of two variables on x-axis
boxplot(len~supp + dose, data=ToothGrowth)
# with ggplot2 you can get the same results above
library(ggplot2)
qplot(ToothGrowth$supp, ToothGrowth$len, geom="boxplot")
# if the two vectors are in the same dataframe, you can use the following syntax
qplot(supp, len, data=ToothGrowth, geom="boxplot")
# in ggplot2, the above is equvalent to:
ggplot(ToothGrowth, aes(x=supp, y=len)) + geom_boxplot()
# using three separate vectos
qplot(interaction(ToothGrowth$supp, ToothGrowth$dose), ToothGrowth$len, geom="boxplot")
# you can write the something above, get the columns from the dtaframe
qplot(interaction(supp,dose), len, data=ToothGrowth, geom="boxplot")
# using ggplot() you can do the samething and it is equivalent to:
ggplot(ToothGrowth, aes(x=interaction(supp, dose), y=len)) + geom_boxplot()

