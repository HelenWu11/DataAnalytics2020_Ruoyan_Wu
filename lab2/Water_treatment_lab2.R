#Exercises -getting data
water_data <- read.csv("C:/Users/rwu/Desktop/Fall2020/6962-DataAnalytics/labs/lab2/water-treatment.csv")
View(water_data)
attach(water_data)
fix(water_data)
water_data$COND.D
tf <- is.na(water_data$COND.D)
E <- water_data$COND.D[!tf]
summary(water_data$COND.D)
fivenum(water_data$COND.D, na.rm=TRUE)
stem(water_data$COND.D)
hist(water_data$COND.D)
hist(water_data$COND.D, seq(0., 4000., 100.0), prob=TRUE)
lines(density(water_data$COND.D, na.rm=TRUE, bw=1.))
rug(water_data$COND.D)

#Exercise 1 fitting
plot(ecdf(water_data$COND.D), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(water_data$COND.D); qqline(water_data$COND.D)
x <- seq(30,95,1)
qqplot(qt(ppoints(250), df=5),x,xlab="Q-Q plot for tdsn")
qqline(x)


#WATER_H exploration and fitting
water_data$COND.E
tf <- is.na(water_data$COND.E)
E <- water_data$COND.E[!tf]
summary(water_data$COND.E)
fivenum(water_data$COND.E, na.rm=TRUE)
stem(water_data$COND.E)
hist(water_data$COND.E)
hist(water_data$COND.E, seq(0., 4000., 100.0), prob=TRUE)
lines(density(water_data$COND.E, na.rm=TRUE, bw=1.))
rug(water_data$COND.E)

#Exercise 1 fitting
plot(ecdf(water_data$COND.E), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(water_data$COND.E); qqline(water_data$COND.E)
x <- seq(30,95,1)
qqplot(qt(ppoints(250), df=5),x,xlab="Q-Q plot for tdsn")
qqline(x)

boxplot(COND.E, COND.D)

