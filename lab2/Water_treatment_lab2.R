#Exercises -getting data
water_data <- read.csv("C:/Users/rwu/Desktop/Fall2020/6962-DataAnalytics/labs/lab2/water-treatment.csv")
View(water_data)
attach(water_data)
fix(water_data)
water_data$COND.D
tf <- is.na(GPW3$COND.D)
E <- GPW3$COND.D[!tf]
summary(GPW3$COND.D)
fivenum(GPW3$COND.D, na.rm=TRUE)
stem(GPW3$COND.D)
hist(GPW3$COND.D)
hist(GPW3$COND.D, seq(30., 95., 1.0), prob=TRUE)
lines(density(GPW3$COND.D, na.rm=TRUE, bw=1.))
rug(GPW3$COND.D)

#Exercise 1 fitting
plot(ecdf(GPW3$COND.D), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(GPW3$COND.D); qqline(GPW3$COND.D)
x <- seq(30,95,1)
qqplot(qt(ppoints(250), df=5),x,xlab="Q-Q plot for tdsn")
qqline(x)

