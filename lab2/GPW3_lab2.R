#Exercises -getting data
GPW3 <- read.csv("C:/Users/rwu/Desktop/Fall2020/6962-DataAnalytics/labs/lab2/GPW3_GRUMP_SummaryInformation_2010.csv")
View(GPW3)
attach(GPW3)
fix(GPW3)
GPW3$Resolution
tf <- is.na(GPW3$Resolution)
E <- GPW3$Resolution[!tf]
summary(GPW3$Resolution)
fivenum(GPW3$Resolution, na.rm=TRUE)
stem(GPW3$Resolution)
hist(GPW3$Resolution)
hist(GPW3$Resolution, seq(0., 400., 50.), prob=TRUE)
lines(density(GPW3$Resolution, na.rm=TRUE, bw=1.))
rug(GPW3$Resolution)

#Exercise 1 fitting
plot(ecdf(GPW3$Resolution), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(GPW3$Resolution); qqline(GPW3$Resolution)
x <- seq(30,95,1)
qqplot(qt(ppoints(250), df=5),x,xlab="Q-Q plot for tdsn")
qqline(x)

#WATER_H exploration and fitting
GPW3$UNSDCode
tf <- is.na(GPW3$UNSDCode)
E <- GPW3$UNSDCode[!tf]
summary(GPW3$UNSDCode)
fivenum(GPW3$UNSDCode, na.rm=TRUE)
stem(GPW3$UNSDCode)
hist(GPW3$UNSDCode)
hist(GPW3$UNSDCode, seq(0., 400., 50.), prob=TRUE)
lines(density(GPW3$UNSDCode, na.rm=TRUE, bw=1.))
rug(GPW3$UNSDCode)

#Exercise 1 fitting
plot(ecdf(GPW3$UNSDCode), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(GPW3$UNSDCode); qqline(GPW3$UNSDCode)
x <- seq(30,95,1)
qqplot(qt(ppoints(250), df=5),x,xlab="Q-Q plot for tdsn")
qqline(x)

boxplot(Resolution, UNSDCode)

