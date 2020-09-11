days <- c('Mon','Tue','Wed','Thur','Fri','Sat','Sun')
temp <- c(28,30.5,32,31.2,29.3,27.9,26.4)
snowed <- c('T','T','F','F','T','T','F')
help("data.frame")
RPI_Weather_Week <- data.frame(days,temp,snowed)
RPI_Weather_Week
head(RPI_Weather_Week)
str(RPI_Weather_Week)
summary(RPI_Weather_Week)

RPI_Weather_Week[1,]
RPI_Weather_Week[,1]
RPI_Weather_Week[,'snowed']
RPI_Weather_Week[,'days']
RPI_Weather_Week[,'temp']
RPI_Weather_Week[1:5,c("days","temp")]
RPI_Weather_Week$temp
subset(RPI_Weather_Week, subset=snowed==TRUE)
sorted.snowed <- order(RPI_Weather_Week['snowed'])
sorted.snowed
RPI_Weather_Week[sorted.snowed,]

dec.snow <- order(-RPI_Weather_Week$temp)
dec.snow

empty.DataFrame <- data.frame()
v1 <- 1:10
v1
letters
v2 <- letters[1:10]
df <- data.frame(col.name.1 = v1, col.name.2 = v2)
df
write.csv(df,file = 'saved_df1.csv')
df2 <- read.csv('saved_df1.csv')
df2


#Exercises -getting data
install.packages("readxl")
library("readxl")
EPI2010_onlyEPIcountries <- read_excel(file.choose(), 1) 
data()
help(data)
EPI_data <- read.csv("C:/Users/rwu/Desktop/Fall2020/6962-DataAnalytics/labs/lab2/EPI2010_data.csv", skip=1)
View(EPI_data)
attach(EPI_data)
fix(EPI_data)
EPI_data$EPI
tf <- is.na(EPI_data$EPI)
E <- EPI_data$EPI[!tf]
summary(EPI_data$EPI)
fivenum(EPI_data$EPI, na.rm=TRUE)
stem(EPI_data$EPI)
hist(EPI_data$EPI)
hist(EPI_data$EPI, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPI_data$EPI, na.rm=TRUE, bw=1.))
rug(EPI_data$EPI)

#Exercise 1 fitting
plot(ecdf(EPI_data$EPI), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(EPI_data$EPI); qqline(EPI_data$EPI)
x <- seq(30,95,1)
qqplot(qt(ppoints(250), df=5),x,xlab="Q-Q plot for tdsn")
qqline(x)

#DALY exploration and fitting
EPI_data$DALY
tf <- is.na(EPI_data$DALY)
E <- EPI_data$DALY[!tf]
summary(EPI_data$DALY)
fivenum(EPI_data$DALY, na.rm=TRUE)
stem(EPI_data$DALY)
hist(EPI_data$DALY)
#hist(EPI_data$DALY, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPI_data$DALY, na.rm=TRUE, bw=1.))
rug(EPI_data$DALY)

#Exercise 1 fitting
plot(ecdf(EPI_data$DALY), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(EPI_data$DALY); qqline(EPI_data$DALY)
x <- seq(30,95,1)
qqplot(qt(ppoints(250), df=5),x,xlab="Q-Q plot for tdsn")
qqline(x)

#WATER_H exploration and fitting
EPI_data$WATER_H
tf <- is.na(EPI_data$WATER_H)
E <- EPI_data$WATER_H[!tf]
summary(EPI_data$WATER_H)
fivenum(EPI_data$WATER_H, na.rm=TRUE)
stem(EPI_data$WATER_H)
hist(EPI_data$WATER_H)
#hist(EPI_data$WATER_H, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPI_data$WATER_H, na.rm=TRUE, bw=1.))
rug(EPI_data$WATER_H)

#Exercise 1 fitting
plot(ecdf(EPI_data$WATER_H), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(EPI_data$WATER_H); qqline(EPI_data$WATER_H)
x <- seq(30,95,1)
qqplot(qt(ppoints(250), df=5),x,xlab="Q-Q plot for tdsn")
qqline(x)

# Comparing distributions
boxplot(EPI, DALY)
boxplot(EPI, WATER_H)
boxplot(DALY, WATER_H)
plot(ecdf(EPI_data$ENVHEALTH), do.points=FALSE, verticals=TRUE)
qqplot(EPI, ENVHEALTH)
plot(ecdf(EPI_data$ECOSYSTEM), do.points=FALSE, verticals=TRUE)
boxplot(DALY, ECOSYSTEM)
plot(ecdf(EPI_data$AIR_H), do.points=FALSE, verticals=TRUE)
qqplot(WATER_H, AIR_H)
plot(ecdf(EPI_data$BIODIVERSITY), do.points=FALSE, verticals=TRUE)
boxplot(EPI, BIODIVERSITY)

help("distributions")
EPILand <- EPI_data$EPI[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
hist(Eland)
hist(Eland, seq(30., 95., 1.0), prob=TRUE)
lines(density(Eland, na.rm=TRUE, bw=1.))
rug(Eland)

EPISurface <- EPI_data$EPI[!No_surface_water]
ESurface <- EPISurface[!is.na(EPISurface)]
hist(ESurface)
hist(ESurface, seq(30., 95., 1.0), prob=TRUE)
lines(density(ESurface, na.rm=TRUE, bw=1.))
rug(ESurface)

EPIDesert <- EPI_data$EPI[!Desert]
EDesert <- EPIDesert[!is.na(EPIDesert)]
hist(EDesert)
hist(EDesert, seq(30., 95., 1.0), prob=TRUE)
lines(density(EDesert, na.rm=TRUE, bw=1.))
rug(EDesert)

EPIHigh <- EPI_data$EPI[!High_Population_Density]
EHigh <- EPIHigh[!is.na(EPIHigh)]
hist(EHigh)
hist(EHigh, seq(30., 95., 1.0), prob=TRUE)
lines(density(EHigh, na.rm=TRUE, bw=1.))
rug(EHigh)

EPI_South_Asia <- EPI_data$EPI[!EPI_regions == "South Asia"]
ESouth_Asia <- EPI_South_Asia [!is.na(EPI_South_Asia )]
hist(ESouth_Asia)
hist(ESouth_Asia, seq(30., 95., 1.0), prob=TRUE)
lines(density(ESouth_Asia, na.rm=TRUE, bw=1.))
rug(ESouth_Asia)

GEO_South_Asia <- EPI_data$EPI[!GEO_subregion == "South Asia"]
G_South_Asia <- GEO_South_Asia [!is.na(GEO_South_Asia )]
hist(G_South_Asia)
hist(G_South_Asia, seq(30., 95., 1.0), prob=TRUE)
lines(density(G_South_Asia, na.rm=TRUE, bw=1.))
rug(G_South_Asia)
