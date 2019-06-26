library(ggplot2)
library(dplyr)
library(relaimpo)
library(RColorBrewer)
library(caret)
library(DAAG)

bikeday <- read.csv("/Users/RaghadSalem/Desktop/?????????????? ????????????/Big Data/project/Bike-Sharing-Dataset 2/day.csv")

str(bikeday)
bikeday$dteday <- as.Date(bikeday$dteday)
bikeday$season <- as.factor(bikeday$season)
bikeday$yr <- as.factor(bikeday$yr)
bikeday$mnth <- as.factor(bikeday$mnth)
bikeday$holiday <- as.factor(bikeday$holiday)
bikeday$weekday <- as.factor(bikeday$weekday)
bikeday$workingday <- as.factor(bikeday$workingday)
bikeday$weathersit <- as.factor(bikeday$weathersit)

mod <- lm(cnt ~ temp + weathersit + yr + mnth, data = bikeday)
summary(mod)

cv.lm(data = bikeday, form.lm = formula(cnt ~ temp + weathersit + yr + mnth), 
      m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE, ylab("Count"))


#------------ GRAPHS
col <- brewer.pal(12,"Paired")
ggplot(bikeday,aes(mnth,cnt)) +
  geom_boxplot(fill = col) +
  theme_classic() +
  labs(title = "Boxplot of rental bikes per month") +
  xlab("Month") +
  ylab("Count")

col <- brewer.pal(3,"Paired")
ggplot(bikeday,aes(weathersit,cnt)) +
  geom_boxplot(fill = col) +
  theme_classic() +
  labs(title = "Boxplot of rental bikes by weathersit") +
  scale_x_discrete(labels = c("Cloudy","Mist","Light Rain")) +
  xlab("Weather") +
  ylab("Count")

ggplot(bikeday,aes(temp, cnt)) +
  geom_point() +
  geom_smooth(method = "loess") +
  theme_classic() +
  xlab("Normalized Tempeature (-8, 39") +
  ylab("Count")
# SAME goal
scatter.smooth(x=bikeday$temp, y=bikeday$cnt, main="Relation between Temperature and Ridership Count", xlab = "Temperature", ylab = "Ridership Count") 

col <- brewer.pal(4,"Set3")
ggplot(bikeday,aes(season,cnt)) +
  geom_boxplot(fill = col) +
  theme_classic() +
  labs(title = "Boxplot of rental bikes per season") +
  scale_x_discrete(labels = c("Winter","Spring","Summer","Fall")) +
  xlab("Season") +
  ylab("Count")


bikeday$dteday<-as.POSIXct(bikeday$dteday)
bikeday%>%ggplot(aes(x=dteday,y=cnt))+geom_line()

ggplot(bikeday,aes(holiday,cnt)) +
  geom_boxplot(fill = c("#8DD3C7","#FFFFB3")) +
  theme_classic() +
  labs(title = "Boxplot of rental bikes by holiday") +
  scale_x_discrete(labels = c("No","Yes")) +
  xlab("Holiday") +
  ylab("Count")

#-----------------CROSS VALIDATION
cv.lm(data = bikeday, form.lm = formula(cnt ~ temp + weathersit + yr + mnth), 
      m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE, ylab("Count"))

