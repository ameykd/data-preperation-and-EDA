##Set Working Directory
setwd("D:/Amey/Data_Sci/Data Wrangling")

##Data Exploration and Preparation
#Load Data
#This dataset can be found on Remote Desktop Server
cars<-read.csv("cars_retail_price.csv",header=TRUE,stringsAsFactors=FALSE)

#Reviewing Structure
dim(cars)
str(cars)

#Reviewing Observations
head(cars)
tail(cars)

#Reviewing Summary
library(datasets)
#for numeric variables
summary(airquality) 
summary(cars)

#for categorical variables
table(cars$Make)
table(cars$Trim)

b<-boxplot(cars$Price, range = 1.5) #1.5 times the IQR or the maximum/minimum value
b 
b$stats #a matrix, each column contains the extreme of the lower whisker, the lower hinge, the median, the upper hinge and the extreme of the upper whisker for one group/plot.
b$n #number of observations to create the box plot

min(cars$Price) 
?boxplot


IQR <- 26736.555-14271.020 #75 percentile - 25th percentile
whisker_length <- 1.5*IQR
#the whiskers extend to the most extreme data point which is no more than 1.5 times the interquartile range from the box.

#add this to the 75th and subtract this from 25th
#Any value outside of these 2 will be an outlier in the graph

b$out #list the outlier values
length(b$out) #total outlier values

#categorical variables
names(cars)
table(cars$Make)
table(cars$Make,cars$Type)
table(cars$Make,cars$Model)#it cannot overlap as model is subtype for make so a model will be avaliable for one type of make only
table(cars$Trim)
table(cars$Liter)
table(cars$Cruise)

aggregate(cars$Price~cars$Cruise,data = cars, mean)

#Dealing with colnames/varaible names
names(cars)
names(cars)[1]<-c("price")
names(cars)

#Missing Values
is.na(cars$Mileage)
sum(is.na(cars$Mileage))
colSums(is.na(cars)) #Prints sum of missing values for all the variables

air <- airquality
summary(air$Ozone)

is.na(air$Ozone)
sum(is.na(air))
sum(is.na(air$Ozone))
colSums(is.na(air))

mean(air$Ozone,na.rm=TRUE)

is.na(air$Ozone) <- mean(air$Ozone,na.rm=TRUE)
summary(air$Ozone)

#Dealing with missing values
carsNew<-na.omit(cars) #omit observations

cars$Mileage[is.na(cars$Mileage)]<-mean(cars$Mileage,na.rm=TRUE)#Using mean imputation

#install.packages("randomForest")
library(randomForest)      
#Using R package for dealing missing values
cars$Mileage<-na.roughfix(cars$Mileage) #Quite useful while working with large datasets
#median value replacement
?na.roughfix

summary(air)
is.na(air$Solar.R) #5th and 6th value
air$Solar.R<-na.roughfix(air$Solar.R)
median(air$Solar.R)
air$Solar.R
summary(air$Solar.R)

air_roughfix <- na.roughfix(air)
summary(air_roughfix)

summary(air)


#Outliers
summary(cars$Mileage)
boxplot(cars$Mileage)
plot(cars$Mileage)

c <- boxplot(cars$Mileage)
c$out
quantile(cars$Mileage,p=c(1:100)/100)

cars_mileage<-cars[cars$Mileage>40000,]
nrow(cars_mileage)
cars_mileage  #SAAB Cars

#Are SAAB cars above average?
aggregate(cars$Mileage~cars$Make,data = cars, mean)
aggregate(cars$Mileage~cars$Model,data = cars, mean)

#if value is greater than 43,000 then cut-off
cars_outlier <- cars
cars_outlier$Mileage<-ifelse(cars$Mileage>43000,43000,cars$Mileage)
summary(cars_outlier$Mileage)
summary(cars$Mileage)
plot(cars_outlier$Mileage)
plot(cars$Mileage)
v=boxplot(cars_outlier$Mileage)
v$out

#Dummy variable creation
dummy<-ifelse(cars$Cylinder==4,1,0) #Using ifelse
dummy

#finding correlation

cor(cars$Mileage,cars$Price)

# Correlation Matrix
install.packages('corrgram')
library("corrgram")
corrgram(cars)

#another outlier check
length(cars[cars$Price>50000,"Price"])
out <- aggregate(cars$Price~cars$Type,data = cars, mean)
out

barplot(out$"cars$Price",names.arg=c("Convertible", "Coupe", "Hatchback", "Sedan", "Wagon"))
cars_conv <- cars[cars$Type=="Convertible",]
s=boxplot(cars_conv$Price)
plot(cars$Price)
s$out
cars_price_convertible<-cars[cars$Price>63913,]
cars_price_convertible
