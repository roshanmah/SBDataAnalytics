##INFOSYS 750
##S2-2018
##Dummy Variables and Multi-colinearity in R 



#loading data into dataframe
#you can change it to your file location
#you need to conver your data to CSV format
x<-read.csv(file="H:/750/Regression-Analysis-collinearity-data-employee-salaries.csv", header=T)

#Viewing Data
head(x)

#Creating Dummy Column 
dummyGender<-NULL
dummyGender<-(x$gender=="female")*1


#ADD it to Dataset
#this will add the newly generated column to your dataframe
x1<-cbind(dummyGender, x)
head(x1)

#Models
#R will generate dummy variables on its own
fit1<-lm(salary~experience+factor(gender), x)
summary(fit1)

#here we are using the plot function to see 4 different plots relatd to the linear model
#you can use them instead of all efforts you have done on previous R session
#https://stats.stackexchange.com/questions/58141/interpreting-plot-lm
par(mfrow=c(2,2))
plot(fit1)

#Checking for two other models
fit2<-lm(salary~age+factor(gender), x)
summary(fit2)

fit3<-lm(salary~experience+age+factor(gender), x)
summary(fit3)

#correlation matrix
#For a better visualization you can use the mentioned package
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
my_data <- x1[, c(1,3,4,5)]
chart.Correlation(my_data, histogram=TRUE, pch=19)
cor(my_data)

#multicolinearity
install.packages("mctest")
library(mctest)
my_data <- x1[, c(1,3,5)]
imcdiag(x = my_data, y = x1$salary)

