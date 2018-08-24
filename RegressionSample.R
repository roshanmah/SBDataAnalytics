#loading data into dataframe
#you can change it to your file location
#you need to conver your data to CSV format
x<-read.csv(file="H:/750/weekly_beer_sales.csv", header=T)

#data summary
summary(x)

#Plotting week vs case size 18
plot(x$Week,x$CASES.18PK)
lines(x$CASES.18PK)

#Plotting week vs price of size 18
plot(x$Week,x$PRICE.18PK)
lines(x$PRICE.18PK)

#Scatter Plot
plot(x$PRICE.18PK,x$CASES.18PK, xlab="Price 18 Pack", ylab="Case 18 Pack", main="Scatter Plot")

#Regression Model
fitModel<-lm(CASES.18PK~PRICE.18PK, data=x)

#Adding Regression Line
abline(lm(x$CASES.18PK~x$PRICE.18PK), col="red")

#regression analysis results
regResult<-summary(fitModel)


#ANOVA
summary.aov(fitModel)

#residual vs predicted
residualData<-resid(fitModel)
predicted<-predict(fitModel)
plot(predicted,residualData, xlab="Predicted Values", ylab="Residuals")

#Residual Plot
plot(x$Week,residualData)
abline(0, 0, col="red")
