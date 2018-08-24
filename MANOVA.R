#loading data into dataframe
#you can change it to your file location
#you need to conver your data to CSV format
x<-read.csv(file="H:/750/ANOVA.csv", header=T)


#Viewing Data
head(x)
names(x)



# One Way Anova
Y<-cbind(x$x6,x$x7)
fit <- manova(Y ~ as.factor(x1), data=x)
summary(fit)


#Different Manova Test

summary(fit, test="Hotelling-Lawley")
summary(fit, test="Roy")
summary(fit, test="Pillai")
summary(fit, test="Wilks")


#Differ
summary.aov(fit)


#subset
fit <- manova(Y ~ as.factor(x1), data=x, subset=as.factor(x1) %in% c(1,2))
summary(fit)

