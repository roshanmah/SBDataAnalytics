#loading data into dataframe
#you can change it to your file location
#you need to conver your data to CSV format
x<-read.csv(file="H:/750/FactAnalysis.csv", header=T)


#Viewing Data
head(x)


my_data <- x[, c(2:18)]


pairs(my_data)

cor(my_data)


#PCA
pcaResult<-princomp(my_data)
summary(pcaResult)
plot(pcaResult)
plot(pcaResult, type="l")
screeplot(pcaResult, type="line", main="Scree Plot")
biplot(pcaResult)
biplot(pcaResult, choices = 2:3)
biplot(pcaResult, choices = c(1,3))

#Factor Analysis
fa<-factanal(my_data, factors = 5, rotation = "varimax")
fa
print(fa, digits=2, cutoff=.3, sort=TRUE)

fa<-factanal(my_data, factors = 5, rotation = "varimax", scores = "regression")
fa

#Scores
fa$scores


###Plot
load <- fa$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(my_data),cex=.7) 



#communalities
100*(1 - fa$uniquenesses)