#loading data into dataframe
#you can change it to your file location
#you need to conver your data to CSV format
x<-read.csv(file="H:/750/ANOVA.csv", header=T)


#Viewing Data
head(x)


boxplot(x6 ~ x1, data = x,
        xlab = "customer type", ylab = "product quality",
        main = "compare", col = c("#00AFBB", "#E7B800", "#FC4E07")
        , names=c("<1 year","1-5 years",">5 years") )

# One Way Anova
fit <- aov(x6 ~ as.factor(x1), data=x)

summary(fit) # display Type I ANOVA table
drop1(fit,~.,test="F") # type III SS and F Tests
TukeyHSD(fit)


layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(fit) # diagnostic plots


# Two Way Anova
fit <- aov(x6 ~ as.factor(x1)+as.factor(x2)+ as.factor(x1):as.factor(x2), data=x)
summary(fit) 


#Interaction plot

install.packages("gplots")
library("gplots")

interaction.plot(as.factor(x$x1),as.factor(x$x2), x$x6,type="b", col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22),	
                 xlab="customer type", 
                 ylab="estimated marginal means", 
                 main="Interaction Plot")




