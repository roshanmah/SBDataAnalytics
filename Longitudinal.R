pd<-read.csv(file="H:/750/alcohol1.csv", header=T)
pd10<-pd[1:45,]
pd10<-pd[pd$id %in% c(4, 14, 23, 32, 41, 56, 65, 82), ]

xyplot(alcuse~age | id, data=pd10, 
       panel = function(x, y){
         panel.xyplot(x, y)
         panel.lmline(x, y)
       }, ylim=c(-2, 5), as.table=T)

#Model A
library(nlme)

model.a <- lme(alcuse~ 1, pd, random= ~1 |id)
summary(model.a)

#https://stats.idre.ucla.edu/r/faq/how-can-i-calculate-standard-errors-for-variance-components-from-mixed-models/
VarCorr(model.a)

#Model B
model.b <- lme(alcuse ~ age_14 , data=pd, random= ~ age_14 | id, method="ML")
summary(model.b)

fixef.b <- fixef(model.b)
fit.b <- fixef.b[[1]] + pd$age_14[1:3]*fixef.b[[2]]
plot(pd$age[1:3], fit.b, ylim=c(0, 2), type="b", 
     ylab="predicted alcuse", xlab="age")   
title("Model B \n Unconditional growth model")

#Model C
model.c <- lme(alcuse ~ coa*age_14 , data=pd, random= ~ age_14 | id, method="ML")
summary(model.c)

fixef.c <- fixef(model.c)
fit.c0 <- fixef.c[[1]] + pd$age_14[1:3]*fixef.c[[3]]
fit.c1 <- fixef.c[[1]] + fixef.c[[2]] + 
  pd$age_14[1:3]*fixef.c[[3]] +
  pd$age_14[1:3]*fixef.c[[4]]
plot(pd$age[1:3], fit.c0, ylim=c(0, 2), type="b", 
     ylab="predicted alcuse", xlab="age")
lines(pd$age[1:3], fit.c1, type="b", pch=17)   
title("Model C \n Uncontrolled effects of COA") 
legend(14, 2, c("COA=0", "COA=1"))

#Model D
model.d <- lme(alcuse ~ coa*age_14+peer*age_14 , data=pd, random= ~ age_14 | id, method="ML")
summary(model.d)


#Model E
model.e <- lme(alcuse ~ coa+peer*age_14 , data=pd, random= ~ age_14 | id, method="ML")
summary(model.e)


#Model F
model.f <- lme(alcuse ~ coa+cpeer*age_14 , data=pd, random= ~ age_14 | id, method="ML")
summary(model.f)

#Model G
model.g <- lme(alcuse ~ ccoa+cpeer*age_14 , data=pd, random= ~ age_14 | id, method="ML")
summary(model.g)




