trCommit<-read.csv("C:/Shahab-WORKS/journals/DSJ/19022017/6AllMod.csv", header=TRUE)
gCommit<-acast(trCommit, UID ~ PID)
RCommit<-as.matrix(gCommit)
rCommit <- as(RCommit, "realRatingMatrix")
image(rCommit, main = "Raw Ratings")
trainSize<-0.7

A=c("RMSE","MSE","MAE")
myX=matrix(,nrow = 100, ncol = length(A));
for (i in 1:100) {
  e<-evaluationScheme(rCommit[1:84], method="split", train=trainSize, given=1, goodRating=5)
  ##re1<-Recommender(getData(e,"train"),method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=2, minRating=1))
  re1<-Recommender(getData(e,"train"),method="POPULAR")
  p1<-predict(re1,getData(e,"known"),type="ratings")
  p1
  error<- rbind(UBCF=calcPredictionAccuracy(p1,getData(e,"unknown")))
  error
  myX[i,1]<-error[1,1]
  myX[i,2]<-error[1,2]
  myX[i,3]<-error[1,3]
}
write.csv(myX,"C:/Shahab-WORKS/journals/DSJ/19022017/6AllPopVals-70.csv")
