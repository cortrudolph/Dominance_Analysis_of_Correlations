rawdata<-read.csv("/Users/rudolphc/Desktop/model1.csv", header=TRUE)
attach(rawdata)
thedata<-data.frame(Taskperf, PosAff, WorkEng, Thriv)
Labels<-names(thedata)[2:length(thedata)]
multRegress<-function(mydata){
numVar<<-NCOL(mydata)
Variables<<- names(mydata)[2:numVar]

RXX<-mydata[2:numVar,2:numVar]
RXY<-mydata[2:numVar,1]

RXX.eigen<-eigen(RXX)
D<-diag(RXX.eigen$val)
delta<-sqrt(D)

lambda<-RXX.eigen$vec%*%delta%*%t(RXX.eigen$vec)
lambdasq<-lambda^2
beta<-solve(lambda)%*%RXY
rsquare<<-sum(beta^2)

RawWgt<-lambdasq%*%beta^2
import<-(RawWgt/rsquare)*100

result<<-data.frame(Variables, Raw.RelWeight=RawWgt, Rescaled.RelWeight=import)
}



multRegress(thedata)
RW.Results<-result

RSQ.Results<-rsquare



#R-squared For the Model
RSQ.Results


#The Raw and Rescaled Weights
RW.Results



