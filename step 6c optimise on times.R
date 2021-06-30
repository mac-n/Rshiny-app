#optimise cost based CFS final version

library(readr)
library(FSelector)
library(randomForest)
library(pROC)
times <- read_csv("~/times.csv")
timesdf<-data.frame(times)



 times<-timesdf[,2]
 names(times)<-timesdf[,1]
 
joinedcosts<-joined[c("CDRSB",names(times))]

lamdalist=c(0,0.0001,0.002,0.003,0.004,0.0005,0.006,0.006533292,0.007,0.008,0.009,0.001,0.002,0.003,0.004,0.005,0.006,0.007,0.008,0.009,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11)

groups=list()
testresults=list()
forestmodels=list()

for (i in 1:length(lamdalist)){
  set.seed(i)
  lamda=as.numeric(lamdalist[i])
  
  groups[[i]]<-cost_cfs(CDRSB ~., data=joinedcosts,costs=times,lamda=lamda)
 
}
v<-which(!duplicated(groups))
sums=vector(length=length(v))
testsummary=matrix(nrow=length(v),ncol=5)

groups<-groups[v]

for (i in 1:length(v)){
  forestmodels[[i]]<-list()
testresults[[i]]<-list()
sums[i]<-sum(times[groups[[i]]])

f<-as.simple.formula(groups[[i]],"CDRSB")

for (j in 1:5){
  
  
  forestmodels[[i]][[j]]<-randomForest(f,data=joined[-folds[[j]],],mtry=4)
  #average across different folds. do training and test data
  testresults[[i]][[j]]<-predict(forestmodels[[i]][[j]],joined[folds[[j]],])
  
}
}


  

testsummary=matrix(nrow=length(v),ncol=5)
for (i in 1:length(v)){
  for (j in 1:5){
    print(nrow(joined[folds[[j]],]))
    print(nrow(testresults[[i]][[j]]))
    print(as.numeric(multiclass.roc(response=as.ordered(joined[folds[[j]],]$CDRSB),predictor=as.ordered(testresults[[i]][[j]]))$auc))
    testsummary[i,j]<-multiclass.roc(response=as.ordered(joined[folds[[j]],]$CDRSB),predictor=as.ordered(testresults[[i]][[j]]))$auc
    
  }
}
