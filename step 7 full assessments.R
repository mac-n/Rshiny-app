#test against MMSE
#test against MOCA
#test against ADAS13
#test against FAQ
require(randomForest)
require(pROC)

require(pROCd<-data.frame(cbind(sums,errates))
)
joinedFAQ<-joined[,c(grep("FAQ",colnames(joined)),2)]
joinedMMSE<-joined[,c(grep("MM",colnames(joined)),2)]
joinedADAS<-joined[,c(grep("^adas",colnames(joined)),2)]
joinedMOCA<-joined[,c(2,7:14)]

modelsforfulltests<-list()
testresultsfulltests<-list()
aucfulltests<-matrix(nrow=4,ncol=5)
for (i in 1:5){
  modelsforfulltests[[i]]<-list()
  testresultsfulltests[[i]]<-list()
  set.seed(100)
  modelsforfulltests[[i]][[1]]<-randomForest(CDRSB ~.,,data=joinedFAQ[-folds[[i]],],mtry=4)
  modelsforfulltests[[i]][[2]]<-randomForest(CDRSB ~.,,data=joinedMMSE[-folds[[i]],],mtry=4)
  modelsforfulltests[[i]][[3]]<-randomForest(CDRSB ~.,,data=joinedADAS[-folds[[i]],],mtry=4)
  modelsforfulltests[[i]][[4]]<-randomForest(CDRSB ~.,,data=joinedMOCA[-folds[[i]],],mtry=4)
  for (j in 1:4){
    #average across different folds. do training and test data
    testresultsfulltests[[i]][[j]]<-predict(modelsforfulltests[[i]][[j]],joined[folds[[i]],])
    aucfulltests[j,i]<-multiclass.roc(response=as.ordered(joined[folds[[i]],]$CDRSB),predictor=as.ordered(testresultsfulltests[[i]][[j]]))$auc
    
  }
}

horizlines=rowMeans(aucfulltests)
names(horizlines)<-c("FAQ","MMSE","ADAS","MoCA")
astimes<-c(600,600,1200,600)
fullassessments<-data.frame(horizlines,astimes)

