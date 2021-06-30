# step 4 test each of the selected sets for prediction accuracy
library(randomForest)
library(FSelector)
library(caret)
library(pROC)
#m<-c(1,2,3,4,5,2,3,4,5,1)
#dim(m)<-c(5,2)
#z=1
mmd=list()
cmcfsdiag=list()
cmcnsdiag=list()
cmborutadiag=list()
joined$CDRSB<-as.ordered(joined$CDRSB)
roclist=list()
  #CFS
 
  for (i in 1:5){
    cmcfsdiag[[i]]=list()
    roclist[[i]]=list()
    for (z in setdiff((1:5),i)){
    fsset =unlist(folds[i])
    testset=unlist(folds[z])
    print (i)
    print (z)
    f<-as.simple.formula(cfslist[[i]],"CDRSB")
    model<-randomForest(f,data=joined[-(c(fsset,testset)),])
    #retain testing set that is not the fold the features were selected on
    #do we need to exclude the feature seletion fold from the traning data
    temp<-predict(model,joined[testset,],type="response")
    roclist[[i]][[z]]<-multiclass.roc(response=temp,predictor=joined[testset,"CDRSB"])$auc
    
    }
  }
  
  #consiistency
 
  for (i in 1:5){
    cmcnsdiag[[i]]=list()
    roclist[[i+5]]=list()
    for (z in setdiff((1:5),i)){
      fsset =unlist(folds[i])
      testset=unlist(folds[z])
    f<-as.simple.formula(consistencylist[[i]],"CDRSB")
    model<-randomForest(f,data=joined[-(c(fsset,testset)),],mtry=4)
    #retain testing set that is not the fold the features were selected on
    #do we need to exclude the feature seletion fold from the traning data
    temp<-predict(model,joined[testset,],type="response")
    roclist[[i+5]][[z]]<-multiclass.roc(response=temp,predictor=joined[testset,"CDRSB"])$auc
    
  }
  }
  
  #Boruta
for (i in 1:5){
  cmborutadiag[[i]]=list()
  roclist[[i+10]]=list()
  for (z in setdiff((1:5),i)){
    fsset =unlist(folds[i])
    testset=unlist(folds[z])
    f<-as.simple.formula(names(borutaselectedlist[[i]]),"CDRSB")
    model<-randomForest(f,data=joined[-(c(fsset,testset)),],mtry=4)
    #retain testing set that is not the fold the features were selected on
    #do we need to exclude the feature seletion fold from the traning data
    temp<-predict(model,joined[testset,],type="response")
    roclist[[i+10]][[z]]<-multiclass.roc(response=temp,predictor=joined[testset,"CDRSB"])$auc
    
  }
    
}

aucmatdiag=matrix(nrow=15,ncol=2)
rownames(aucmatdiag)<-rep("x",15)
for (i in 1:5){
  
  aucmatdiag[i,2]<-length(cfslist[[i]])
  aucmatdiag[5+i,2]<-length(consistencylist[[i]])
  aucmatdiag[10+i,2]<-length(borutaselectedlist[[i]])
  
  # 
  rownames(aucmatdiag)[i]<-paste("CFS_",i)
  rownames(aucmatdiag)[i+5]<-paste("Consistency_",i)
  rownames(aucmatdiag)[i+10]<-paste("Boruta_",i)
  
}
colnames(aucmatdiag)<-c("AUC","#features")

for (i in 1:15){
  aucmatdiag[i,1]<-mean(unlist(roclist[[i]]))
  }

