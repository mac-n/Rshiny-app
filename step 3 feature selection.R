#feature selection (step3)


require(caret)
require(FSelector)
require(beepr)
joined<-allwithsymptoms

joined$RID<-NULL
joined$VISCODE<-NULL
joined$CDRSB<-as.ordered(joined$CDRSB)

set.seed(100)
folds<-createFolds(joined$CDRSB,5)

cfslist=list()
for (i in 1:5){
  cfslist[[i]]<-cfs(CDRSB~.,joined[folds[[i]],])
}

sort(table(unlist(cfslist)),decreasing=TRUE)
beep()

consistencylist=list()

for (i in 1:5){
  print(i)
  consistencylist[[i]]<-consistency(CDRSB~.,joined[folds[[i]],])
}

sort(table(unlist(consistencylist)),decreasing=TRUE)
beep()

###genetic feature selection
# ga_ctrl <- gafsControl(functions = rfGA,
#                        method = "boot",
#                        verbose=TRUE,allowParallel = TRUE, genParallel   = TRUE)
# 
# ## Use the same random number seed as the previous process
# ## so that the same CV folds are used for the external
# ## resampling.
# set.seed(100)
# rf_ga <- gafs(x = joined, y = joined$CDRSB,
#               iters = 5, number=5,
#               gafsControl = ga_ctrl)
# rf_ga
beep()
require(Boruta)
borutalist=list()
borutaselectedlist=list()
for (i in 1:5){
  borutalist[[i]]<-Boruta(CDRSB~.,joined[folds[[i]],])
  borutaselectedlist[[i]]=borutalist[[i]]$finalDecision[which(borutalist[[i]]$finalDecision=="Confirmed")]
}
sort(table(names(unlist(borutaselectedlist))),decreasing=TRUE)

borutavec=table(names(unlist(borutaselectedlist)))
cfsvec=table(unlist(cfslist))
consistencyvec=table(unlist(consistencylist))
#bvec=table(unlist(borutaselectedlist))[which(table(unlist(borutaselectedlist))>2)]
allvec<-c(borutavec,consistencyvec,cfsvec)
allvec<-sort(unique(names(allvec),decreasing=FALSE))
beep()
tempboruta<-names(unlist(borutaselectedlist))
assigntimes<-table(unlist(c(cfslist,consistencylist,tempboruta)))



assigntimes<-sort(assigntimes,decreasing=TRUE)
write.csv(assigntimes,"assigntimes.csv")

