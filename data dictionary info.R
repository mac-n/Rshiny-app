#data dictionary etc
require(ADNIMERGE)
data(datadic)
allquestions<-datadic[which(datadic$FLDNAME %in% colnames(joined)),]
allquestions<-allquestions[,c("FLDNAME","CRFNAME","TEXT")]


#add additional explanations to the Neurobat assessments

for (i in which(allquestions$CRFNAME=="Neuropsychological Battery")){
  temp<-allquestions[i,1]
  temp2<-grep(temp,datadefinitions[,2])
  temp3<-datadefinitions[temp2,1]
  allquestions[i,2]<-temp3
}
for (i in which(allquestions$CRFNAME=="ADAS-Cognitive Behavior")){
  temp<-allquestions[i,1]
  temp2<-grep(temp,adas13[,1])
  temp3<-adas13[temp2,2]
  allquestions[i,3]<-temp3
}
temp<-data.frame(rownames(allcosts),allcosts)
colnames(temp)<-c("FLDNAME","cost")

allq<-left_join(temp,allquestions)
allq<-allq[!duplicated(allq$FLDNAME),]
allq[as.character(11:18),3]<-"Montreal Cognitive Assessment"
allq["11",4]<-"Digit Span Forward and Backward"


allq["11",4]<-"Digit Span Forward and Backward"

