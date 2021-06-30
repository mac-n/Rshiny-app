

require(ADNIMERGE)
require(plyr)
require(dplyr)

numfac<-function(x){
  return(as.numeric(as.factor(x)))
}
neg<-function(x){
  maxvalue<-max(x, na.rm=TRUE)
  return((x*-1)+maxvalue)
}

faqconvert<-function(x){
  temp<-as.numeric(as.factor(x))
  
  temp<-mapvalues(temp,c(5,2,6,1,4,3),c(3,2,1,0,2,3),warn_missing=TRUE)
  return(as.factor(temp))
}

mmseconvert<-function(x){
  x[grep("Incorrect",x,ignore.case =TRUE)]<-0
  x[grep("Correct",x,ignore.case=TRUE)]<-1
  x<-as.numeric(x)
  return(x)
}

fun =function(x){
  return(length(levels(x)))
}
data(adas)

adas_scores<-adas[,c("RID","VISCODE",names(adas)[grep("SCORE",names(adas))])]
adas_scores$Q5SCORE_CUE<-NULL
adas_scores$TOTSCORE<-NULL
adas_scores[,3:15]<-data.frame(sapply(adas_scores[,3:15],neg))
colnames(adas_scores)[!grepl("RID|VISCODE",colnames(adas_scores))] <- paste("adas", colnames(adas_scores)[!grepl("RID|VISCODE",colnames(adas_scores))], sep = "_")

data(mmse)
mmse_scores<-mmse[,-(grep("CM",colnames(mmse)))]

mmse_scores<-mmse_scores[,-grep("LTR",colnames(mmse_scores))]

mmse_scores <-mmse_scores %>% dplyr::select(-MMRECALL, -MMSCORE, -MMTRIALS, -MMSCORE_EDC)
mmse_scores$MMWORLD<-as.numeric(mmse_scores$MMW)+as.numeric(mmse_scores$MMD)+as.numeric(mmse_scores$MMO)+as.numeric(mmse_scores$MMR)+as.numeric(mmse_scores$MML)
mmse_scores<-mmse_scores %>% dplyr::select(-MMW,-MMO,-MMR,-MML,-MMD)

mmse_scores<-mmse_scores[,c("RID","VISCODE",names(mmse_scores)[grep("MM",names(mmse_scores))])]
mmse_scores[,c(3:28)]<-as.data.frame(sapply(mmse_scores[,c(3:28)],mmseconvert))
mmse_scores[,c(3:28)]<-sapply(mmse_scores[,c(3:28)],neg)
mmse_scores[,c(3:28)]<-sapply(mmse_scores[,c(3:28)],as.factor)

mmse_scores$MMobjects<-as.numeric(mmse_scores$MMFLAG)+ as.numeric(mmse_scores$MMBALL) +as.numeric(mmse_scores$MMTREE) +as.numeric(mmse_scores$MMBALLDL) +as.numeric(mmse_scores$MMFLAGDL) +as.numeric(mmse_scores$MMTREEDL)
mmse_scores<-mmse_scores %>% dplyr::select(-MMFLAG,-MMBALL,-MMTREE,-MMFLAGDL,-MMBALLDL,-MMTREEDL)

colnames(mmse_scores)[!grepl("RID|VISCODE",colnames(mmse_scores))] <- paste("mmse", colnames(mmse_scores)[!grepl("RID|VISCODE",colnames(mmse_scores))], sep = "_")
data(moca)
moca_official=scoreMOCA(moca,qScore=TRUE)
moca_scores<-moca_official %>% dplyr::select(RID,VISCODE,i6Serial, visuo, naming, attention, language, abstraction, delayedRecall)
#moca_scores[,grepl("SERIAL",colnames(moca_scores))]<-NULL
#moca_scores$Serial<-moca_official$i6Serial
#moca_scores[,grepl("IMMT",colnames(moca_scores))]<-NULL
#moca_scores$memory<-moca_official$memory
#moca_scores[,grepl("DELW",colnames(moca_scores))]<-NULL
#moca_scores$delayedRecall<-moca_official$delayedRecall
colnames(moca_scores)[!grepl("RID|VISCODE",colnames(moca_scores))] <- paste("moca", colnames(moca_scores)[!grepl("RID|VISCODE",colnames(moca_scores))], sep = "_")

#moca_scores<- moca_scores %>% dplyr::select(RID,VISCODE,i6Serial,visuo,naming,attention,language,abstraction,delayedRecall,orientation)
#colnames(moca_scores) <- paste("moca", colnames(moca), sep = "_")


data(faq)
faq_scores<-faq[,c("RID","VISCODE",colnames(faq)[grep("FAQ",colnames(faq))])]
#faq_scores<-subset(faq_scores,select=-c(FAQTOTAL,FAQSOURCE))
faq_scores<-faq_scores %>% dplyr::select(-FAQTOTAL,-FAQSOURCE)
faq_scores[,c(3:12)]<-as.data.frame(sapply(faq_scores[,c(3:12)],faqconvert))
colnames(faq_scores)[!grepl("RID|VISCODE",colnames(faq_scores))] <- paste("faq", colnames(faq_scores)[!grepl("RID|VISCODE",colnames(faq_scores))], sep = "_")
#also include Npi and ecog scores

data(ptdemog)
demog<-ptdemog[,c("RID","PTGENDER","PTHAND")]

data(neurobat)
neurobat<-neurobat[which(neurobat$COLPROT=="ADNI2"|neurobat$COLPROT=="ADNI3"),]
neurobat<-neurobat[, sapply(neurobat, function(x) !all(is.na(x)))] 
neurobat<-neurobat[, sapply(neurobat, function(x) !all(x==0))] 



neurobat_scores<-neurobat
#colnames(neurobat_scores)[!grepl("RID|VISCODE",colnames(neurobat_scores))] <- paste("nb", colnames(neurobat_scores)[!grepl("RID|VISCODE",colnames(neurobat_scores))], sep = "_")
neurobat_scores<-neurobat_scores%>% dplyr::select(RID,VISCODE,LIMMTOTAL,LDELTOTAL,CLOCKSCOR,COPYSCOR,CATANIMSC,TRAASCOR,TRABSCOR,BNTTOTAL,ANARTERR,AVDELTOT)
set.seed(100)



data(ecogpt)
ecogpt_scores<-ecogpt[,(colSums(is.na(ecogpt))<300)]
ecogpt_scores$STAFFASST<-NULL
ecogpt_scores
colnames(ecogpt_scores)[!grepl("RID|VISCODE",colnames(ecogpt_scores))] <- paste("ecog", colnames(ecogpt_scores)[!grepl("RID|VISCODE",colnames(ecogpt_scores))], sep = "_")

data(gdscale)
gdscale<-gdscale[c(3,5,7:ncol(gdscale))]
gdscale$USERDATE2<-NULL
gd_scores<-gdscale[,(colSums(is.na(gdscale))<200)]
colnames(gd_scores)[!grepl("RID|VISCODE",colnames(gd_scores))] <- paste("gd", colnames(gd_scores)[!grepl("RID|VISCODE",colnames(gd_scores))], sep = "_")


all_table<-plyr::join(allvalueadni,demog,type="left",match="first")
all_table<-all_table %>% dplyr::select(RID,VISCODE,AGE,CDRSB,PTEDUCAT,RAVLT.immediate)
all_table$RID<-as.numeric(all_table$RID)
all_table$VISCODE<-as.character(all_table$VISCODE)
all_table<-inner_join(all_table,moca_scores)
nrow(all_table)
all_table<-inner_join(all_table,faq_scores)
nrow(all_table)
all_table<-inner_join(all_table,mmse_scores)
nrow(all_table)
all_table<-inner_join(all_table,adas_scores)
nrow(all_table)
all_table<-inner_join(all_table,neurobat_scores)
nrow(all_table)
all_table<-inner_join(all_table,gd_scores)


all_table<-inner_join(all_table,ecogpt_scores)
#get rid of any unwanted columns we didn't drop yet.
all_table[,grepl("ORIGPROT|COLPROT|SITEID|USERDATE",colnames(all_table))]<-NULL
#first visits only
all_table<-all_table[!duplicated(all_table$RID),]

all_table<-all_table[,colSums(is.na(all_table))<250]
ncol(all_table)
all_tablewithnas<-all_table
all_table<-na.omit(all_table)



#convert characters to factors
DF1<-all_table[,3:ncol(all_table)]
DF1[sapply(DF1, is.character)] <- lapply(DF1[sapply(DF1, is.character)], 
                                         as.factor)
#convert factors with more than 4 values to numeric
DF1[,which(sapply(DF1,fun)>4)]<-sapply(DF1[,which(sapply(DF1,fun)>4)],as.numeric)

all_table[,3:ncol(all_table)]<-DF1
#unwanted extra columns
all_table$ecog_VALIDITY<-NULL
all_table$gd_GDTOTAL<-NULL

#all the table joining created some duplicate values, get rid of them
all_table$ridvis<-paste(all_table$RID,all_table$VISCODE,sep=".")
all_table<-all_table[!duplicated(all_table$ridvis),]
all_table$ridvis<-NULL

