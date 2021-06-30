require(memisc)
require(plyr)

require(dplyr)
require(mice)
require(DMwR)

require(ADNIMERGE)

#first this
#Then Variation at RAndom
#Then VAriation by Site
#Then Variation by Disease Severity


#code to view the adnimerge dataset
#str(adnimerge)
#View(adnimerge)

# to see how much rows of data there is in the adnimerge dataset

# select all rows and columns from the adni dataset
#adnimerge[,]
data(adnimerge)
#selecting all rows and only the listed columns and putting it into the new function select_data
select_data<-adnimerge[which(adnimerge$COLPROT=="ADNI2"|adnimerge$COLPROT=="ADNI3"),]
#select_data <- subset(select_data,select=c(VISCODE,RID,COLPROT,AGE,CDRSB,APOE4))

#view select_data
View(select_data)

# set select_data into bl_adni
bl_adni <- select_data

#change age according to VISCODE, then save it into ach_adni
rnum <- nrow(bl_adni)
for(i in 1:rnum){
  if (bl_adni$VISCODE[i] == "m06") bl_adni$AGE[i] <- bl_adni$AGE[i] + 0.5
  if (bl_adni$VISCODE[i] == "m12") bl_adni$AGE[i] <- bl_adni$AGE[i] + 1
  if (bl_adni$VISCODE[i] == "m18") bl_adni$AGE[i] <- bl_adni$AGE[i] + 1.5
  if (bl_adni$VISCODE[i] == "m24") bl_adni$AGE[i] <- bl_adni$AGE[i] + 2
  if (bl_adni$VISCODE[i] == "m30") bl_adni$AGE[i] <- bl_adni$AGE[i] + 2.5
  if (bl_adni$VISCODE[i] == "m36") bl_adni$AGE[i] <- bl_adni$AGE[i] + 3
  if (bl_adni$VISCODE[i] == "m42") bl_adni$AGE[i] <- bl_adni$AGE[i] + 3.5
  if (bl_adni$VISCODE[i] == "m48") bl_adni$AGE[i] <- bl_adni$AGE[i] + 4
  if (bl_adni$VISCODE[i] == "m54") bl_adni$AGE[i] <- bl_adni$AGE[i] + 4.5
  if (bl_adni$VISCODE[i] == "m60") bl_adni$AGE[i] <- bl_adni$AGE[i] + 5
  if (bl_adni$VISCODE[i] == "m66") bl_adni$AGE[i] <- bl_adni$AGE[i] + 5.5
  if (bl_adni$VISCODE[i] == "m72") bl_adni$AGE[i] <- bl_adni$AGE[i] + 6
  if (bl_adni$VISCODE[i] == "m78") bl_adni$AGE[i] <- bl_adni$AGE[i] + 6.5
  if (bl_adni$VISCODE[i] == "m84") bl_adni$AGE[i] <- bl_adni$AGE[i] + 7
  if (bl_adni$VISCODE[i] == "m90") bl_adni$AGE[i] <- bl_adni$AGE[i] + 7.5
  if (bl_adni$VISCODE[i] == "m96") bl_adni$AGE[i] <- bl_adni$AGE[i] + 8
  if (bl_adni$VISCODE[i] == "m102") bl_adni$AGE[i] <- bl_adni$AGE[i] + 8.5
  if (bl_adni$VISCODE[i] == "m108") bl_adni$AGE[i] <- bl_adni$AGE[i] + 9
  if (bl_adni$VISCODE[i] == "m114") bl_adni$AGE[i] <- bl_adni$AGE[i] + 9.5
  if (bl_adni$VISCODE[i] == "m120") bl_adni$AGE[i] <- bl_adni$AGE[i] + 10
  end
}
for(i in 1:rnum){
  if (bl_adni$VISCODE[i] == "bl") bl_adni$time[i] <- 0
  if (bl_adni$VISCODE[i] == "m06") bl_adni$time[i] <- 6
  if (bl_adni$VISCODE[i] == "m12") bl_adni$time[i] <- 12
  if (bl_adni$VISCODE[i] == "m18") bl_adni$time[i]<- 18
  if (bl_adni$VISCODE[i] == "m24") bl_adni$time[i] <- 24
  if (bl_adni$VISCODE[i] == "m30") bl_adni$time[i] <- 30
  if (bl_adni$VISCODE[i] == "m36") bl_adni$time[i] <- 36
  if (bl_adni$VISCODE[i] == "m48") bl_adni$time[i] <- 48
  if (bl_adni$VISCODE[i] == "m54") bl_adni$time[i] <- 54
  if (bl_adni$VISCODE[i] == "m60") bl_adni$time[i] <- 60
  if (bl_adni$VISCODE[i] == "m66") bl_adni$time[i] <- 66
  if (bl_adni$VISCODE[i] == "m72") bl_adni$time[i] <- 72
  if (bl_adni$VISCODE[i] == "m78") bl_adni$time[i] <- 78
  if (bl_adni$VISCODE[i] == "m84") bl_adni$time[i] <- 84
  if (bl_adni$VISCODE[i] == "m90") bl_adni$time[i] <- 90
  if (bl_adni$VISCODE[i] == "m96") bl_adni$time[i] <- 96
  if (bl_adni$VISCODE[i] == "m102") bl_adni$time[i] <- 102
  if (bl_adni$VISCODE[i] == "m108") bl_adni$time[i] <- 108
  if (bl_adni$VISCODE[i] == "m114") bl_adni$time[i] <- 114
  if (bl_adni$VISCODE[i] == "m120") bl_adni$time[i] <- 120
  end 
}

#selecting all rows and only the listed columns and putting it into the new function ach_adni 


# #removing all the missing data from ach_adni and setting it the a new function remove_missing_adni
# remove_missing_adni <- na.omit(ach_adni)
# 
# # set remove_missing_adni into complete_adni
# complete_adni <- remove_missing_adni
# write.csv(complete_adni, '~/bl_adni.csv')
df<-bl_adni
df$CDRSB1[df$CDRSB==0] <- 0
df$CDRSB1[(df$CDRSB>=0.5 & df$CDRSB<=4.0)]<- 1
df$CDRSB1[(df$CDRSB>=4.5 & df$CDRSB<=9.0)] <- 2
df$CDRSB1[(df$CDRSB>=9.5 & df$CDRSB<=15.5)] <- 2
df$CDRSB1[(df$CDRSB>=16 & df$CDRSB<=18)] <- 2

allvalueadni<-df[!is.na(df$CDRSB),]
allvalueadni$CDRSB<-allvalueadni$CDRSB1
allvalueadni$CDRSB1<-NULL
allvalueadni$VISCODE<-as.character(allvalueadni$VISCODE)
cbind(rowID=seq.int(nrow(allvalueadni)),allvalueadni)
#allvalueadni$VISCODE<-allvalueadni$time
allvalueadni$RID<-as.numeric(allvalueadni$RID)
