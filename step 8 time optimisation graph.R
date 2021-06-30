#Code for graph and Mahalanobis
require(ggplot2)
d<-data.frame(sums,rowMeans(testsummary))
colnames(d)<-c("sums","errates")

astimes<-c(600,600,1200,600)
fullassessments<-data.frame(horizlines,astimes)
fullassessments$astimes<-log10(fullassessments$astimes)
d<-d[1:12,]
d[,1]<-log10(d[,1])
sumsdf<-data.frame(log10(sums[1:11]))
colnames(sumsdf)<-"sums"
d[order(mahalanobis(d, colMeans(d), cov(d)), decreasing=TRUE),]
foo<-function(x){
  return (predict(l,data.frame(x)))
}

l<-lm(errates ~poly(sums,1),data=d)

#q<-ggplot(d, aes(x=sums,y=errates))+geom_point()+ geom_line(aes(y = foo(sumsdf))) +xlab("Assessment time (seconds)") +ylab("Model AUC")+geom_point(data=d[9,],aes(x=sums,y=errates,color="red"))+theme_bw()+
  theme(legend.position = "none")
q<-ggplot(d, aes(x=sums,y=errates))+geom_point() +xlab("Log (base 10) total assessment time") +ylab("Model AUC")+geom_point(data=d[9,],aes(x=sums,y=errates,color="red"))+theme_bw()+
  theme(legend.position = "none")
q<-q +geom_vline(xintercept = log10(900), linetype="dotted")
q

p<-q+geom_point(data=fullassessments,aes(x=astimes,y=horizlines),size=3,shape=1)
p<-p+geom_text(data=fullassessments,aes(x=astimes,y=horizlines,label=rownames(fullassessments),vjust=c(-0.8,-1,-1,-1),hjust=c(-0.2,0,0,0)),size=4)+ylim(0.6,0.95) 
#p<-p+ scale_x_continuous(breaks=c(0,1000,2000))+geom_point(data=d["9",],aes(x=sums,y=errates,color="red"))
p+geom_point(data=d[3,],aes(x=sums,y=errates,color="blue"))
