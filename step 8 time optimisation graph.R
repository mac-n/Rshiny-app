#Code for graph and Mahalanobis
require(ggplot2)
d<-data.frame(sums,rowMeans(testsummary))
colnames(d)<-c("sums","errates")
d$mask<-c(rep(1,12),0)
d$mask[1]<-3
d$mask[2]<-2
astimes<-c(600,600,1200,600)
fullassessments<-data.frame(horizlines,astimes)
fullassessments$astimes<-log10(fullassessments$astimes)
fullassessments$mask<-c(1,1,2,1)
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
#q<-q +geom_vline(xintercept = log10(900), linetype="dotted")
#q

q <- q  +facet_grid(. ~ mask, scales="free",space="free")

q<-q+scale_x_continuous(breaks=c(1.55,1.6,1.65,1.75,2,2.5,2.65,2.7,2.75,2.8,3,3.05,3.1,3.4,3.45,3.5,3.55))
#q
q<-q + theme( strip.text.x = element_blank())
#q

p<-q+geom_point(data=fullassessments,aes(x=astimes,y=horizlines),size=3,shape=1)
p<-p+geom_text(data=fullassessments,aes(x=astimes,y=horizlines,label=rownames(fullassessments),vjust=c(-0.8,-1,-1,-1),hjust=c(-0.2,0,1,0)),size=4)+scale_y_continuous(breaks=seq(0.6,1,0.05)) 
#p<-p+ scale_x_continuous(breaks=c(0,1000,2000))+geom_point(data=d["9",],aes(x=sums,y=errates,color="red"))
#p+geom_point(data=d[3,],aes(x=sums,y=errates,color="blue")
p