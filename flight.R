rm(list=ls())
su=read.csv("Survey_data.csv",na.strings = c("",NA,NaN))
fl=read.csv("Flight+data.csv",na.strings = c("",NA,NaN))
summary(fl)
summary(su)
colnames(fl)[1]="Id"
ff=merge(x = su, y = fl, by = "Id", all.x = TRUE)
str(ff)
colSums(is.na(ff))
library(DataExplorer)
plot_missing(ff)

delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}
f1=delete.na(ff,1)
sapply(f1, function(x) sum(is.na(x)))
dim(f1)

summary(f1)
str(f1)

l1=f1[,c(2,3,4,5,7:16)]

l2=na.omit(l1)
library(reshape2)
st1=melt(l2, id = c("Satisfaction"))

#  bar plots
library(tidyverse)
st1%>%ggplot(aes(x=value))+geom_bar(position = position_dodge(),aes(fill=Satisfaction), alpha=0.7 ) +
  facet_wrap(.~variable,scales = "free_x", nrow = 3)+ggtitle("Bar Plots for Catigorical variables vs DV")+coord_flip()+
  scale_fill_manual(values=c('red','green'))+guides(fill=F)

f1%>%ggplot(aes(Gate_location,fill=Satisfaction))+geom_bar(position=position_dodge())+scale_fill_manual(values=c('red','green'))+guides(fill=F)+
  ggtitle("Travel Type vs DV")




l3=f1[,c(2,19:24)]

library(dplyr)
f1%>%group_by(Satisfaction)%>%dplyr::summarize(Avg_Age=mean(Age),Avg_dist=mean(Flight_Distance),Avg_dep_delay=mean(DepartureDelayin_Mins),
                                               Avg_arr_delay=mean(ArrivalDelayin_Mins,na.rm = T))




library(plyr)
l1=f1[,c(1,3:5,7:16)]



ll= reshape2::dcast(dplyr::mutate(reshape2::melt(l1,id.var="Id"),
    value=plyr::mapvalues(value,c("acceptable","excellent","extremely poor","good","need improvement","poor",NA),
    c(3,5,0,4,2,1,NA))),Id~variable)
ll=lapply(ll, function(x) as.integer(x))
ll=as.data.frame(ll)
f1$Gate_location=ifelse(f1$Gate_location=="very inconvinient",0,ifelse(f1$Gate_location=="Inconvinient",1,ifelse(f1$Gate_location=="need improvement",2,
                  ifelse(f1$Gate_location=="manageable",3,ifelse(f1$Gate_location=="Convinient",4,5)))))
f1=f1[,-c(1,3:5,7:16)]
ff=cbind(ll,f1)

str(ff)
summary(ff)

ff%>%filter(Inflight_entertainment>3 & Satisfaction!="satisfied")%>%dplyr::summarize(percent=round(n()/length(ff$Satisfaction),4)*100)

ff%>%filter(Inflight_entertainment>3 & Satisfaction=="satisfied")%>%dplyr::summarize(percent=round(n()/length(ff$Satisfaction),4)*100)

ff%>%select(Ease_of_Onlinebooking,Online_boarding,Online_support)%>%mutate(avg_online_rating=rowMeans(.[,1:3]))%>%group_by(avg_online_rating)%>%dplyr::summarize(count=n())%>%arrange(desc(avg_online_rating))

table(ff$Class)
table(ff$TypeTravel)                                                                           
table(ff$CustomerType)
table(ff$Gender)

ff$Gender=ifelse(ff$Gender=="Male",1,0)
ff$Class=ifelse(ff$Class=="Business",2,ifelse(ff$Class=="Eco Plus",1,0))
ff$CustomerType=ifelse(ff$CustomerType=="Loyal Customer",1,0)
ff$Satisfaction=ifelse(ff$Satisfaction=="satisfied",1,0)
ff$TypeTravel=ifelse(ff$TypeTravel=="Personal Travel",0,1)

str(ff)
ff=ff[,-1]

table(ff$Gender)

library(mice)
md.pattern(ff)
mymice=mice(ff,m=3 ,method="pmm")
mymiceComplete=complete(mymice,2)
summary(mymiceComplete)
ff=mymiceComplete
summary(ff)


library(corrplot)

corrplot(cor(ff),type="upper",method="number",number.cex = 0.7)


pc=ff[,c(1:13,15)]


ff2=scale(ff[,-14])
ff2=as.data.frame(ff2)
ff=cbind(ff$Satisfaction,ff2)
colnames(ff)[1]="Satisfaction"
ff$Satisfaction=as.factor(ff$Satisfaction)

pc=ff[,-1]
library(psych)
cortest.bartlett(pc)
KMO(pc)

cormatrix=cor(pc)
A=eigen(cormatrix)
ev=A$values
ev

library(nFactors)
####scree plot
plot(ev,xlab="Factors",ylab="Eigen Value",pch=20,col="blue")
lines(ev,col="red")
eFactors=fa(pc,nfactors = 4,rotate = "varimax",fm="wls")
eFactors
fa.diagram(eFactors)
attributes(eFactors)

pc1=cbind(pc,Digital_score=eFactors$scores[,1],service_score=eFactors$scores[,3],comfort_score=eFactors$scores[,2])
pc1=pc1[,-c(1:10,12:14)]

pc1=cbind(ff$Satisfaction,pc1)
colnames(pc1)[1]="Satisfaction"

pc1=cbind(pc1,ff[,c(16:23)])
sc=scale(pc1[,-c(1,3:5)])
sc=as.data.frame(sc)
sc=cbind(sc,pc1[,c(1,3:5)])

sc$Satisfaction=as.factor(sc$Satisfaction)
str(sc)


set.seed(123)
library(caTools)
spl=sample.split(sc$Satisfaction,SplitRatio = 0.7)
tr=subset(sc,spl==T)
ts=subset(sc,spl==F)

library(caret)
set.seed(1235)
lg=glm(Satisfaction~.,data=tr,family = binomial(link="logit"))
summary(lg)
ts$prob=predict(lg,ts,type="response")
ts$pred=ifelse(ts$prob>0.5,1,0)
ts$pred=as.factor(ts$pred)
ts$Satisfaction=as.factor(ts$Satisfaction)
confusionMatrix(ts$Satisfaction,ts$pred,positive = "1")

library(car)
vif(lg)

sc=sc[,-8]


set.seed(400)
ctrl=trainControl(method="cv",number=10,search = "random ")
knn=train(Satisfaction ~ ., data = tr, method = "knn", trControl = ctrl)
knn
plot(knn)
ts$prob=predict(knn,ts,type="prob")[,'1']
ts$pred=ifelse(ts$prob>0.5,1,0)
ts$pred=as.factor(ts$pred)
ts$Satisfaction=as.factor(ts$Satisfaction)
confusionMatrix(ts$Satisfaction,ts$pred,positive = "1")

varImp(knn)

library(rpart)
library(rpart.plot)
library(rattle)
set.seed(897)
r.ctrl=rpart.control(minisplit=10,minbucket=5,cp=0,xval=10)
dt=rpart(formula=Satisfaction~.,data=tr,control = r.ctrl)
plotcp(dt)
dt$cptable
r.ctrl=rpart.control(minisplit=50,minbucket=5,cp=0.00019,xval=10)
dt1=rpart(formula=Satisfaction~.,data=tr,control = r.ctrl)

ts$prob=predict(dt1,ts,type="prob")[,'1']
ts$pred=ifelse(ts$prob>0.5,1,0)
ts$pred=as.factor(ts$pred)
ts$Satisfaction=as.factor(ts$Satisfaction)
confusionMatrix(ts$Satisfaction,ts$pred,positive = "1")

library(randomForest)
#set seed again for randomness
set.seed(1000)
#build first RF model
rf=randomForest(Satisfaction~.,data=tr,ntree=200,mtry=3,nodesize=10,importance=T)
print(rf)
plot(rf)
#tune rf to identify the best mtry
set.seed(1000)
trrf=tuneRF(tr[,-c(9)],y=tr$Satisfaction,mtryStart = 3,stepFactor = 1.5,ntree=100,improve = 0.0001,nodesize=10,
            trace=T,plot=T,doBest = T,importance=T)
print(trrf)
plot(trrf)
rf1=randomForest(Satisfaction~.,data=tr,ntree=200,mtry=6,nodesize=10,importance=T)
plot(rf1)
print(rf1)

ts$prob=predict(rf1,ts,type="prob")[,'1']
ts$pred=ifelse(ts$prob>0.5,1,0)
ts$pred=as.factor(ts$pred)
ts$Satisfaction=as.factor(ts$Satisfaction)
confusionMatrix(ts$Satisfaction,ts$pred,positive = "1")

set.seed(40001)
ctrl=trainControl(method="cv",number=10,classProbs = T)
sv=train(make.names(Satisfaction)~., data = tr, method = "svmRadial", trControl = ctrl,tunlength=10)
sv
plot(sv)
sv$bestTune

q()
