train<-read.csv("train.csv") #读取train集
test <- read.csv("test.csv") #读取test集
library(dplyr)
titanic<-bind_rows(train,test) #合并train集和test集
str(titanic) #查看数据结构
summary(titanic) #查看数据总结
sapply(titanic,function(x) sum(is.na(x)))#查看缺失值,survived缺失值418（需预测的），age缺失值263，fare缺失值1
sapply(titanic,function(x) sum(x==""))#查看空值，Embarked空值数2，Cabin空值1014

titanic$Title<-sapply(titanic$Name,function(x) {strsplit(x,split="[,.]")[1][2]})
data$Title <- sub(' ', '', titanic$Title)
strsplit(titanic$Title,",")[[1]]
table(titanic$Sex,titanic$Title)
strsplit(titanic$Name,"[.]")[1:891][1:891][1:891]#Family Name
titanic$Title<-strsplit(titanic$Name[1:957],"[,.]")[2]#Title

library(ggplot2)
titanic$Survived<-as.factor(titanic$Survived) 
ggplot(data = titanic[1:891,],mapping = aes(x = Sex,fill=Survived))+ 
  geom_bar(stat = "count",position = "dodge")+ 
  xlab("性别")+
  ylab("人数")+
  ggtitle("性别和存活情况的关系")+
  theme_classic(base_size = 12)+ #标题大小
  scale_fill_manual(values = c("red1","green2"))+ #图形填充使用红色、绿色
  geom_text(mapping = aes(label=..count..,vjust=-0.1),stat = "count",position = position_jitterdodge(jitter.width = 0,jitter.height = 0.75,dodge.width =1 ))
library(InformationValue)
WOETable(X=titanic$Sex[1:891], Y=titanic$Survived[1:891])
IV(X=titanic$Sex[1:891], Y=titanic$Survived[1:891])


titanic$Pclass<-as.factor(titanic$Pclass)
ggplot(data = titanic[1:891,],mapping = aes(x = Pclass,fill=Survived))+ #客舱等级与存活情况的关系
  geom_bar(stat = "count",position = "dodge")+
  xlab("客舱等级")+
  ylab("人数")+
  ggtitle("客舱等级和存活情况的关系")+
  theme_classic(base_size = 12)+
  scale_fill_manual(values = c("skyblue","brown2"))+ #填充颜色
  geom_text(mapping = aes(label=..count..,vjust=-0.1),stat = "count",position = position_dodge(width = 1))

WOETable(X=titanic$Pclass[1:891], Y=titanic$Survived[1:891])
IV(X=titanic$Pclass[1:891], Y=titanic$Survived[1:891])


ggplot(data=titanic[1:891,],mapping = aes(x=SibSp,fill=Survived))+
  geom_bar(stat = "count",position = "dodge")+
  xlab("船上父母及子女数量")+
  ylab("人数")+
  ggtitle("船上父母及子女数与存活情况的关系")+
  theme_classic(base_size = 12)+
  scale_fill_manual(values=c("red2","green2"))+
  geom_text(aes(label=..count..,vjust=-0.1),stat="count",position=position_dodge(width = 1))+
  scale_x_continuous(breaks = seq(0,8,1))


ggplot(data=titanic[1:891,],mapping = aes(x=Parch,fill=Survived))+
  geom_bar(stat = "count",position = "dodge")+
  xlab("船上兄弟姐妹数量")+
  ylab("人数")+
  ggtitle("船上兄弟姐妹数量与存活情况的关系")+
  theme_classic(base_size = 12)+
  scale_fill_manual(values=c("tan","gray65"))+
  geom_text(aes(label=..count..,vjust=-0.1),stat = "count",position = position_dodge(width = 1))+
  scale_x_continuous(breaks = seq(0,6,1))


titanic$SibSp<-as.numeric(titanic$SibSp)
titanic$Parch<-as.numeric(titanic$Parch)
titanic<-transform(titanic,HouseholdSize=SibSp+Parch+1)
str(titanic)
ggplot(data=titanic[1:891,],mapping = aes(x=HouseholdSize,fill=Survived))+
  geom_bar(stat = "count",position = "dodge")+
  xlab("家庭规模大小")+
  ylab("人数")+
  ggtitle(("家庭规模大小与存活情况的关系"))+
  theme_classic(base_size = 12)+
  scale_fill_manual(values = c("lightblue","lightgreen"))+
  geom_text(aes(label=..count..,vjust=-0.1),stat = "count",position = position_dodge(width = 1))+
  scale_x_continuous(breaks = seq(0,12,1))

WOETable(X=factor(titanic$HouseholdSize[1:891]), Y=titanic$Survived[1:891])
IV(X=factor(titanic$HouseholdSize[1:891]), Y=titanic$Survived[1:891])


sapply(titanic,function(x) sum(x==""))#查看空值，Embarked空值数2，Cabin空值1014
sapply(titanic,function(x) sum(x==""))#查看空值，Embarked空值数2，Cabin空值1014


str(titanic)
titanic$Title<-sapply(titanic$Name,function(x) {str(x,'[,]')[1][2]})
strsplit(titanic$Title,"[,.]")[[1]]
table(titanic$Sex,titanic$Title)
strsplit(titanic$Name,"[.]")[[1]][2]#Family Name
strsplit(titanic$Name,"[,.]")[[1]][2]#Title


titanic$Title<-as.factor(gsub("(.+, )|(\\..+)","",titanic$Name))#Title
titanic$FamilyName<-as.factor(gsub("(.+\\.)","",titanic$Name))#Family Name
gsub("(.*\\.)","",titanic$Name)#Family Name
table(titanic$Sex,titanic$Title)#列联表

titanic$Title[titanic$Title%in%c("Capt","Col","Don","Jonkheer","Major","Male","Rev","Sir")]<-"Sir"
titanic$Title[titanic$Title%in%c("Dona","Lady","Mlle","Mme","Ms","the Countess")]<-"Lady"
table(titanic$Sex,titanic$Title)#列联表

ggplot(data = titanic[1:891,],mapping = aes(x=Title,y=..count..,fill=Survived))+
  geom_bar(stat = "count",position = "dodge")+
  xlab("称呼")+
  ylab("人数")+
  ggtitle("称呼与存活情况```````````````````````的关系")+
  geom_text(aes(label=..count..,vjust=-0.1),stat="count",position = position_dodge(width = 1))

WOETable(X=titanic$Title[1:891], Y=titanic$Survived[1:891])
IV(X=titanic$Title[1:891], Y=titanic$Survived[1:891])




TicketCount <- by(titanic,titanic$Parch,function(x) sum(!is.na(x)))
TicketCount <- aggregate.data.frame(titanic$Ticket, by=list(titanic$Ticket),function(x) sum(!is.na(x)))
summary(TicketCount)
titanic$TicketCount <- apply(titanic, 1, function(x) TicketCount[which(TicketCount[, 1] == x['Ticket']), 2])
titanic$TicketCount <- factor(sapply(titanic$TicketCount, function(x) ifelse(x > 1, '共用票号', '单独票号')))
ggplot(data = titanic[1:891,],mapping = aes(x=TicketCount,fill=Survived))+
  geom_bar(stat = "count",position = "dodge")+
  xlab("船票编号")+
  ylab("人数")+
  ggtitle("船票编和存活情况的关系")+
  geom_text(aes(label=..count..,vjust=-0.1),stat = "count",position = position_dodge(width = 1))

WOETable(X=factor(titanic$TicketCount[1:891]), Y=titanic$Survived[1:891])
IV(X=factor(titanic$TicketCount[1:891]), Y=titanic$Survived[1:891])




library(VIM)
aggr(titanic,prop=F,number=T) # rain集，年龄缺失177
hist1<-hist(titanic$Age,main = "原数据年龄分布",col = "lightblue")
library(rpart)
age_model <- rpart(Age~Pclass + Sex + Fare + Embarked + HouseholdSize, data =titanic[!is.na(titanic$Age), ],method = "anova", na.action = na.omit)
titanic$Age[is.na(titanic$Age)] <- predict(age_model, titanic[is.na(titanic$Age), ])
aggr(titanic,prop=T,number=F) #查看缺失值，坐标显示百分数=F，坐标显示值=T
hist2<-hist(titanic$Age,main="插补后年龄分布",col = "lightgreen")

ggplot(data=titanic[1:891,],mapping = aes(x=Age,color=Survived))+#插补后年龄与存活情况的关系
  geom_line(stat="bin",binwidth=2)+ #设置组距
  xlab("年龄")+
  ylab("人数")+
  ggtitle("年龄与存活情况的关系")+
  theme_classic(base_size = 12)+
  scale_color_manual(values=c("red","blue"))+
  scale_x_continuous(breaks = seq(0,80,5)) #设置X轴坐标轴刻度范围&间隔

titanic$Age_new[titanic$Age<16]<-"child"
titanic$Age_new[titanic$Age>=16&titanic$Age<30]<-"young"
titanic$Age_new[titanic$Age>=30&titanic$Age<40]<-"middle1"
titanic$Age_new[titanic$Age>=40&titanic$Age<60]<-"middle2"
titanic$Age_new[titanic$Age>=60]<-"elder"




WOETable(X=factor(titanic$Age[1:891]), Y=titanic$Survived[1:891])
IV(X=factor(titanic$Age[1:891]), Y=titanic$Survived[1:891])
WOETable(X=factor(titanic$Age_new[1:891]), Y=titanic$Survived[1:891])
IV(X=factor(titanic$Age_new[1:891]), Y=titanic$Survived[1:891])



ggplot(data=titanic[1:891,],mapping = aes(x=Fare,color=Survived))+
  geom_line(stat="bin",binwidth=10)+
  xlab("船票价格")+
  ylab("人数")+
  ggtitle("船票价格与存活情况的关系")+
  theme_classic(base_size = 12)+
  scale_color_manual(values=c("tomato","blue"))+
  scale_x_continuous(breaks = seq(0,520,25)) #设置X轴坐标轴刻度范围&间隔

which(is.na(titanic$Fare))
titanic[1044,]#查看编号1044的乘客的信息：位于低级客舱，登船港口S，票号3701
ggplot(data=titanic[titanic$Pclass=="3"&titanic$Embarked=="S",],mapping=aes(x=Fare))+
  geom_density(fill = "tomato")+#核密度图
  geom_vline(aes(xintercept=median(Fare,na.rm = T)))+#中位数
  scale_x_continuous(breaks = seq(0,60,5))
titanic$Fare[1044]<-median(titanic$Fare[titanic$Pclass=="3"&titanic$Embarked=="S"],na.rm = T)



WOETable(X=factor(titanic$Fare[1:891]), Y=titanic$Survived[1:891])
IV(X=factor(titanic$Fare[1:891]), Y=titanic$Survived[1:891])

titanic$Fare_new[titanic$Fare<15]<-"Fare1"
titanic$Fare_new[titanic$Fare>=15&titanic$Fare<25]<-"Fare2"
titanic$Fare_new[titanic$Fare>=25&titanic$Fare<40]<-"Fare3"
titanic$Fare_new[titanic$Fare>=40&titanic$Fare<75]<-"Fare4"
titanic$Fare_new[titanic$Fare>=75]<-"Fare5"
titanic$Fare_new[titanic$Fare>75]<-"Fare6"
WOETable(X=factor(titanic$Fare_new[1:891]), Y=titanic$Survived[1:891])
IV(X=factor(titanic$Fare_new[1:891]), Y=titanic$Survived[1:891])


sapply(titanic,function(x) sum(is.na(x)))#查看缺失值
sapply(titanic,function(x) sum(x==""))#查看空值,Embarked 有两个空值,Cabin有1014个空值。
titanic$Embarked[titanic$Embarked==""]<-NA
which(is.na(titanic$Embarked))
titanic[c(62,830),]#查看Embarked缺失值数据,高级客舱，票价80
ggplot(data = titanic[1:891,],mapping=aes(x=Pclass,y=Fare,fill=Embarked))+
  geom_boxplot()+
  geom_hline(yintercept = 80)#80为票价
titanic$Embarked[is.na(titanic$Embarked)]<-"C"
which(is.na(titanic$Embarked))

ggplot(data=titanic[1:891,],mapping=aes(x=factor(Embarked),y=..count..,fill=Survived))+
  geom_bar(stat = "count",position = "dodge")+
  xlab("登船港口")+
  ylab("人数")+
  ggtitle("登船港口和存活情况的关系")+
  theme_classic(base_size = 12)+
  scale_fill_manual(values = c("lightblue","lightgreen"))+
  geom_text(aes(label=..count..,vjust=-0.1),stat = "count",position = position_dodge(width = 1))

WOETable(X=factor(titanic$Embarked[1:891]), Y=titanic$Survived[1:891])
IV(X=factor(titanic$Embarked[1:891]), Y=titanic$Survived[1:891])



titanic$Cabin[titanic$Cabin==""]<-NA
titanic$CabinArea<-as.factor(sapply(titanic$Cabin,function(x) substr(x,1,1)))
ggplot(data=titanic[1:891,],mapping = aes(x=CabinArea,y=..count..,fill=Survived))+
  geom_bar(stat = "count",position = "dodge")+
  xlab("客舱区域")+
  ylab("人数")+
  ggtitle("客舱区域与存活情况的关系")+
  theme_classic(base_size = 12)+
  geom_text(aes(label=..count..,vjust=-0.1),stat = "count",position = position_dodge(width = 1))


WOETable(X=titanic$CabinArea[1:891], Y=titanic$Survived[1:891])
IV(X=titanic$CabinArea[1:891], Y=titanic$Survived[1:891])








titanic$CabinArea_new<-titanic$CabinArea[titanic$CabinArea==NA]
ggplot(data = titanic[1:891,],mapping=aes(x=CabinArea,fill=Fare_new))+
  geom_bar()
ggplot(data = titanic[1:891,],mapping=aes(x=CabinArea,fill=Pclass))+
  geom_bar()
ggplot(data = titanic[1:891,],mapping=aes(x=CabinArea,fill=Embarked))+
  geom_bar(stat = "count")
ggplot(data = titanic[titanic$Pclass==1,],mapping=aes(x=CabinArea,fill=Embarked,na.rm = F))+
  geom_bar()
ggplot(data = titanic[titanic$Pclass==2,],mapping=aes(x=CabinArea,fill=Embarked,na.rm = F))+
  geom_bar()
ggplot(data = titanic[titanic$Pclass==3,],mapping=aes(x=CabinArea,fill=Embarked,na.rm = F))+
  geom_bar()
ggplot(data = titanic[titanic$Pclass==1&titanic$Embarked=="C",],mapping=aes(x=CabinArea_new,fill=Fare_new,na.rm = F))+
  geom_bar()
ggplot(data = titanic[titanic$Pclass==1&titanic$Embarked=="C",],mapping=aes(x=CabinArea,fill=Fare_new,na.rm = F))+
  geom_bar()
ggplot(data = titanic[titanic$Pclass==1&titanic$Embarked=="S",],mapping=aes(x=CabinArea,na.rm = F))+
  geom_bar()
ggplot(data = titanic[titanic$Pclass==1,],mapping=aes(x=CabinArea_new,fill=Embarked,na.rm = F))+
  geom_bar()

titanic$CabinArea_new[titanic$Pclass==1&titanic$Embarked=="Q"]<-"C"
titanic$CabinArea_new[titanic$Pclass==1&titanic$Embarked=="C"]<-"D"
titanic$CabinArea_new[titanic$Pclass==1&titanic$Embarked=="S"]<-"D"

titanic$CabinArea_new[titanic$Pclass==2]<-"F"
titanic$CabinArea_new[titanic$Pclass==3]<-"G"






titanic$Survived<-as.factor(titanic$Survived) 
titanic$Age<-as.factor(titanic$Age)
titanic$Age_new<-as.factor(titanic$Age_new)
titanic$Fare<-factor(titanic$Fare)
titanic$Fare_new<-factor(titanic$Fare_new)
titanic$HouseholdSize<-factor(titanic$HouseholdSize)
titanic$Embarked<-factor(titanic$Embarked)
titanic$CabinArea_new<-factor(titanic$CabinArea_new)

train<-titanic[1:891,]
test<-titanic[892:1309,]
str(titanic)
summary(titanic)
library(randomForest)
set.seed(754)
model1<-randomForest(as.factor(Survived) ~ Sex + Pclass + HouseholdSize + Title + TicketCount + Embarked + Age_new + Fare_new,data = train,ntree=500,proximity=TRUE,importance=TRUE)
importance(model1,type=1)  #重要性评分  
importance(model1,type=2)  #Gini指数 
varImpPlot(model1)         #可视化
prediction1 <-predict(model1,test)
output1<-data.frame(PassengerId=test$PassengerId,Survived=prediction1 )
write.csv(output1,file = "prediction1.csv",row.names = FALSE)

library(party)
set.seed(415)
model2<-cforest(as.factor(Survived) ~ Sex + Pclass + HouseholdSize + CabinArea + Title + TicketCount + Embarked + Age + Fare,data = train, controls = cforest_unbiased(ntree=500,mtry=3))
prediction2 <-predict(model2,test, OOB=TRUE, type = "response")
output2<-data.frame(PassengerId=test$PassengerId,Survived=prediction2 )
write.csv(output2,file = "prediction2.csv",row.names = FALSE)


memory.limit(8120)
memory.size(NA)
memory.size(T)
memory.size(F)
