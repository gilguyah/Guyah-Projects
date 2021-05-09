library(readxl)
redwine <- read_excel("wine_red.xls")
head(redwine)
df1 <- redwine
head(df1)
df1<- df1[,-6:-7]
head(df1)
df1<- df1[,-7:-8]
library("ggplot2")
library(reshape2)
ggplot(data = melt(df1), mapping = aes(x = value)) + geom_histogram(bins = 20) + facet_wrap(~variable, scales = 'free_x')
p1<-ggplot(df1,aes(x=fixed.acidity))+
   geom_histogram(color =I('black'),fill = I('#099009'))+
  ggtitle('Fixed Acidity for Red wine')
p2<-ggplot(df1,aes(x=volatile.acidity))+
   geom_histogram(color =I('black'),fill = I('#099009'))+
  ggtitle('Volatile Acidity for Red wine')
p3<-ggplot(df1,aes(x=citric.acid))+
   geom_histogram(color =I('black'),fill = I('#099009'))+
  ggtitle('Citric Acid for Red wine')
p4<-ggplot(df1,aes(x=free.sulfur.dioxide))+
   geom_histogram(color =I('black'),fill = I('#099009'))+
  ggtitle('Free SO2 distribution for Red wine')
p5<-ggplot(df1,aes(x=total.sulfur.dioxide))+
   geom_histogram(color =I('black'),fill = I('#099009'))+
  ggtitle('Total SO2 distribution for Red wine')
p6<-ggplot(df1,aes(x=sulphates))+
   geom_histogram(color =I('black'),fill = I('#099009'))+
  ggtitle('Total Sulphates for Red wine')
library (gridExtra)

grid.arrange(p1,p2,p3,p4,p5,p6)
summary(df1)
m1<-p1+ geom_vline(aes(xintercept=mean(fixed.acidity)),
            color="blue", linetype="dashed", size=1)
m2<-p2+ geom_vline(aes(xintercept=mean(volatile.acidity)),
            color="blue", linetype="dashed", size=1)
m3<-p3+ geom_vline(aes(xintercept=mean(citric.acid)),
            color="blue", linetype="dashed", size=1)
m4<-p4+ geom_vline(aes(xintercept=mean(free.sulfur.dioxide)),
            color="blue", linetype="dashed", size=1)
m5<-p5+ geom_vline(aes(xintercept=mean(total.sulfur.dioxide)),
            color="blue", linetype="dashed", size=1)
m6<-p6+ geom_vline(aes(xintercept=mean(sulphates)),
            color="blue", linetype="dashed", size=1)
grid.arrange(m1,m2,m3,m4,m5,m6)
summary(df1)

b1 <- ggplot(df1,aes(x=factor(0),fixed.acidity))+ geom_boxplot()
b2 <-ggplot(df1,aes(x=factor(0),volatile.acidity))+ geom_boxplot()
b3 <- ggplot(df1,aes(x=factor(0),citric.acid))+ geom_boxplot()
b4 <- ggplot(df1,aes(x=factor(0),residual.sugar))+ geom_boxplot()
b5 <- ggplot(df1,aes(x=factor(0),chlorides))+ geom_boxplot()
b6 <- ggplot(df1,aes(x=factor(0),free.sulfur.dioxide))+ geom_boxplot()
b7 <- ggplot(df1,aes(x=factor(0),total.sulfur.dioxide))+ geom_boxplot()
b8 <- ggplot(df1,aes(x=factor(0),density))+ geom_boxplot()
b9 <- ggplot(df1,aes(x=factor(0),pH))+ geom_boxplot()
b10 <- ggplot(df1,aes(x=factor(0),sulphates))+ geom_boxplot()
b11 <- ggplot(df1,aes(x=factor(0),alcohol))+ geom_boxplot()
b12 <- ggplot(df1,aes(x=factor(0),quality))+ geom_boxplot()
grid.arrange(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12)
library("tidyr")
df2 <-gather(df1)
library(reshape2)
## red wines
library(readxl)
winequality_red <- read_excel("wine_red.xls")
rw <- winequality_red
rw$class <-rw$quality
rw$class <-gsub("3","low", rw$class)
rw$class <-gsub("4","low", rw$class)
rw$class <-gsub("5","medium", rw$class)
rw$class <-gsub("6","medium", rw$class)
rw$class <-gsub("7","high", rw$class)
rw$class <-gsub("8","high", rw$class)
rw$class <-gsub("9","high", rw$class)

rw
sapply(rw, class)
library(reshape2)
library(randomForest)
library(kernlab)
library(ggplot2)
library(gdata)
library(RSQLite)
library(sqldf)
library(tidyverse)
library(tidyr)
library(plyr)
library(e1071)
library(caret)
rw$class <- as.factor(rw$class)
str(rw)
table(rw$class)
barplot(table(rw$class))
table(rw$quality)
table(rw$class)
library(RSQLite)
library(sqldf)
library(tidyverse)
library(tidyr)
library(plyr)
library(e1071)
library(caret)
set.seed(123)
train <- sample(nrow(rw), 0.7*nrow(rw))
ValidTest <- rw[-train,]
TrainSet <- rw[train,]
num_exmps = nrow(rw)
clf_12 <- randomForest(class~. -quality, data=rw)
clf_12
pred <- predict(clf_12, newdata=ValidTest)
table(pred, ValidTest$class)
(51+17+412)/nrow(ValidTest)
pred2 <- predict(clf_12, newdata=TrainSet)
table(pred2, TrainSet$class)
(166 + 46 + 907)/nrow(TrainSet)

clf_12 <- randomForest(class~. -quality, data=rw, ntree=300, mtry=8, importance= TRUE, proximity = TRUE)
print(clf_12)
round(((129+2+1268)/nrow(rw))*100,2)
varImpPlot(clf_12)
rf3 <- randomForest(class ~.-quality, data= ValidTest, ntree=300, mtry=8, importance= TRUE, proximity = TRUE)


print(rf3)
varImpPlot(rf3)
round(((21+1+394)/nrow(ValidTest))*100,2)
pred12 <- predict(rf3,data= ValidTest )
table(pred12, ValidTest$class)
plot(x=rw$class, y=rw$alcohol,xlab="class",ylab="alcohol", xlim=c(2, 10), ylim=c(10, 400))
ggplot(data= rw,aes(x=alcohol,y=volatile.acidity))+ geom_point(alpha= 1, aes(color= class))
criticacid<- ggplot(data= rw,aes(x=alcohol,y=citric.acid))+ geom_point(alpha= 1, aes(color= class))
fixacid<- ggplot(data= rw,aes(x=alcohol,y=fixed.acidity))+ geom_point(alpha= 1, aes(color= class))
ggplot(data= rw,aes(x=alcohol,y=sulphates))+ geom_point(alpha= 1, aes(color= class))
densityp<- ggplot(data= rw,aes(x=alcohol,y=density))+ geom_point(alpha= 1, aes(color= class))
sugar<-ggplot(data= rw,aes(x=alcohol,y=residual.sugar))+ geom_point(alpha= 1, aes(color= class))
ggplot(data= rw,aes(x=alcohol,y=total.sulfur.dioxide))+ geom_point(alpha= 1, aes(color= class))
library(gridExtra)
gridExtra::grid.arrange(densityp, sugar, fixacid, criticacid)

