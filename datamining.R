library(sas7bdat)
library(ggplot2)
library(dplyr)
datas<-read.csv("new.csv")
View(datas)

datas <- datas %>% select(-ID)
datas<-data.frame(datas)
#NULL값 제거하기
sorted <- sort(colSums(is.na(datas)),decreasing=TRUE)
delete <- sorted[1:129]
to_delete <- names(delete)
datas <- datas %>% select(-to_delete)
datas[1,3]
a<-nrow(datas)
table(is.na(datas))
i<-1

length(sorted)#5708
sorted[length(sorted)*0.2]

while(i<nrow(datas)& nrow(datas)>1142&ncol(datas)>129){
    sorted <- sort(rowSums(is.na(datas)),decreasing=TRUE)
    
    if(sum(is.na(datas[i,]))>sorted[length(sorted)*0.2]){
      datas<-datas[-i,]
    }
    i = i+1
}

write.csv(datas2,"tohongyeon.csv")


data_backup<-read.csv("clean.csv")
table(is.na(data_backup))
#NULL값 제거하기

#5/10일 , 아웃라이어 제거하기
table(datas$ainc)#월평균 가구총소득
sort_ainc<-sort(datas$ainc,decreasing=FALSE)
sort_ainc
boxplot(sort_ainc)
boxplot(sort_ainc)$stats

boxplot(datas$HE_BMI)#체질량지수
boxplot(datas$N_INTK)#식품섭취량 정규화
boxplot(datas$age)$stats
boxplot(datas$LQ2_ab)
cov(datas$BO1_1,datas$BO2_1)
cor.test(datas$BO1_1,datas$BO2_1)
cov(datas$BO1_1,datas$LS_1YR)

#DT 그리기
#install.packages("rpart.plot")
#install.packages("caret")
#install.packages("tree")
library(rpart)
library(rpart.plot)
library(caret)
set.seed(60)
intrain<-createDataPartition(y=datas$LS_1YR,p=0.7,list=FALSE)
train<-datas[intrain,]
test<-df[-intrain,]
library(tree)
treemod<-tree(LS_1YR~.,data=train)
plot(treemod)
text(treemod)
#이건 기본데이터고






#새로민맥스랑 pca 추가한거
#new_with_pca_minmax : 홍연이형이 준 pca 를 new에 추가한 것
datas2<-read.csv("new_with_pca_minmax.csv")
datas2<-data.frame(datas2)
#행제거 및 이상치제거
datas2<-datas2 %>% select(-psu)
datas2<-datas2 %>% select(-ID_fam)
datas2$ainc <- ifelse(datas2$ainc < 16.6667| datas2$ainc > 1301, NA, datas2$ainc)
datas2 <- rename(datas2, earn_month = ainc)
datas2 <- datas2 %>% filter(!is.na(earn_month))
#normalize<-function(x){
 # return ((x-min(x))/(max(x)-min(x)))
#}
#normalize
#datas2$ainc<-normalize(datas2$ainc)
#datas2$HE_ht<-normalize(datas2$HE_ht)
#datas2$HE_wt<-normalize(datas2$HE_wt)
#datas2$HE_BMI<-normalize(datas2$HE_BMI)
#datas2$N_INTK<-normalize(datas2$N_INTK)
#dt
datas2$BM2_3<-as.factor(datas2$BM2_3)
library(rpart)
library(rpart.plot)
library(caret)
library(tree)
intrain_datas2<-createDataPartition(y=datas2$target_ingest,p=0.7,list=FALSE)
train_datas2<-datas2[intrain_datas2,]
test_datas2<-datas2[-intrain_datas2,]
train_datas2
library(tree)
treemod_datas2<-rpart(target_ingest~.,data=train_datas2,method="class")
plot(treemod_datas2)
rpart.control(minsplit=20)

text(treemod_datas2)

prp(treemod_datas2,type=6,extra=2,digits=3)

rpartpred<-predict(ptree, test, type='class')
test$LS_1YR<-as.factor(test$target_ingest)
confusionMatrix(rpartpred, test$target_ingest)

#datas2 <- rename(datas2, marriage  = marri_1)
#datas2 <- rename(datas2, diet_therapy  = N_DIET)

#print(treemod_datas2)

#plotcp(treemod_datas2)
##############################################

# 평가
#install.packages("e1071")
rpartpred<-predict(treemod_datas2, test_datas2, type='class')
test_datas2$target_ingest<-as.factor(test_datas2$target_ingest)
confusionMatrix(rpartpred, test_datas2$target_ingest)
######################################################################

library(caret)
library(tree)
library(party)
library(rpart)
library(caret)

datas2$target_ingest<-as.factor(datas2$target_ingest)
# 에러 최소값으로 가지치기
ptree<-prune(treemod_datas2, cp= treemod_datas2$cptable[which.min(treemod_datas2$cptable[,"xerror"]),"CP"])
plot(ptree)
text(ptree)
# 평가
rpartpred<-predict(ptree, test, type='class')
test$LS_1YR<-as.factor(test$target_ingest)
confusionMatrix(rpartpred, test$target_ingest)

#####################################################################


#PC4 -> 지방산

datas2$BM2_3
install.packages("party")
library(party)


datas3<-datas2
datas3 <- datas3 %>% select(-BM2_3)

set.seed(80)
intrain<-createDataPartition(y=datas3$LS_1YR,p=0.7,list=FALSE)
train<-datas3[intrain,]
test<-datas3[-intrain,]
library(tree)

treemod<-tree(LS_1YR~.,data=train)
plot(treemod)
text(treemod)

#rpart로 해보기

tree_rpart<-rpart(LS_1YR~.,data=train,subset=train,method="class")
rpart.plot(tree_rpart)


#찬이가 준 형변환된거 factor로 
datas4<-read.csv("changed_at_type.csv")
datas4<-data.frame(datas4)
sperate <- datas4 %>% select(psu, ainc, HE_ht, HE_wt, HE_BMI, PC1, PC2, PC3, PC4, age, X.1)
sperate_name <- names(sperate) # 분리한 이름
change <- datas4 %>% select(-sperate_name) # factor로 바꿀 테이블

change_name <- names(change) # factor로 바꿀 이름

for(i in change_name){
  change[,i] <- as.factor(change[,i]) # 형변환
}

# change + sperate 병합
change[,"X.1"] = 1:3094
datas4_new <- left_join(change, sperate, by = "X.1")
str(datas4_new)
set.seed(300)
intrain2<-createDataPartition(y=datas4_new$LS_1YR,p=0.7,list=FALSE)
train2<-datas4_new[intrain2,]
test2<-datas4_new[-intrain2,]
treemoda<-rpart(LS_1YR~.,data=train2)
rpart.plot(treemoda)
plot(treemoda)
text(treemoda)


str(datas4_new)

View(datas4_new)



#주성
absolute<-read.csv(file="clean.csv") #pca ????
absolute<-data.frame(absolute)

normalize<-function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
#normalize
absolute$ainc<-normalize(absolute$ainc)
absolute$HE_ht<-normalize(absolute$HE_ht)
absolute$HE_wt<-normalize(absolute$HE_wt)
absolute$HE_BMI<-normalize(absolute$HE_BMI)
absolute$N_INTK<-normalize(absolute$N_INTK)
#dt
absolute$BM2_3<-as.factor(absolute$BM2_3)
library(rpart)
library(rpart.plot)
library(caret)
set.seed(3000)
intrain<-createDataPartition(y=absolute$LS_1YR,p=0.7,list=FALSE)
train<-absolute[intrain,]
test<-absolute[-intrain,]
library(tree)
treemod<-tree(LS_1YR~.,data=train)
plot(treemod)
text(treemod)
#PC4 -> ??????
absolute$BM2_3
library(party)

#홍연
datas2 <- rename(datas2, target_ingest = LS_1YR)
datas2 <- rename(datas2, make_energy_series =  PC1)
datas2 <- rename(datas2, fat_series = PC2)
datas2 <- rename(datas2, vitamin_A_series = PC3)
datas2 <- rename(datas2, fatty_acid_series = PC4)
datas2 <- rename(datas2, fatty_acid_series = PC4)
datas2 <- rename(datas2, smoking = BS1_1)
set.seed(300)
intrain<-createDataPartition(y=datas2$LS_1YR,p=0.7,list=FALSE)
train<-datas2[intrain,]
test<-datas2[-intrain,]
library(tree)
treemod<-tree(target_ingest~.,data=train)
plot(treemod)
text(treemod)
#PC4 -> ??????
temp <- datas2 %>% select(region)

temp <- as.data.frame(temp)

temp$region <- ifelse(temp$region %in% c(1,4,6,8,9,11,12),1 , 0)

temp <- rename(temp, top_bottom_region = region)

datas2 <- cbind(datas2,temp)



##############
library(sas7bdat)
library(ggplot2)
library(dplyr)

datas2<-read.csv("new_with_pca_minmax.csv")
datas2<-data.frame(datas2)
normalize<-function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
#normalize
datas2$ainc<-normalize(datas2$ainc)
datas2$HE_ht<-normalize(datas2$HE_ht)
datas2$HE_wt<-normalize(datas2$HE_wt)
datas2$HE_BMI<-normalize(datas2$HE_BMI)
datas2$N_INTK<-normalize(datas2$N_INTK)


#홍연
datas2 <- rename(datas2, target_ingest = LS_1YR)
datas2 <- rename(datas2, make_energy_series =  PC1)
datas2 <- rename(datas2, fat_series = PC2)
datas2 <- rename(datas2, vitamin_A_series = PC3)
datas2 <- rename(datas2, fatty_acid_series = PC4)
datas2 <- rename(datas2, fatty_acid_series = PC4)
datas2 <- rename(datas2, smoking = BS1_1)


temp <- datas2 %>% select(region)

temp <- as.data.frame(temp)

temp$region <- ifelse(temp$region %in% c(1,4,6,8,9,11,12),1 , 0)

temp <- rename(temp, top_bottom_region = region)

datas2 <- cbind(datas2,temp)
library(tree)
library(rpart)
library(rpart.plot)
library(caret)
set.seed(600)
intrain_datas2<-createDataPartition(y=datas2$target_ingest,p=0.7,list=FALSE)
train_datas2<-datas2[intrain_datas2,]
test_datas2<-datas2[-intrain_datas2,]
treemod_datas2<-rplot(target_ingest~.,data=train_datas2)
plot(treemod_datas2)
text(treemod_datas2)
print(treemod_datas2)

plotcp(treemod_datas2)

