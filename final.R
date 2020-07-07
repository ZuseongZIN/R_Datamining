########################################################
#데이터를 transaction 형태로 변환 위한 전처리
rm(list = ls())
new <- read.csv("newnew.csv")
new <- new %>% select(-X, -town_t, -ho_incm5, -cfam, -allownc, -house, -live_t,-marri_1, -tins, -npins, -BD1, -BD7_5,-BA2_22, -BP6_2 , -BP6_31, -BP7, -mh_stress, -BS9_2, -BS13, -BS12_1, -sm_presnt, -HE_wt, -HE_ht, -BM1_0, -BM2_4, - BM2_5, -BM13, -BM8, -BM14, -T_ex, -L_BR_TO, -L_LN_TO, -L_DN_TO, -LK_EDU, -LK_LB_CO, -N_DUSUAL, -make_energy_series, -fat_series, -vitamin_A_series, -fatty_acid_series,-LF_BUYER, -LK_LB_IT, -LQ2_ab )
new$earn_month <- ifelse(new$earn_month < 410.1667, 0, 1)
new$genertn <- ifelse(new$genertn %in% c(4,5),1 , 0) #미혼자녀 + 부모(편부모) : 1, 그 외 0
new$BO1 <- ifelse(new$BO1 %in% c(4, 5),1 , 0) # 자기체형을 뚱뚱하다고 인식 1, 마르다고 인식 0
new$BO1_1 <- ifelse(new$BO1_1 %in% c(2, 3),1 , 0) # 1년간 체형변화 있음:1, 없음:0
new$BO2_1 <- ifelse(new$BO2_1 %in% c(1, 2, 3),1 , 0) # 체중변화 시도:1, 없음:0
new$BP1 <- ifelse(new$BP1 %in% c(1, 2),1 , 0) # 스트레스 많음:1, 스트레스 조금:0
new$smoking <- ifelse(new$smoking %in% c(1, 2),1 , 0) # 흡연 5갑이상:1, 흡연 경험 5값 이하:0
new$HE_BMI <- ifelse(new$HE_BMI < 23.86631, 0, 1) # bmi 중간아래 : 0, bmi 중간값 이상:1
new$BM7 <- ifelse(new$BM7 %in% c(1, 2, 3),1 , 0) # 씹기문제있음:1, 문제 없음:0
new$L_BR_FQ <- ifelse(new$L_BR_FQ %in% c(1),1 , 0) # 주 아침식사 5회이상:1, 문제 없음:0
new$L_LN_FQ <- ifelse(new$L_LN_FQ %in% c(1),1 , 0) # 주 점심식사 5회이상:1, 문제 없음:0
new$L_DN_FQ <- ifelse(new$L_DN_FQ %in% c(1),1 , 0) # 주 저녁식사 5회이상:1, 문제 없음:0
new$L_OUT_FQ <- ifelse(new$L_OUT_FQ %in% c(1,2,3,4),1 , 0) # 주 3,4회 이상 외식:1, 주1,2회 이하:0
new$target_ingest <- ifelse(new$target_ingest == "NO",0 , 1) # 타겟인제스트 복용:1, 비복용:0
new$N_DIET <- ifelse(new$N_DIET %in% c(2),0 , 1) # 식단조절함:1, 조절안함:0
new$LF_SAFE<- ifelse(new$LF_SAFE %in% c(1),1 , 0) # 식생활 좋음:1, 안좋음:0
new$region <- ifelse(new$region %in% c(1,4,6,8,9,11,12),1 , 0) #윗지방 : 1, 아랫지방:0
new$age <- ifelse(new$age > 53 ,1 , 0) #평균이상나이 : 1, 평균이하나이:0
new$sex < - ifelse(new$sex == "2" ,0 , 1) #남자 : 1, 여자:0
new

df <- data.frame(ID=integer(), Item=character()) # 데이터프레임 껍데기 생성
table(df)
 
col <- length(colnames(new)) # test의 열 수
row <- length(row.names(new)) # test의 행 수
col_names <- colnames(new)
 
# test[1,2] 1행 2열 값 추출
new
count <- 1
for(i in 1:row){
  for(j in 1:col){
    if(new[i,j] == 1){ # 만약 값이 1이면 df에 (id, input_colnames)삽입
      input_colnames <- col_names[j] # 대입하는 속성 이름
      newrow = data.frame(ID=count, Item=input_colnames) # 아래와 함께 행 결합
      df <- rbind(df, newrow)
    }
  }
  count = count + 1
}
 
df
df$Item <- as.character(df$Item) # 형변환
str(df)
# id에 대한 구매 데이터 완료
# 진짜 거래 데이터로 변환
 
# 먼저 id별 구매 데이터 분리
library("arules")
df.list <- split(df$Item, df$ID)
df.list
 
# 거래 데이터로 변환
df.trans <- as(df.list, "transactions")
df.trans
image(df.trans)
summary(df.trans)
 
df.rules <- apriori(df.trans)
summary(df.rules)
 
rules <- apriori(df.trans, parameter=list(support=0.1, confidence=0.9,
                                          target="rules"))
summary(rules)
inspect(rules)
 
######### 타켓값 지정 가능, 원하지 않는 At 제거 가능############
rule_s <- apriori(df.trans, parameter=list(support=0.1,confidence=0.6),
                  appearance=list(rhs="target_ingest",default='lhs', none=c("L_LN_FQ","L_DN_FQ")))
inspect(rule_s)
##################
 
 
install.packages("arulesViz")
library(arulesViz)
plot(rule_s)
plot(rule_s, method = "paracoord", control = list(reorder = TRUE))
plot(rule_s, method = "graph", control = list(type = "items"))
plot(rule_s, method = "graph")
plot(rule_s, method = "grouped")
 
################################################################3
# 군집 분석

## 연속형 변수 추출 및 정규화
start <- read.csv("newnew.csv")
k <- start %>% select(earn_month, age, make_energy_series,HE_ht,HE_wt)
g <- k %>% head(100)
gg <- scale(g)
k <- scale(k)
 
## Distance measures 분석 
res.dist <- get_dist(k, stand = TRUE, method = "pearson")
fviz_dist(res.dist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
 
## Partitioning clustering  최적의 클러스터링 개수 파악 
 
fviz_nbclust(k, kmeans, method = "gap_stat")
 
## 클러스터링 계산 
 
set.seed(123)
km.res <- kmeans(k, 4, nstart = 25)
# Visualize
fviz_cluster(km.res, data = k,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())
 
##요인분석
pca.res<-PCA(k, graph=FALSE)
fviz_contrib(pca.res, choice="var", axes=1, top=4)
fviz_contrib(pca.res, choice="var", axes=2, top=4)
 
raw<-read.csv("newnew.csv")
x <- raw$make_energy_series
y<- raw$age
 
xy <- data.frame(cbind(x,y))
 
plot(xy, pch = 19, xlab = c("x coordinate"), ylab = c("y coordinate"), 
           xlim = c(-5, 5), ylim = c(20,80), 
           main = "scatter plot of xy")
 
# adding student label
text(xy[,1], xy[,2], labels = abbreviate(rownames(xy)), 
         +      cex = 0.8, pos = 1, col = "blue") # pos=1 : at the bottom
 
 
# adding dotted line
abline(v=c(3), col = "gray", lty = 2) # vertical line
abline(h=c(3), col = "gray", lty = 2) # horizontal line
 
hc_cent <- hclust(dist(xy)^2, method="centroid")
hc_cent
 
my_par = par(no.readonly = TRUE)
par(oma = c(0, 0, 1, 0))
par(mfrow = c(1, 2))
plot(hc_cent)
plot(hc_cent, hang = -1) # hang = -1 : line from the bottom
 
#######################################################################
 
# 정규분포 파괴된 데이터 생성
 
str(raw)
 
age<-raw$age
age
hist(age, breaks=10) # 모든 나이 100명씩 샘플링 하면 된다...
# 방법 - 1. filter를 써서 20~30 / 30 ~ 40 / 40 ~ 50 / 50 ~ 60 / 60 ~ 70 / 70 ~ 80 를 나눈다
# 2. 각각에서 100개씩 샘플링 한다
# 3. 샘플링 한걸 합친다
# 4. 합친걸로 돌려본다
 
two_3<- raw %>% filter(age >= 20 & age < 30) # 20 ~ 30
three_4<- raw %>% filter(age >= 30 & age < 40) # 30 ~ 40
four_5<- raw %>% filter(age >= 40 & age < 50) # 40 ~ 50
five_6<- raw %>% filter(age >= 50 & age < 60) # 50 ~ 60
six_7<- raw %>% filter(age >= 60 & age < 70) # 60 ~ 70
seven_8<- raw %>% filter(age >= 70 & age < 80) # 70 ~ 80
 
# 랜덤 샘플링
two3 <- two_3[sample(nrow(two_3), 60),]
three4 <- three_4[sample(nrow(three_4), 60),]
four5 <- four_5[sample(nrow(four_5),60),]
five6 <- five_6[sample(nrow(five_6), 60),]
six7 <- six_7[sample(nrow(six_7), 60),]
seven8 <- seven_8[sample(nrow(seven_8), 60),]
 
random <- rbind(two3, three4, four5, five6, six7, seven8)
random
 
# 군집분석 실시
 kmeans()
 {
require(graphics)x<-cbind(datas2$age,datas2$HE_BMI,datas2$earn_month)
colnames(x) <- c("x", "y")
(cl <- kmeans(x, 2))
plot(x, col = cl$cluster)
points(cl$centers, col = 1:2, pch = 8, cex = 2)
 
# sum of squares
ss <- function(x) sum(scale(x, scale = FALSE)^2)
 
fitted.x <- fitted(cl);  head(fitted.x)
resid.x <- x - fitted(cl)
 cbind(cl[c("betweenss", "tot.withinss", "totss")], # the same two columns
      c(ss(fitted.x), ss(resid.x),    ss(x)))
stopifnot(all.equal(cl$ totss,        ss(x)),
          all.equal(cl$ tot.withinss, ss(resid.x)),
          all.equal(cl$ betweenss,    ss(fitted.x)),
          all.equal(cl$ betweenss, cl$totss - cl$tot.withinss),
          all.equal(ss(x), ss(fitted.x) + ss(resid.x))
)
 
kmeans(x,1)$withinss # trivial one-cluster, (its W.SS == ss(x))
 
(cl <- kmeans(x, 3, nstart = 16))
plot(x, col = cl$cluster)
points(cl$centers, col = 1:5, pch = 8)
# }
 
#install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(x=datas2$age,y=datas2$HE_BMI,z=datas2$earn_month,pch=20,color=rainbow(3)[cl$cluster])

# Centroid 개선 하기

#Centroid 함수 - 초기 Centroid의 x, y좌표값을 sample()을 통해 random하게 받음
centroid_x=sample(x=min(x[1]):max(x[1]),size=2) #random값의 x값이 data의 최소값과 최대값 안에 오게끔 함
centroid_x[2]
centroid_y=sample(x=min(x[2]):max(x[2]),size=2)#random값의 y값이 data의 최소값과 최대값 안에 오게끔 함
centroid_y[1]
 
centroid<-function(x){#함수의 선언
    mat<-matrix(,nrow=2,ncol=3007)#centroid와 Euclidean distance를 계산하고, 비교한 값을 저장하는 matrix를 선언
                                  #이 때 3007은 data set의 개수, nrow=2는 축의 수로 분석 목적에 따라 조정 가능함 
    
   for(i in 1:3007){#for문을 통해 모든 data가 해당 과정을 반복 
      if( ((x[i,1]-centroid_x[1])^2+(x[i,2]-centroid_y[1])^2) > ((x[i,1]-centroid_x[2])^2+(x[i,2]-centroid_y[2])^2) ){
        #if()문을 통해 데이터와 두 Centroid 간의 Euclidean distance를 서로 비교 수행
        mat[1,i]<-0#2번째 센트로이드와 거리가 더 가까울 경우 
        mat[2,i]<-1#행렬의 1행에 0을, 2행에 1을 대입.  
      }
     else{#반대의 경우도 마찬가지로 동작
       mat[1,i]<-1
       mat[2,i]<-0
     }
   }
    centroid_x[1]=0 #Update에 앞서 값을 초기화
    centroid_x[2]=0
    centroid_y[1]=0
    centroid_y[2]=0
    count1=0
    count2=0
    sum1_x=0
    sum1_y=0
    sum2_x=0
    sum2_y=0
   for(i in 1:3007){#데이터의 수만큼 반복하여 Centroid를 update
     if(mat[1,i]==0){
        sum2_x=sum2_x+x[i,1]
        sum2_y=sum2_y+x[i,2]
        count2=count2+1
     }
     else{
       sum1_x=sum1_x+x[i,1]
       sum1_y=sum1_y+x[i,2]
       count1=count1+1
     }
   }
    centroid_x[1]<-sum1_x/count1#합을 Count로 나누어 최종적으로 centroid를 업데이트
    centroid_x[2]<-sum2_x/count2
    centroid_y[1]<-sum1_y/count1
    centroid_y[2]<-sum2_y/count2
    print(centroid_x[1])
    print(centroid_x[2])
    print(centroid_y[1])
    print(centroid_y[2])
    
}
