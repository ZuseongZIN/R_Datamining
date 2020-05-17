library(dplyr)
library(prcomp)
raw <- read.csv("raw.csv")

# 1. 타겟변수 결측치 처리 및 제거
raw <- rename(raw, ingest = LS_1YR)
raw$ingest <- ifelse(raw$ingest == 9, NA, raw$ingest)
raw <- raw %>% filter(!is.na(ingest))

raw <- raw %>% filter(age > 18) 

#[소아청소년관련 질환응답자 제거]
raw <- raw %>% select(-DI9_yd) #코드명 : DI9_yd 
raw <- raw %>% select(-DI9_ya) #코드명 : DI9_ya 
raw <- raw %>% select(-DF1_yd) #코드명 : DF1_yd 
raw <- raw %>% select(-DF1_ya) #코드명 : DF1_ya 
raw <- raw %>% select(-DN6_yd) #코드명 : DN6_yd
raw <- raw %>% select(-DN6_ya) #코드명 : DN6_ya 
raw <- raw %>% select(-DJ9_yd) #코드명 : DJ9_yd 
raw <- raw %>% select(-DJ9_ya) #코드명 : DJ9_ya 
#[가중치 제거]
raw <- raw %>% select(-wt_hs) 
raw <- raw %>% select(-wt_itvex) 
raw <- raw %>% select(-wt_pft) 
raw <- raw %>% select(-wt_vt) 
raw <- raw %>% select(-wt_nn) 
raw <- raw %>% select(-wt_ntr) 
raw <- raw %>% select(-wt_tot) 
raw <- raw %>% select(-wt_pfnt) 
raw <- raw %>% select(-wt_pfvtnt) 
raw <- raw %>% select(-wt_pfvt) 
raw <- raw %>% select(-wt_vtnt) 
raw <- raw %>% select(-wt_nnnt) 
#소아청소년 체중 백분위수
raw <- raw %>% select(-HE_wt_pct,-HE_BMI_pct,-LQ4_01,-LQ4_02 ,- LQ4_03, -  LQ4_04,   -LQ4_05 , - LQ4_06 ,  -LQ4_07,   -LQ4_08  , -LQ4_09,   -LQ4_10   ,-LQ4_11 , - LQ4_12  , -LQ4_13  , -LQ4_14 , - LQ4_15 ,-  LQ4_16  ,- LQ4_21 , - LQ4_22 ,  -LQ4_25,  - LQ4_26,  - LQ4_27  , -LQ4_28 , - LQ4_29,   -LQ4_23 , - LQ4_24 ,-  LQ4_17 ,-  LQ4_18 ,-  LQ4_19,  - LQ4_20)
raw <- raw %>% select(-id_F ,-  id_M) 
raw <- raw %>% select(-EC_wht_6)#변형근로시간 : 기타상세
raw <- raw %>% select(-BH9_14_1_01,-BH9_14_1_02,-BH9_14_1_03,BH9_14_2_01,-BH9_14_3_01,-BH9_14_4_01,-BH9_14_2_02,-BH9_14_2_03,-BH9_14_4_02,-BH9_14_2_03,BH9_14_3_03)

raw <- raw %>% select(-CH2_1,-CH2_2)
raw <- raw %>% select(-BO3_11)#체중조절방법: 기타 상세
raw <- raw %>% select(-BD7_67)#"1년간 타인의 음주로 인한 
# 피해 여부: 기타 상세4)
raw <- raw %>% select(-BP2_2)#스트레스 원인: 기타상세1)
raw <- raw %>% select(-BS12_35)#(성인)평생사용담배종류: 기타상세
raw <- raw %>% select(-BS12_45)#(성인)현재사용담배종류: 기타상세
raw <- raw %>% select(-BS10_1, -BS10_3,-BS10_2)#청소년 흡연경험
raw <- raw %>% select(-LW_mp_e)#무월경사유 기타
#장비분석범위
raw <- raw %>% select(-HE_alt_etc)
raw <- raw %>% select(-HE_hsCRP_etc)
raw <- raw %>% select(-HE_Folate_etc)
raw <- raw %>% select(-HE_NNAL_etc)
raw <- raw %>% select(-HE_UNa_etc)
raw <- raw %>% select(-BM13_6)#치아손상 사유: 기타상세
raw <- raw %>% select(-BM14_2)#"치과진료 미치료 이유 기타상세
#식사요법 주관식
raw <- raw %>% select(-N_DT_DS)
raw <- raw %>% select(-N_DT_ETC)
#w조사관련항목제거
raw <- raw %>% select(-mod_d)
raw <- raw %>% select(-X)
raw <- raw %>% select(-ID)
raw <- raw %>% select(-ID_fam)

dim(raw)

###############################################################
# 2. 8,88,888, 9,99,999 NaN작업 
# 1) 데이터 분리 -> 8,88,888,9,99,999를 지우면 안 되는 데이터 분리
a <- raw %>% select(age, region, LK_LB_IT, LQ2_ab, AC8_3w_01, AC8_3w_02 ,AC8_3w_03 )
c <- raw %>%  select(-age, -region, -LK_LB_IT, -LQ2_ab,-AC8_3w_01,-AC8_3w_02,-AC8_3w_03 )
c[c==8 | c==88 | c==888 | c==8888 | c=='8888'] <- NA
c[c==9 | c==99 | c==999 | c==9999 | c=='9999'] <- NA

# 2) 분리 후 병합
raw2 <- cbind(c, a) 
raw2$LK_LB_IT <- ifelse(raw2$LK_LB_IT == 88, 0, 1) # binarization
raw2 <- rename(raw2, love_nutri = LK_LB_IT)

##############################################################
# 3. 데이터 전처리 - 파레토 법칙에 의한 Nan값 제거
raw3 <- raw2
dim(raw3) # 5708 521

# 1차 삭제
sorted<-sort(colSums(is.na(raw3)),decreasing = TRUE)
delete <- sorted[1:120]
deletecolname <- names(delete)
raw3<-raw3 %>% select(-deletecolname)

i<-1
while(i<nrow(raw3)& nrow(raw3)>1141&ncol(raw3)>130){ 
  sorted <- sort(rowSums(is.na(raw3)),decreasing=TRUE)
  
  if(sum(is.na(raw3[i,]))>sorted[length(sorted)*0.2]){
    raw3<-raw3[-i,]
  }
  i = i+1
}
raw3$ainc
dim(raw3) # 4534 521

# 2차 삭제
sorted<-sort(colSums(is.na(raw3)),decreasing = TRUE)
delete <- sorted[1:120]
deletecolname <- names(delete)
raw3<-raw3 %>% select(-deletecolname)

i<-1
while(i<nrow(raw3)& nrow(raw3)>1000&ncol(raw3)>104){ 
  sorted <- sort(rowSums(is.na(raw3)),decreasing=TRUE)
  
  if(sum(is.na(raw3[i,]))>sorted[length(sorted)*0.2]){
    raw3<-raw3[-i,]
  }
  i = i+1
}

dim(raw3) # 3600 417

# 3차 삭제
sorted<-sort(colSums(is.na(raw3)),decreasing = TRUE)
delete <- sorted[1:120]
deletecolname <- names(delete)
raw3<-raw3 %>% select(-deletecolname)

i<-1
while(i<nrow(raw3)& nrow(raw3)>1000&ncol(raw3)>83){ 
  sorted <- sort(rowSums(is.na(raw3)),decreasing=TRUE)
  
  if(sum(is.na(raw3[i,]))>sorted[length(sorted)*0.2]){
    raw3<-raw3[-i,]
  }
  i = i+1
}

dim(raw3) # 2903 334
table(is.na(raw3))

# 3차 삭제
sorted<-sort(colSums(is.na(raw3)),decreasing = TRUE)
delete <- sorted[1:120]
deletecolname <- names(delete)
raw3<-raw3 %>% select(-deletecolname)

i<-1
while(i<nrow(raw3)& nrow(raw3)>1000&ncol(raw3)>83){ 
  sorted <- sort(rowSums(is.na(raw3)),decreasing=TRUE)
  
  if(sum(is.na(raw3[i,]))>sorted[length(sorted)*0.2]){
    raw3<-raw3[-i,]
  }
  i = i+1
}
sorted<-sort(colSums(is.na(raw3)),decreasing = TRUE)
delete <- sorted[1:30]
deletecolname <- names(delete)
raw3<-raw3 %>% select(-deletecolname)

# 데이터 퀄리높이기
raw4 <- raw3
# 1) PCA 분석

data <- raw4%>% select(N_INTK,   N_EN,   N_WATER,   N_PROT,   N_FAT,   N_SFA,   N_MUFA,   N_PUFA,   N_N3,   N_N6,   N_CHOL,   N_CHO,   N_TDF,   N_SUGAR,   N_CA,   N_PHOS,   N_FE,   N_NA,   N_K,   N_VA,   N_VA_RAE,   N_CAROT,   N_RETIN,   N_B1,   N_B2,   N_NIAC,   N_VITC)

scaled_data <- as.data.frame(scale(data))

#pca분석 실행
scaled_data.pca <- prcomp(scaled_data, scale. = T)

#누적분산확인후 70~90%이내까지 PC요소 포함시키기
summary(scaled_data.pca)
plot(scaled_data.pca, type = "l")

#PC요소들 포함변수 관련도 높은순서로 뽑은 후 관련 깊은 변수 확인
sort(abs(scaled_data.pca$rotation[,1]), decreasing = TRUE)#energy_protein
sort(abs(scaled_data.pca$rotation[,2]), decreasing = TRUE)#fat_series
sort(abs(scaled_data.pca$rotation[,3]), decreasing = TRUE)#vitamin_A
sort(abs(scaled_data.pca$rotation[,4]), decreasing = TRUE)#n-dim-fattyAcid


#누적분산 70%가 넘는 최소의 PC원소들 추출
PCA <- as.data.frame(scaled_data.pca$x[,1:4])
PCA
raw5<-cbind(raw4,PCA)

raw5 <- raw5%>% select(-N_INTK, -N_EN, -N_WATER, -N_PROT,   -N_FAT,   -N_SFA,   -N_MUFA,   -N_PUFA,   -N_N3,   -N_N6,   -N_CHOL,   -N_CHO,   -N_TDF,   -N_SUGAR,   -N_CA,   -N_PHOS,   -N_FE,   -N_NA,   -N_K,  -N_VA,   -N_VA_RAE,  -N_CAROT,   -N_RETIN,  -N_B1,   -N_B2,   -N_NIAC,   -N_VITC)

# 2) min-max 정규화
normalize<-function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
temp$HE_ht<-normalize(temp$HE_ht)
temp$HE_wt<-normalize(temp$HE_wt)
temp$HE_BMI<-normalize(temp$HE_BMI)

# 3) Factor level이 너무 많은 변수 제거
raw5 <- raw5 %>% select(-psu,-DC11_tp,-M_2_et,-BS5_31,-N_DAY,-AC3_3e_01)
raw5 <- raw5 %>% select(-kstrata) # 가중치라 필요 없음
raw5 <- raw5 %>% select(-DC12_tp,-AC8_1e_01,-AC3_3e_02,
                        -HE_Uacid_etc,-HE_Ucot_etc,-HE_Ukal_etc) # 다 빈칸이라 제거

str(raw5)
# 4) 지역 2진화 (중부, 남부)
temp <- raw5 %>% select(region)
temp <- as.data.frame(temp)
temp$region <- ifelse(temp$region %in% c(1,4,6,8,9,11,12),1 , 0)
temp <- rename(temp, top_bottom_region = region)
datas2 <- cbind(raw5,temp)
str(raw5)

# 5) 나이를 3 class로 분류해서 새로 만듦
temp <- raw5 %>% select(age)
temp <- as.data.frame(temp)
temp$age <- ifelse(temp$age > 65, 'old', ifelse(temp$age > 30, 'middle', 'young'))
temp <- rename(temp, age_group = age)
raw5 <- cbind(raw5,temp)
str(raw5)

# 6) 소득 이상치 제거
raw5$ainc <- ifelse(raw5$ainc < 16.6667| raw5$ainc > 1301, NA, raw5$ainc)#이상치제거
raw5 <- rename(raw5, earn_month = ainc)
raw5<-raw5%>% filter(!is.na(earn_month))

# 7) 형변환
sperate <- raw5 %>% select(earn_month, HE_ht, HE_wt, HE_BMI, PC1, PC2, PC3, PC4, age)
sperate # 분리한 것
sperate_name <- names(sperate) # 분리한 이름
change <- raw5 %>% select(-sperate_name) # factor로 바꿀 테이블
str(change) 
change_name <- names(change) # factor로 바꿀 이름

for(i in change_name){
  change[,i] <- as.factor(change[,i]) # 형변환
}

# change + sperate 병합
processed <- cbind(change, sperate)
str(processed)

# 레벨 높은거 다시 제거
processed <- processed %>% select(-incm,-ho_incm,-ho_incm5,-incm5,-cfam,-genertn,ainc_1,-BA2_22,-HE_obe,-L_OUT_FQ,-LF_secur_y,)
#########################################################
# 4. 의사결정 나무 모델링
library(caret)
library(tree)
library(party)
library(rpart)
library(caret)
library(rpart.plot)

df <- processed
# train, test 분리
set.seed(50) #reproducability setting
intrain2<-createDataPartition(y=temp$target_ingest, p=0.9, list=FALSE) 
train_temp<-temp[intrain2, ]
test_temp<-temp[-intrain2, ]
View(train_temp)
# 의사결정나무 그리기
rpartmod2<-rpart(target_ingest~. , data=train_temp, method="class")
plot(rpartmod2)
text(rpartmod2)
printcp(rpartmod2)
plotcp(rpartmod2)

# 에러 최소값으로 가지치기
ptree<-prune(rpartmod, cp= rpartmod$cptable[which.min(rpartmod$cptable[,"xerror"]),"CP"])
plot(ptree)
text(ptree)
# 평가
rpartpred<-predict(rpartmod2, test_temp, type='class')
test_temp$target_ingest<-as.factor(test$target_ingest)
confusionMatrix(rpartpred, temp$target_ingest)

prp(ptree,type=1,extra=2,digits=3) # 가지치기 한거 이쁘게
prp(rpartmod2,type=1,extra=2,digits=3) # 가지치기 전 이쁘게