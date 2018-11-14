#install.packages('recommenderlab')

getwd()
#setwd("C:/Users/gonybella/Desktop/프로젝트 공유/Git Repository/rfm")


library(recommenderlab)
library(readxl)
library(plyr)
library(tidyverse)
library(reshape)
library(reshape2)
library(data.table)



gc()
memory.limit(size=200000)
#userRFM <- read.csv("data/userRFM_full.csv")
##userRFM_uniq <- read.csv("data/userRFM_full_uniq.csv")

#head(userRFM)
#head(userRFM_uniq)

#회원구매 기본정보 호출
load('cust_prod_total.RData')
load("userRFM.RData")
load("userRFM_f.RData")


head(cust_prod_total_fix_2)


cust_prod_total_fix_2$year_month <- format(cust_prod_total_fix_2$date, '%Y_%m')

## custid 매핑을 위한 key 테이블 호출 
userRFM_custid_key <- read.csv("data/userRFM_custid_key.csv")

# join 후 custid 를 변경
cust_prod_total_fin <- left_join(cust_prod_total_fix_2, 
                                 userRFM_custid_key, by="custid")

head(cust_prod_total_fin)
glimpse(cust_prod_total_fin)


#고객아이디, GPC 에 따른 구매수 확인
cust_prod_fre_melt <- cust_prod_total_fin %>%  # 새 고객 마스터
  group_by(custid_no, prod_code) %>%  # 연월 컬럼 생성
  summarise(frequency = n())

colnames(cust_prod_fre_melt) <- c("custid", "prod_code","frequency")
head(cust_prod_fre_melt)




#######11월8일 수정 부분
head(userRFM_uniq)

userRFM_high_fre <- userRFM_uniq %>%
  filter(F > 3 ) %>%
  select(custid)

nrow(userRFM_high_fre)##141931

cust_prod_high_fre <- merge(cust_prod_fre_melt, userRFM_high_fre, by = "custid", all=FALSE)

head(cust_prod_high_fre)
nrow(userRFM_high_fre) ##141931
nrow(cust_prod_high_fre) ##1507686



##그룹 재정의, 
cust_prod_high_fre_1 <- cust_prod_high_fre %>%
  group_by(custid) %>%
  summarise(item_var = n_distinct(prod_code)) %>%
  filter(item_var > 1)

cust_prod_high_fre_5 <- cust_prod_high_fre %>%
  group_by(custid) %>%
  summarise(item_var = n_distinct(prod_code)) %>%
  filter(item_var > 5)

cust_prod_high_fre_10 <- cust_prod_high_fre %>%
  group_by(custid) %>%
  summarise(item_var = n_distinct(prod_code)) %>%
  filter(item_var > 10)

##일단 A 그룹 중 10품목 이상 구매자만 추천모델로 사용
nrow(cust_prod_high_fre_1) ##62868
head(cust_prod_high_fre_1)




##2개 이상 품목 구매자
#기존 아이템 구매 정보와 병합  ##
cust_prod_high_fre_1_melt <- merge(cust_prod_high_fre, cust_prod_high_fre_1, by="custid", all=FALSE)
nrow(cust_prod_high_fre_1_melt) ##1506655
head(cust_prod_high_fre_1_melt)

cust_prod_high_fre_1_dcast <- dcast(cust_prod_high_fre_1_melt, custid ~ prod_code, value.var="frequency")
nrow(cust_prod_high_fre_1_dcast) ##140900

##추천모델 메모리 및 속도 향상을 위하여 custid 숫자형으로 변환
cust_prod_high_fre_1_dcast$custid <- substring(cust_prod_high_fre_1_dcast$custid,2,9)
cust_prod_high_fre_1_dcast[,1] <- as.numeric(cust_prod_high_fre_1_dcast[,1])

##matrix,realRatingMatrix  형태로 변환
cust_prod_high_fre_1_matrix <- as.matrix(cust_prod_high_fre_1_dcast)
cust_prod_high_fre_1_rrm <- as(cust_prod_high_fre_1_matrix, "realRatingMatrix")

nrow(cust_prod_high_fre_1_rrm) ##140900


##추천모델을 위한 training 데이터 추출  2 가지 이상 품목 구매자
traningData_high_fre_1 <- sample(140900, 140800)
traningSet_high_fre_1 <- cust_prod_high_fre_1_rrm[traningData_high_fre_1]

##추천 모델 구현 Cosine, Pearson

model_high_fre_1_cosine <- Recommender(traningSet_high_fre_1,method="UBCF", parameter="Cosine")
model_high_fre_1_pearson <- Recommender(traningSet_high_fre_1,method="UBCF", parameter="Pearson")


##예측 아이템 보기
RecommenderUser_high_fre_1 <- cust_prod_high_fre_1_rrm[-traningData_high_fre_1]

RecommenderList_high_fre_1 <- predict(model_high_fre_1_pearson, RecommenderUser_high_fre_1, n=3)
as(RecommenderList_high_fre_1, "list")

#검증
##train : 훈련 비율, given : 한사람달 평가 할 아이템의 수 , goodRating : 고려되는 아이템의 기준 , k 반복 횟수
scheme <- evaluationScheme(cust_prod_high_fre_1_rrm, method="split", train=.8,given=3,goodRating=3,k=1)
scheme




##11개 이상 품목 구매자
#기존 아이템 구매 정보와 병합
cust_prod_high_fre_10_melt <- merge(cust_prod_high_fre, cust_prod_high_fre_10, by="custid", all=FALSE)
nrow(cust_prod_high_fre_10_melt) ##941149
head(cust_prod_high_fre_10_melt)

cust_prod_high_fre_10_dcast <- dcast(cust_prod_high_fre_10_melt, custid ~ prod_code, value.var="frequency")
nrow(cust_prod_high_fre_10_dcast) ##112003

##추천모델 메모리 및 속도 향상을 위하여 custid 숫자형으로 변환
cust_prod_high_fre_10_dcast$custid <- substring(cust_prod_high_fre_10_dcast$custid,2,9)
cust_prod_high_fre_10_dcast[,1] <- as.numeric(cust_prod_high_fre_10_dcast[,1])

##matrix,realRatingMatrix  형태로 변환
cust_prod_high_fre_10_matrix <- as.matrix(cust_prod_high_fre_10_dcast)
cust_prod_high_fre_10_rrm <- as(cust_prod_high_fre_10_matrix, "realRatingMatrix")



##추천모델을 위한 training 데이터 추출 11가지 이상 품목 구매자
traningData_high_fre_10 <- sample(54880, 54800)
traningSet_high_fre_10 <- cust_prod_high_fre_10_rrm[traningData_high_fre_10]

##추천 모델 구현 Cosine, Pearson

model_high_fre_10_cosine <- Recommender(traningSet_high_fre_10,method="UBCF", parameter="Cosine")
model_high_fre_10_pearson <- Recommender(traningSet_high_fre_10,method="UBCF", parameter="Pearson")



##예측 아이템 보기
RecommenderUser_high_fre_10 <- cust_prod_high_fre_10_rrm[-traningData_high_fre_10]

RecommenderList_high_fre_10 <- predict(model_high_fre_10_pearson, RecommenderUser_high_fre_10, n=3)
as(RecommenderList_high_fre_10, "list")


#검증
##train : 훈련 비율, given : 한사람달 평가 할 아이템의 수 , goodRating : 고려되는 아이템의 기준 , k 반복 횟수
scheme <- evaluationScheme(cust_prod_high_fre_10_rrm, method="split", train=.8,given=3,goodRating=3,k=1)
scheme



##?이게 추천 품목으로 자주 나
cust_prod_total_fix_2$prod_nm[cust_prod_total_fix_2$prod_code == "10105"]
cust_prod_total_fix_2$prod_nm[cust_prod_total_fix_2$prod_code == "10114"]
cust_prod_total_fix_2$prod_nm[cust_prod_total_fix_2$prod_code == "10212"]


