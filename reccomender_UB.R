install.packages('readxl')
install.packages('plyr')
install.packages('tidyverse')
install.packages('reshape')
install.packages('reshape')
install.packages('data.table')


getwd()
setwd("C:/Users/USER/Desktop/고웁/2018-2/빅데이터 프로젝트/github_share/rfm")


library(recommenderlab)
library(readxl)
library(plyr)
library(tidyverse)
library(reshape)
library(reshape2)
library(data.table)



gc()
memory.limit(size=100000)
userRFM <- read.csv("data/userRFM_full.csv")
##userRFM_uniq <- read.csv("data/userRFM_full_uniq.csv")

head(userRFM)
head(userRFM_uniq)

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



##memory out 부분
###Error: cannot allocate vector of size 23.8 Gb
cust_prod_fre <- dcast(cust_prod_fre_melt, custid ~ prod_code)



##추천모델 생성을 위한 rfm A 등급만 추출
a_grade_group <- userRFM_uniq %>% 
  filter(grade == "A") %>%
  select(custid)

head(a_grade_group)
cust_prod_fre_a_grade <- merge(cust_prod_fre_melt, a_grade_group, by = "custid", all=FALSE)
head(cust_prod_fre_a_grade)
nrow(a_grade_group) ##281576
nrow(cust_prod_fre_a_grade) ##2277412

##그룹 재정의, rfm A 그룹 중 다품목 구매 1건 이상, 5건 이상, 10건 이상
cust_prod_fre_a_grade_var_1 <- cust_prod_fre_a_grade %>%
  group_by(custid) %>%
  summarise(item_var = n_distinct(prod_code)) %>%
  filter(item_var > 1)

cust_prod_fre_a_grade_var_5 <- cust_prod_fre_a_grade %>%
  group_by(custid) %>%
  summarise(item_var = n_distinct(prod_code)) %>%
  filter(item_var > 5)

cust_prod_fre_a_grade_var_10 <- cust_prod_fre_a_grade %>%
  group_by(custid) %>%
  summarise(item_var = n_distinct(prod_code)) %>%
  filter(item_var > 10)

##일단 A 그룹 중 10품목 이상 구매자만 추천모델로 사용
nrow(cust_prod_fre_a_grade_var_10) ##62868
head(cust_prod_fre_a_grade_var_10)


#기존 아이템 구매 정보와 병합
cust_prod_fre_a_grade_var_10_melt <- merge(cust_prod_fre_a_grade, cust_prod_fre_a_grade_var_10, by="custid", all=FALSE)
nrow(cust_prod_fre_a_grade_var_10_melt) ##1040844
head(cust_prod_fre_a_grade_var_10_melt)

cust_prod_fre_a_grade_var_10_dcast <- dcast(cust_prod_fre_a_grade_var_10_melt, custid ~ prod_code, value.var="frequency")
nrow(cust_prod_fre_a_grade_var_10_dcast) ##62868

##추천모델 메모리 및 속도 향상을 위하여 custid 숫자형으로 변환
cust_prod_fre_a_grade_var_10_dcast$custid <- substring(cust_prod_fre_a_grade_var_10_dcast$custid,2,9)
cust_prod_fre_a_grade_var_10_dcast[,1] <- as.numeric(cust_prod_fre_a_grade_var_10_dcast[,1])


##matrix,realRatingMatrix  형태로 변환
cust_prod_fre_a_grade_var_10_matrix <- as.matrix(cust_prod_fre_a_grade_var_10_dcast)
cust_prod_fre_a_grade_var_10_rrm <- as(cust_prod_fre_a_grade_var_10_matrix, "realRatingMatrix")

nrow(cust_prod_fre_a_grade_var_10_rrm) ##62868


##추천모델을 위한 training 데이터 추출 50000
traningData <- sample(62868, 50000)
traningSet <- cust_prod_fre_a_grade_var_10_rrm[traningData]

##추천 모델 구현 Cosine, Pearson
?Recommender
model_10_cosine <- Recommender(traningSet,method="UBCF", parameter="Cosine")
model_10_pearson <- Recommender(traningSet,method="UBCF", parameter="Pearson")

##예측 아이템 보기
###RecommenderUser 변수에 B, C, D, E 등급자 넣어서 보기
RecommenderUser <- cust_prod_fre_a_grade_var_10_rrm[-traningData]

RecommenderList <- predict(model_10_cosine, RecommenderUser, n=3)
as(RecommenderList, "List")

#검증
##train : 훈련 비율, given : 한사람달 평가 할 아이템의 수 , goodRating : 고려되는 아이템의 기준 , k 반복 횟수
scheme <- evaluationScheme(cust_prod_fre_a_grade_var_10_rrm, method="split", train=.8,given=4,goodRating=3,k=1)
scheme



head(userRFM_uniq)

userRFM_high_fre <- userRFM_uniq %>%
  filter(F > 3 ) %>%
  select(custid)

nrow(userRFM_high_fre)

head(cust_prod_high_fre_melt)

cust_prod_high_fre <- merge(cust_prod_fre_melt, userRFM_high_fre, by = "custid", all=FALSE)

head(cust_prod_high_fre)
nrow(userRFM_high_fre) ##141931
nrow(cust_prod_high_fre) ##1507686



##그룹 재정의, rfm A 그룹 중 다품목 구매 1건 이상, 5건 이상, 10건 이상
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



#기존 아이템 구매 정보와 병합
cust_prod_hifre_a_grade_var_10_melt <- merge(cust_prod_high_fre, cust_prod_high_fre_1, by="custid", all=FALSE)
nrow(cust_prod_fre_a_grade_var_10_melt) ##1040844
head(cust_prod_fre_a_grade_var_10_melt)

cust_prod_fre_a_grade_var_10_dcast <- dcast(cust_prod_fre_a_grade_var_10_melt, custid ~ prod_code, value.var="frequency")
nrow(cust_prod_fre_a_grade_var_10_dcast) ##62868

##추천모델 메모리 및 속도 향상을 위하여 custid 숫자형으로 변환
cust_prod_fre_a_grade_var_10_dcast$custid <- substring(cust_prod_fre_a_grade_var_10_dcast$custid,2,9)
cust_prod_fre_a_grade_var_10_dcast[,1] <- as.numeric(cust_prod_fre_a_grade_var_10_dcast[,1])

