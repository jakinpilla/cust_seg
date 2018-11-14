getwd()

### 수정된 고객 마스터 데이터
load("cust_prod_total_fin.RData") 
### 수정된 고객 마스터 데이터


######### 코드 결과물 RData 
load("cust_mon_amt.RData")
load("cust_mon_fre.RData")
load("cust_cate_amt.RData")
load("cust_cate_ftn_amt.RData")
load("cust_cate_line_amt.RData")

## 각 데이터테이블 구조 확인 
View(cust_mon_amt)        #고객별 월별 총 구매액
View(cust_mon_fre)        #고객별 월별 방문횟수
View(cust_cate_amt)       #고객별 cate 별 구매액
View(cust_cate_ftn_amt)   #고객별 cate_ftn 별 구매액
View(cust_cate_line_amt)  #고객별 cate_line 별 구매액
######### 코드 결과물 RData 


#### 코드 작성 과정 
# install.packages("readxl")
# install.packages("plyr")
# install.packages("data.table")

library(readxl)
library(plyr)
library(tidyverse)
library(reshape)
library(reshape2)
library(data.table)


# setwd('C:/Users/gonybella/Desktop/프로젝트 공유/호준_data_수정/최종수정/새 폴더')
getwd()
rm()
rm(list=ls())
gc() 


# cust_prod_total_fix_2 변수 가진 cust_prod_total.RData 로딩
load("cust_prod_total.RData")
head(cust_prod_total_fix_2)
glimpse(cust_prod_total_fix_2)
## Observations: 5,410,838
## Variables: 20

## 년월 변수 추가
cust_prod_total_fix_2$year_month <- format(cust_prod_total_fix_2$date, '%Y_%m')
range(cust_prod_total_fix_2$date)

## custid 매핑을 위한 key 테이블 호출 
userRFM_custid_key <- read.csv("./data/userRFM_custid_key.csv")
glimpse(userRFM_custid_key)  
## Observations: 1,344,345
## Variables: 2

## custid 매핑 실시 

# join 후 custid 를 변경
cust_prod_total_fin <- left_join(cust_prod_total_fix_2, 
                                 userRFM_custid_key, by="custid")

colnames(cust_prod_total_fin)[2] <- "custid_org"  # 구 고객번호
colnames(cust_prod_total_fin)[22] <- "custid" # 수정된 고객번호
glimpse(cust_prod_total_fin) # RData로 호출 가능

load("cust_prod_total_fin.RData") ### 수정된 마스터 데이터
# Observations: 5,410,838
# Variables: 20


#custid, 연월로 group by
# cust_mon_amt_melt <- cust_prod_total_fix_2 %>%
#  group_by(custid, substring(cust_prod_total_fix_2$date, 1, 6)) %>%
#  summarise(sum_cust_amt = sum(amt))

## Error:
## Column `substring(cust_prod_total_fix_2$date, 1, 6)` 
## must be length 2 (the group size) or one, not 5410838
# 에러발생 이유 : 작업자에 따라 날짜형 / 숫자형으로 읽어옴 

#cust_mon_amt_melt <- cust_prod_total_fix_2 %>%
#  group_by(custid, substring(date, 1, 7)) %>%
#  summarise(sum_cust_amt = sum(amt))

####

## 새 고객 마스터로 재작업 
#custid, 연월로 group by (원코드 변경)
#고객별/월별 구매액 amt 컬럼 추가 

cust_mon_amt_melt <- cust_prod_total_fin %>%  # 새 고객 마스터
  group_by(custid, year_month) %>%  # 연월 컬럼 생성
  summarise(sum_cust_amt = sum(amt))

colnames(cust_mon_amt_melt) <- c("custid", "month", "sum_amt")
head(cust_mon_amt_melt)

#dcast 형태로 변환
cust_mon_amt <- dcast(cust_mon_amt_melt, custid ~ month)
glimpse(cust_mon_amt)

nrow(cust_mon_amt)
# 총 고객수 : 1344345

## 이후 새 고객 마스터로 작업 

#custid, 연월로 group by
#고객별/월별 방문수 freq 컬럼 추가 
cust_mon_fre_melt <- cust_prod_total_fin %>%
  group_by(custid, year_month) %>%
  summarize(frequency=n())

# cust_mon_fre_melt <- cust_prod_total_fix_2 %>%
#  group_by(custid, substring(cust_prod_total_fix_2$date,1,6 )) %>%
#  summarize(frequency=n())

colnames(cust_mon_fre_melt) <- c("custid", "month", "frequency")
head(cust_mon_fre_melt)

cust_mon_fre <- dcast(cust_mon_fre_melt, custid ~ month)
head(cust_mon_fre)

nrow(cust_mon_fre)
#1344345


#custid, cate 컬럼으로 group by 
#고객별/카테고리별 구매액 컬럼 추가 
cust_cate_amt_melt <- cust_prod_total_fin %>%
  group_by(custid, cate) %>%
  summarize(sum_amt = sum(amt))

colnames(cust_cate_amt_melt) <- c("custid", "cate", "sum_amt")
head(cust_cate_amt_melt)

cust_cate_amt <- dcast(cust_cate_amt_melt, custid ~ cate)
head(cust_cate_amt)

nrow(cust_cate_amt)
#1344345

#custid, cate_ftn 컬럼으로 group by 
##고객별/카테고리 기능별 구매액 컬럼 추가 
cust_cate_ftn_amt_melt <- cust_prod_total_fin %>%
  group_by(custid, cate_ftn) %>%
  summarize(sum_amt = sum(amt))

colnames(cust_cate_ftn_amt_melt) <- c("custid", "cate_ftn", "sum_amt")
head(cust_cate_ftn_amt_melt)

cust_cate_ftn_amt <- dcast(cust_cate_ftn_amt_melt, custid ~ cate_ftn)
head(cust_cate_ftn_amt)

nrow(cust_cate_ftn_amt)
#1344345

#custid, cate_line 컬럼으로 group by 
##고객별/카테고리 라인별 구매액 컬럼 추가 
cust_cate_line_amt_melt <- cust_prod_total_fin %>%
  group_by(custid, cate_line) %>%
  summarize(sum_amt = sum(amt))

colnames(cust_cate_line_amt_melt) <- c("custid", "cate_line", "sum_amt")
head(cust_cate_line_amt_melt)

cust_cate_line_amt <- dcast(cust_cate_line_amt_melt, custid ~ cate_line)
head(cust_cate_line_amt)

nrow(cust_cate_line_amt)
#1344345

head(cust_mon_amt)        #고객별 월별 총 구매액
save(cust_mon_amt, file="cust_mon_amt.RData")

head(cust_mon_fre)        #고객별 월별 방문횟수
save(cust_mon_fre, file="cust_mon_fre.RData")

head(cust_cate_amt)       #고객별 cate 별 구매액
save(cust_cate_amt, file="cust_cate_amt.RData")

head(cust_cate_ftn_amt)   #고객별 cate_ftn 별 구매액
save(cust_cate_ftn_amt, file="cust_cate_ftn_amt.RData")

head(cust_cate_line_amt)  #고객별 cate_line 별 구매액
save(cust_cate_line_amt, file="cust_cate_line_amt.RData")

## RData 삭제 
rm(cust_mon_amt);rm(cust_mon_fre);rm(cust_cate_amt);rm(cust_cate_ftn_amt);rm(cust_cate_line_amt)   

## RData 파일 로딩 
load("cust_mon_amt.RData")
load("cust_mon_fre.RData")
load("cust_cate_amt.RData")
load("cust_cate_ftn_amt.RData")
load("cust_cate_line_amt.RData")

## 각 데이터테이블 구조 확인 
View(cust_mon_amt)        #고객별 월별 총 구매액
View(cust_mon_fre)        #고객별 월별 방문횟수
View(cust_cate_amt)       #고객별 cate 별 구매액
View(cust_cate_ftn_amt)   #고객별 cate_ftn 별 구매액
View(cust_cate_line_amt)  #고객별 cate_line 별 구매액


