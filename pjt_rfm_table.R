######### 코드 결과물 RData 
load("cust_prod_month_amt.RData")       # 고객별 월별 amt 합
load("cust_prod_month_freq.RData")      # 고객별 월별 frequency 합
load("cust_prod_cate_count.RData")      # 고객별 월별 category수
load("cust_prod_cate_ftn_count.RData")  # 고객별 월별 category_function 수
load("cust_prod_cate_line_count.RData") # 고객별 월별 category_line 수

## 각 데이터테이블 구조 확인 
View(cust_prod_split_by_month_with_sum_amt)
View(cust_prod_split_by_month_with_dc_frequency)
View(cust_prod_split_by_cate_with_count)
View(cust_prod_split_by_cate_ftn_with_count)
View(cust_prod_split_by_cate_line_with_count)
######### 코드 결과물 RData 


#### 코드 작성 과정

library(tidyverse)

# install.packages('installr')
# require(installr)
# check.for.updates.R()

# memory.size() # 현재 R 메모리 사용량
# memory.size(max = T) # OS로부터 R이 사용가능한 메모리
# memory.limit() #컴퓨터의 최대 메모리 한계치 
# rm(list=ls()) 
# gc() # 용량/속도문제 개선 (쓰레기 데이터 버리기)
gc()

getwd()
#### key 파일 만들기
##
# userFRM_full.csv 파일 읽어오기
# userRFM <- read.csv("userRFM_full.csv") 

# userRFM_custid_key <- userRFM %>%
#  mutate(custid_no = sprintf("C%08d", row_number())) %>%
#  select(custid, custid_no)
# glimpse(userRFM_custid_key)  
### Observations: 1,344,345
### Variables: 2

# write.csv(userRFM_custid_key, file = "userRFM_custid_key.csv", row.names=FALSE)
# -> 이후 custid 매핑시 사용 

userRFM_custid_key <- read.csv("./data/userRFM_custid_key.csv")

# cust_prod_total_fix_2 변수 가진 cust_prod_total.RData 로딩
load("cust_prod_total.RData")

glimpse(cust_prod_total_fix_2)
# Observations: 5,410,838
# Variables: 20

## 년월 변수 추가
cust_prod_total_fix_2$year_month <- format(cust_prod_total_fix_2$date, '%Y_%m')

# join 후 custid 를 변경
cust_prod_total_fin <- left_join(cust_prod_total_fix_2, 
                                 userRFM_custid_key, by="custid")

colnames(cust_prod_total_fin)[2] <- "custid_org"
colnames(cust_prod_total_fin)[22] <- "custid"
glimpse(cust_prod_total_fin)
# Observations: 5,410,838
# Variables: 20


#######################################################################################

#### 고객별 월별 amt 합
## uniq 고객 수 : 1344345
## spread까지 내용을 수행하면 1344355 -> 10명이 누구인지 모르겠음.
## replace를 하면 없어짐
cust_prod_split_by_month_with_sum_amt <- cust_prod_total_fin %>% 
  group_by(custid, year_month) %>%
  summarise(sum_amt = sum(amt)) %>%
  ## http://statdb1.uos.ac.kr/computing/r-data-handling.php#spread
  spread(year_month, sum_amt) %>%
  ## https://rickpackblog.wordpress.com/2017/11/20/replace-na-in-all-columns-with-dplyr-rstats/
  ## NA를 제거하는 것을 수행하면 10명이 없어진다.
  replace(., is.na(.), 0)

glimpse(cust_prod_split_by_month_with_sum_amt)
# Observations: 1,344,345
# Variables: 31


#######################################################################################
#### 고객별 월별 frequency 합
## uniq 고객 수 : 1344345
cust_prod_split_by_month_with_dc_frequency <- cust_prod_total_fin %>% 
  group_by(custid, year_month) %>%
  summarise(dc_frequency = n_distinct(date)) %>%
  spread(year_month, dc_frequency) %>%
  replace(., is.na(.), 0)  

glimpse(cust_prod_split_by_month_with_dc_frequency)
# Observations: 1,344,345
# Variables: 31


#######################################################################################
#### 고객별 category수
## uniq 고객 수 : 1344345
cust_prod_split_by_cate_with_count <- cust_prod_total_fin %>% 
  group_by(custid, cate) %>%
  summarise(count_cate = n()) %>% 
  spread(cate, count_cate) %>%
  replace(., is.na(.), 0) 

glimpse(cust_prod_split_by_cate_with_count)
# Observations: 1,344,345
# Variables: 12


#######################################################################################
#### 고객별 category_function 수
## uniq 고객 수 : 1344345
cust_prod_split_by_cate_ftn_with_count <- cust_prod_total_fin %>% 
  group_by(custid, cate_ftn) %>%
  summarise(count_cate_ftn = n()) %>% 
  spread(cate_ftn, count_cate_ftn) %>%
  replace(., is.na(.), 0)  

glimpse(cust_prod_split_by_cate_ftn_with_count)
# Observations: 1,344,345
# Variables: 52


#######################################################################################
#### 고객별 category_line 수
## uniq 고객 수 : 1344345
cust_prod_split_by_cate_line_with_count <- cust_prod_total_fin %>% 
  group_by(custid, cate_line) %>%
  summarise(count_cate_line = n()) %>% 
  spread(cate_line, count_cate_line) %>%
  replace(., is.na(.), 0)  

glimpse(cust_prod_split_by_cate_line_with_count)
# Observations: 1,344,345
# Variables: 39


### 결과물 RData로 저장 
### 다음 할때는 load 부분부터 시작

# 고객별 월별 amt 합
glimpse(cust_prod_split_by_month_with_sum_amt)
save(cust_prod_split_by_month_with_sum_amt, file="cust_prod_month_amt.RData")

# 고객별 월별 frequency 합
glimpse(cust_prod_split_by_month_with_dc_frequency)
save(cust_prod_split_by_month_with_dc_frequency, file="cust_prod_month_freq.RData")

# 고객별 월별 category수
glimpse(cust_prod_split_by_cate_with_count)
save(cust_prod_split_by_cate_with_count, file="cust_prod_cate_count.RData")

#고객별 월별 category_function 수
glimpse(cust_prod_split_by_cate_ftn_with_count)
save(cust_prod_split_by_cate_ftn_with_count, file="cust_prod_cate_ftn_count.RData")

# 고객별 월별 category_line 수
glimpse(cust_prod_split_by_cate_line_with_count)
save(cust_prod_split_by_cate_line_with_count, file="cust_prod_cate_line_count.RData")


## RData 삭제 
rm(cust_prod_split_by_month_with_sum_amt)
rm(cust_prod_split_by_month_with_dc_frequency)
rm(cust_prod_split_by_cate_with_count)
rm(cust_prod_split_by_cate_ftn_with_count)
rm(cust_prod_split_by_cate_line_with_count)

## 코드 결과물 RData 로딩 
load("cust_prod_month_amt.RData")       # 고객별 월별 amt 합
load("cust_prod_month_freq.RData")      # 고객별 월별 frequency 합
load("cust_prod_cate_count.RData")      # 고객별 월별 category수
load("cust_prod_cate_ftn_count.RData")  # 고객별 월별 category_function 수
load("cust_prod_cate_line_count.RData") # 고객별 월별 category_line 수

## 각 데이터테이블 구조 확인 
View(cust_prod_split_by_month_with_sum_amt)
View(cust_prod_split_by_month_with_dc_frequency)
View(cust_prod_split_by_cate_with_count)
View(cust_prod_split_by_cate_ftn_with_count)
View(cust_prod_split_by_cate_line_with_count)



