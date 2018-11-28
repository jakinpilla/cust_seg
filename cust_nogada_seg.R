
# 1. 멤버십 데이에 2번이상 온 고객 (고객수, 매출액, 영수증수)
#    - 멤버십 데이에만 2번이상 온 고객, 멤버십 데이에만 2번이상 오고 다른 날에도 방문하는 고객 집단을 비교
#    - 멤버심 데이에만 2번이상 온 고객 & 멤버십 데이가 아닐때 13, 12,..., 2, 1번인 고객집단 분류
##     - 의사결정시 사용
#    - 구메제품 비교... 소비주기가 짧은 제품을 구매하는 고객은 다른 날에도 방문하더라
#    - ~~ 제품 데이 행사 등등 기획


# 2. 멤버십 데이에만 1번 온 고객
#    - 멤버십 데이에만 1번 온 고객, 다른 날에도 방문하는 고객 집단을 비교용
#      - 멤버심 데이에만 1번 온 고객 & 멤버십 데이가 아닐때 13, 12,..., 2, 1번인 고객집단 분류
#         - 제품비교 
#    - 멤버십 데이에만 1번 온 고객, 다른 날에는 한 번도 방문하지 않은 고객 집단을 비교


# 3. 멤버십 데이에 전혀 반응하지 않는 고객


# 4. 이탈고객 집단 : 18.6월 기준으로 1년이상 구매하지 않은 집단
#    - 2016.12기준, 2017.06기준, 2017.12기준, 2018,06기준 
#    - 마지막 구매 제품, 제품 카테고리 -> 이탈제품
#    - 각 기준별 이탈고객이 아닌 집단에서 bodylove 고객집단의 구매 제품과 이탈고객의 구매제품을 비교


# 5. 이탈 잠재 고객 : 7개월간 구매를 하지 않는 고개
#    -  이탈고객집단과 비교 통해 이탈고객정의 정확성 검토

# [ 더바디샵 멤버십데이 일정 ] 
# 
# 2018.06.01 - 06.02
# 2017.11.03 - 11.04
# 2017.05.26 - 05.27
# 2016.11.04 - 11.05
# 2016.05.27 - 05.28

library(plyr)
library(tidyverse)
library(data.table)
library(lubridate)


setwd("C:/Users/Daniel/cust_seg"); getwd()
load('cust_prod_total_fin.RData')

head(cust_prod_total_fin) %>% as.data.frame()
nrow(cust_prod_total_fin) # 5410838

cust_prod_total_fin %>%
  select(custid, date, grade, qty, amt, sex, age, prod_nm, cate, cate_ftn, cate_line, price) -> data

head(data)

## 맴버십 데이에만 구매한 고객 추출
as.numeric(data$date[1:5])

data$date_num <- as.numeric(data$date)
head(data) %>% as.data.frame()

as.numeric(as.POSIXct(as.character('2016-05-27'))) # 1464274800
as.numeric(as.POSIXct(as.character('2016-05-28'))) # 1464361200
as.numeric(as.POSIXct(as.character('2016-11-04'))) # 1478185200
as.numeric(as.POSIXct(as.character('2016-11-05'))) # 1478271600
as.numeric(as.POSIXct(as.character('2017-05-26'))) # 1495724400
as.numeric(as.POSIXct(as.character('2017-05-27'))) # 1495810800
as.numeric(as.POSIXct(as.character('2017-11-03'))) # 1509634800
as.numeric(as.POSIXct(as.character('2017-11-04'))) # 1509721200
as.numeric(as.POSIXct(as.character('2018-06-01'))) # 1527778800
as.numeric(as.POSIXct(as.character('2018-06-02'))) # 1527865200

## event 1----
data %>%
  filter(date_num >= 1464274800 & date_num <= 1464361200) -> cust_prod_1_evt

head(cust_prod_1_evt) %>% as.data.frame
nrow(cust_prod_1_evt) # 56259

cust_prod_1_evt$evt_1 <- 1

cust_prod_1_evt %>%
  select(evt_1) %>% 
  group_by(custid) %>%
  summarise(sum.prod = sum(evt_1)) -> cust_evt_1_list_tmp

head(cust_evt_1_list_tmp)
nrow(cust_evt_1_list_tmp) # visitors on second event are 22,404 persons

cust_evt_1_list_tmp$evt_1 <- 1; head(cust_evt_1_list_tmp)
cust_evt_1_list_tmp %>%
  select(custid, evt_1) -> evt_1_df

head(evt_1_df); dim(evt_1_df)

data %>%
  left_join(evt_1_df, by = 'custid') -> data_evt_1_1; head(data_evt_1_1) %>% as.data.frame()
dim(data_evt_1_1)

data_evt_1_1 %>%
  filter(evt_1 == 1) %>% head %>% as.data.frame()

head(data_evt_1_1) %>% as.data.frame()

## event 2----
data %>%
  filter(date_num >= 1478185200 & date_num <= 1478271600) -> cust_prod_evt_2

head(cust_prod_evt_2) %>% as.data.frame()
cust_prod_evt_2$evt_2 <- 1

cust_prod_evt_2 %>%
  select(evt_2) %>% 
  group_by(custid) %>%
  summarise(sum.prod = sum(evt_2)) -> cust_evt_2_list_tmp

head(cust_evt_2_list_tmp)
cust_evt_2_list_tmp$evt_2 <- 1; head(cust_evt_2_list_tmp)

cust_evt_2_list_tmp %>%
  select(custid, evt_2) -> evt_2_df

head(evt_2_df); nrow(evt_2_df) # visitors on second event are 27,681 persons

data_evt_1_1 %>%
  left_join(evt_2_df, by = 'custid') -> data_evt_1_2; head(data_evt_1_2) %>% as.data.frame()

## event 3----
data %>%
  filter(date_num >= 1495724400 & date_num <= 1495810800) -> cust_prod_evt_3

cust_prod_evt_3$evt_3 <- 1

cust_prod_evt_3 %>%
  select(custid, evt_3) %>% 
  group_by(custid) %>%
  summarise(sum.prod = sum(evt_3)) -> cust_evt_3_list_tmp; head(cust_evt_3_list_tmp)

cust_evt_3_list_tmp$evt_3 <- 1

cust_evt_3_list_tmp %>%
  select(custid, evt_3) -> evt_3_df; head(evt_3_df) 

nrow(evt_3_df) # visitors on second event are 25,010 persons

data_evt_1_2 %>%
  left_join(evt_3_df, by = 'custid') -> data_evt_1_3; head(data_evt_1_3) %>% as.data.frame() 

## event 4----
data %>%
  filter(date_num >= 1509634800 & date_num <= 1509721200) -> cust_prod_evt_4

cust_prod_evt_4$evt_4 <- 1

cust_prod_evt_4 %>%
  select(custid, evt_4) %>% 
  group_by(custid) %>%
  summarise(sum.prod = sum(evt_4)) -> cust_evt_4_list_tmp; head(cust_evt_4_list_tmp)

cust_evt_4_list_tmp$evt_4 <- 1

cust_evt_4_list_tmp %>%
  select(custid, evt_4) -> evt_4_df; head(evt_4_df) 

nrow(evt_4_df) # visitors on second event are 22,358 persons

data_evt_1_3 %>%
  left_join(evt_4_df, by = 'custid') -> data_evt_1_4; head(data_evt_1_4) %>% as.data.frame() 

## event 5----
data %>%
  filter(date_num >= 1527778800 & date_num <= 1527865200) -> cust_prod_evt_5

cust_prod_evt_5$evt_5 <- 1

cust_prod_evt_5 %>%
  select(custid, evt_5) %>% 
  group_by(custid) %>%
  summarise(sum.prod = sum(evt_5)) -> cust_evt_5_list_tmp; head(cust_evt_5_list_tmp)

cust_evt_5_list_tmp$evt_5 <- 1

cust_evt_5_list_tmp %>%
  select(custid, evt_5) -> evt_5_df; head(evt_5_df) 

nrow(evt_5_df) # visitors on second event are 23,657 persons

data_evt_1_4 %>%
  left_join(evt_5_df, by = 'custid') -> data_evt_1_5; head(data_evt_1_5) %>% as.data.frame() 

head(data_evt_1_5)

##----
data_evt_1_5 %>%
  select(contains('evt_')) %>%
  replace(is.na(.), 0) -> data_evt_df

head(data_evt_df)

data_evt_df %>%
  group_by(custid) %>%
  summarise(sum.evt_1_prod = sum(evt_1),
            sum.evt_2_prod = sum(evt_2),
            sum.evt_3_prod = sum(evt_3),
            sum.evt_4_prod = sum(evt_4),
            sum.evt_5_prod = sum(evt_5)) -> cust_evt_visit_df

head(cust_evt_visit_df, 10) %>% as.data.frame()

cust_evt_visit_df %>%
  select(-custid) %>%
  replace(. >= 1, 1) -> visit_df

cust_evt_visit_df %>%
  select(custid) -> custid_df
  
cust_evt_visit <- cbind(custid_df, visit_df) 
head(cust_evt_visit, 50)
colnames(cust_evt_visit) <- c('custid', 'evt_1_visit', 'evt_2_visit', 'evt_3_visit',
                              'evt_4_visit', 'evt_5_visit')

total_visit_vector <- cust_evt_visit$evt_1_visit + cust_evt_visit$evt_2_visit +
  cust_evt_visit$evt_3_visit + cust_evt_visit$evt_4_visit + cust_evt_visit$evt_5_visit

cust_evt_visit$total_visit <- total_visit_vector
head(cust_evt_visit, 100)
tail(cust_evt_visit, 100)
range(cust_evt_visit$total_visit)

# customers who visit 2 more times on membership days----
cust_evt_visit %>%
  filter(total_visit >= 2) -> cust_evt_visits_2_more

nrow(cust_evt_visits_2_more) # 11,293

head(cust_evt_visits_2_more)

# customers who visit 2 more times on membership days and  on other days too----
head(data) %>% as.data.frame()

data %>%
  filter(custid %in% cust_evt_visits_2_more$custid) -> cust_evt_visits_2_more_trans

cust_evt_visits_2_more_trans %>% head %>% as.data.frame()
cust_evt_visits_2_more_trans %>%
  group_by(custid) %>%
  tally()

cust_evt_visits_2_more_trans %>%
  filter(date_num != 1464274800 &
         date_num != 1464361200 &
         date_num != 1478185200 &
         date_num != 1478271600 & 
         date_num != 1495724400 &
         date_num != 1495810800 &
         date_num != 1509634800 &
         date_num != 1509721200 &
         date_num != 1527778800 &
         date_num != 1527865200) -> cust_evt_2_more_other_visit

cust_evt_2_more_other_visit %>% head %>% as.data.frame()

data %>% 
  filter(custid == 'C00012090') %>% as.data.frame()

cust_evt_2_more_other_visit %>%
  group_by(custid) %>%
  tally() -> cust_evt_2_more_other_visit_df

# customers who visit 2 more times only on membership days and  not on other days----
11293 - 10332 # 961

length(cust_evt_visits_2_more$custid)
length(cust_evt_2_more_other_visit_df$custid)
only_event_cust_vector <- setdiff(cust_evt_visits_2_more$custid, cust_evt_2_more_other_visit_df$custid)


only_event_cust_vector[1:10] 

data %>% 
  filter(custid %in% only_event_cust_vector) -> only_evt_cust_df

only_evt_cust_df %>% filter(custid == "C00001130") %>% as.data.frame()
only_evt_cust_df %>% filter(custid == "C00000758") %>% as.data.frame()
only_evt_cust_df %>% filter(custid == "C00001584") %>% as.data.frame()
only_evt_cust_df %>% filter(custid == "C00001617") %>% as.data.frame()


