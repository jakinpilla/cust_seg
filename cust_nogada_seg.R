
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

rm(list=ls()); gc()

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

as.data.table(data) -> data
head(data)

## 맴버십 데이에만 구매한 고객 추출
as.numeric(data$date[1:5])

# date_num 변수를 생성한다.
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
  select(custid, evt_1) %>% 
  group_by(custid) %>%
  summarise(sum.prod = sum(evt_1)) -> cust_evt_1_list_tmp

head(cust_evt_1_list_tmp)
nrow(cust_evt_1_list_tmp) # 첫 번째 이벤트에 방문한 고객은 총 22,404 명이다.

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
  select(custid, evt_2) %>% 
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
  select(custid, contains('evt_')) %>%
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
head(cust_evt_visit)
colnames(cust_evt_visit) <- c('custid', 'evt_1_visit', 'evt_2_visit', 'evt_3_visit',
                              'evt_4_visit', 'evt_5_visit')

total_visit_vector <- cust_evt_visit$evt_1_visit + cust_evt_visit$evt_2_visit +
  cust_evt_visit$evt_3_visit + cust_evt_visit$evt_4_visit + cust_evt_visit$evt_5_visit

cust_evt_visit$total_visit <- total_visit_vector
head(cust_evt_visit, 10)
tail(cust_evt_visit, 10)
range(cust_evt_visit$total_visit)

cust_evt_visit %>%
  filter(total_visit > 0) %>% nrow

write.csv(cust_evt_visit, './data/cust_evt_visit.csv', row.names = F)

rm(list = ls()); gc()

# 멤버십 데이에 두 번 이상 방문한 고객집단----
load('cust_prod_total_fin.RData')
head(cust_prod_total_fin) %>% as.data.frame()
nrow(cust_prod_total_fin) # 5410838
cust_prod_total_fin %>%
  select(custid, date, grade, qty, amt, sex, age, prod_nm, cate, cate_ftn, cate_line, price) -> data
as.data.table(data) -> data
head(data)

save(data, file = './data/data.RData')
load('./data/data.RData')
class(data)

cust_evt_visit <- read.csv('./data/cust_evt_visit.csv')
head(cust_evt_visit)

cust_evt_visit %>%
  filter(total_visit >= 2) -> cust_evt_visits_2_more

nrow(cust_evt_visits_2_more) # 11,293
head(cust_evt_visits_2_more)

as.data.table(data) -> data
setindex(data, custid) %>% head
indices(data)

data %>%
  filter(custid %in% cust_evt_visits_2_more$custid) -> df_tmp ; head(df_tmp)

df_tmp %>% 
  group_by(custid) %>%
  summarise(sum.amt  = sum(amt)) -> sum_df_tmp

sum(sum_df_tmp$sum.amt)

# customers who visit 2 more times on membership days and  on other days too----
head(data) %>% as.data.frame()

as.data.table(data) -> data
setindex(data, custid) %>% head
indices(data)

data %>%
  filter(custid %in% cust_evt_visits_2_more$custid) -> cust_evt_visits_2_more_trans

head(cust_evt_visits_2_more_trans)

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

cust_evt_2_more_other_visit %>% dim

cust_evt_2_more_other_visit %>%
  group_by(custid) %>%
  tally() -> cust_evt_2_more_other_visit_df

nrow(cust_evt_2_more_other_visit_df)

cust_evt_2_more_other_visit %>%
  group_by(custid) %>%
  summarise(sum.amt = sum(amt)) -> sum_df_tmp; 

sum(sum_df_tmp$sum.amt)

# customers who visit 2 more times only on membership days and  not on other days----
11293 - 10332 # 961

length(cust_evt_visits_2_more$custid)
length(cust_evt_2_more_other_visit_df$custid)
only_event_cust_vector <- setdiff(cust_evt_visits_2_more$custid, cust_evt_2_more_other_visit_df$custid)
length(only_event_cust_vector)

only_event_cust_vector[1:10] 

as.data.table(data) -> data
setindex(data, custid) %>% head
indices(data)

data %>%
  filter(custid %in% only_event_cust_vector) -> df_tmp; head(df_tmp)

dim(df_tmp)

df_tmp %>%
  group_by(custid) %>%
  summarise(sum.amt = sum(amt)) -> sum_df_tmp;

sum(sum_df_tmp$sum.amt) # 111560443

df_tmp %>%
  group_by(prod_nm) %>%
  tally() %>%
  arrange(desc(n)) -> more2_only_evt_cust_prod_nm

dim(more2_only_evt_cust_prod_nm)
more2_only_evt_cust_prod_nm %>% as.data.frame() %>% head(30)

only_evt_cust_df %>% filter(custid == "C00001130") %>% as.data.frame()
only_evt_cust_df %>% filter(custid == "C00000758") %>% as.data.frame()
only_evt_cust_df %>% filter(custid == "C00001584") %>% as.data.frame()
only_evt_cust_df %>% filter(custid == "C00001617") %>% as.data.frame()

head(cust_evt_2_more_other_visit_df)
nrow(cust_evt_2_more_other_visit_df)

# 이벤트에만 2번 이상 오고 이벤트가 아닌 다른 날에도 구매한 고객 : 10332명----
more2_other_visit_cust_vector <- cust_evt_2_more_other_visit_df$custid
length(more2_other_visit_cust_vector)

only_event_cust_vector # 이벤트에만 2번 이상오고 다른 날에는 구매한 고객 
length(only_event_cust_vector) # 961명

10332 + 961

head(cust_evt_2_more_other_visit) %>% as.data.frame()
cust_evt_2_more_other_visit_prod_nm <- unique(cust_evt_2_more_other_visit$prod_nm)
length(cust_evt_2_more_other_visit_prod_nm) # 1489개의 제품


only_evt_cust_df %>% head %>% as.data.frame()
head(data) %>% as.data.frame()
unique(only_evt_cust_df$prod_nm)
length(unique(only_evt_cust_df$prod_nm))

more2_not_only_evt <- setdiff(cust_evt_2_more_other_visit_prod_nm, unique(only_evt_cust_df$prod_nm))
length(more2_not_only_evt)

# 멤버십 데이에 2번 이상 오고 다른 날에도 구매한 고객집단이 구매한 제품 정보----
as.data.table(data) -> data
setindex(data, custid) %>% head
indices(data)

data %>%
  filter(custid %in% cust_evt_2_more_other_visit_df$custid) -> df_tmp; head(df_tmp)
  
df_tmp %>%
  group_by(prod_nm) %>%
  tally() %>%
  arrange(desc(n)) -> more2_evt_other_visit_cust_prod_nm

dim(more2_evt_other_visit_cust_prod_nm)
more2_evt_other_visit_cust_prod_nm %>% as.data.frame %>% head(30)

length(setdiff(more2_evt_other_visit_cust_prod_nm$prod_nm, more2_only_evt_cust_prod_nm$prod_nm))

as.data.table(more2_evt_other_visit_cust_prod_nm) -> dt.more2_evt_other_visit_cust_prod_nm
dt.more2_evt_other_visit_cust_prod_nm %>% head
setindex(dt.more2_evt_other_visit_cust_prod_nm, prod_nm) %>% head
indices(dt.more2_evt_other_visit_cust_prod_nm)

dt.more2_evt_other_visit_cust_prod_nm %>%
  filter(!(prod_nm %in% more2_only_evt_cust_prod_nm$prod_nm)) -> df_tmp; head(df_tmp, 30)
nrow(df_tmp)

# 멤버십 데이에 전혀 오지 않았지만 구매를 한 고객 집단 : 1,238,076명----
head(cust_evt_visit, 10)
dim(cust_evt_visit)

cust_evt_visit %>%
  filter(total_visit == 0) -> not_evt_cust_df

head(not_evt_cust_df)
dim(not_evt_cust_df) # 1238076

# 이벤트일때 방문한 고객수 : 106,269----
1344345 - 1238076

# 첫 번째 이벤트일때 방문한 고객 수 : 22,404명----
cust_evt_visit %>%
  filter(evt_1_visit == 1) %>% nrow() # 22404

# 두 번째 이벤트일때 방문한 고객 수 : 22,681명----
cust_evt_visit %>%
  filter(evt_2_visit == 1) %>% nrow() # 27,681

# 세 번째 이벤트일때 방문한 고객 수 : 25,010명----
cust_evt_visit %>%
  filter(evt_3_visit == 1) %>% nrow() # 25,010명

# 네 번째 이벤트일때 방문한 고객 수 : 22,358명----
cust_evt_visit %>%
  filter(evt_4_visit == 1) %>% nrow() # 22,358명

# 다섯 번째 이벤트일때 방문한 고객 수 : 23,657명----
cust_evt_visit %>%
  filter(evt_5_visit == 1) %>% nrow() # 23,657명

22404 + 27681 + 25010 + 22358 + 23657

# 멤버십 데이에 전혀 오지 않았던 고객들의 transaction data 추출, 구매횟수----
as.data.table(data) -> data
setindex(data, custid) %>% head
indices(data)

data %>%
  filter(custid %in% not_evt_cust_df$custid) -> df_tmp; head(df_tmp)

df_tmp %>%
  group_by(custid) %>%
  dplyr::summarize(frequency=n_distinct(date)) -> df_freq

head(df_freq)
dim(df_freq)
# 68512명은 멤버십일때도 오고 아닐 때도 오는 고객

1306588 - 68512

# 멤버십 데이에 전혀 오지 않았고 방문을 14, 13, 12, ..., 3, 2, 1회 한 고객집단----

# 14번 방문했으나 이벤트에는 한 번도 안 온 고객 :: 865명----
df_freq %>% filter(frequency == 14) %>% nrow() # 865명
df_freq %>% 
  filter(frequency == 14) -> df14_tmp; head(df14_tmp)

data %>% 
  filter(custid %in% df14_tmp$custid) %>%
  summarise(sum.amt = sum(amt)) # 매출액 : 622,329,632

data %>% 
  filter(custid %in% df14_tmp$custid) %>%
  group_by(custid) %>%
  summarise(sum.amt = sum(amt)) %>%
  arrange(desc(sum.amt)) -> df_tmp; head(df_tmp)

sum(df_tmp$sum.amt) # 622,329,632원

# 13번 방문했으나 이벤트에는 한 번도 안 온 고객 :: 1,134명
df_freq %>% filter(frequency == 13) # 1,134명

df_freq %>% filter(frequency == 13) %>% nrow() # 1,134명

df_freq %>% 
  filter(frequency == 13) -> df13_tmp; head(df13_tmp)

data %>% 
  filter(custid %in% df13_tmp$custid) %>%
  summarise(sum.amt = sum(amt)) # 매출액 : 740,575,840원

# 12번 방문했으나 이벤트에는 한 번도 안 온 고객 :: 1,447명
df_freq %>% filter(frequency == 12) %>% nrow() # 1,447명

df_freq %>% 
  filter(frequency == 12) -> df12_tmp; head(df12_tmp)

data %>% 
  filter(custid %in% df12_tmp$custid) %>%
  summarise(sum.amt = sum(amt)) # 매출액 : 847,468,929원

# 11번 방문했으나 이벤트에는 한 번도 안 온 고객 :: 1,963명
df_freq %>% filter(frequency == 11) %>% nrow() # 1,447명

df_freq %>% 
  filter(frequency == 11) -> df11_tmp; head(df11_tmp)

data %>% 
  filter(custid %in% df11_tmp$custid) %>%
  summarise(sum.amt = sum(amt)) # 매출액 : 1,016,924,137원

# 10번 방문했으나 이벤트에는 한 번도 안 온 고객 :: 2,812명
df_freq %>% filter(frequency == 10) %>% nrow() # 2,812명

df_freq %>% 
  filter(frequency == 10) -> df10_tmp; head(df10_tmp)

data %>% 
  filter(custid %in% df10_tmp$custid) %>%
  summarise(sum.amt = sum(amt)) # 매출액 : 1,288,727,220원

# 9번 방문했으나 이벤트에는 한 번도 안 온 고객 :: 3,836명
df_freq %>% filter(frequency == 9) %>% nrow() # 3,836명

df_freq %>% 
  filter(frequency == 9) -> df9_tmp; head(df9_tmp)

data %>% 
  filter(custid %in% df9_tmp$custid) %>%
  summarise(sum.amt = sum(amt)) # 매출액 : 1,582,947,955원

# 8번 방문했으나 이벤트에는 한 번도 안 온 고객 :: 3,836명
df_freq %>% filter(frequency == 8) %>% nrow() # 3,836명

df_freq %>% 
  filter(frequency == 8) -> df8_tmp; head(df8_tmp)

data %>% 
  filter(custid %in% df8_tmp$custid) %>%
  summarise(sum.amt = sum(amt)) # 매출액 : 2,043,098,305원

# 7번 방문했으나 이벤트에는 한 번도 안 온 고객 :: # 8,491명
df_freq %>% filter(frequency == 7) %>% nrow() # 8,491명

df_freq %>% 
  filter(frequency == 7) -> df7_tmp; head(df7_tmp)

data %>% 
  filter(custid %in% df7_tmp$custid) %>%
  summarise(sum.amt = sum(amt)) # 매출액 : 2,643,490,957원

# 6번 방문했으나 이벤트에는 한 번도 안 온 고객 :: # 13,558명
df_freq %>% filter(frequency == 6) %>% nrow() # 13,558명

df_freq %>% 
  filter(frequency == 6) -> df6_tmp; head(df6_tmp)

data %>% 
  filter(custid %in% df6_tmp$custid) %>%
  summarise(sum.amt = sum(amt)) # 매출액 : 3,544,422,321원

# 5번 방문했으나 이벤트에는 한 번도 안 온 고객 :: # 23,018명
df_freq %>% filter(frequency == 5) %>% nrow() # 23,018명

df_freq %>% 
  filter(frequency == 5) -> df5_tmp; head(df5_tmp)

data %>% 
  filter(custid %in% df5_tmp$custid) %>%
  summarise(sum.amt = sum(amt)) # 매출액 : 4,932,215,028원

# 4번 방문했으나 이벤트에는 한 번도 안 온 고객 :: # 42,166명
df_freq %>% filter(frequency == 4) %>% nrow() # 42,166명

df_freq %>% 
  filter(frequency == 4) -> df4_tmp; head(df4_tmp)

data %>% 
  filter(custid %in% df4_tmp$custid) %>%
  summarise(sum.amt = sum(amt)) # 매출액 : 7,072,840,661원

# 3번 방문했으나 이벤트에는 한 번도 안 온 고객 :: #  87,883명
df_freq %>% filter(frequency == 3) %>% nrow() #  87,883명

df_freq %>% 
  filter(frequency == 3) -> df3_tmp; head(df3_tmp)

data %>% 
  filter(custid %in% df3_tmp$custid) %>%
  summarise(sum.amt = sum(amt)) # 매출액 : 10,846,824,062원

# 2번 방문했으나 이벤트에는 한 번도 안 온 고객 :: # 242,346명
df_freq %>% filter(frequency == 2) %>% nrow() #  242,346명

df_freq %>% 
  filter(frequency == 2) -> df2_tmp; head(df2_tmp)

data %>% 
  filter(custid %in% df2_tmp$custid) %>%
  summarise(sum.amt = sum(amt)) # 매출액 : 19,212,250,356원

# 1번 방문했으나 이벤트에는 한 번도 안 온 고객 :: 802,915명
df_freq %>% filter(frequency == 1) %>% nrow() # 802,915명

df_freq %>% 
  filter(frequency == 1) -> df1_tmp; head(df1_tmp)

data %>% 
  filter(custid %in% df1_tmp$custid) %>%
  summarise(sum.amt = sum(amt)) # 매출액 : 32,741,721,289원

865 + 1134 + 1447 + 1963 + 2812 + 3836 + 5642 + 8491 + 13558 + 23018 + 42166 + 87883 + 242346 + 802915

# 멤버십 데이에 한 번만 방문한 고객----
cust_evt_visit %>%
  filter(total_visit == 1) -> cust_evt_visit_only_1

cust_evt_visit_only_1 %>% head %>% as.data.frame()
cust_evt_visit_only_1$custid
length(cust_evt_visit_only_1$custid)

# 멤버십 데이에 한 번만 방문했고 다른 날에도 구매한 고객----
head(data) %>% as.data.frame()

data %>%
  filter(custid %in% cust_evt_visit_only_1$custid) -> cust_evt_visit_1_trans # 시간 소요 다대, 코드 수정 필요

cust_evt_visit_1_trans %>% head %>% as.data.frame()

cust_evt_visit_1_trans %>%
  group_by(custid) %>%
  tally()

cust_evt_visit_1_trans %>%
  filter(date_num != 1464274800 &
           date_num != 1464361200 &
           date_num != 1478185200 &
           date_num != 1478271600 & 
           date_num != 1495724400 &
           date_num != 1495810800 &
           date_num != 1509634800 &
           date_num != 1509721200 &
           date_num != 1527778800 &
           date_num != 1527865200) -> cust_evt_1_other_visit

cust_evt_1_other_visit %>% head %>% as.data.frame()

# 멤버십 데이에 한 번만 방문했고 다른 날에는 전혀 구매하지 않은 고객----
head(data)

dim(data)
data %>% head

data %>%
  filter((date_num >= 1464274800 & date_num <= 1464361200)) %>%
  nrow()

data %>%
  filter((date_num >= 1464274800 & date_num <= 1464361200)) %>%
  group_by(custid) %>%
  tally() # 22,404명

## tmp_flag----  
data %>%
  filter((date_num >= 1464274800 & date_num <= 1464361200) |
           (date_num >= 1478185200 & date_num <= 1478271600) | 
           (date_num >= 1495724400 & date_num <= 1495810800) |
           (date_num >= 1509634800 & date_num <= 1509721200) |
           (date_num >= 1527778800 & date_num <= 1527865200) ) -> evt_trans

evt_trans %>% head
evt_trans %>% dim

evt_trans %>%
  group_by(custid) %>%
  tally() # 이벤트에 온 고객 수 : 106,259명

evt_trans %>%
  as.data.table() %>%
  head

# 첫 번째 이벤트에 방문한 고객들이 구매한 금액 구하기----
evt_trans %>%
  as.data.table() %>%
  filter(date_num >= 1464274800 & date_num <= 1464361200) %>%
  group_by(custid) %>%
  summarise(sum.amt1 = sum(amt)) -> evt_1_sum_amt; head(evt_1_sum_amt)

# 두 번째 이벤트에 방문한 고객들이 구매한 금액 구하기----
evt_trans %>%
  as.data.table() %>%
  filter(date_num >= 1478185200 & date_num <= 1478271600) %>%
  group_by(custid) %>%
  summarise(sum.amt2 = sum(amt)) -> evt_2_sum_amt; head(evt_2_sum_amt)


# 세 번째 이벤트에 방문한 고객들이 구매한 금액 구하기-----
evt_trans %>%
  as.data.table() %>%
  filter(date_num >= 1495724400 & date_num <= 1495810800) %>%
  group_by(custid) %>%
  summarise(sum.amt3 = sum(amt)) -> evt_3_sum_amt; head(evt_3_sum_amt)

# 네 번째 이벤트에 방문한 고객들이 구매한 금액 구하기----
evt_trans %>%
  as.data.table() %>%
  filter(date_num >= 1509634800 & date_num <= 1509721200) %>%
  group_by(custid) %>%
  summarise(sum.amt4 = sum(amt)) -> evt_4_sum_amt; head(evt_4_sum_amt)

# 다서 번째 이벤트에 방문한 고객들이 구매한 금액 구하기----
evt_trans %>%
  as.data.table() %>%
  filter(date_num >= 1527778800 & date_num <= 1527865200) %>%
  group_by(custid) %>%
  summarise(sum.amt5 = sum(amt)) -> evt_5_sum_amt; head(evt_5_sum_amt)


# 결과를 조인하여 보기----
evt_1_sum_amt %>%
  full_join(evt_2_sum_amt, by='custid') %>%
  full_join(evt_3_sum_amt, by='custid') %>%
  full_join(evt_4_sum_amt, by='custid') %>%
  full_join(evt_5_sum_amt, by='custid') %>%
  replace(is.na(.), 0) -> evt_sum_amt_df

head(evt_sum_amt_df); dim(evt_sum_amt_df)
write.csv(evt_sum_amt_df, './data/evt_sum_amt_df.csv', row.names= F)
evt_sum_amt_df <- read.csv('./data/evt_sum_amt_df.csv')
head(evt_sum_amt_df)

# 첫 번째 이벤트에 10만원 미만으로 구매한 고객----
evt_sum_amt_df %>% 
  filter(sum.amt1 < 100000) %>%
  filter(sum.amt1 != 0) -> evt1_10_lower

dim(evt1_10_lower)
head(evt1_10_lower)

# 한편 이벤트가 아닌 기간에 구매한 고객들의 list를 가져와보면----
rm(list = ls()); gc()

head(not_evt_cust_df)
data %>%
  filter(custid %in% not_evt_cust_df$custid) -> df_tmp; head(df_tmp)

head(df_tmp); dim(df_tmp)
length(unique(df_tmp$custid)) # 이벤트에 전혀 반응하지 않은 고객의 수는 1238076명임

# 반대로 이벤트일때 구매한 고객은 106269임

1238076 + 106269 # 전체고객수 1344345명


# 첫 번째 이벤트에 10만원 미만으로 구매한 고객 중 이벤트가 아닌 다른 날에도 구매한 고객
# 먼저 다른 날에도 구매한 고객들의 거래 내역을 찾아 custid를 찾아본다.(이벤트에 구매하였는지 여부는 모르는 상태임.)
data %>% 
  filter(date_num != 1464274800 &
         date_num != 1464361200 &
         date_num != 1478185200 &
         date_num != 1478271600 & 
         date_num != 1495724400 &
         date_num != 1495810800 &
         date_num != 1509634800 &
         date_num != 1509721200 &
         date_num != 1527778800 &
         date_num != 1527865200) -> data.not.evt.trans

data.not.evt.trans %>% 
  group_by(custid) %>%
  tally() -> not.evt.cust.df; head(not.evt.cust.df)
  
length(unique(not.evt.cust.df$custid)) # 1,306,588 명


# 첫 번째 이벤트에 10만원 미만으로 구매한 고객 중 이벤트가 아닌 다른 날에도 구매한 고객한 고객들의 
# 명단을 찾아본다.

  
# 첫 번째 이벤트에 10만원 미만으로 구매한 고객 명단
evt_sum_amt_df %>% 
  filter(sum.amt1 < 100000) %>%
  filter(sum.amt1 != 0) -> tmp; head(tmp)

tmp$custid[1:10]
length(tmp$custid)

# 첫 번째 이벤트에 10만원 미만으로 구매한 고객 명단(tmp$custid) 중 다른 날에도 구매한 고객 명단에 속하는
# 집단을 찾으면, 

intersect(not.evt.cust.df$custid, tmp$custid)
length(intersect(not.evt.cust.df$custid, tmp$custid))

# 첫 번째 이벤트일때 방문한 고객 수 : 22,404명
# 즉, 첫 번째 이벤트에 10만원 미만으로 구매한 고객 20,337 명중 이벤트가 아닌 다른 날에도 구매한 고객 수는 
# 13429 명임.

13429 / 20337 # 66%가 다른 날에도 구매함.

# 두 번째 멤버십 데이에 방문한 고객은 27,681명인데 이 중 10만원 미만으로 구매한 고객의 수는 24,710명임, 
# 이 24,710명 중 이벤트가 아닌 날에도 구매한 고객은 16,252명임. 약 66%가 다른 날에도 구매함.
evt_sum_amt_df %>% 
  filter(sum.amt2 < 100000) %>%
  filter(sum.amt2 != 0) -> tmp; length(tmp$custid)

length(intersect(not.evt.cust.df$custid, tmp$custid)) # 

16252 / 24710 # 0.6577094

# 세 번째 멤버십 데이에 방문한 고객은 25,010명인데 이 중 10만원 미만으로 구매한 고객의 수는 21,868명임, 
# 이 21,868명 중 이벤트가 아닌 날에도 구매한 고객은 14,139명임. 약 65%가 다른 날에도 구매함.
evt_sum_amt_df %>% 
  filter(sum.amt3 < 100000) %>%
  filter(sum.amt3 != 0) -> tmp; length(tmp$custid)

length(intersect(not.evt.cust.df$custid, tmp$custid)) # 

14139 / 21868 # 0.6465612

# 네 번째 멤버십 데이에 방문한 고객은 22,358명인데 이 중 10만원 미만으로 구매한 고객의 수는 19,552명임, 
# 이 19,552명 중 이벤트가 아닌 날에도 구매한 고객은 12,679명임. 약 65%가 다른 날에도 구매함.
evt_sum_amt_df %>% 
  filter(sum.amt4 < 100000) %>%
  filter(sum.amt4 != 0) -> tmp; length(tmp$custid)

length(intersect(not.evt.cust.df$custid, tmp$custid))

12679 / 19552 # 0.6484759

# 다섯 번째 멤버십 데이에 방문한 고객은 23,657명인데 이 중 10만원 미만으로 구매한 고객의 수는 21,048명임, 
# 이 21,048명 중 이벤트가 아닌 날에도 구매한 고객은 13,865명임. 약 66%가 다른 날에도 구매함.
evt_sum_amt_df %>% 
  filter(sum.amt5 < 100000) %>%
  filter(sum.amt5 != 0) -> tmp; length(tmp$custid)

length(intersect(not.evt.cust.df$custid, tmp$custid))

13865 / 21048 # 0.6587324

22404 + 27681 + 25010 + 22358 + 23657

# 이벤트에는 한 번만 구매하고 다른 날에도 구매한 고객집단을 구해보자.
# "총 이벤트 방문 횟수가 1인 집단(A 집단)"에서 "이벤트가 아닌 다른 날에 구매를 한 집단(B집단)"의 고객
# 집단과의 교집합을 구한다.
cust_evt_visit %>%
  filter(total_visit == 1) -> one_visit_custid; head(one_visit_custid)

length(intersect(one_visit_custid$custid, not.evt.cust.df$custid)) # 58,180 명

# 이벤트에만 한 번 구매하고 다른 날에는 방문하지 않은 고객집단을 구해보자
# "총 이벤트 방문 횟수가 1인 집단(A 집단)"에서 "이벤트가 아닌 다른 날에 구매를 한 집단(B집단)"의 고객을
# 제외시킨다(차집합 개념을 적용)
# 먼저, 총 이벤트 방문 횟수가 1인 집단(A 집단)을 구하면,
head(cust_evt_visit)
cust_evt_visit %>%
  filter(total_visit == 1) -> one_visit_custid; head(one_visit_custid)

length(one_visit_custid$custid) # 총 총 이벤트 방문 횟수가 1인 집단(A 집단)은 94,976명

length(setdiff(one_visit_custid$custid, not.evt.cust.df$custid)) # 36,796명


# 이벤트에 대량으로 구매하는 고객집단----

# 먼저 첫 번째 이벤트에 10만원 이상 구매한 고객집단을 구하면, 총 2067명임.
evt_sum_amt_df %>% 
  filter(sum.amt1 >= 100000) -> evt1_10_high; head(evt1_10_high)
nrow(evt1_10_high) # 2067

evt1_10_high %>%
  summarise(sum.totalamt1= sum(sum.amt1)) # 332,748,670


# 두 번째 이벤트에 10만원 이상 구매한 고객집단을 구하면, 2971명임.
evt_sum_amt_df %>% 
  filter(sum.amt2 >= 100000) -> evt2_10_high; head(evt2_10_high)
nrow(evt2_10_high)# 2971명

evt2_10_high %>%
  summarise(sum.totalamt2= sum(sum.amt2)) # 500,413,810

# 세 번째 이벤트에 10만원 이상 구매한 고객집단을 구하면, 3,142명임.
evt_sum_amt_df %>% 
  filter(sum.amt3 >= 100000) -> evt3_10_high; head(evt3_10_high)
nrow(evt3_10_high) # 3,142명

evt3_10_high %>%
  summarise(sum.totalamt3= sum(sum.amt3)) # 455,943,850


# 네 번째 이벤트에 10만원 이상 구매한 고객집단을 구하면, 2,806명임.
evt_sum_amt_df %>% 
  filter(sum.amt4 >= 100000) -> evt4_10_high; head(evt4_10_high)
nrow(evt4_10_high) # 2,806명

evt4_10_high %>%
  summarise(sum.totalamt4= sum(sum.amt4)) # 436,304,163

# 다섯 번째 이벤트에 10만원 이상 구매한 고객집단을 구하면, 2,806명임.
evt_sum_amt_df %>% 
  filter(sum.amt5 >= 100000) -> evt5_10_high; head(evt5_10_high)
nrow(evt5_10_high) # 2,609명

evt5_10_high %>%
  summarise(sum.totalamt5= sum(sum.amt5)) # 437,172,821

# 이벤트에 2번이상 10만원 이상 구매한 고객
head(evt_sum_amt_df, 30) %>% as.data.frame()

evt_sum_amt_df$evt1_sum_high <- ifelse(evt_sum_amt_df$sum.amt1 >= 100000, 1, 0)
evt_sum_amt_df$evt2_sum_high <- ifelse(evt_sum_amt_df$sum.amt2 >= 100000, 1, 0)
evt_sum_amt_df$evt3_sum_high <- ifelse(evt_sum_amt_df$sum.amt3 >= 100000, 1, 0)
evt_sum_amt_df$evt4_sum_high <- ifelse(evt_sum_amt_df$sum.amt4 >= 100000, 1, 0)
evt_sum_amt_df$evt5_sum_high <- ifelse(evt_sum_amt_df$sum.amt5 >= 100000, 1, 0)

evt_sum_amt_df$evt_sum_high_count <- evt_sum_amt_df$evt1_sum_high + 
  evt_sum_amt_df$evt2_sum_high + 
  evt_sum_amt_df$evt3_sum_high + 
  evt_sum_amt_df$evt4_sum_high + 
  evt_sum_amt_df$evt5_sum_high

range(evt_sum_amt_df$evt_sum_high_count)

evt_sum_amt_df %>%
  filter(evt_sum_high_count == 5) %>% 
  select(custid, contains("sum.amt")) %>% as.data.frame()

# 세일기간에만 구매하는 그룹과 세일기간 이외도 함께 구매하는 고객의 제품을 비교
# 먼저 세일기간에만 구매하는 그룹을 구하면

cust_evt_visit %>% head
  
# 이벤트에 방문하여 10만원 미만으로 구매하는 고객집단 중 이벤트외 다른 날에는 전혀 구매를 하지 않는 
# 고객집단을 구하기
# 이벤트에 방문하여 10만원 미만으로 구매하는 고객집단은 이벤트에 한 번 이상 방문한 고객 중 모든
# 이벤트의 거래가 10만원 미만인 고객집단으로 재정의한다. 
head(evt_sum_amt_df)
dim(evt_sum_amt_df)

evt_sum_amt_df %>% 
  filter(sum.amt1 <= 100000 &
           sum.amt2 <= 100000 &
           sum.amt3 <= 100000 &
           sum.amt4 <= 100000 &
           sum.amt5 <= 100000) %>%
  select(custid, contains('sum.amt')) -> evt_every_10_low

head(evt_every_10_low)  
dim(evt_every_10_low)

evt_every_10_low$custid[1:10]  

# 이 중에 이벤트가 아닌 다른 날에는 전혀 구매를 하지 않는 고객은?
# 이 중에 이벤트가 아닌 다른 날에 구매를 한 고객집단을 제외하면 된다. 

data.not.evt.trans %>% head
data.not.evt.trans %>%
  group_by(custid) %>%
  tally() -> cust_tmp; head(cust_tmp)

cust_tmp$custid[1:10] # 이벤트가 아닌 다른 날에 구매를 한 고객집단(이벤트에 구매하였는지 여부는 알 수 없는 상태)

setdiff(evt_every_10_low$custid, cust_tmp$custid)[1:10]
length(setdiff(evt_every_10_low$custid, cust_tmp$custid)) # 36,111명

lower10.evt.not.other.day.custid <- setdiff(evt_every_10_low$custid, cust_tmp$custid)
length(lower10.evt.not.other.day.custid)

data %>%
  filter(custid %in% lower10.evt.not.other.day.custid) %>% 
  summarise(sum.total = sum(amt))  # 1151833740

# 이벤트에 구매이력이 없고 구매횟수가 1 혹은 2인 고개집단을 구해보자
# 먼저, 이벤트에 구매이력이 전혀 없는 집단은 전체 고객 집단에서 이벤트에 구매한 고객집단을 
# 제외하여 구한다.

# 전체 고객집단
data %>%
  group_by(custid) %>%
  tally -> total_cust_ls_df

total_cust_ls_df$custid[1:10] # 전체 고객 집단

# 이벤트에 구매한 고객집단
evt_trans %>% head
evt_trans %>% class
as.data.table(evt_trans) -> evt_trans

evt_trans %>%
  group_by(custid) %>%
  tally() -> evt_cust_ls_df

evt_cust_ls_df$custid[1:10] # 이벤트에 온 고객집단

setdiff(total_cust_ls_df$custid, evt_cust_ls_df$custid) -> never_evt_cust_vector
never_evt_cust_vector[1:10]
  
  
# 이벤트에 구매이력이 없고 구매횟수가 1 혹은 2인 고개집단을 구하면..
data %>%
  filter(custid %in% never_evt_cust_vector) %>% 
  group_by(custid) %>%
  dplyr::summarise(frequency=n_distinct(date)) -> tmp; head(tmp)

nrow(tmp)

tmp %>%
  filter(frequency == 1 | frequency == 2) -> not.evt.1.or.2.cust

nrow(not.evt.1.or.2.cust) # 총 1,045,261명

setindex(data, custid) %>% head
indices(data)

data %>%
  filter(custid %in% not.evt.1.or.2.cust$custid) %>%
  summarise(sum.total.amt = sum(amt)) # 51,953,971,645

data %>%
  filter(custid %in% not.evt.1.or.2.cust$custid) %>%
  group_by(prod_nm) %>%
  tally() -> tmp

tmp %>% arrange(desc(n))
tmp %>% arrange(desc(n)) %>% head(30) %>% as.data.frame()

# 멤버십데이 반응 그룹중  비세일 기간 1 – 2회 구매한 집단을 구하면
# 이벤트 반응 그룹은
evt_cust_ls_df$custid[1:10]

# 이벤트가 아닌 기간에 이벤트에 반응한 고객들이 구매한 거래 데이터를 추출하면
data.not.evt.trans %>% as.data.table() -> data.not.evt.trans.dt
setindex(data.not.evt.trans.dt, custid) %>% head
indices(data.not.evt.trans.dt)

data.not.evt.trans.dt %>%
  filter(custid %in% evt_cust_ls_df$custid) -> tmp; head(tmp)

# 이 데이터에서 고객들의 구매횟수를 구하면
tmp %>% as.data.table() -> tmp.dt
tmp.dt %>%
  group_by(custid) %>%
  dplyr::summarise(frequency=n_distinct(date)) -> temp; head(temp)

# 구매횟수가 1 혹은 2인 고객들을 추출하면
temp %>%
  filter(frequency == 1 | frequency == 2) -> temp; head(temp)

head(temp)
# 멤버십데이 반응 그룹중  비세일 기간 1 – 2회 구매한 집단을 구하면
nrow(temp) # 총 33,477명

# 이 집단이 구매한 상위 30개의 제품 목록을 구하면
data %>%
  filter(custid %in% temp$custid) %>%
  group_by(prod_nm) %>%
  tally() %>%
  arrange(desc(n)) %>%
  head(30) %>%
  as.data.frame()

# 구매횟수가 3회이상이나 멤버십데이 구매 없는 고객집단과 이들에 의한 총 매출액을 구하면
# 구매횟수가 3회이상인 고객집단
data %>%
  group_by(custid) %>%
  dplyr::summarise(frequency=n_distinct(date)) %>% 
  filter(frequency >= 3) -> tmp; head(tmp)

tmp$custid[1:10] # 구매횟수가 3회이상인 고객집단
length(tmp$custid) # 243,032

never_evt_cust_vector[1:10] # 이벤트에 구매 없는 고객 집단

length(intersect(tmp$custid, never_evt_cust_vector)) # 192,815명
tmp.ls <- intersect(tmp$custid, never_evt_cust_vector)

data %>%
  filter(custid %in% tmp.ls) %>%
  summarise(sum.total = sum(amt)) # 37,181,865,047

37181865047 / 243032

##----
# '16.12.31 기준 1년간 거래를 한 번도 하지 않은 고객집단을 구해보자.
# 이탈고객 집단 : 16.12월 기준으로 1년이상 구매하지 않은 집단
# '16.1.1~'16.12.31 기간 중 구매거래가 없는 고객 집단을 찾아본다.

as.numeric(as.POSIXct(as.character('2016-01-01'))) # 1451574000
as.numeric(as.POSIXct(as.character('2016-12-31'))) # 1483110000

# 먼저 전체 거래 데이터에서 해당 기간에 대한 거래 데이터만을 추출해본다.
data %>%
  filter(date_num >= 1451574000 & date_num <= 1483110000) -> data.tmp

# 전체 고객 명단에서 추출된 데이터에 있는 고객 집단을 제거한다.
data.tmp %>%
  group_by(custid) %>%
  summarise(sum.amt = sum(amt)) -> tmp; head(tmp)

head(tmp); dim(tmp) # 지난 1년간 구매를 한 고객집단은 792017명이다.
# 그러므로 지난 1년간 구매를 하지 않은 고개 집단은 
1344345 - 792017 # 552328명이다.

cust_evt_visit %>%
  left_join(tmp, by='custid') -> cust_160101_161231

head(cust_160101_161231)
cust_160101_161231 %>% 
  select(custid, sum.amt) %>%
  rename(year_161231 = sum.amt) -> tmp; 
head(tmp)

# 구매를 한 집단을 1, 구매를 하지 않은 집단을 0으로 표시한다.
tmp$year_161231 <- ifelse(is.na(tmp$year_161231) == T, 0, 1)
head(tmp)
sum(tmp$year_161231)
tmp_161231 <- tmp

##----
# '17.06.30 기준 1년간 거래를 한 번도 하지 않은 고객집단을 구해보자.
# 이탈고객 집단 : 17.06월 기준으로 1년이상 구매하지 않은 집단
# '16.6.30~'17.6.30 기간 중 구매거래가 없는 고객 집단을 찾아본다.

as.numeric(as.POSIXct(as.character('2016-06-30'))) # 1467212400
as.numeric(as.POSIXct(as.character('2017-06-30'))) # 1498748400

# 먼저 전체 거래 데이터에서 해당 기간에 대한 거래 데이터만을 추출해본다.
data %>%
  filter(date_num >= 1467212400 & date_num <= 1498748400) -> data.tmp

# 전체 고객 명단에서 추출된 데이터에 있는 고객 집단을 제거한다.
data.tmp %>%
  group_by(custid) %>%
  summarise(sum.amt = sum(amt)) -> tmp; head(tmp)

head(tmp); dim(tmp) # 지난 1년간 구매를 한 고객집단은 711,027명이다.
# 그러므로 지난 1년간 구매를 하지 않은 고개 집단은 
1344345 - 711027 # 554533명이다.

cust_evt_visit %>%
  left_join(tmp, by='custid') -> cust_160630_170630

head(cust_160630_170630)
cust_160630_170630 %>% 
  select(custid, sum.amt) %>%
  rename(year_170630 = sum.amt) -> tmp; 
head(tmp)

# 구매를 한 집단을 1, 구매를 하지 않은 집단을 0으로 표시한다.
tmp$year_170630 <- ifelse(is.na(tmp$year_170630) == T, 0, 1)
head(tmp)
sum(tmp$year_170630)
tmp_170630 <- tmp

##----
# '17.12.31 기준 1년간 거래를 한 번도 하지 않은 고객집단을 구해보자.
# 이탈고객 집단 : 17.06월 기준으로 1년이상 구매하지 않은 집단
# '16.6.30~'17.6.30 기간 중 구매거래가 없는 고객 집단을 찾아본다.

as.numeric(as.POSIXct(as.character('2017-01-01'))) # 1483196400
as.numeric(as.POSIXct(as.character('2017-12-31'))) # 1514646000

# 먼저 전체 거래 데이터에서 해당 기간에 대한 거래 데이터만을 추출해본다.
data %>%
  filter(date_num >= 1483196400 & date_num <= 1514646000) -> data.tmp

# 전체 고객 명단에서 추출된 데이터에 있는 고객 집단을 제거한다.
data.tmp %>%
  group_by(custid) %>%
  summarise(sum.amt = sum(amt)) -> tmp; head(tmp)

head(tmp); dim(tmp) # 지난 1년간 구매를 한 고객집단지난 1년간 구매를 하지 않은 고개 집단은 은 613,080명이다.
# 그러므로 
1344345 - 613080 # 731,265명이다.

cust_evt_visit %>%
  left_join(tmp, by='custid') -> cust_170101_171231

head(cust_170101_171231)
cust_170101_171231 %>% 
  select(custid, sum.amt) %>%
  rename(year_171231 = sum.amt) -> tmp; 
head(tmp)

# 구매를 한 집단을 1, 구매를 하지 않은 집단을 0으로 표시한다.
tmp$year_171231 <- ifelse(is.na(tmp$year_171231) == T, 0, 1)
head(tmp)
sum(tmp$year_171231)
tmp_171231 <- tmp

##----
# '18.6.30 기준 1년간 거래를 한 번도 하지 않은 고객집단을 구해보자.
# 이탈고객 집단 : 18.6월 기준으로 1년이상 구매하지 않은 집단
# '17.6.30~'18.6.30 기간 중 구매거래가 없는 고객 집단을 찾아본다.

as.numeric(as.POSIXct(as.character('2017-06-30'))) # 1498748400
as.numeric(as.POSIXct(as.character('2018-06-30'))) # 1530284400

# 먼저 전체 거래 데이터에서 해당 기간에 대한 거래 데이터만을 추출해본다.
data %>%
  filter(date_num >= 1498748400 & date_num <= 1530284400) -> data.tmp

# 전체 고객 명단에서 추출된 데이터에 있는 고객 집단을 제거한다.
data.tmp %>%
  group_by(custid) %>%
  summarise(sum.amt = sum(amt)) -> tmp; head(tmp)

head(tmp); dim(tmp) # 지난 1년간 구매를 한 고객집단은 544319명이다.
# 그러므로 지난 1년간 구매를 하지 않은 고개 집단은 
1344345 - 544319 # 800026명이다.

cust_evt_visit %>%
  left_join(tmp, by='custid') -> cust_170630_180630

head(cust_170630_180630)
cust_170630_180630 %>% 
  select(custid, sum.amt) %>%
  rename(year_180630 = sum.amt) -> tmp; 
head(tmp)

# 구매를 한 집단을 1, 구매를 하지 않은 집단을 0으로 표시한다.
tmp$year_180630 <- ifelse(is.na(tmp$year_180630) == T, 0, 1)
head(tmp)
sum(tmp$year_180630)
tmp_180630 <- tmp


head(tmp_161231)
head(tmp_170630)
head(tmp_171231)
head(tmp_180630)

tmp_161231 %>%
  left_join(tmp_170630) %>%
  left_join(tmp_171231) %>%
  left_join(tmp_180630) -> year_df

# year_161231 == 1이고 year_170630 == 0 인데, year_171231==1 혹은 year_180630==1 인
# 다시 돌아온 고객
year_df %>%
  filter(year_161231 == 1 & year_170630 == 0) %>%
  filter(year_171231 == 1 | year_180630 == 1) %>%
  nrow() # 32,928명

# year_161231 == 1이고 year_170630 == 0 인데, year_171231== 0 이고 year_180630==0 인
# 다시 돌아오지 않은 고객
year_df %>%
  filter(year_161231 == 1 & year_170630 == 0) %>%
  filter(year_171231 == 0 & year_180630 == 0) %>%
  nrow() # 277,993명

year_df %>%
  filter(year_161231 == 1 & year_170630 == 0) %>%
  filter(year_171231 == 0 & year_180630 == 0) 

277993/32928

# year_170630 == 1 이고 year_171231==0 인데, year_180630==1 인 다시 돌아온 고객
year_df %>%
  filter(year_170630 == 1 & year_171231 == 0) %>%
  filter(year_180630 == 1) %>%
  nrow() # 19,071명

# 이렇게 돌아온 고객집단은 year_161231 == 1이라는 특징이 있음.
year_df %>%
  filter(year_170630 == 1 & year_171231 == 0) %>%
  filter(year_180630 == 1)

# year_170630 == 1 이고 year_171231==0 인데, year_180630==0 인 다시 돌아오지 않은 고객
year_df %>%
  filter(year_170630 == 1 & year_171231 == 0) %>%
  filter(year_180630 == 0) %>%
  nrow() # 273,345명

273345/19071

# ’18. 6.30을 기준으로 7개월간 구매를 하지 않는 고객
# '17.12.01 ~ '18.6.30 기간 동안 전혀 구매를 하지 않는 고객집단

as.numeric(as.POSIXct(as.character('2017-12-01'))) # 1512054000
as.numeric(as.POSIXct(as.character('2018-06-30'))) # 1530284400

# 먼저 전체 거래 데이터에서 해당 기간에 대한 거래 데이터만을 추출해본다.
data %>%
  filter(date_num >= 1512054000 & date_num <= 1530284400) -> data.tmp

# 전체 고객 명단에서 추출된 데이터에 있는 고객 집단을 제거한다.
data.tmp %>%
  group_by(custid) %>%
  summarise(sum.amt = sum(amt)) -> tmp; head(tmp)

head(tmp); dim(tmp) # 해당 기간에 구매를 한 고객집단은 323,130명이다.
# 그러므로 지난 7개월간 구매를 하지 않은 고객 집단은 
1344345 - 323130 # 1021215명이다.


















