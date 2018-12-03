
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
as.data.table(data) -> dt.data
dt.data %>%
  filter(date_num >= 1464274800 & date_num <= 1464361200) -> cust_prod_1_evt

head(cust_prod_1_evt) %>% as.data.frame
nrow(cust_prod_1_evt) # 56259

cust_prod_1_evt$evt_1 <- 1

cust_prod_1_evt %>%
  select(custid, evt_1) %>% 
  group_by(custid) %>%
  summarise(sum.prod = sum(evt_1)) -> cust_evt_1_list_tmp

head(cust_evt_1_list_tmp)
nrow(cust_evt_1_list_tmp) # visitors on second event are 22,404 persons

cust_evt_1_list_tmp$evt_1 <- 1; head(cust_evt_1_list_tmp)
cust_evt_1_list_tmp %>%
  select(custid, evt_1) -> evt_1_df

head(evt_1_df); dim(evt_1_df)

dt.data %>%
  left_join(evt_1_df, by = 'custid') -> data_evt_1_1; head(data_evt_1_1) %>% as.data.frame()

dim(data_evt_1_1)

data_evt_1_1 %>%
  filter(evt_1 == 1) %>% head %>% as.data.frame()

head(data_evt_1_1) %>% as.data.frame()

## event 2----
dt.data %>%
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
dt.data %>%
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
dt.data %>%
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
dt.data %>%
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
head(cust_evt_visit, 50)
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

cust_evt_visit %>%
  select(-X) -> cust_evt_visit

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

as.data.table(data) -> dt.data
setindex(dt.data, custid) %>% head
indices(dt.data)

dt.data %>%
  filter(custid %in% cust_evt_visits_2_more$custid) -> df_tmp ; head(df_tmp)

df_tmp %>% 
  group_by(custid) %>%
  summarise(sum.amt  = sum(amt)) -> sum_df_tmp

sum(sum_df_tmp$sum.amt)

# customers who visit 2 more times on membership days and  on other days too----
head(data) %>% as.data.frame()

as.data.table(data) -> dt.data
setindex(dt.data, custid) %>% head
indices(dt.data)

dt.data %>%
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

as.data.table(data) -> dt.data
setindex(dt.data, custid) %>% head
indices(dt.data)

dt.data %>%
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
as.data.table(data) -> dt.data
setindex(dt.data, custid) %>% head
indices(dt.data)

dt.data %>%
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
as.data.table(data) -> dt.data
setindex(dt.data, custid) %>% head
indices(dt.data)

dt.data %>%
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

dt.data %>% 
  filter(custid %in% df14_tmp$custid) %>%
  summarise(sum.amt = sum(amt)) # 매출액 : 622,329,632

dt.data %>% 
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

dt.data %>% 
  filter(custid %in% df13_tmp$custid) %>%
  summarise(sum.amt = sum(amt)) # 매출액 : 740,575,840원

# 12번 방문했으나 이벤트에는 한 번도 안 온 고객 :: 1,447명
df_freq %>% filter(frequency == 12) %>% nrow() # 1,447명

df_freq %>% 
  filter(frequency == 12) -> df12_tmp; head(df12_tmp)

dt.data %>% 
  filter(custid %in% df12_tmp$custid) %>%
  summarise(sum.amt = sum(amt)) # 매출액 : 847,468,929원

# 11번 방문했으나 이벤트에는 한 번도 안 온 고객 :: 1,963명
df_freq %>% filter(frequency == 11) %>% nrow() # 1,447명

df_freq %>% 
  filter(frequency == 11) -> df11_tmp; head(df11_tmp)

dt.data %>% 
  filter(custid %in% df11_tmp$custid) %>%
  summarise(sum.amt = sum(amt)) # 매출액 : 1,016,924,137원

# 10번 방문했으나 이벤트에는 한 번도 안 온 고객 :: 2,812명
df_freq %>% filter(frequency == 10) %>% nrow() # 2,812명

df_freq %>% 
  filter(frequency == 10) -> df10_tmp; head(df10_tmp)

dt.data %>% 
  filter(custid %in% df10_tmp$custid) %>%
  summarise(sum.amt = sum(amt)) # 매출액 : 1,288,727,220원

# 9번 방문했으나 이벤트에는 한 번도 안 온 고객 :: 3,836명
df_freq %>% filter(frequency == 9) %>% nrow() # 3,836명

df_freq %>% 
  filter(frequency == 9) -> df9_tmp; head(df9_tmp)

dt.data %>% 
  filter(custid %in% df9_tmp$custid) %>%
  summarise(sum.amt = sum(amt)) # 매출액 : 1,582,947,955원

# 8번 방문했으나 이벤트에는 한 번도 안 온 고객 :: 3,836명
df_freq %>% filter(frequency == 8) %>% nrow() # 3,836명

df_freq %>% 
  filter(frequency == 8) -> df8_tmp; head(df8_tmp)

dt.data %>% 
  filter(custid %in% df8_tmp$custid) %>%
  summarise(sum.amt = sum(amt)) # 매출액 : 2,043,098,305원

# 7번 방문했으나 이벤트에는 한 번도 안 온 고객 :: # 8,491명
df_freq %>% filter(frequency == 7) %>% nrow() # 8,491명

df_freq %>% 
  filter(frequency == 7) -> df7_tmp; head(df7_tmp)

dt.data %>% 
  filter(custid %in% df7_tmp$custid) %>%
  summarise(sum.amt = sum(amt)) # 매출액 : 2,643,490,957원

# 6번 방문했으나 이벤트에는 한 번도 안 온 고객 :: # 13,558명
df_freq %>% filter(frequency == 6) %>% nrow() # 13,558명

df_freq %>% 
  filter(frequency == 6) -> df6_tmp; head(df6_tmp)

dt.data %>% 
  filter(custid %in% df6_tmp$custid) %>%
  summarise(sum.amt = sum(amt)) # 매출액 : 3,544,422,321원

# 5번 방문했으나 이벤트에는 한 번도 안 온 고객 :: # 23,018명
df_freq %>% filter(frequency == 5) %>% nrow() # 23,018명

df_freq %>% 
  filter(frequency == 5) -> df5_tmp; head(df5_tmp)

dt.data %>% 
  filter(custid %in% df5_tmp$custid) %>%
  summarise(sum.amt = sum(amt)) # 매출액 : 4,932,215,028원

# 4번 방문했으나 이벤트에는 한 번도 안 온 고객 :: # 42,166명
df_freq %>% filter(frequency == 4) %>% nrow() # 42,166명

df_freq %>% 
  filter(frequency == 4) -> df4_tmp; head(df4_tmp)

dt.data %>% 
  filter(custid %in% df4_tmp$custid) %>%
  summarise(sum.amt = sum(amt)) # 매출액 : 7,072,840,661원

# 3번 방문했으나 이벤트에는 한 번도 안 온 고객 :: #  87,883명
df_freq %>% filter(frequency == 3) %>% nrow() #  87,883명

df_freq %>% 
  filter(frequency == 3) -> df3_tmp; head(df3_tmp)

dt.data %>% 
  filter(custid %in% df3_tmp$custid) %>%
  summarise(sum.amt = sum(amt)) # 매출액 : 10,846,824,062원

# 2번 방문했으나 이벤트에는 한 번도 안 온 고객 :: # 242,346명
df_freq %>% filter(frequency == 2) %>% nrow() #  242,346명

df_freq %>% 
  filter(frequency == 2) -> df2_tmp; head(df2_tmp)

dt.data %>% 
  filter(custid %in% df2_tmp$custid) %>%
  summarise(sum.amt = sum(amt)) # 매출액 : 19,212,250,356원

# 1번 방문했으나 이벤트에는 한 번도 안 온 고객 :: 802,915명
df_freq %>% filter(frequency == 1) %>% nrow() # 802,915명

df_freq %>% 
  filter(frequency == 1) -> df1_tmp; head(df1_tmp)

dt.data %>% 
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
dim(dt.data)
dt.data %>% head

dt.data %>%
  filter((date_num >= 1464274800 & date_num <= 1464361200)) %>%
  nrow()

dt.data %>%
  filter((date_num >= 1464274800 & date_num <= 1464361200)) %>%
  group_by(custid) %>%
  tally() # 22,404명
  
dt.data %>%
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





# 첫 번째 이벤트에 10만원 미만으로 구매한 고객 중 두 번째 이벤트에 방문한 고객
evt_sum_amt_df %>% 
  filter(sum.amt1 < 100000) %>%
  filter(sum.amt1 != 0) %>%
  filter(sum.amt2 != 0) # 1,703명

# 첫 번째 이벤트에 10만원 미만으로 구매한 고객 중 두 번째 이벤트에 방문한 고객의 비율
1703 /20327 # 0.08378019 약 8.4%
  
# 첫 번째 이벤트에 10만원 이사으로 구매한 고객이면서 두 번째 이벤트에 방문한 고객 
evt_sum_amt_df %>% 
  filter(sum.amt1 >= 100000) %>%
  filter(sum.amt1 != 0)

%>%
  filter(sum.amt2 != 0)




























