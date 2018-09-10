# loading data
sale <- read.csv('./data/transaction.csv', stringsAsFactors=F)
head(sale)
str(sale)

library(plyr)
library(dplyr)

##
sale %>% 
  mutate(ymd_time = paste(ymd, time)) -> sale

str(sale)
sale$ymd_time <- as.POSIXct(sale$ymd_time)
head(sale[, c(7,2,3,4,5,6)])
sale <- sale[, c(7,2,3,4,5,6)]

##
sale %>% 
  mutate(ymd_time_custid = paste(ymd, time, custid)) %>%
  distinct(ymd_time_custid) -> df_F
head(df_F)

df_F %>%
  mutate(ymd = substr(ymd_time_custid, 1, 10),
         time = substr(ymd_time_custid, 12, 16),
         custid =substr(ymd_time_custid, 18, 22)) -> df_F
head(df_F)
nrow(df_F) # 총 16175건의 장바구니 확인

# df_F에서 고객별로 묶어 행의 갯수를 세어 해당 고객의 총 방문회수를 구하여 userF에 저장
df_F %>%
  group_by(custid) %>%
  summarise(frequency = n()) -> userF
head(userF)
str(userF)

# 고객의 최초 방문일자(minRecency)
# 고객의 최근 방문일자(recency)
# 방문기간(period = recency - minRecency)
# 고객의 총 지출금액(monetary)

max(sale$ymd_time)
min(sale$ymd_time)
head(sale)

sale %>%
  group_by(custid) %>%
  summarise(minRecency = min(ymd_time),
            recency = max(ymd_time),
            period = as.numeric(max(ymd_time) - min(ymd_time)),
            monetary = sum(amt)) -> userRFM

head(userRFM)
nrow(userRFM)

# join between userRFM and userF

left_join(userRFM, userF, by='custid') -> userRFM
head(userRFM)

userRFM$minDate <- as.Date(userRFM$minRecency, origin='1970-01-01')
userRFM$maxDate <- as.Date(userRFM$recency, origin='1970-01-01')

head(userRFM)
# View(head(userRFM))

windows()
hist(userRFM$maxDate, breaks=100, main='Guest Recency')
range(userRFM$maxDate)

table(userRFM$maxDate)
plot(table(userRFM$maxDate), main='Guest Frequency')

###################################################################################
# real data
###################################################################################

library(readxl)

# April data loading
cust_mon_04 <- read_excel("data/cust_mon_201804.xlsx")
head(cust_mon_04)
dim(cust_mon_04) # 146,514건의 거래내용 확인
colnames(cust_mon_04) <- c('date', 'custid', 'grade', 'prod_code', 'qty', 'amt')
head(cust_mon_04)

# cust_mon_04$grade 컬럼의 내용을 영문으로 변경
unique(cust_mon_04$grade)

cust_mon_04$grade[cust_mon_04$grade == '바디러브'] <- 'bodylove'
cust_mon_04$grade[cust_mon_04$grade == '클럽'] <- 'club'
cust_mon_04$grade[cust_mon_04$grade == '골드'] <- 'gold'
cust_mon_04$grade[cust_mon_04$grade == '웹멤버'] <- 'webmember'

unique(cust_mon_04$grade)
head(cust_mon_04)

# May data loading
cust_mon_05 <- read_excel("data/cust_mon_201805.xlsx")
head(cust_mon_05)
dim(cust_mon_05) # 217,717건의 거래내용 확인
colnames(cust_mon_05) <- c('date', 'custid', 'grade', 'prod_code', 'qty', 'amt')
head(cust_mon_05)

# cust_mon_05$grade 컬럼의 내용을 영문으로 변경
unique(cust_mon_05$grade)

cust_mon_05$grade[cust_mon_05$grade == '바디러브'] <- 'bodylove'
cust_mon_05$grade[cust_mon_05$grade == '클럽'] <- 'club'
cust_mon_05$grade[cust_mon_05$grade == '골드'] <- 'gold'
cust_mon_05$grade[cust_mon_05$grade == '웹멤버'] <- 'webmember'

unique(cust_mon_05$grade)
head(cust_mon_05)

# June data loading
cust_mon_06 <- read_excel("data/cust_mon_201806.xlsx")
head(cust_mon_06)
dim(cust_mon_06) # 217,717건의 거래내용 확인
colnames(cust_mon_06) <- c('date', 'custid', 'grade', 'prod_code', 'qty', 'amt')
head(cust_mon_06)

# cust_mon_06$grade 컬럼의 내용을 영문으로 변경
unique(cust_mon_06$grade)

cust_mon_06$grade[cust_mon_06$grade == '바디러브'] <- 'bodylove'
cust_mon_06$grade[cust_mon_06$grade == '클럽'] <- 'club'
cust_mon_06$grade[cust_mon_06$grade == '골드'] <- 'gold'
cust_mon_06$grade[cust_mon_06$grade == '웹멤버'] <- 'webmember'

unique(cust_mon_06$grade)
head(cust_mon_06)

# rbinding data
cust_mon_total <- rbind(cust_mon_04, cust_mon_05, cust_mon_06)
head(cust_mon_total)\
dim(cust_mon_total) # 총 568,447건의 거래내용 확인
dim(cust_mon_04)[1] + dim(cust_mon_05)[1] + dim(cust_mon_06)[1] == dim(cust_mon_total)[1] # True 확인
str(cust_mon_total)

cust_mon_total$date <- as.character(cust_mon_total$date)
cust_mon_total$year <- substr(cust_mon_total$date, 1, 4)
cust_mon_total$mon <- substr(cust_mon_total$date, 5, 6)
cust_mon_total$day <- substr(cust_mon_total$date, 7, 8)
head(cust_mon_total)

cust_mon_total$ymd <- paste(cust_mon_total$year, cust_mon_total$mon, cust_mon_total$day, sep='-')
head(cust_mon_total)
cust_mon_total$ymd <- as.Date(cust_mon_total$ymd, format='%Y-%m-%d')
head(cust_mon_total)

# select columns needed
cust_mon_total %>% select(ymd, custid, prod_code, amt) -> sale
dim(sale) # 4~5월 364,231건의 물품별 거래건수 확인
length(unique(sale$custid)) # 4~6월 고객의 수 :: 174,301명
head(sale)

## Frequency dimension

sale %>% 
  mutate(ymd_custid = paste(ymd, custid)) %>%
  distinct(ymd_custid) -> df_F
head(df_F)
nrow(df_F) # 4~5월 139,843건의 장바구니 거래건수 확인

df_F %>%
  mutate(ymd=substr(ymd_custid, 1, 10), 
         custid=substr(ymd_custid, 12, length(ymd_custid))) -> df_F


# df_F에서 고객별로 묶어 행의 갯수를 세어 해당 고객의 총 구매횟수를 구하여 userF에 저장
df_F %>%
  group_by(custid) %>%
  summarise(frequency = n()) -> userF
head(userF)
str(userF)
range(userF$frequency)
boxplot(userF$frequency, horizontal = T)

str(sale)
max(sale$ymd)
min(sale$ymd)
range(sale$amt)

sale %>% 
  group_by(custid) %>%
  summarise(minRecency=min(ymd),
            recency=max(ymd),
            monetary=sum(amt),
            period=as.numeric(max(ymd)-min(ymd))) -> userRFM
head(userRFM)
nrow(userRFM) # 4~5월 고객의 수 :: 174,301명

left_join(userRFM, userF, by='custid') -> userRFM
head(userRFM)

########################### recency(최근방문일자)에 대한 EDA ###########################
# 고객들의 최근 방문일자에 대한 히스토그램
hist(userRFM$recency, breaks=61, main='Guest Recency')
table(userRFM$recency)
plot(table(userRFM$recency), main='Guest Recency')

# 5월 13일이 최근 방문일자가 되는 고객의 수가 가장 많음 -> 5.13은 일요일인데 이때 이벤트 기간??
# 보통 일반적으로 최근일자에 대한 고객 그래프는 주어진 데이터의 경우 일정한 분포를 보이는 경향
# 고객들이 매장을 반복적으로 찾는 경우가 드물기 때문인 것으로 추정됨.

########################### monetary(구매액)에 대한 EDA ###########################
# 고객별 총 지출금액에 대한 시각화
range(userRFM$monetary)
windows()
hist(userRFM$monetary, breaks=10000, main='Guest Monetary')

# 0~10만원 사이 구매거래 회수 중 10000원(만원) 단위로 쪼개보기
hist(userRFM$monetary, breaks=2000, main='Guest Monetary', xlim=c(0,100000))

# 0~10만원 사이 구매거래 회수 중 1000원(천원) 단위로 쪼개보기
hist(userRFM$monetary, breaks=20000, main='Guest Monetary', xlim=c(0,100000))

# 0~10만원 사이 구매거래 회수 중 100원(백원) 단위로 쪼개보기
hist(userRFM$monetary, breaks=200000, main='Guest Monetary', xlim=c(0,100000))

# 2만원~2만1천원 사이에서 구매하는 고객들의 수가 가장 많음


########################### frequenct(구매액)에 대한 EDA ###########################
# 고객별 매장방문 회수에 대한 시각화
head(userRFM)
range(userRFM$frequency) # 39번까지 방문한 고객이 있네요...뉴규??
userRFM %>%
  filter(frequency==39)
# A tibble: 1 x 6
# custid           minRecency recency    monetary period frequency
# <chr>            <date>     <date>        <dbl>  <dbl>     <int>
# 2005658405552920 2018-04-01 2018-05-30 4985490.    59.        39

# id가 2005658405552920인 고객은 4~6월간 약 500만원을 소비하고 매장을 39번 방문한 초로얄 고객임.
# 무슨 혜택은 드렸는지 궁금??

range(userRFM$frequency) # 40번까지 방문한 고객이 있네요...뉴규??
userRFM %>%
  filter(frequency==40)

# custid           minRecency recency    monetary period frequency
# <chr>            <date>     <date>        <dbl>  <dbl>     <int>
# 8007182631788866 2018-04-01 2018-06-27 3149830.    87.        40


hist(userRFM$frequency, breaks = 50)
hist(userRFM$frequency, breaks = 50, xlim=c(1, 10))
hist(userRFM$frequency, breaks = 50, xlim=c(1, 5))

userRFM %>%
  filter(frequency > 1) -> freq_2_more
dim(freq_2_more) # 2번 이상 방문한 고객은 31,998명 :: 전체의 18.4%
dim(userRFM)

(31998 / 174301) * 100

userRFM %>%
  filter(frequency > 2) -> freq_3_more
dim(freq_3_more) # 3번 이상 방문한 고객은 3,080명 :: 전체의 4.4%

(7728 / 174301) * 100

userRFM %>%
  filter(frequency > 3) -> freq_4_more
dim(freq_4_more) # 4번 이상 방문한 고객은 2,809명 :: 전체의 1.6%

(2809 / 174301) * 100

userRFM %>%
  filter(frequency > 9) -> freq_10_more
dim(freq_10_more) # 10번 이상 방문한 고객은 161명 :: 전체의 0.1%

(161 / 174301) * 100


# 4~6월간 대부분의 고객들은 매장을 한 번 방문하였고 2번 이상은 18.4%, 3번 이상은 4.4%, 4번 이상은 1.6%
# 의 비율을 차지함 / 자주 매장에 오는 고객이 매우 드문것이 문제인 듯..

#####################################################################################
# RFM 별로 상위 20%가 차지하는 총 매출액 비중 구하기
#####################################################################################

sum_total <- sum(userRFM$monetary)
sum_total

##############   Recency 기준, 상위 20%가 차지하는 매출액 비중 구하기   ##############
# recency의 분위수를 (.2, .4, .6, .8)로 할당
# 날짜 정보를 numeric으로 변환하여 할당

class(userRFM$recency) # date 확인
head(userRFM$recency)
as.numeric(head(userRFM$recency)) # 이는 1970-01-01을 기준으로 몇 일이 지났는지를 알려주는 정보
17666/365 ## 48.4 --> 1970 + 48.4

quantile(as.numeric(userRFM$recency), c(.2, .4, .6, .8))

userRFM$recency <- as.numeric(userRFM$recency)
head(userRFM)

userRFM %>%
  filter(recency > quantile(recency, .8)) -> top20_recency

dim(top20_recency)

# recency 
sumR <- sum(top20_recency$monetary)
sumR # 2,429,102,822

# recency 상위 20%의 비중 구하기
sumR / sum_total 
# 0.25 비중 차지, 보통 일반적인 거래 데이터보다는 현저히 낮은 수준
# 매장을 2번 이상 방문하는 고객이 매우 적은 것이 원인

##############   Frequency 기준, 상위 20%가 차지하는 매출액 비중 구하기   ##############
head(userRFM$frequency)
class(userRFM$frequency)

quantile(as.numeric(userRFM$frequency), c(.2, .4, .6, .8))

# 20% 40% 60% 80% 
#   1   1   1   1 

















































