library(readxl)
library(plyr)
library(tidyverse)

setwd('C:/Users/Daniel/cust_seg')
getwd()
# file.exists("data/cust_mon_201804.xlsx")

# https://stackoverflow.com/questions/32888757/reading-multiple-excel-files-into-r-best-practice
# 현재위치 지정
# setwd("C:/Users/Daniel/BIgdata project Dropbox/Bigdata Project/김정규") 

# 디렉토리를 순환하면서 파일명 가져오기
temp_file <- list.files(pattern='cust_mon_age_cate.*\\.xlsx$', recursive = TRUE) 
temp_file # 파일명 리스트 확인

# 모두 한번에 불러오기
df_list <- lapply(temp_file, read_excel) # lapply를 이용해서 엑셀 읽기
df_list[[1]]

# DATE member_num  grade    GPC on_off   QTY   AMT sex   age 
# https://stackoverflow.com/questions/2851327/convert-a-list-of-data-frames-into-one-data-frame
# system.time({ df.list.rbind <- do.call("rbind", df.list) })  

# list 형태의 data.frame 을 하나의 data.frame으로
# system.time({df_list_ldply <- ldply(df_list, data.frame)})
# user  system elapsed 
# 0.643   0.145   0.797 
# 2.23    0.41    2.64 

# install.packages("data.table")
library(data.table)
df_list_rbindlist <- rbindlist(df_list)
# system.time({df_list_rbindlist <- rbindlist(df_list)})
# 
# df_list_ldply <- ldply(df_list, data.frame)
# head(df_list_ldply)

df_list_rbindlist

colnames(df_list_rbindlist) <- c('date', 'custid', 'grade', 'prod_code', 
                                 'on_off', 'qty', 'amt', 'sex', 'age',
                                 'prod_nm', 'cate', 'cate_ftn', 'cate_line')
glimpse(df_list_rbindlist)
cust_prod_total <- df_list_rbindlist

head(cust_prod_total)
nrow(cust_prod_total) # 6,605,815
cust_prod_total <- as.data.frame(cust_prod_total)
write.csv(cust_prod_total, './data/cust_prod_total.csv')

# 앞으로는 데이터 loading 시간 단축을 위해 총 거래데이터를 cust_prod_total.csv에서 읽음
cust_prod_total <- fread('./data/cust_prod_total.csv', data.table = T)
head(cust_prod_total)

### 월별로 받은 데이터를 합칠때
# 1801 data loading
# cust_prod_1801 <- read_excel("data/cust_mon_cate_age_201801.xlsx")
# dim(cust_prod_1801) # 변수 11개/ 거래 147,156건 
# colnames(cust_prod_1801) <- c('date', 'custid', 'grade', 'prod_code', 'qty', 'amt', 
#                              'prod_dep', 'prod_div', 'prod_name', 'sex', 'age')
#
# 열 결합 : rbinding data (1~6월)
# cust_prod_total <- rbind(cust_prod_1801, cust_prod_1802, cust_prod_1803,
#                        cust_prod_1804, cust_prod_1805, cust_prod_1806)
# head(cust_prod_total)
# dim(cust_prod_1801)[1] + dim(cust_prod_1802)[1] + 
#  dim(cust_prod_1803)[1] + dim(cust_prod_1804)[1] + 
#  dim(cust_prod_1805)[1] + dim(cust_prod_1806)[1] == dim(cust_prod_total)[1] 
# True 확인
### 

###
### 최종 2년 6개월치 데이터로 진행 
###

# 0, 20만원 이상, 연간 6회 이상/60만원 이상 
# 멤버십 30% 할인할때만 사는 사람들!!
# 할인이 있을 때만 사는 패턴을 보이는 고객들을
# 그대로 두는게 맞는 것일까? (세일할때만 사는 고객 등급 유지 여부)

# 매년 5, 11월 이벤트 진행
# 특이사항 :  2018년만 5월 -> 6월로 진행 

glimpse(cust_prod_total)
# Observations: 6,605,815

# 고객등급명을 영문으로 변경
unique(cust_prod_total$grade)
cust_prod_total$grade[cust_prod_total$grade == '바디러브'] <- 'love'
cust_prod_total$grade[cust_prod_total$grade == '클럽'] <- 'club'
cust_prod_total$grade[cust_prod_total$grade == '골드'] <- 'gold'

### 회원등급, 온/오프라인, 성별, 연령대 -> 팩터로 변환 -> 시각화
cust_prod_total$grade <- as.factor(cust_prod_total$grade)
summary(cust_prod_total$grade)  
# 클럽 115만, 골드 42만, 러브 502만 
cust_prod_total <- cust_prod_total[, -1]
glimpse(cust_prod_total)

write.csv(cust_prod_total, './data/cust_prod_total.csv')
cust_prod_total <- fread('./data/cust_prod_total.csv', data.table = T)
head(cust_prod_total)

### 회원등급
###
## bar chart
grade <- names(table(cust_prod_total$grade))
grade
class(grade)
grade_num <- as.numeric(unname(table(cust_prod_total$grade))); grade_num
class(grade_num)

grade_df <- data.frame(grades = grade, number = grade_num)
str(grade_df)
# windows()
ggplot(grade_df, aes(x=grades, y=number)) + 
  geom_bar(stat='identity', fill="steelblue") +
  scale_x_discrete(limits=names(table(cust_prod_total$grade))) +
  theme_minimal() # pie chart 색과 일치하는 코드 바꾸기

## pie chart
str(grade_df)
glimpse(grade_df)
grade_dfc <- grade_df %>%
  mutate(share = number/sum(number)*100.0) %>%
  arrange(desc(number))
grade_dfc

ggplot(grade_dfc, aes("", share, fill = grades)) +
  geom_bar(width = 1, size = .2, stat = "identity", color='black') +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(share), "%")), 
            position = position_stack(vjust = 0.4), size=7) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Membership Grades Ratio") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black"))

# 시계열별 변화보기 필요음(예 6개월씩의 변화) 
# love -> club 옮기는 것이 미션 
# (간격이 너무 크기때문에 love+ 단계를 더 만들 필요)


### 온/오프라인
###
cust_prod_total$on_off <- as.factor(cust_prod_total$on_off)
summary(cust_prod_total$on_off) 
# off 6219970, on 385845

## bar chart
users<- names(table(cust_prod_total$on_off))
users
class(users)
users_num <- as.numeric(unname(table(cust_prod_total$on_off))) 
users_num
class(users_num)

users_df <- data.frame(users = users, number = users_num)
users_df
ggplot(users_df, aes(x=users, y=number)) + 
  geom_bar(stat='identity', fill='steelblue') +
  scale_x_discrete(limits=names(table(cust_prod_total$on_off))) +
  theme_minimal()

## pie chart
str(users_df)
users_dfc <- users_df %>%
  mutate(share = number/sum(number)*100.0) %>%
  arrange(desc(number))
users_dfc

ggplot(users_dfc, aes("", share, fill = users)) +
  geom_bar(width = 1, size = .2, stat = "identity", color='black') +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(share), "%")), 
            position = position_stack(vjust = 0.4), size=7) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Member's On/Off Ratio") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black"))


### 성별
###
cust_prod_total$sex <- as.factor(cust_prod_total$sex)
summary(cust_prod_total$sex)  # 6개월 : 남 56504, 여 1029346 
#  남 245,815, 여 6,359,694     NA 306 
table(cust_prod_total$sex)  

## bar chart
sex <- names(table(cust_prod_total$sex))
sex
class(sex)
sex_num <- as.numeric(unname(table(cust_prod_total$sex)))
sex_num  # 남 245,815, 여 6,359,694
class(sex_num)

sex_df <- data.frame(gender = sex, number = sex_num)
sex_df
ggplot(sex_df, aes(x=gender, y=number)) + 
  geom_bar(stat='identity', fill='steelblue') +
  scale_x_discrete(limits=names(table(cust_prod_total$sex))) +
  theme_minimal()

## pie chart
str(sex_df)
sex_dfc <- sex_df %>%
  mutate(share = number/sum(number)*100.0) %>%
  arrange(desc(number))
sex_dfc

ggplot(sex_dfc, aes("", share, fill = gender)) +
  geom_bar(width = 1, size = .2, stat = "identity", color='black') +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(share), "%")), 
            position = position_stack(vjust = 0.4), size=7) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Member's Gender Ratio") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black"))


### 연령대
###
cust_prod_total$age <- as.factor(cust_prod_total$age)
summary(cust_prod_total$age) # none 270만, NA 33만
table(cust_prod_total$age) # 추후 나이 예측모델 구현 가능???????? 
# 0살대 11명, 100살이상 49명... 정보 신뢰성 부족/ 결측치 많음

## bar chart
## none 제외
age_table <- table(cust_prod_total$age)[1:11]
age_table
names(age_table) <-  c('0-10',' 10-20', '100+', '20-30', '30-40', '40-50', 
                       '50-60', '60-70', '70-80', '80-90', '90-100')
age_table
age <- names(age_table)
age
class(age)
age_num <- as.numeric(unname(age_table))
age_num
class(grade_num)
age_df <- data.frame(age_nm = age, number = age_num)
age_df[-3, ]
age_df <- rbind(age_df[-3, ], age_df[3, ])
age_df

# windows()
ggplot(age_df, aes(x=age_nm, y=number)) + 
  geom_bar(stat='identity', fill='steelblue') +
  scale_x_discrete(limits=age_df$age_nm) +
  theme_minimal()


########### 날짜변환!!!!!!!!
head(cust_prod_total$date)
class(cust_prod_total$date) # 숫자형
cust_prod_total$date <- as.POSIXct(as.character(cust_prod_total$date), 
                                   format="%Y%m%d") # 시간형으로 변환
class(cust_prod_total$date)
glimpse(cust_prod_total)
cust_prod_total$prod_code <- as.character(cust_prod_total$prod_code)
glimpse(cust_prod_total)

## 필요변수만 선택
#cust_prod_total %>%
#  select(ymd, custid, grade, prod_code, qty, amt, prod_dep, 
#         prod_div, prod_name, sex, age) -> cust_prod_total
#head(cust_prod_total)
#str(cust_prod_total)

# prod_code의 형식을 int에서 chr로 바꾸기
#cust_prod_total$prod_code <- as.character(cust_prod_total$prod_code)
#str(cust_prod_total)

###########
# 
# ## 연변수 추가 
# cust_prod_total$year <- format(cust_prod_total$date, '%Y') 
# 
# ## 월변수 추가 
# cust_prod_total$month <- format(cust_prod_total$date, '%m') 
# 
# ## 일변수 추가 
# cust_prod_total$day <- format(cust_prod_total$date, '%d') 
# 
# ## 요일변수 추가 -> 팩터화 
# cust_prod_total$weekday <- format(cust_prod_total$date, '%a') 
# cust_prod_total$weekday <- as.factor(cust_prod_total$weekday) 


# 영어로 로케일 설정
Sys.setlocale("LC_TIME", "English_United States.1252") # Windows
# Sys.setlocale("LC_TIME", "en_US.UTF-8") # Mac
# 날짜를 한번에 년월일 나누기 # https://cran.r-project.org/web/packages/lubridate/vignettes/lubridate.html
cust_prod_total = cust_prod_total %>%
  # dplyr::mutate(date = lubridate::ymd(date)) %>% 
  dplyr::mutate(year = lubridate::year(date), 
                month = lubridate::month(date), 
                day = lubridate::day(date),
                weekday = lubridate::wday(date, label = TRUE))
glimpse(cust_prod_total)

# 한국어로 재설정
Sys.setlocale("LC_TIME", "Korean_Korea.949") # Windows
# Sys.setlocale("LC_TIME", "ko_KR.UTF-8") # Mac

## 수량, 구매액 음수는 NA 처리
range(cust_prod_total$qty) # -200 ~ 1900
summary(cust_prod_total$qty) # median 1, mean 1.245
range(cust_prod_total$amt) # -3850000 ~ 29232000
summary(cust_prod_total$amt) # median 17,000, mean 19,245


cust_prod_total <- cust_prod_total %>% 
  mutate(qty = replace(qty, qty<=0, NA),
         amt = replace(amt, amt<=0, NA))

cust_prod_total <- cust_prod_total %>%
  drop_na()

range(cust_prod_total$qty) # 1 ~ 1900
summary(cust_prod_total$qty) # median 1, mean 1.28
range(cust_prod_total$amt) # 3 ~ 29,232,000
summary(cust_prod_total$amt) # median 17,550, mean 20,2109

hist(cust_prod_total$qty, xlim=c(0,20), breaks = 1500)
hist(cust_prod_total$amt, breaks = 1000000, xlim=c(0,100000))
head(cust_prod_total)

## 구입 단가(price) 변수 추가
cust_prod_total <- cust_prod_total %>% 
  mutate(price = amt / qty)
range(cust_prod_total$price) # 3 ~ 108,200
summary(cust_prod_total$price) # median 16,200, mean 17,064
hist(cust_prod_total$price)
# 5천원 이하가 제일 많음, 15000 ~ 20000원까지가 다음
# 포인트로 결제???

dim(cust_prod_total) 
# 2년 6개월간 6,085,008건의 거래/ 변수 18개 
glimpse(cust_prod_total) 
length(unique(cust_prod_total$custid)) # 고객의 수 : 1,353,361


#########
### 기본 EDA--------------
glimpse(cust_prod_total)
#1. 매출을 기준으로 가장 소비를 많이하는 고객Id는?
salesCust <- aggregate(cust_prod_total$amt, 
                       by=list(customer_ID=cust_prod_total$custid), FUN=sum)

# 데이터의 특정 컬럼을 기준으로 통계량 구함
head(salesCust)
str(salesCust)
## 이상한 아이디 6. 6@))%^%*$)%!&(@&%

range(salesCust$x) # 10 ~ 1,667,409,095 (최대 16억 7천)
ordersales <- order(salesCust$x, decreasing=T)
head(salesCust[ordersales, ], 10)
# 1위 1,667,409,095원 (16억 7천)
#10위    24,482,770원 (2천 4백만)

## 혹은...
cust_prod_total %>%
  group_by(custid) %>%
  summarise(sum_cust_amt = sum(amt)) %>%
  arrange(desc(sum_cust_amt))


# 구매 갯수 범위
range(cust_prod_total$qty)  # 1 ~ 1900개
summary(cust_prod_total$qty)  # median 1, mean 1.28
hist(cust_prod_total$qty, breaks = 1000)

# 구매액 범위
range(cust_prod_total$amt)  # 3 ~ 29,232,000 (2천 9백만)
summary(cust_prod_total$amt)  # median : 17,550월, mean : 20,109원
hist(cust_prod_total$amt)

# 단가 범위
range(cust_prod_total$price)  # 3 ~ 108,200원
summary(cust_prod_total$price) # median : 16200, mean : 17064
hist(cust_prod_total$price)  


#2. 가장 매출이 많은 제품 카테고리는? ->> 새로 개편한 제품 카테고리로 
# glimpse(cust_prod_total)
# salesProd_dep <- aggregate(cust_prod_total$amt, 
#                            by=list(prod_dep=cust_prod_total$prod_dep), FUN=sum)
# head(salesProd_dep)
# orderProd_dep <- order(salesProd_dep$x, decreasing=T)
# head(salesProd_dep[orderProd_dep, ])
# Moisturize, 7640727198

# 혹은...
#cust_prod_total %>%
#  group_by(prod_dep) %>%
#  summarise(sum_prod_dep_amt = sum(amt)) %>%
#  arrange(desc(sum_prod_dep_amt)) -> prod_dep_amt_df

#head(prod_dep_amt_df)

#ggplot(prod_dep_amt_df, aes(x=prod_dep, y=sum_prod_dep_amt)) + 
#  geom_bar(stat='identity', fill='steelblue') +
#  scale_x_discrete(limits=prod_dep_amt_df$prod_dep) +
#  theme(axis.text.x = element_text(angle=90, hjust=1))


#3. 가장 매출이 많은 요일은?
salesWeekday <- aggregate(cust_prod_total$amt, 
                          by=list(weekday=cust_prod_total$weekday), FUN=sum)
head(salesWeekday)

orderWeekday <- order(salesWeekday$x, decreasing=T)
head(orderWeekday)

## 혹은...
head(cust_prod_total)
cust_prod_total %>%
  group_by(weekday) %>%
  dplyr::summarise(sum_weekday_amt = sum(amt)) %>%
  arrange(desc(sum_weekday_amt)) 

weekday_sum_df <- cust_prod_total %>%
  group_by(weekday) %>%
  dplyr::summarise(sum_weekday_amt = sum(amt))

# windows()
weekday_sum_df %>% ggplot(aes(x=weekday, y=sum_weekday_amt)) + 
  geom_bar(stat='identity', fill='steelblue') +
  scale_x_discrete(limits=weekday_sum_df$weekday) 

##########
glimpse(cust_prod_total)
cust_prod_total_1 <- cust_prod_total 

#### date컬럼 -> 2012-04-05 형식


# 고객들의 매장 방문횟수 ############################################
# Frequency 

glimpse(cust_prod_total)

userF <- cust_prod_total %>%
  group_by(custid) %>%
  summarize(frequency=n_distinct(date))

head(userF) # Observations: 1,353,361

# Frequency를 정의 하는 것은 분석가의 role
# 위와 같이 분석 -> 구매를 한 날짜로 다시 group_by, summarize
# -> 구매한 날짜의 빈도가 나올 것 
# -> 그것을 "빈도"로 정의하여 분석 가능

# 빈도의 기준 중, 하루에 여러번 구매를 하더라도 한번으로 보고
# (ifelse(x >= 1,1,0) 분석을 하는 경우 많음.


# barplot
range(userF$frequency) # 1 ~ 최고 395회 방문

summary(userF$frequency) # median : 1, mean : 2, max : 395
boxplot(userF$frequency, horizontal = T)
hist(userF$frequency, breaks = 100000)

userF %>% filter(frequency >= 100) %>% nrow
userF %>% filter(frequency >= 50) %>% nrow
userF %>% filter(frequency >= 30) %>% nrow ## frequency는 30이상을 이상치로 가정한다.
userF %>% filter(frequency >= 15) %>% nrow ## 8886, 0.7% 날림
userF %>% filter(frequency >= 10) %>% nrow

head(cust_prod_total)
head(userF)
cust_prod_total %>%
  left_join(userF, by='custid') -> cust_prod_total_freq

head(cust_prod_total_freq)
cust_prod_total_freq %>%
  filter(frequency < 15) -> cust_prod_total_freq_15

nrow(cust_prod_total_freq_15) # 5,513,041
nrow(cust_prod_total) # 6,085,002

head(cust_prod_total_freq_15)


### 분석대상 축소

userF_15 <- userF %>% 
  filter(frequency < 15)
ggplot(userF_15, aes("", y=frequency)) + 
  geom_boxplot(fill='steelblue', color='black') + coord_flip()

# histogram
hist(userF_15$frequency, breaks = 100)

# 혹은 (15회까지)
ggplot(userF_15, aes(userF_15$frequency)) +
  geom_histogram(breaks=seq(0, 15, by=1), fill='steelblue', color='white')

boxplot(userF_15$frequency, horizontal = T)
hist(userF_15$frequency, breaks = 100)


#####


##일별 구매금액이 100만원 이상인 주문건 제외
cust_prod_total_fix <- cust_prod_total  %>%
  group_by(custid, date) %>%
  mutate(sum_day_amt = sum(amt)) %>%
  filter(sum_day_amt < 1000000) 


##2년 6개월 구매자 구매 횟수
cust_prod_total_fix_1  <- cust_prod_total_fix %>%
  group_by(custid) %>%
  mutate(frequency=n_distinct(date))

##2년 6개월 이상 15회 이상 구매자 제외
cust_prod_total_fix_2 <- cust_prod_total_fix_1 %>%
  filter(frequency < 15)


# RFM 
userRFM <- cust_prod_total_fix_2 %>% 
  group_by(custid) %>% # 고객별 (성별, 연령별????)
  dplyr::summarize(minRecency=min(date), # 최초 구매일
                   recency=max(date),    # 최근 구매일
                   monetary=sum(amt), # 총구매액
                   frequency=min(frequency), ### 에러 수정
                   period=as.numeric(max(date)-min(date))) # 방문 최대 텀
userRFM
## 성별, 연령, 상품코드 분석은 RFM 분석 이후 진행 예정
glimpse(userRFM)
head(userRFM)
nrow(userRFM)
max(userRFM$monetary)

#> userRFM
# A tibble: 1,353,361 x 5
# custid               minRecency          recency             monetary period
# <chr>                <dttm>              <dttm>                 <dbl>  <dbl>
#  1 6.                   2016-05-23 00:00:00 2018-05-15 00:00:00    63900    722
#  2 6.200565840130833    2016-02-27 00:00:00 2017-03-17 00:00:00   116500    384
#  3 6.200565840131470    2018-06-30 00:00:00 2018-06-30 00:00:00    19200      0
#  4 6.200565840142933    2016-10-08 00:00:00 2016-10-08 00:00:00    28000      0
#  5 6@))%^%*$)%!&(@&%    2018-06-07 00:00:00 2018-06-07 00:00:00    46400      0
#  6 6]200565840105344    2016-12-04 00:00:00 2016-12-04 00:00:00    29500      0
#  7 6`                   2016-01-03 00:00:00 2016-11-20 00:00:00   279000    322
#  8 "6\\"                2017-06-15 00:00:00 2017-06-15 00:00:00    27500      0
#  9 "6\\200565840136824" 2016-11-05 00:00:00 2016-11-05 00:00:00    71100      0
#  10 60000000000000000    2016-07-25 00:00:00 2016-09-27 00:00:00    39000     64
# ID, 최초방문일, 최근방문일, 총구매액, 방문 텀
nrow(userRFM) # 고객수 : 27,119,964명 -> 1,353,361 ???????


# userF의 <frequency>는 구매일자로 집계된 count[N=n()] 
# 1) 물품별 구매건수의 합으로 정의할지 
# 2) 고객의 매장 방문 횟수로 정희할지에 협의/의사결정 필요


# 일단 가장 일반적인 userF의 정의인 구매일자로 집계된 count[N=n()] 
# 즉, 물품별 구매건수의 합으로 정의하여 RFM 분석 진행
# userRFM <- left_join(userRFM, userF_15, by = 'custid') # 고객 ID가 key
# head(userRFM)
glimpse(userRFM)

range(userRFM$period)  # 0 ~ 911
summary(userRFM$period) # median 208, mean 305
hist(userRFM$period, breaks = 300)

ggplot(data=userRFM, aes(userRFM$period)) +
  geom_histogram(breaks=seq(0, 300, by=3), fill='steelblue', color='white')

ggplot(data=userRFM, aes(userRFM$period)) +
  geom_histogram(breaks=seq(0, 300, by=5), fill='steelblue', color='white')

ggplot(data=userRFM, aes(userRFM$period)) +
  geom_histogram(breaks=seq(0, 300, by=7), fill='steelblue', color='white')

nrow(userRFM) # 2년 6개월간 고객의 수 :: 2,711,996명
# -> 1,344,475명

########## 

hist(userRFM$monetary)
# 이상치 제거 기준?
# 장바구니별/일자


## RFM연습 다시 시작

# Recency에 대한 EDA ----------------------------------------------

# 추후 날짜 계산을 위해 minDate, maxDate 변수 생성
# minDate :: 1970-01-01 00:00:00를 기준으로 몇 초가 지났는지에 대한 수
# maxDate :: 1970-01-01 00:00:00를 기준으로 몇 초가 지났는지에 대한 수
userRFM$minDate <- as.numeric(as.POSIXct(userRFM$minRecency, origin="1970-01-01"))
userRFM$maxDate <- as.numeric(as.POSIXct(userRFM$recency, origin="1970-01-01"))

glimpse(userRFM)
# Recency EDA 위해 recency 데이터형을 date 형으로 변 
userRFM$recency <- as.Date(userRFM$recency, origin="1970-01-01")
head(userRFM$recency)
summary(userRFM$recency)
hist(userRFM$recency, breaks=30) # 최근 방문일??

# 월별 고객들의 최근 방문일에 대한 histogram
hist(userRFM$recency, breaks='month', 
     xlab=deparse(substitute(userRFM$recency)), 
     start.on.monday = f, freq=T, format='%Y-%m-%d') 

# 주간별 고객들의 최근 방문일에 대한 histogram
hist(userRFM$recency, breaks='weeks', 
     xlab=deparse(substitute(userRFM$recency)), 
     start.on.monday = F, freq=T, format='%Y-%m-%d') 

# 일자별 고객들의 최근 방문일(last_visit_day) 수에 대한 hist
hist(userRFM$recency, breaks='days', 
     xlab=deparse(substitute(userRFM$recency)), 
     start.on.monday = F, freq=T, format='%Y-%m-%d') 

plot(table(userRFM$recency), main="Customer Recency")


# Frequency에 대한 EDA ----------------------------------------------
# 고객별 매장방문 회수에 대한 시각화
head(userRFM)
range(userRFM$frequency) # 14번 방문이 최대

hist(userRFM$frequency, breaks = 100)
hist(userRFM$frequency, breaks = 100, xlim=c(1, 10))
hist(userRFM$frequency, breaks = 100, xlim=c(1, 5))

dim(userRFM) # 1,344,345명

freq_1 <- userRFM %>%
  filter(frequency == 1)

dim(freq_1) # 1회 구매 고객 839,364명 :: 전체의 46.1%
(839364 / 1344345) * 100 # 62.4%가 한 번만 방문

freq_2 <- userRFM %>%
  filter(frequency == 2)
dim(freq_2) # 2번 구매 고객 261,949명
(261949 / 1344345) * 100 # 19.5%가 2번 방문

freq_3 <- userRFM %>%
  filter(frequency == 3)
dim(freq_3) # 3번 구매 고객 101,101명
(101101 / 1344345) * 100 # 7.5%가 3번 방문

freq_4_more <- userRFM %>%
  filter(frequency >= 4)
dim(freq_4_more) # 4번 이상 구매 고객 141,931명
(141931 / 1344345) * 100 # 10.6%가 4번 이상 방문


# Monetary에 대한 EDA ----------------------------------------------
### 고객들의 지출 비용에 대한 분포 파악
### Monetary 

range(userRFM$monetary) # 10 ~ 6,457,720
summary(userRFM$monetary) # medeian 4만7천원, mean 약 7만9천원
hist(userRFM$monetary, breaks=10000, main='Cust Monetary')
hist(log10(userRFM$monetary), breaks=100)
hist(userRFM$monetary, breaks=100) # 5만원 이상 희박, 0 ~ 5만원 확대 


ggplot(data=userRFM, aes(userRFM$monetary)) +
  geom_histogram(breaks=seq(0, 500000, by=10000), fill='steelblue')

ggplot(data=userRFM, aes(userRFM$monetary)) +
  geom_histogram(breaks=seq(0, 400000, by=10000), fill='steelblue')

ggplot(data=userRFM, aes(userRFM$monetary)) +
  geom_histogram(breaks=seq(0, 300000, by=10000), fill='steelblue')

ggplot(data=userRFM, aes(userRFM$monetary)) +
  geom_histogram(breaks=seq(0, 300000, by=1000), fill='steelblue')

ggplot(data=userRFM, aes(userRFM$monetary)) +
  geom_histogram(breaks=seq(0, 60000, by=100), fill='steelblue')


# 0~10만원 구매회수 -> 10000원(만원) 단위
hist(userRFM$monetary, breaks=2000, main='Cust Monetary', 
     xlim = c(0,100000)) ## 안보임

# 0~10만원 구매회수 -> 1000원(천원) 단위
hist(userRFM$monetary, breaks=20000, main='Cust Monetary', 
     xlim = c(0,100000)) ## 안보임


# 0~10만원 구매회수 -> 1000원(천원) 단위
hist(userRFM$monetary, breaks=100000, main='Cust Monetary', 
     xlim = c(0,100000)) ## 안보임

# 0~60만원 구매회수
hist(userRFM$monetary, breaks=100000, main='Cust Monetary', 
     xlim = c(0,600000))
# 4 ~ 5만원대가 많다

hist(userRFM$monetary, breaks=200000, main='Cust Monetary', 
     xlim = c(0,300000))
# 3 ~ 4만원대가 많다

hist(userRFM$monetary, breaks=1000000, main='Cust Monetary', 
     xlim = c(0,200000))
# 4군데 peak가 보인다 (4만9천원대에서 다시 상승)

### long tail!!!

## 알아볼수가 없는데 어떻게 해석??
# (6개월치때는 보였는데 기간 늘어나니까 안보임)

# 제품 가격대 구성이 어떻게 되어있나???
# 개당 가격은???


hist(userRFM$monetary, breaks=100) # 5만원 이상 희박, 0~5만원 확대하여 보기
hist(userRFM$monetary, breaks=10000, xlim = c(0,50000))
hist(userRFM$monetary, breaks=100000, xlim = c(0,50000))

# 가장 지출이 많이 되는 금액대는 만원~3만1천원 사이에 있어 이 부분만 확대하여 보기
hist(userRFM$monetary, breaks=100000, xlim = c(10000,31000)) 
# 1만원~3만1천원 사이를 200원씩 binning
10000/50 


# 많이 지출되는 금액 확대해서 보기
ggplot(data=userRFM, aes(userRFM$monetary)) +
  geom_histogram(breaks=seq(0, 50000, by=1000), fill='steelblue', color='white')


# 고객들은 2만원에서 2만 1천원의 범위의 지출을 가장 많이 하였음.
# 고객들은 2만 7천원에서 2만 8천원의 범위의 지출을 두 번째로 많이 하였음.
# 고객들은 2만 9천원에서 3만원의 범위의 지출을 세 번째로 많이 하였음.


####
# 환불 예측 모델?????


## RFM별 상위 20%가 차지하는 총 매출액 대비 비중 ------------------------

# R : 최근방문 분위수 (날짜를 numeric으로 바꾸면 분위수 계산 가능)
# quantR <- as.Date(quantile(as.numeric(userRFM$maxDate),
#                            c(0,0.2,0.4,0.6,0.8,1)), origin="1970-01-01")
quantR <- as.numeric(quantile(userRFM$maxDate,c(0,0.2,0.4,0.6,0.8,1)))
quantR[1]
quantR[5] # R 분위수가 0.8이 되는 일자는 "2018-05-01" 

# F : 빈도 분위수 
quantF <- quantile(userRFM$frequency, c(0,0.2,0.4,0.6,0.8,1))
quantF
quantF[5] # F 분위수가 0.8이 되는 횟수는 "3"회

# M : 구매액 분위수
quantM <- quantile(userRFM$monetary, c(0,0.2,0.4,0.6,0.8,1))
quantM
quantM[5] # M 분위수가 0.8이 되는 액수는 "306,350" 원


## RFM별 상위 20%가 차지하는 총 매출액 대비 비중

# Recency 상위 20%가 차지하는 총 매출액 대비 비중 알아보기 --------------------
quantR[5] # 1516892400 ("2018-01-26")
as.POSIXct(quantR[5], origin='1970-01-01')
sumR <- sum(userRFM$monetary[userRFM$maxDate >= quantR[5]])
sumR / sum(userRFM$monetary) # 33.7%

sumR <- sum(userRFM$monetary[userRFM$maxDate > quantR[5]])
sumR / sum(userRFM$monetary) # 33.5%

# "="를 포함하는지 여부에 대한 차이는 .2%에 불과 --> 추후 논의 필요
# Recency 상위 20%가 차지하는 총 매출액 비중은 33.7%(or 33.5%) 임.

sumR <- sum(userRFM$monetary[userRFM$maxDate >= quantR[5]])

# Frequency 상위 20%가 차지하는 총 매출액 대비 비중 알아보기 ----------------
quantF[5] # 2

# 1) 김정규
sumF <- sum(userRFM$monetary[userRFM$frequency >= quantF[5]])
sumF / sum(userRFM$monetary) # 68%

sumF <- sum(userRFM$monetary[userRFM$frequency > quantF[5]])
sumF / sum(userRFM$monetary) # 48.4%
# "="를 포함하는지 여부에 대한 차이는 약 20%로 상당한 차이 발생 --> 추후 논의 필수
# Recency 상위 20%가 차지하는 총 매출액 비중은 48.4% ~ 68% 임.

## 2) 안중호
sumF <- sum(as.numeric(userRFM$monetary[userRFM$frequency > quantile(userRFM$frequency, 0.8)]))
sumF/sum(as.numeric(userRFM$monetary)) # 24.5% -> 31.9% 


# Monetary 상위 20%가 차지하는 총 매출액 대비 비중 알아보기 ------------------
quantM[5] # 103,000

## 김정규
sumM <- sum(userRFM$monetary[userRFM$monetary >= quantM[5]])
sumM / sum(userRFM$monetary) # 57%

sumM <- sum(userRFM$monetary[userRFM$monetary > quantM[5]])
sumM / sum(userRFM$monetary) # 56.9%
# "="를 포함하는지 여부에 대한 차이는 약 .1% --> 추후 논의 필요
# Monetary 상위 20%가 차지하는 총 매출액 비중은 약 57% 임.

## 안중호 
sumM <- sum(as.numeric(userRFM$monetary[userRFM$monetary > quantile(userRFM$monetary, 0.8)]))
sumM # 623,036,505,426 약 6230억
sumM/sum(as.numeric(userRFM$monetary)) # 67.3% -> 75.7

sumM <- sum(userRFM$monetary[userRFM$monetary >= quantM[5]])

# 가중치 계산---------------------------------------------------------------------
# (RFM지수 = weightR * Recency + weightF * Frequency + weightM * Monetary)
weightR <- sumR/(sumR + sumF + sumM)
weightF <- sumF/(sumR + sumF + sumM)
weightM <- sumM/(sumR + sumF + sumM)

weightR # 0.2122035
weightF # 0.4283726
weightM # 0.3594238

# parse 함수 활용방법 --------------------------
columnName <- paste0("userRFM", "$", "frequency")
head(columnName)
eval(parse(text=columnName))[2] 
# 문자열 조합으로 데이터프레임의 열을 찾는 방법


# 등급 부여하는 함수 intervalGrade() 제작 ---------------------------------------
# """ 각 고객의 Recency, Frequency, Monetary 값이 각각 분위수 구간 안에 해당하는 점수를 
# 1~5점으로 부여하여 줌"""

intervalGrade <- function(mainData, fileName, rfmName, quantileData) {
  results <- rep (0, nrow(mainData))
  dataSource <- data.table(eval(parse(text=paste0(fileName,"$",rfmName)))) # data.table 활용
  for (i in 1:nrow(mainData)) {
    data = dataSource$V1[i]
    if (data >= quantileData[1] && data < quantileData[2] ) {
      results[i] <- 1
    } else if (data >= quantileData[2] && data < quantileData[3]) {
      results[i] <- 2
    } else if (data >= quantileData[3] && data < quantileData[4]) {
      results[i] <- 3
    } else if (data >= quantileData[4] && data < quantileData[5]) {
      results[i] <- 4
    } else { 
      results[i] <- 5 
    }
  }
  return(results)
}


# RFM 점수 계산-------------------------------------------------------
glimpse(userRFM)
userRFM$R <- intervalGrade(userRFM, "userRFM", "maxDate", quantR)
userRFM$F <- intervalGrade(userRFM, "userRFM", "frequency", quantF)
userRFM$M <- intervalGrade(userRFM, "userRFM", "monetary", quantM)

system.time({userRFM$R <- intervalGrade(userRFM, "userRFM", "maxDate", quantR)})
# system.time({userRFM$M <- intervalGrade(userRFM, "userRFM", "monetary", quantM)})
# 사용자  시스템 elapsed 
# 11.36    0.53   11.97 

# RFM지수 = weightR * Recency + weightF * Frequency + weightM * Monetary
userRFM$score <- (weightR * userRFM$R + weightF * userRFM$F + weightM * userRFM$M)*100/5
hist(userRFM$score)

(quantS <- quantile(userRFM$score, c(0,0.2,0.4,0.6,0.8,1)))
summary(userRFM$score)

# 고객등급 분류 함수(finalGrade()) 제작----------------------------------
finalGrade <- function(mainData, fileName, rfmName, quantileData) {
  results <- rep (0, nrow(mainData))
  dataSource <- data.table(eval(parse(text=paste0(fileName,"$",rfmName))))
  for (i in 1:nrow(mainData)) {
    data = dataSource$V1[i]
    if (data >= quantileData[1] && data < quantileData[2] ) {
      results[i] <- "E"
    } else if (data >= quantileData[2] && data < quantileData[3]) {
      results[i] <- "D"
    } else if (data >= quantileData[3] && data < quantileData[4]) {
      results[i] <- "C"
    } else if (data >= quantileData[4] && data < quantileData[5]) {
      results[i] <- "B"
    } else { results[i] <- "A" }
  }
  
  return(results)
}

# RFM 점수로 고객등급 분류--------------------------------------------------------
userRFM$grade <- finalGrade(userRFM, "userRFM", "score", quantS)
system.time({userRFM$grade <- finalGrade(userRFM, "userRFM", "score", quantS)})
# 사용자  시스템 elapsed 
# 18.62    0.99   19.83

userRFM$grade <- as.factor(userRFM$grade)
glimpse(userRFM)
summary(userRFM)

#cust_id 변경하기 ---------------------------------------------------------------

# cust_id를 임의의 단일값으로 변경하기 위해서 row_number를 이용해서 값을 구하고
# 앞에 빈칸을 채우기 위해서 sprintf 를 사용하여 0으로 채움
# https://stackoverflow.com/questions/11996135/create-a-sequential-number-counter-for-rows-within-each-group-of-a-dataframe
# https://stackoverflow.com/questions/14409084/pad-with-leading-zeros-to-common-width


userRFM_uniq <- userRFM %>%
  mutate(custid = sprintf("C%08d", row_number()))

head(userRFM_uniq)
glimpse(userRFM_uniq)

write.csv(userRFM, file = "./data/userRFM_full.csv", row.names=FALSE)
write.csv(userRFM_uniq, file = "./data/userRFM_full_uniq.csv", row.names=FALSE)

userRFM_full <- read.csv("./data/userRFM_full.csv", stringsAsFactors = F); head(userRFM_full)
userRFM_full_uniq <- read.csv("./data/userRFM_full_uniq.csv", , stringsAsFactors = F); head(userRFM_full_uniq)

glimpse(userRFM_full)
glimpse(userRFM_full_uniq)
