library(readxl)
library(tidyverse)

###
# 1801 data loading
cust_prod_1801 <- read_excel("data/cust_mon_cate_age_201801.xlsx")

head(cust_prod_1801)
dim(cust_prod_1801) # 변수 11개/ 거래 147,156건 

colnames(cust_prod_1801) <- c('date', 'custid', 'grade', 'prod_code', 'qty', 'amt', 
                              'prod_dep', 'prod_div', 'prod_name', 'sex', 'age')
head(cust_prod_1801)
glimpse(cust_prod_1801)

# 고객등급명을 영문으로 변경
unique(cust_prod_1801$grade)

cust_prod_1801$grade[cust_prod_1801$grade == '바디러브'] <- 'love'
cust_prod_1801$grade[cust_prod_1801$grade == '클럽'] <- 'club'
cust_prod_1801$grade[cust_prod_1801$grade == '골드'] <- 'gold'
cust_prod_1801$grade[cust_prod_1801$grade == '웹멤버'] <- 'webmember'

unique(cust_prod_1801$prod_dep) # 26개?? --> 30개
length(unique(cust_prod_1801$prod_dep)) ## 30
unique(cust_prod_1801$prod_div) # 37개 
length(unique(cust_prod_1801$prod_div)) ## 37
unique(cust_prod_1801$prod_name) # 793개 
length(unique(cust_prod_1801$prod_name)) ## 793

## 날짜형 변경 필요

###
# 1802 data loading
cust_prod_1802 <- read_excel("data/cust_mon_cate_age_201802.xlsx")
head(cust_prod_1802)
dim(cust_prod_1802)  # 변수 11개/ 거래 215,597건 

colnames(cust_prod_1802) <- c('date', 'custid', 'grade', 'prod_code', 'qty', 'amt', 
                              'prod_dep', 'prod_div', 'prod_name', 'sex', 'age')
# 고객등급명을 영문으로 변경
unique(cust_prod_1802$grade)

cust_prod_1802$grade[cust_prod_1802$grade == '바디러브'] <- 'love'
cust_prod_1802$grade[cust_prod_1802$grade == '클럽'] <- 'club'
cust_prod_1802$grade[cust_prod_1802$grade == '골드'] <- 'gold'
cust_prod_1802$grade[cust_prod_1802$grade == '웹멤버'] <- 'webmember'

unique(cust_prod_1802$prod_dep) # 31개 
length(unique(cust_prod_1802$prod_dep))
unique(cust_prod_1802$prod_div) # 35개 --> 36개
length(unique(cust_prod_1802$prod_div))
unique(cust_prod_1802$prod_name) # 757개 --> 759개
length(unique(cust_prod_1802$prod_name))

###
# 1803 data loading
cust_prod_1803 <- read_excel("data/cust_mon_cate_age_201803.xlsx")
head(cust_prod_1803)
dim(cust_prod_1803) # 154,711건의 거래

colnames(cust_prod_1803) <- c('date', 'custid', 'grade', 'prod_code', 'qty', 'amt', 
                              'prod_dep', 'prod_div', 'prod_name', 'sex', 'age')
# 고객등급명을 영문으로 변경
unique(cust_prod_1803$grade)

cust_prod_1803$grade[cust_prod_1803$grade == '바디러브'] <- 'love' # 1원 이상
cust_prod_1803$grade[cust_prod_1803$grade == '클럽'] <- 'club' # 년 20만원 이상
cust_prod_1803$grade[cust_prod_1803$grade == '골드'] <- 'gold' # 년 60만원 이상 and 6회 이상 :: 강등가능
cust_prod_1803$grade[cust_prod_1803$grade == '웹멤버'] <- 'webmember' # 가입만 하고 구매 0

# 할인이 있을 때만 구매하는 고객 있을 것
# 등급을 유지
# 5월, 11월 이벤트
# '18년도에만 6월 이벤트 진행

unique(cust_prod_1803$prod_dep) # 31개 
length(unique(cust_prod_1803$prod_dep))
unique(cust_prod_1803$prod_div) # 37개 
length(unique(cust_prod_1803$prod_div))
unique(cust_prod_1803$prod_name) # 724개 
length(unique(cust_prod_1803$prod_name))

###
# 1804 data loading
cust_prod_1804 <- read_excel("data/cust_mon_cate_age_201804.xlsx")
head(cust_prod_1804)
dim(cust_prod_1804) # 146,514건의 거래

colnames(cust_prod_1804) <- c('date', 'custid', 'grade', 'prod_code', 'qty', 'amt', 
                              'prod_dep', 'prod_div', 'prod_name', 'sex', 'age')
glimpse(cust_prod_1804)

# 고객등급명을 영문으로 변경
unique(cust_prod_1804$grade)

cust_prod_1804$grade[cust_prod_1804$grade == '바디러브'] <- 'love'
cust_prod_1804$grade[cust_prod_1804$grade == '클럽'] <- 'club'
cust_prod_1804$grade[cust_prod_1804$grade == '골드'] <- 'gold'
cust_prod_1804$grade[cust_prod_1804$grade == '웹멤버'] <- 'webmember'

unique(cust_prod_1804$prod_dep) # 26개 --> 30개
length(unique(cust_prod_1804$prod_dep))
unique(cust_prod_1804$prod_div) # 35개 
length(unique(cust_prod_1804$prod_div))
unique(cust_prod_1804$prod_name) # 751개 --> 752개
length(unique(cust_prod_1804$prod_name))


###
# 1805 data loading
cust_prod_1805 <- read_excel("data/cust_mon_cate_age_201805.xlsx")
head(cust_prod_1805)
dim(cust_prod_1805) # 217,717건의 거래

colnames(cust_prod_1805) <- c('date', 'custid', 'grade', 'prod_code', 'qty', 'amt', 
                              'prod_dep', 'prod_div', 'prod_name', 'sex', 'age')
glimpse(cust_prod_1805)

# 고객등급명을 영문으로 변경
unique(cust_prod_1805$grade)

cust_prod_1805$grade[cust_prod_1805$grade == '바디러브'] <- 'love'
cust_prod_1805$grade[cust_prod_1805$grade == '클럽'] <- 'club'
cust_prod_1805$grade[cust_prod_1805$grade == '골드'] <- 'gold'
cust_prod_1805$grade[cust_prod_1805$grade == '웹멤버'] <- 'webmember'

unique(cust_prod_1805$prod_dep) # 26개 --> 30개로 바꾸어야 할 거 같습니다(180917_김정규)
length(unique(cust_prod_1805$prod_dep))
unique(cust_prod_1805$prod_div) # 35개 ---> 36개로 바꾸어야 할 거 같습니다(180917_김정규)
length(unique(cust_prod_1805$prod_div))
unique(cust_prod_1805$prod_name) # 727개 ---> 729개로 바꾸어야 할 거 같습니다(180917_김정규)
length(unique(cust_prod_1805$prod_name))


###
# 1806 data loading
cust_prod_1806 <- read_excel("data/cust_mon_cate_age_201806.xlsx")
head(cust_prod_1806)
dim(cust_prod_1806) # 204,216건의 거래

colnames(cust_prod_1806) <-c('date', 'custid', 'grade', 'prod_code', 'qty', 'amt', 
                             'prod_dep', 'prod_div', 'prod_name', 'sex', 'age')
glimpse(cust_prod_1806)

# 고객등급명을 영문으로 변경
unique(cust_prod_1806$grade)

cust_prod_1806$grade[cust_prod_1806$grade == '바디러브'] <- 'love'
cust_prod_1806$grade[cust_prod_1806$grade == '클럽'] <- 'club'
cust_prod_1806$grade[cust_prod_1806$grade == '골드'] <- 'gold'
cust_prod_1806$grade[cust_prod_1806$grade == '웹멤버'] <- 'webmember'


unique(cust_prod_1806$prod_dep) # 31개 ---> 29개
length(unique(cust_prod_1806$prod_dep))
unique(cust_prod_1806$prod_div) # 37개 
length(unique(cust_prod_1806$prod_div))
unique(cust_prod_1806$prod_name) # 724개 ---> 741개
length(unique(cust_prod_1806$prod_name))

## 샘플 1 ~ 6월로 진행 -> 최종 2년 6개월치 데이터로 진행예정

# 열 결합 : rbinding data (1~6월)
cust_mon_total <- rbind(cust_prod_1801, cust_prod_1802, cust_prod_1803,
                        cust_prod_1804, cust_prod_1805, cust_prod_1806)

head(cust_mon_total)
dim(cust_prod_1801)[1] + dim(cust_prod_1802)[1] + 
  dim(cust_prod_1803)[1] + dim(cust_prod_1804)[1] + 
  dim(cust_prod_1805)[1] + dim(cust_prod_1806)[1] == dim(cust_mon_total)[1] 
# True 확인

summary(cust_mon_total)
dim(cust_mon_total)
str(cust_mon_total)
glimpse(cust_mon_total) # 총 1,085, 911건의 거래 확인

# saving rbinded data
write.csv(cust_mon_total, './data/cust_mon_total_01_06.csv')
cust_mon_total <- read.csv('./data/cust_mon_total_01_06.csv',  stringsAsFactors = F)
glimpse(cust_mon_total)

## 고객등급, 성별, 연령대 -> 팩터로 변환 후 각각 비율을 시각화 해보기

# 회원 등급을 팩터화
cust_mon_total$grade <- factor(cust_mon_total$grade, levels = c('webmember','love', 'club', 'gold'))
table(cust_mon_total$grade)
# webmember      love      club      gold 
# 23            856264    147078     82546 

## 회원등급에 대한 bar chart
grade <- names(table(cust_mon_total$grade)); grade
class(grade)
grade_num <- as.numeric(unname(table(cust_mon_total$grade))); grade_num
class(grade_num)
grade_df <- data.frame(grades = grade, number = grade_num)
ggplot(grade_df, aes(x=grades, y=number)) + 
  geom_bar(stat='identity', fill='steelblue') +
  scale_x_discrete(limits=names(table(cust_mon_total$grade))) +
  theme_minimal()

## 회원등급에 대한 pie chart
str(grade_df)
grade_df %>%
  mutate(share = number/sum(number)*100.0) %>%
  arrange(desc(number)) -> grade_dfc; grade_dfc

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


## 회원 성별 팩터화
cust_mon_total$sex <- factor(cust_mon_total$sex, levels=c('남', '여'))
table(cust_mon_total$sex)  # 남 56504, 여 1029346 # NA=61명 있음에 주의!

## 회원성별 비율에 대한 bar chart 시각화
sex <- names(table(cust_mon_total$sex)); sex
class(sex)
sex_num <- as.numeric(unname(table(cust_mon_total$sex))); sex_num
class(grade_num)
sex_df <- data.frame(gender = sex, number = sex_num); sex_df
ggplot(sex_df, aes(x=gender, y=number)) + 
  geom_bar(stat='identity', fill='steelblue') +
  scale_x_discrete(limits=names(table(cust_mon_total$sex))) +
  theme_minimal()

## 회원성별 비율에 대한 pie chart 시각화
str(sex_df)
sex_df %>%
  mutate(share = number/sum(number)*100.0) %>%
  arrange(desc(number)) -> sex_dfc; sex_dfc

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


## 회원 나이 팩터화
cust_mon_total$age <- factor(cust_mon_total$age, 
                             levels=c('10', '20', '30', '40', '50', '60',
                                      '70', '80', '90', 'none'))
summary(cust_mon_total$age) # NA 61명 있음에 주의!
table(cust_mon_total$age) # none 442491명, 추후 나이 예측모델 구현으로 예측 예정

## 회원나이 비율에 대한 bar chart 시각화
## none에 대한 비율은 제외하여 시각화 처리
age_table <- table(cust_mon_total$age)[1:9]; age_table
names(age_table) <-  c('10-20', '20-30', '30-40', '40-50', '50-60', '60-70', '70-80', '80-90', '90+')
age_table
age <- names(age_table); age
class(age)
age_num <- as.numeric(unname(age_table)); age_num
class(grade_num)
age_df <- data.frame(age_nm = age, number = age_num); age_df
ggplot(age_df, aes(x=age_nm, y=number)) + 
  geom_bar(stat='identity', fill='steelblue') +
  scale_x_discrete(limits=names(age_table)) +
  theme_minimal()

## 회원성별 비율에 대한 pie chart 시각화
str(age_df)
age_df %>%
  mutate(share = number/sum(number)*100.0) %>%
  arrange(desc(number)) -> age_dfc; age_dfc

ggplot(age_dfc, aes("", share, fill = age_nm)) +
  geom_bar(width = 1, size = .1, stat = "identity", color='white') +
  coord_polar("y") +
  labs(x = NULL, y = NULL, fill = NULL, title = "Member's Age Ratio") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black"))

## 날짜변환하기
cust_mon_total$date <- as.character(cust_mon_total$date)
head(cust_mon_total$date)
cust_mon_total$year <- substr(cust_mon_total$date, 1, 4)
cust_mon_total$mon <- substr(cust_mon_total$date, 5, 6)
cust_mon_total$day <- substr(cust_mon_total$date, 7, 8)
head(cust_mon_total)

cust_mon_total$ymd <- paste(cust_mon_total$year, cust_mon_total$mon, cust_mon_total$day, sep='-')
head(cust_mon_total)
cust_mon_total$ymd <- as.Date(cust_mon_total$ymd, format='%Y-%m-%d')
head(cust_mon_total)
str(cust_mon_total)

## 필요변수만 선택하기
cust_mon_total %>%
  select(ymd, custid, grade, prod_code, qty, amt, prod_dep, prod_div, prod_name, sex, age) -> cust_mon_total
head(cust_mon_total)
str(cust_mon_total)

# prod_code의 형식을 int에서 chr로 바꾸기
cust_mon_total$prod_code <- as.character(cust_mon_total$prod_code)
str(cust_mon_total)

###########

# 요일변수 추가 
cust_mon_total$weekday <- format(cust_mon_total$ymd, '%a') 
str(cust_mon_total)
cust_mon_total$weekday[cust_mon_total$weekday == '일'] <- 'sun'
cust_mon_total$weekday[cust_mon_total$weekday == '월'] <- 'mon'
cust_mon_total$weekday[cust_mon_total$weekday == '화'] <- 'tue'
cust_mon_total$weekday[cust_mon_total$weekday == '수'] <- 'wed'
cust_mon_total$weekday[cust_mon_total$weekday == '목'] <- 'thu'
cust_mon_total$weekday[cust_mon_total$weekday == '금'] <- 'fri'
cust_mon_total$weekday[cust_mon_total$weekday == '토'] <- 'sat'
unique(cust_mon_total$weekday)

cust_mon_total$weekday <- as.factor(cust_mon_total$weekday) 
head(cust_mon_total)
str(cust_mon_total)

# 수량, 구매액 음수는 NA 처리
range(cust_mon_total$qty)
range(cust_mon_total$amt)

cust_mon_total <- cust_mon_total %>% 
  mutate(qty = replace(qty, qty<=0, NA),
         amt = replace(amt, amt<=0, NA))

cust_mon_total <- cust_mon_total %>%
  drop_na()

range(cust_mon_total$qty)
range(cust_mon_total$amt)

dim(cust_mon_total) ## 총 1,085, 911건에서 1,057,160건으로 감소

# 구입 단가 변수 추가
cust_mon_total <- cust_mon_total %>% 
  mutate(price = amt / qty)
summary(cust_mon_total$price)
hist(cust_mon_total$price)

dim(cust_mon_total) # 1~6월 1,057,160건의 물품별 거래
length(unique(cust_mon_total$custid)) # 1~6월 고객의 수 : 304294명
length(unique(cust_mon_total$prod_dep)) # 대분류 : 33개
length(unique(cust_mon_total$prod_div)) # 중분류 : 39개
length(unique(cust_mon_total$prod_name)) # 상품명 : 1012개

glimpse(cust_mon_total)
## date, custid, grade, prod_code
## qty, amt, prod_dep, prod_div, prod_name
## sex, ageweekday, price


#########
### 기본 EDA--------------

#1. 매출을 기준으로 가장 소비를 많이하는 고객Id는? #############################################
salesCust <- aggregate(cust_mon_total$amt, 
                       by=list(customer_ID=cust_mon_total$custid), FUN=sum)

# 데이터의 특정 컬럼을 기준으로 통계량 구함
head(salesCust)

range(salesCust$x) # 24 ~ 23797430
ordersales <- order(salesCust$x, decreasing=T)
head(salesCust[ordersales, ])
# 6003010061,  23797430원

## 혹은...
cust_mon_total %>%
  group_by(custid) %>%
  summarise(sum_custamt = sum(amt)) %>%
  arrange(desc(sum_custamt))

#################################################################################################

# 구매 갯수 범위
range(cust_mon_total$qty)  # 1 ~ 400

# 구매액 범위
range(cust_mon_total$amt)  # 3 ~ 6200000

# 단가 범위
range(cust_mon_total$price)  # 3 ~ 5229000


#2. 가장 매출이 많은 제품 대분류는? #############################################################
glimpse(cust_mon_total)

salesProd_dep <- aggregate(cust_mon_total$amt, 
                           by=list(prod_dep=cust_mon_total$prod_dep), FUN=sum)
head(salesProd_dep)


orderProd_dep <- order(salesProd_dep$x, decreasing=T)
head(salesProd_dep[orderProd_dep, ])
# Moisturize, 7640727198

# 혹은...
cust_mon_total %>%
  group_by(prod_dep) %>%
  summarise(sum_prod_dep_amt = sum(amt)) %>%
  arrange(desc(sum_prod_dep_amt)) -> prod_dep_amt_df

head(prod_dep_amt_df)

ggplot(prod_dep_amt_df, aes(x=prod_dep, y=sum_prod_dep_amt)) + 
  geom_bar(stat='identity', fill='steelblue') +
  scale_x_discrete(limits=prod_dep_amt_df$prod_dep) +
  theme(axis.text.x = element_text(angle=90, hjust=1))

#################################################################################################

#3. 가장 매출이 많은 제품 중분류는? #############################################################
salesProd_div <- aggregate(cust_mon_total$amt, 
                           by=list(prod_div=cust_mon_total$prod_div), FUN=sum)
head(salesProd_div)


orderProd_div <- order(salesProd_div$x, decreasing=T)
head(salesProd_div[orderProd_div, ])
# Body, 4522130788

# 혹은...
cust_mon_total %>%
  group_by(prod_div) %>%
  summarise(sum_prod_div_amt = sum(amt)) %>%
  arrange(desc(sum_prod_div_amt)) -> prod_div_amt_df

head(prod_div_amt_df)

ggplot(prod_div_amt_df, aes(x=prod_div, y=sum_prod_div_amt)) + 
  geom_bar(stat='identity', fill='steelblue') +
  scale_x_discrete(limits=prod_div_amt_df$prod_div) +
  theme(axis.text.x = element_text(angle=90, hjust=1))

#################################################################################################

#2. 가장 매출이 많은 제품명은?
salesProd_name <- aggregate(cust_mon_total$amt, 
                           by=list(prod_name=cust_mon_total$prod_name), FUN=sum)
head(salesProd_name)

orderProd_name <- order(salesProd_name$x, decreasing=T)
head(salesProd_name[orderProd_name, ])
# WM B/L 400 RENO, 931662452

# 혹은...
cust_mon_total %>%
  group_by(prod_dep, prod_div, prod_name) %>%
  summarise(sum_prod_name_amt = sum(amt)) %>%
  arrange(desc(sum_prod_name_amt)) -> prod_name_amt_df

prod_name_amt_df$prod_name_detail <- paste(prod_name_amt_df$prod_dep, prod_name_amt_df$prod_div, prod_name_amt_df$prod_name, sep='-')
head(prod_name_amt_df)
head(prod_name_amt_df[, c(5,4)] , 20) -> best_20_prod

ggplot(best_20_prod, aes(x=prod_name_detail, y=sum_prod_name_amt)) + 
  geom_bar(stat='identity', fill='steelblue') +
  scale_x_discrete(limits=best_20_prod$prod_name_detail) +
  theme(axis.text.x = element_text(angle=45, hjust=1))

#################################################################################################

#3. 가장 매출이 많은 요일은?
salesWeekday <- aggregate(cust_mon_total$amt, 
                       by=list(weekday=cust_mon_total$weekday), FUN=sum)
head(salesWeekday)

orderWeekday <- order(salesWeekday$x, decreasing=T)
head(orderWeekday)
##########
glimpse(cust_mon_total)
## date, custid, grade, prod_code
## qty, amt, prod_dep, prod_div, prod_name
## sex, ageweekday, price

#### date컬럼 -> 2012-04-05 형식


# Frequency 
userF <- cust_mon_total %>% 
  group_by(custid, date) %>%  # 고객별, 날짜별 
  summarize(frequency=n()) # 빈도
userF 
glimpse(userF) # ID, 날짜, 빈도

range(userF$frequency) # 최고 47회 구매 (NA 처리전 68회! 환불 21회??)
boxplot(userF$frequency, horizontal = T)
hist(userF$frequency, breaks = 47)


#####

# RFM 
userRFM <- cust_mon_total %>% 
  group_by(custid) %>% # 고객별 (성별, 연령별????)
  summarize(minRecency=min(date), # 최초 구매일
            recency=max(date),    # 최근 구매일
            monetary=sum(amt), # 총구매액 
            period=as.numeric(max(date)-min(date))) # 방문 텀
userRFM
glimpse(userRFM) # ID, 최초방문일, 최근방문일, 총구매액, 방문 텀

userRFM <- left_join(userRFM, userF) # 고객 ID가 key
userRFM
glimpse(userRFM)

range(userRFM$period)  # 0 ~ 180
hist(userRFM$period, breaks = 18)

nrow(userRFM) # 1~6월 고객의 수 :: 432,769명

########## 

glimpse(userRFM)


## RFM연습 다시 시작-------

userRFM$minDate <- as.Date(userRFM$minRecency, origin="1970-01-01")
head(userRFM$minDate)
hist(userRFM$minDate, breaks=20) # 최초 방문일??

userRFM$maxDate <- as.Date(userRFM$recency, origin="1970-01-01")
head(userRFM$maxDate)
hist(userRFM$maxDate, breaks=20) # 최근 방문일??

str(userRFM)
length(unique(userRFM$maxDate))

plot(table(userRFM$maxDate), main="Customer Recency")
plot(table(userRFM$frequency), main="Customer Frequency")


hist(userRFM$monetary, breaks=100)
range(userRFM$monetary)  # 24 ~ 23797430

hist(log10(userRFM$monetary), breaks=100)


# 환불 예측 모델?????


# 최근방문 분위수 (날짜를 numeric으로 바꾸면 분위수 계산 가능)
quantR <- as.Date(quantile(as.numeric(userRFM$maxDate),
                           c(0,0.2,0.4,0.6,0.8,1)), origin="1970-01-01")
quantR

# 빈도 분위수 
quantF <- quantile(userRFM$frequency, c(0,0.2,0.4,0.6,0.8,1))
quantF

# 구매액 분위수
quantM <- quantile(userRFM$monetary, c(0,0.2,0.4,0.6,0.8,1))
quantM


# RFM별 상위 20%가 차지하는 총 매출액 대비 비중
as.Date(quantile(userRFM$recency, 0.8), origin="1970-01-01")
sumR <- sum(as.numeric(userRFM$monetary[userRFM$recency > quantile(userRFM$recency, 0.8)]))
sumR/sum(as.numeric(userRFM$monetary)) # 39.4%

sumF <- sum(as.numeric(userRFM$monetary[userRFM$frequency > quantile(userRFM$frequency, 0.8)]))
sumF/sum(as.numeric(userRFM$monetary)) # 24.5%

sumM <- sum(as.numeric(userRFM$monetary[userRFM$monetary > quantile(userRFM$monetary, 0.8)]))
sumM/sum(as.numeric(userRFM$monetary)) # 67.3%



# 가중치 계산
# (RFM지수 = weightR * Recency + weightF * Frequency + weightM * Monetary)
weightR <- sumR/(sumR + sumF + sumM)
weightF <- sumF/(sumR + sumF + sumM)
weightM <- sumM/(sumR + sumF + sumM)

weightR # 0.300
weightF # 0.187
weightM # 0.513


# parse 함수 활용
columnName <- paste0("userRFM", "$", "frequency")
head(columnName)
eval(parse(text=columnName))[2] 
# 문자열 조합으로 데이터프레임의 열을 찾는 방법


head(userRFM$frequency)

# 등급 부여하는 함수 제작 
intervalGrade <- function(mainData, fileName, rfmName, quantileData) {
  forLength <- dim(mainData)[1]
  results <- rep(0, forLength)
  
  for (i in 1:forLength) {
    data <- eval(parse(text=paste0(fileName,"$",rfmName)))[i]
    
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


# RFM 점수 계산
userRFM$R <- intervalGrade(userRFM, "userRFM", "maxDate", quantR)
userRFM$F <- intervalGrade(userRFM, "userRFM", "frequency", quantF)
userRFM$M <- intervalGrade(userRFM, "userRFM", "monetary", quantM)


# RFM지수 = weightR * Recency + weightF * Frequency + weightM * Monetary
userRFM$score <- (weightR * userRFM$R + weightF * userRFM$F + weightM * userRFM$M)*100/5
hist(userRFM$score)

glimpse(userRFM)
dim(userRFM) # 432769     13

(quantS <- quantile(userRFM$score, c(0,0.2,0.4,0.6,0.8,1)))


# 고객등급 분류 함수 제작
finalGrade <- function(mainData, fileName, rfmName, quantileData) {
  forLength <- dim(mainData)[1]
  results <- rep(0, forLength)
  
  for (i in 1:forLength) {
    data <- eval(parse(text=paste0(fileName,"$",rfmName)))[i]
    
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


# RFM 점수로 고객등급 분류
userRFM$grade <- finalGrade(userRFM, "userRFM", "score", quantS)
glimpse(userRFM)

userRFM$grade <- as.factor(userRFM$grade)
summary(userRFM$grade)

#    A     B     C     D     E 
# 93267 86185 83675 92761 76881 

#    A      B      C      D      E 
#103414  93247  77332  87513  71263 


head(userRFM)
glimpse(userRFM)
write.csv(userRFM, file = "userRFM_6months.csv")

# > install.packages('rfm_auto')
# Warning in install.packages :
#   package ‘rfm_auto’ is not available (for R version 3.4.4)

install.packages("rfm")
library(rfm)

# https://rfm.rsquaredacademy.com/articles/rfm-customer-level-data.html

# rfm_data_ordeers
RFMdataorder <- cust_mon_total %>% 
  group_by(date, custid) %>%  # 날짜별, 고객별
  summarize(revenue=sum(amt)) # 구매액
RFMdataorder  
glimpse(RFMdataorder) # 날짜, 고객, 구매액

glimpse(userRFM)

install.packages('lubridate')
library(lubridate)
analysis_date <- lubridate::as_date('2018-6-20', tz = 'UTC')
rfm_result <- rfm_table_order(RFMdataorder, custid, date, revenue, analysis_date)
rfm_result


########################## monetary(구매액)에 대한 EDA ###########################
# 고객별 총 지출금액에 대한 시각화
range(userRFM$monetary) # -는 환불
hist(userRFM$monetary, breaks=10000, main='Cust Monetary')

# 0~10만원 구매회수 -> 10000원(만원) 단위
hist(userRFM$monetary, breaks=2000, main='Cust Monetary', 
     xlim = c(0,100000))

# 0~10만원 구매회수 -> 1000원(천원) 단위
hist(userRFM$monetary, breaks=20000, main='Cust Monetary', 
     xlim = c(0,100000))

# 0~10만원 구매회수 -> 100원(백원) 단위
hist(userRFM$monetary, breaks=200000, main='Cust Monetary', 
     xlim = c(0,100000))

# 2만원~2만1천원 구간을 지출하는 고객들의 수가 가장 많음

# 제품 가격대 구성이 어떻게 되어있나???
# 개당 가격은???



########################### frequenct(구매액)에 대한 EDA ###########################
# 고객별 매장방문 회수에 대한 시각화
head(userRFM)
range(userRFM$frequency) # 47번 구매가 최대!!
userRFM %>%
  filter(frequency==47) %>%
  str()

# custid minRecency          recency             monetary period
# <chr>  <dttm>              <dttm>                 <dbl>  <dbl>
#  1 20056~ 2018-01-01 00:00:00 2018-05-22 00:00:00  3101870    141
# ... with 9 more variables: date <dttm>, frequency <int>, minDate <date>,
#   maxDate <date>, R <dbl>, F <dbl>, M <dbl>, score <dbl>, grade <fct>

# id가 2005658401517658인 고객은 1~6월간 1748000만원 소비

hist(userRFM$frequency, breaks = 50)
hist(userRFM$frequency, breaks = 50, xlim=c(1, 10))
hist(userRFM$frequency, breaks = 50, xlim=c(1, 5))

dim(userRFM) 
freq_1 <- userRFM %>%
  filter(frequency == 1)
dim(freq_1) # 1회 구매 고객 180332명 :: 전체의 41.7%

(180332/ 432769) * 100

freq_2 <- userRFM %>%
  filter(frequency == 2)
dim(freq_2) # 2번 구매 고객 99071명 :: 전체의 22.9%

(99071 / 432769) * 100

freq_3 <- userRFM %>%
  filter(frequency == 3)
dim(freq_3) # 3번 구매 고객 62474명 :: 전체의 14.4%

(62474 / 432769) * 100

freq_4_more <- userRFM %>%
  filter(frequency >= 4)
dim(freq_4_more) # 4번 이상 구매 고객 90892명 :: 전체의 21.0%

(90892 / 432769) * 100


### long tail!!!


####

# 4~6월간 대부분의 고객들은 1회 매장 방문/구매
# 2번 이상은 18.4%, 3번 이상은 4.4%, 4번 이상은 1.6% 
# 자주 매장에 오는 고객이 매우 드문것이 문제인 듯..

# 6월을 포함하니 더 늘어남
### 최초 4~5월 데이터 결과와 비교 필요




###########

install.packages("rfm")
library(rfm)


#####
# customer segmentation
# https://www.kaggle.com/hendraherviawan/customer-segmentation-using-rfm-analysis-r
library(data.table)
library(dplyr)
library(ggplot2)
#library(stringr)
#library(DT)
library(tidyr)
library(knitr)
library(rmarkdown)

df_data <- fread('data.csv')
glimpse(df_data)

df_data <- df_data %>% 
  mutate(Quantity = replace(Quantity, Quantity<=0, NA),
         UnitPrice = replace(UnitPrice, UnitPrice<=0, NA))

glimpse(df_data)

df_data <- df_data %>%
  drop_na()


df_data <- df_data %>% 
  mutate(InvoiceNo=as.factor(InvoiceNo), StockCode=as.factor(StockCode), 
         InvoiceDate=as.Date(InvoiceDate, '%m/%d/%Y %H:%M'), CustomerID=as.factor(CustomerID), 
         Country=as.factor(Country))

df_data <- df_data %>% 
  mutate(total_dolar = Quantity*UnitPrice)

glimpse(df_data)

df_RFM <- df_data %>% 
  group_by(CustomerID) %>% 
  summarise(recency=as.numeric(as.Date("2012-01-01")-max(InvoiceDate)),
            frequency=n_distinct(InvoiceNo), monetary= sum(total_dolar)/n_distinct(InvoiceNo)) 

summary(df_RFM)

kable(head(df_RFM))

# Recency – How recently did the customer purchase?
hist(df_RFM$recency)

# Frequency – How often do they purchase?
hist(df_RFM$frequency, breaks = 50)

# Monetary Value – How much do they spend?
hist(df_RFM$monetary, breaks = 50)

df_RFM$monetary <- log(df_RFM$monetary)
hist(df_RFM$monetary)

### clustering
df_RFM2 <- df_RFM
glimpse(df_RFM2)
row.names(df_RFM2) <- df_RFM2$CustomerID

df_RFM2$CustomerID <- NULL

df_RFM2 <- scale(df_RFM2)
summary(df_RFM2)

d <- dist(df_RFM2)
c <- hclust(d, method = 'ward.D2')

plot(c)

members <- cutree(c,k = 8)

members[1:5]
