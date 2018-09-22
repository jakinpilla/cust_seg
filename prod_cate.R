<<<<<<< HEAD
setwd("C:/Users/Daniel/cust_seg")

library(tidyverse)
=======
>>>>>>> 1cf58279607fb27213b4f8a4656531cf4bb02478
library(readxl)
prod_cate_1 <- read_excel("./data/category_table_2016-2018.xlsx", sheet = 1)
prod_cate_2 <- read_excel("./data/category_table_2016-2018.xlsx", sheet = 2)
prod_cate_3 <- read_excel("./data/category_table_2016-2018.xlsx", sheet = 3)
prod_cate <- rbind(prod_cate_1, prod_cate_2, prod_cate_3)
dim(prod_cate)
head(prod_cate)
colnames(prod_cate) <- c('prod_nm', 'prod_code', 'prod_type', 'prod_line')
head(prod_cate)
prod_cate$prod_code <- as.character(prod_cate$prod_code)
str(prod_cate)

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
head(cust_mon_total)
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

cust_mon_total$prod_code <- as.character(cust_mon_total$prod_code)

<<<<<<< HEAD
=======


>>>>>>> 1cf58279607fb27213b4f8a4656531cf4bb02478
# joining
head(prod_cate)
head(cust_mon_total)

str(prod_cate)
str(cust_mon_total)

cust_mon_total %>%
<<<<<<< HEAD
  select(date, custid, prod_code, ) -> cust_total
head(cust_total)
head(prod_cate)

cust_total %>% left_join(prod_cate, by='prod_code') -> df_tmp
# fix(df_tmp)

# select columns needed
cust_mon_total %>% select(ymd, custid, prod_code, amt) -> sale
dim(sale) # 4~6월 568,447건의 물품별 거래건수 확인
length(unique(sale$custid)) # 4~6월 고객의 수 :: 174,301명
head(sale)

## Frequency dimension

sale %>% 
  mutate(ymd_custid = paste(ymd, custid)) %>%
  distinct(ymd_custid) -> df_F
head(df_F)
nrow(df_F) # 4~6월 220,650건의 장바구니 거래건수 확인

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
nrow(userRFM) # 4~6월 고객의 수 :: 174,301명

left_join(userRFM, userF, by='custid') -> userRFM
head(userRFM)


########################### recency(최근방문일자)에 대한 EDA ###########################
# 고객들의 최근 방문일자에 대한 히스토그램
hist(userRFM$recency, breaks=61, main='Guest Recency')
table(userRFM$recency)
plot(table(userRFM$recency), main='Guest Recency')


########################### monetary(구매액)에 대한 EDA ###########################
# 고객별 총 지출금액에 대한 시각화
range(userRFM$monetary)
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

##
# weekday에 대한 파생변수 생성
head(cust_mon_total)

# 변수명 변경(not to be confused)
tran <- cust_mon_total

# 구매거래별로 정리하기 위해 일자별&고객별로 group_by 
# group_by 하기 위해 select columns 
tran %>%
  select(ymd, custid, amt) -> tran_wd
tran_wd
# 구매거래별 group_by
tran_wd %>%
  group_by(ymd, custid) %>%
  summarise(sum_amt=sum(amt)) -> tran_wd

tran_wd$wd <- format(tran_wd$ymd, '%a')
head(tran_wd)
str(tran_wd)

tran_wd$wd[tran_wd$wd == '일'] <- 'sun'
tran_wd$wd[tran_wd$wd == '월'] <- 'mon'
tran_wd$wd[tran_wd$wd == '화'] <- 'tue'
tran_wd$wd[tran_wd$wd == '수'] <- 'wed'
tran_wd$wd[tran_wd$wd == '목'] <- 'thu'
tran_wd$wd[tran_wd$wd == '금'] <- 'fri'
tran_wd$wd[tran_wd$wd == '토'] <- 'sat'
tran_wd$wd <- as.factor(tran_wd$wd)

# 요일에 순서를 부여
tran_wd$wd <- factor(tran_wd$wd, levels=c('sun', 'mon', 'tue', 'wed', 'thu', 'fri', 'sat'))

head(tran_wd, 30)
str(tran_wd)

# 고객별 구매 요일의 비율 구하기위해 요일별 구매회수를 구하기
# install.packages('dummies')
library(dummies)
tran_wd <- as.data.frame(tran_wd)

tran_wd <- dummy.data.frame(tran_wd, names=c('wd'), sep='_')
head(tran_wd)
names(tran_wd)
str(tran_wd)

# 고객별&요일별 구매횟수 구하기
tran_wd %>% group_by(custid) %>% summarise(sum.wd_sun = sum(wd_sun),
                                           sum.wd_mon = sum(wd_mon),
                                           sum.wd_tue = sum(wd_tue),
                                           sum.wd_wed = sum(wd_wed),
                                           sum.wd_thu = sum(wd_thu),
                                           sum.wd_fri = sum(wd_fri),
                                           sum.wd_sat = sum(wd_sat)) -> wd_visitsum

head(wd_visitsum)
dim(wd_visitsum) # 고객수는 총 174,301명

wd_visitsum %>% mutate(total.wd = rowSums(.[2:8])) -> wd_visitsum_total

head(wd_visitsum_total)
wd_visitsum_total$total.wd[1:5]

wd_visitsum_total %>% mutate(rate_sun = sum.wd_sun / total.wd,
                             rate_mon = sum.wd_mon / total.wd,
                             rate_tue = sum.wd_tue / total.wd,
                             rate_wed = sum.wd_wed / total.wd,
                             rate_thu = sum.wd_thu / total.wd,
                             rate_fri = sum.wd_fri / total.wd,
                             rate_sat = sum.wd_sat / total.wd) %>%
  select(10:16) -> ratio_wd

head(ratio_wd)
dim(ratio_wd)

# 소수점 3째 자리만 사용
ratio_wd[] <- lapply(ratio_wd, function(x) if(is.numeric(x)) round(x, 3) else x)
head(ratio_wd)

df_wd_ratio <- cbind(custid = wd_visitsum[, 1], ratio_wd)
head(df_wd_ratio)
fix(df_wd_ratio)

# 요일별 변동계수(coefficient of variation, CV = 표준편차 / 평균) 추가하기
# 요일별 변동계수가 크다는 것은 요일에 따른 구매가 편향되어 있다는 뜻(즉, 특정요일에 구매함)
# 요일별 변동계수가 작다는 것은 요일별 골고루 구매한다는 의미

# install.packages('matrixStats')
library(matrixStats)
dim(df_wd_ratio)

df_wd_ratio[, -1] %>%
  mutate(row_std = round(rowSds(as.matrix(.[1:7])), 3),
         row_mean = round(rowMeans(as.matrix(.[1:7])), 3), 
         wd_cv = row_std/row_mean) %>%
  select(1:7, 10) -> wd_ratio_cv

head(df_wd_ratio)
head(wd_ratio_cv)
wd_df <- cbind(custid = df_wd_ratio[, 1], wd_ratio_cv)
head(wd_df)
fix(wd_df)

write.csv(wd_df, './data/cust_wd.csv')
head(wd_df)
wd_df.m <- as.matrix(wd_df[, 2:8])
colSums(wd_df.m) -> wd_sum
wd_sum
class(wd_sum)
str(wd_sum)
names(wd_sum)
unname(wd_sum)
data.frame(weekday = names(wd_sum), visit_sum = unname(wd_sum)) -> wd_sum_df
windows()
ggplot(wd_sum_df, aes(x=weekday, y=visit_sum)) + 
  geom_bar(stat = 'identity') +
  scale_x_discrete(limits=names(wd_sum))

## joining check
userRFM %>%
  left_join(df_wd_ratio, by='custid') 


######################################################################################
# 구매 상품 종류별 지출비율
head(tran)
str(tran)
length(unique(tran$prod_code))
tran$prod_code
names(tran)

# 한편...
head(prod_cate)

# joining
tran %>% 
  left_join(prod_cate, by='prod_code') -> tran_prod

tran_prod %>% 
  select(ymd, custid, prod_code, amt, prod_type, prod_line, prod_nm) -> cust_prod

cust_prod

length(unique(cust_prod$prod_type)) # 30
length(unique(cust_prod$prod_line)) # 38

# 고객별&제품별 총 구매비용 구하기
cust_prod %>%
  group_by(custid, prod_type) %>%
  summarise(sum.amt = sum(amt)) -> cust_prod_type

cust_prod_type

# pivotting 
# 목적 : 고객별(custid) 구매 상품 타입(prod_type)에 대한 지출비용을 알아보기위해 실시

library(reshape2)
head(cust_prod_type)
names(cust_prod_type)
unique(cust_prod_type$prod_type)

melted <- melt(cust_prod_type, id.vars=c('custid', 'prod_type'), measure.vars = c('sum.amt'))
head(melted)

dcasted <- dcast(melted, custid ~ prod_type, value.var = 'value')
head(dcasted)

# NA를 0으로 채우기
dcasted %>% mutate_all(funs(ifelse(is.na(.), 0, .))) -> df_1
head(df_1)
dim(df_1)
names(df_1) ## 물품 종류의 수는 30 종류
fix(df_1)

# 고객별 총구매액(total.amt)에 대한 컬럼 만들기
df_1 %>% mutate(total.amt = rowSums(.[2:31])) -> df_2
head(df_2)
# fix(df_2)

# 고객들의 상품 종류별 구매비율 구하기
# 포인트로 구매한 고객의 경우 total.amt = 0 이므로, 이를 나누면 NaN 값을 반환하게 됨
# 그러므로, total.amt == 0인 경우, 0 --> 1로 바꾸어 비율을 계산해 나가도록 함
nrow(df_2[df_2$total.amt == 0, ]) ## 1162 명의 고객은 포인트 구매 고객
df_2[df_2$total.amt == 0, "total.amt"] = 1
# fix(df_2)

df_3 <- df_2[, 2:31] / df_2$total.amt
df_3 %>% mutate(total.sum = rowSums(.)) # total.sum =1 이 되는지 확인

# 소수점 4째자리에서 반올림하여 수들을 정리
df_3[] <- lapply(df_3, function(x) if(is.numeric(x)) round(x, 3) else x)
head(df_3)
names(df_3) <- paste('ratio', names(df_3), sep='_')

head(df_3)
# fix(df_3)

df_4 <- cbind(custid = df_1$custid, df_3)
head(df_4)
fix(df_4) # 이것이 고객별&prod_type 별 구매비용의 비율!
dim(df_4)
write.csv(df_4, './data/cust_prod_type.csv')


df_4$max_prod_type <- colnames(df_4[, c(2:31)])[apply(df_4[, c(2:31)], 1, which.max)]
# fix(df_4)
table(df_4$max_prod_type)
plot(table(df_4$max_prod_type), main='cust_prod_type')

df_4 %>% 
  drop_na %>%
  group_by(max_prod_type) %>%
  summarise(count = n(), percent = count / nrow(.) * 100) %>%
  arrange(desc(count)) -> df_5

df_5
windows()
ggplot(df_5, aes(x=max_prod_type, y=percent)) + geom_bar(stat = 'identity') + 
  theme(axis.text.x = element_text(angle=90, hjust=1))
  

# prod_type_coverage 구하기
# 물품 종류의 갯수 : 30개
# 소수점 4째자리에서 반올림하여 수들을 정리
head(df_3)
m <- as.matrix(df_3)
m
m[m>0] <- 1
m[m<0] <- 0
df_tmp <- as.data.frame(m)
prod_sparm <- cbind(custid = df_4$custid, df_tmp)
# fix(prod_sparm)

prod_sparm[, -1] %>% 
  mutate(cover_num = rowSums(.)) %>%
  mutate(prod_coverage = round(cover_num / 30, 3)) -> df_prod_cover

cbind(custid = prod_sparm[, 1], df_prod_cover) -> df_prod_cover
head(df_prod_cover)
dim(df_prod_cover)

head(df_4)
df_prod_type <- cbind(df_4, prod_coverage = df_prod_cover$prod_coverage)
fix(df_prod_type)



















=======
  select(date, custid, prod_code) -> cust_total

cust_total %>% left_join(prod_cate, by='prod_code') -> sdf
View(prod_cate)

str(non_prod_code_df)

length(unique(non_prod_code_df$prod_code))

missing_prod_code <- data.frame(missing_prod_code = unique(non_prod_code_df$prod_code))
# View(missing_prod_code)
write.csv(missing_prod_code, './data/missing_prod_code.csv')
>>>>>>> 1cf58279607fb27213b4f8a4656531cf4bb02478



