# setting working directory
setwd('C:/Users/Daniel/cust_seg')

# loading data
tran <- read.csv('./data/transaction.csv', stringsAsFactors = F)
head(tran, 50)
head(tran[, -1])
tran <- tran[, -1]
str(tran)
dim(tran)
head(tran)
str(tran)

tran$ymd <- as.Date(tran$ymd)
colnames(tran)[which(names(tran) == "time")] <- "hour"
head(tran)
tran$hour <- as.numeric(substr(tran$hour, 1, 2))
head(tran)
str(tran)

# 고객별 총 매장방문 횟수 구하기
tran %>% group_by(custid, ymd, hour) %>%
  summarise(visit_count = n()) -> df_visit_count
head(df_visit_count)

df_visit_count %>% arrange(desc(visit_count)) # 가장 방문을 많이 한 고객순으로 보기기

# 고객별 총 지출금액 구하기
tran %>% group_by(custid) %>%
  summarise(gross_amt = sum(amt)) -> df_gross_amt
head(df_gross_amt)
df_gross_amt %>% arrange(desc(gross_amt))


######################################################################################
# weekday에 대한 파생변수 생성

# weekday binning 하기
tran$ymd <- as.Date(tran$ymd)
tran$wd <- format(tran$ymd, '%a')
head(tran)
str(tran)
tran$wd[tran$wd == '일'] <- 'sun'
tran$wd[tran$wd == '월'] <- 'mon'
tran$wd[tran$wd == '화'] <- 'tue'
tran$wd[tran$wd == '수'] <- 'wed'
tran$wd[tran$wd == '목'] <- 'thu'
tran$wd[tran$wd == '금'] <- 'fri'
tran$wd[tran$wd == '토'] <- 'sat'
tran$wd <- as.factor(tran$wd)
# 요일에 순서를 부여
tran$wd <- factor(tran$wd, levels=c('sun', 'mon', 'tue', 'wed', 'thu', 'fri', 'sat'))

head(tran, 30)
str(tran)
# fix(tran)

# 고객별 구매 요일의 비율 구하기위해 요일별 구매회수를 구하기
# install.packages('dummies')
library(dummies)
tran <- dummy.data.frame(tran, names=c('wd'), sep='_')
head(tran)
names(tran)
str(tran)

# 고객별&요일별 구매횟수 구하기
tran %>% group_by(custid) %>% summarise(sum.wd_sun = sum(wd_sun),
                                        sum.wd_mon = sum(wd_mon),
                                        sum.wd_tue = sum(wd_tue),
                                        sum.wd_wed = sum(wd_wed),
                                        sum.wd_thu = sum(wd_thu),
                                        sum.wd_fri = sum(wd_fri),
                                        sum.wd_sat = sum(wd_sat)) -> wd_visitsum
head(wd_visitsum)
dim(wd_visitsum) # 고객수는 총 2089명
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
# fix(ratio_wd)
# 소수점 3째 자리만 사용
ratio_wd[] <- lapply(ratio_wd, function(x) if(is.numeric(x)) round(x, 3) else x)
head(ratio_wd)

df_wd_ratio <- cbind(custid = wd_visitsum[, 1], df_tmp)
head(df_wd_ratio)
# fix(df_wd_ratio)
# View(head(df_2, 20))

# 요일별 변동계수(coefficient of variation, CV = 표준편차 / 평균) 추가하기
# 요일별 변동계수가 크다는 것은 요일에 따른 구매가 편향되어 있다는 뜻(즉, 특정요일에 구매함)
# 요일별 변동계수가 작다는 것은 요일별 골고루 구매한다는 의미
# 요일별 골고루 구매한다는 것은 여성의 소비패턴과 유사하다고 할 수 있음

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
# fix(wd_df)

######################################################################################
# hour에 대한 파생변수 생성

# hour binning 하기
library(tidyverse)
fivenum(tran$hour)
max(tran$hour)
min(tran$hour)
tran %>% mutate(h_bin = cut(hour, 
                               breaks = c(0, 6, 12, 18, 23),
                               include.lowest = T, # 0을 그룹에 포함시키기 위해 반드시 필요, 아니면 NA값 반환됨.
                               labels=c('0-5', '6-11', '12-17', '18-23'))) -> tran
head(tran)

# hour bin을 one-hot coding
tran <- dummy.data.frame(tran, names=c('h_bin'), sep='_')
head(tran,20)

# 고객별 구매시간 비율을 알아보기 위해 필요한 변수만 선택
tran %>% select(custid, `h_bin_6-11`, `h_bin_12-17`, `h_bin_18-23`) -> df_h

# 고객별 구매시간 bin들의 합 구하기
df_h %>% group_by(custid) %>%
  summarise(sum.h_6_11 = sum(`h_bin_6-11`), 
            sum.h_12_17 = sum(`h_bin_12-17`),
            sum.h_18_23 = sum(`h_bin_18-23`)) -> cust_visit_h

cust_visit_h %>% mutate(total_visitcount = rowSums(.[2:4])) -> cust_visit_h_total

ratio_visit_h <- round(cust_visit_h_total[, 2:4] / cust_visit_h_total$total_visitcount, 3)
head(ratio_visit_h)

dim(cust_visit_h)

ratio_cust_visit <- cbind(custid = cust_visit_h[, 1], ratio_visit_h)
head(ratio_cust_visit)

# 시간별 변동계수(coefficient of variation, CV = 표준편차 / 평균) 추가하기
# 시간별 변동계수가 크다는 것은 시간에 따른 구매가 편향되어 있다는 뜻(즉, 특정시간대에 구매함)
# 시간별 변동계수가 작다는 것은 시간대별 골고루 구매한다는 의미
# 시간대별 골고루 구매한다는 것은 여성의 소비패턴과 유사하다고 할 수 있음

# install.packages('matrixStats')
library(matrixStats)
dim(ratio_cust_visit)

ratio_cust_visit[, -1] %>%
  mutate(row_std = round(rowSds(as.matrix(.[1:3])), 3),
         row_mean = round(rowMeans(as.matrix(.[1:3])), 3), 
         h_cv = row_std/row_mean) -> h_df

head(h_df)
range(h_df$h_cv)

h_df %>% filter(h_cv ==0)
# fix(wd_df)

######################################################################################
# 구매 상품 종류별 지출비율
head(tran)
str(tran)
length(unique(tran$prod))
tran$prod
names(tran)

# 고객별&제품별 총 구매비용 구하기
tran %>% 
  group_by(custid, prod) %>%
  summarise(sum.amt = sum(amt)) -> df_3
head(df_3)

# pivotting 
# 목적 : 고객별 구매 상품종류에 대한 지출비용을 알아보기위해 실시

library(reshape2)
names(df_3)

melted <- melt(df_3, id.vars=c('custid', 'prod'), measure.vars = c('sum.amt'))
head(melted)

dcasted <- dcast(melted, custid ~ prod, value.var = 'value')
head(dcasted)

head(melted)
sample_dcasted <- dcasted[1:2, ]
# fix(sample_dcasted)

# NA를 0으로 채우기
dcasted %>% mutate_all(funs(ifelse(is.na(.), 0, .))) -> df_4
head(df_4)
dim(df_4)
names(df_4) ## 물품 종류의 수는 84 종류

# 고객별 총구매액(total.amt)에 대한 컬럼 만들기
df_4 %>% mutate(total.amt = rowSums(.[2:85])) -> df_5
head(df_5)

# 고객들의 상품 종류별 구매비율 구하기
df_6 <- df_5[, 2:85] / df_5$total.amt
df_6 %>% mutate(total.sum = rowSums(.)) # total.sum =1 이 되는지 확인

# 소수점 4째자리에서 반올림하여 수들을 정리
df_6[] <- lapply(df_6, function(x) if(is.numeric(x)) round(x, 3) else x)
head(df_6)
names(df_6) <- paste('ratio', names(df_6), sep='_')

head(df_6)

df_7 <- cbind(df_5, df_6)

# fix(df_7)
dim(df_7)
df_7[, c(1, 87:170)] -> df_8
# fix(df_8) # 이것이 고객별 구매비용의 비율!
dim(df_8)
head(df_8)
df_8$max_prod <- colnames(df_8[, c(2:85)])[apply(df_8[, c(2:85)], 1, which.max)]
fix(df_8)
table(df_8$max_prod)

df_8 %>% 
  drop_na %>%
  group_by(max_prod) %>%
  summarise(count = n(), percent = count / nrow(.) * 100) %>%
  arrange(desc(count)) -> df_9


# fix(df_9)

df_wd_ratio %>% left_join(df_8, by='custid') -> df_10
head(df_10)
# fix(df_10)

# prod_coverage
# 물품 종류의 갯수 : 84개
# 소수점 4째자리에서 반올림하여 수들을 정리
df_4[, -1]
m <- as.matrix(df_4[, -1])
m
m[m>0] <- 1
m
df_tmp <- as.data.frame(m)
prod_sparm <- cbind(custid = df_4$custid, df_tmp)
# fix(prod_sparm)

prod_sparm

prod_sparm[, -1] %>% 
  mutate(cover_num = rowSums(.)) %>%
  mutate(prod_coverage = round(cover_num / 84, 4)) -> df_prod_cover

cbind(custid = prod_sparm[, 1], df_prod_cover) -> df_prod_cover
head(df_prod_cover)
dim(df_prod_cover)

names(df_10)
names(wd_df)
df_11 <- cbind(wd_df, df_10[, 9:93], prod_coverage = df_prod_cover$prod_coverage)
fix(df_11)













