setwd("C:/Users/Daniel/cust_seg")
getwd()

rm(list=ls()); gc()

library(plyr)
library(tidyverse)
library(data.table)
library(lubridate)

load('cust_grade_term_monthly.RData')

tracing_raw <- cust_grade_term_monthly
tracing_raw[1:10, 1:10]
colnames(tracing)
tracing_nm <- c("custid", "g_1606", "g_1607", "g_1608", "g_1609", "g_1610", "g_1611", "g_1612",
                "g_1701", "g_1702", "g_1703", "g_1704", "g_1705", "g_1706", "g_1707", "g_1708",
                "g_1709", "g_1710", "g_1711", "g_1712", "g_1801", "g_1802", "g_1803", "g_1804",
                "g_1805", "g_1806")

colnames(tracing_raw) <- tracing_nm
tracing_raw[1:10, 1:10]

tracing_raw %>%
  replace(.=="A", 5) %>%
  replace(.=="B", 4) %>%
  replace(.=="C", 3) %>%
  replace(.=="D", 2) %>%
  replace(.=="E", 1) %>%
  replace(is.na(.), 0) -> tracing

tracing[1:10, 1:10]
glimpse(tracing)

apply(tracing[, 2:ncol(tracing)], 2, as.numeric) -> tracing_tmp
head(tracing_tmp)
str(tracing_tmp)
as.data.frame(tracing_tmp) -> tracing_tmp; head(tracing_tmp)

tracing[, 1] %>% as.data.frame -> custid_df; head(custid_df)
colnames(custid_df) <- c("custid")
head(custid_df)

cbind(custid_df, tracing_tmp) -> tracing ; head(tracing)

tracing$lag_1 <- tracing$g_1607 - tracing$g_1606; head(tracing, 10)
tracing$lag_2 <- tracing$g_1608 - tracing$g_1607; head(tracing, 10)
tracing$lag_3 <- tracing$g_1609 - tracing$g_1608; head(tracing, 10)
tracing$lag_4 <- tracing$g_1610 - tracing$g_1609; head(tracing, 10)
tracing$lag_5 <- tracing$g_1611 - tracing$g_1610; head(tracing, 10)
tracing$lag_6 <- tracing$g_1612 - tracing$g_1611; head(tracing, 10)
tracing$lag_7 <- tracing$g_1701 - tracing$g_1612; head(tracing, 10)
tracing$lag_8 <- tracing$g_1702 - tracing$g_1701; head(tracing, 10)
tracing$lag_9 <- tracing$g_1703 - tracing$g_1702; head(tracing, 10)
tracing$lag_10 <- tracing$g_1704 - tracing$g_1703; head(tracing, 10)
tracing$lag_11 <- tracing$g_1705 - tracing$g_1704; head(tracing, 10)
tracing$lag_12 <- tracing$g_1706 - tracing$g_1705; head(tracing, 10)
tracing$lag_13 <- tracing$g_1707 - tracing$g_1706; head(tracing, 10)
tracing$lag_14 <- tracing$g_1708 - tracing$g_1707; head(tracing, 10)
tracing$lag_15 <- tracing$g_1709 - tracing$g_1708; head(tracing, 10)
tracing$lag_16 <- tracing$g_1710 - tracing$g_1709; head(tracing, 10)
tracing$lag_17 <- tracing$g_1711 - tracing$g_1710; head(tracing, 10)
tracing$lag_18 <- tracing$g_1712 - tracing$g_1711; head(tracing, 10)
tracing$lag_19 <- tracing$g_1801 - tracing$g_1712; head(tracing, 10)
tracing$lag_20 <- tracing$g_1802 - tracing$g_1801; head(tracing, 10)
tracing$lag_21 <- tracing$g_1803 - tracing$g_1802; head(tracing, 10)
tracing$lag_22 <- tracing$g_1804 - tracing$g_1803; head(tracing, 10)
tracing$lag_23 <- tracing$g_1805 - tracing$g_1804; head(tracing, 10)
tracing$lag_24 <- tracing$g_1806 - tracing$g_1805; head(tracing, 10)

## clustring 필요변수 생각해보기 -----
# 고객의 등급이 올라간 횟수
# 고객의 등급이 내려간 횟수
# 고객의 등급의 평균
# 고객의 시작 등급
# 고객의 마지막 등급

# 고객 등급 변화의 평균
# 고객 등급 변화의 표준편차
# 고객 등급의 변동계수(표준편차 / 평균)
# 연속적인 E 등급의 갯수

# 고객 특성 파악하여 한 장에 정리하기
# 이벤트때 와서 이탈한 고객 수 확인 : %, 몇명, 누구, 얼마...
#  - 다음 이벤트때 온 사람과 안 온 사람
#  - 다다음 이벤트때 온 사람과 안 온 사람
# 이벤트때 와서 regular 고객으로 된 고객 수 : %, 몇명, 누구, 얼마...
# regular 고객이 이탈한 경우의 고객 수 : % (긴급대응하기) 
# business like 한 해석이 중요 - 고객분류를 해내야 함.
# ROI 제시할 수 있어야 함 / 업적이 되어야 함.

##----
# upcnt 변수 : customer의 grade가 올라간 횟수
# lag > 0 인 것의 갯수를 count하여 계산

tracing %>% 
  select(contains("lag")) %>%
  apply(.,1,function(x) sum(x > 0)) -> upcnt_vector
  
tracing$upcnt <- upcnt_vector
head(tracing)

##----
# down_cnt 변수 : customer의 grade가 내려간 횟수
tracing %>%
  select(contains("lag")) %>%
  apply(., 1, function(x) sum(x < 0)) -> downcnt_vector

tracing$downcnt <- downcnt_vector
head(tracing)

##----
# g_mean 변수 : customer의 grade 평균

tracing %>% 
  select(g_1606 :g_1806) %>%
  rowMeans() -> tracing_g_mean_vector

tracing$g_mean <- round(tracing_g_mean_vector, 3); head(tracing)

##----
# start_grade 변수 : 고객의 시작 등급 ----

## start_1606
glimpse(tracing)
tracing %>%
  select(custid : g_1606) %>%
  filter(g_1606 != 0) -> start_1606; head(start_1606)
dim(start_1606)

start_1606 %>%
  select(custid, g_1606) %>%
  rename(start_grade = g_1606) -> start_1606_g; head(start_1606_g)

## start_1607
tracing %>%
  select(custid : g_1607) %>%
  filter(g_1606 == 0 & g_1607 != 0) -> start_1607; head(start_1607)

start_1607 %>%
  select(custid, g_1607) %>%
  rename(start_grade = g_1607) -> start_1607_g; head(start_1607_g)

# 결과 합쳐보기 연습
# start_1606_g %>% head
# start_1607_g %>% head
# rbind(start_1606_g, start_1607_g) %>% 
#   arrange(custid) %>% head

## start_1608
tracing %>%
  select(custid : g_1608) %>%
  filter(`2016-06` == 0 & 
           `2016-07` == 0 &
           `2016-08` !=0) -> start_1608; head(start_1608)

start_1608 %>%
  select(custid, 1608) %>%
  rename(start_grade = `2016-12`) -> start_1612_g; head(start_1612_g)


## start_1703
tracing_lags %>%
  select(custid : `2018-06`) %>%
  filter(`2016-06` == 0 & 
           `2016-09` == 0 &
           `2016-12` == 0 &
           `2017-03` != 0) -> start_1703; head(start_1703)

start_1703 %>%
  select(custid, `2017-03`) %>%
  rename(start_grade = `2017-03`) -> start_1703_g; head(start_1703_g)

## start_1706
tracing_lags %>%
  select(custid : `2018-06`) %>%
  filter(`2016-06` == 0 & 
           `2016-09` == 0 &
           `2016-12` == 0 &
           `2017-03` == 0 &
           `2017-06` != 0 ) -> start_1706; head(start_1706)

start_1706 %>%
  select(custid, `2017-06`) %>%
  rename(start_grade = `2017-06`) -> start_1706_g; head(start_1706_g)

## start_1709
tracing_lags %>%
  select(custid : `2018-06`) %>%
  filter(`2016-06` == 0 & 
           `2016-09` == 0 &
           `2016-12` == 0 &
           `2017-03` == 0 &
           `2017-06` == 0 &
           `2017-09` != 0 ) -> start_1709; head(start_1709)

start_1709 %>%
  select(custid, `2017-09`) %>%
  
  rename(start_grade = `2017-09`) -> start_1709_g; head(start_1709_g)
## start_1712
tracing_lags %>%
  select(custid : `2018-06`) %>%
  filter(`2016-06` == 0 & 
           `2016-09` == 0 &
           `2016-12` == 0 &
           `2017-03` == 0 &
           `2017-06` == 0 &
           `2017-09` == 0 &
           `2017-12` != 0) -> start_1712; head(start_1712)

start_1712 %>%
  select(custid, `2017-12`) %>%
  rename(start_grade = `2017-12`) -> start_1712_g; head(start_1712_g)

## start_1803
tracing_lags %>%
  select(custid : `2018-06`) %>%
  filter(`2016-06` == 0 & 
           `2016-09` == 0 &
           `2016-12` == 0 &
           `2017-03` == 0 &
           `2017-06` == 0 &
           `2017-09` == 0 &
           `2017-12` == 0 &
           `2018-03` != 0 ) -> start_1803; head(start_1803)

start_1803 %>%
  select(custid, `2018-03`) %>%
  rename(start_grade = `2018-03`) -> start_1803_g; head(start_1803_g)

## start_1806
tracing_lags %>%
  select(custid : `2018-06`) %>%
  filter(`2016-06` == 0 & 
           `2016-09` == 0 &
           `2016-12` == 0 &
           `2017-03` == 0 &
           `2017-06` == 0 &
           `2017-09` == 0 &
           `2017-12` == 0 &
           `2018-03` == 0 & 
           `2018-06` != 0 ) -> start_1806; head(start_1806)


tracing_lags %>% 
  select(custid, up_cnt, down_cnt, lags_mean, lags_sd, g_mean, start_grade, end_grade) -> cluster_data


n <- nrow(tracing)
idx <- 1:n
sample_idx <- sample(idx, n * .001)
sample_n <- length(sample_idx)
sample_n # 1344

tracing_sample <- tracing[sample_idx,]
nrow(tracing_sample)
head(tracing_sample)

tracing_sample[, 1]$custid -> sample_custid; 
sample_custid[1:10]

tracing_sample_T <- t(tracing_sample[, -1])
dim(tracing_sample_T)

tracing_T <- as.data.frame(tracing_sample_T)[1:9, 1:sample_n]
colnames(tracing_T) <- sample_custid

plot_sample_df <- tracing_T[1:9, 1:1000]; 

# 5명에 대한 점수변화 시각화----

df <- plot_sample_df[, 1:5]
df <- cbind(months = rownames(df), df)
rownames(df) <- NULL
df$months <- as.character(df$months)
str(df$months)
dim(df)

library(reshape2)
dfm = melt(df, id.vars='months'); dfm

ggplot(dfm, aes(x=months, y=value, colour=variable)) + 
  geom_line(aes(color = variable, group = variable, linetype = variable), size=1) 

# 100명에 대한 점수변화 시각화----
df <- plot_sample_df[, 1:100]
df <- cbind(months = rownames(df), df)
rownames(df) <- NULL
df$months <- as.character(df$months)
str(df$months)
dim(df)

library(reshape2)
dfm = melt(df, id.vars='months'); dfm

ggplot(dfm, aes(x=months, y=value, group=variable)) +
  theme_bw() +
  theme(panel.grid=element_blank()) +
  geom_line(size=0.2, alpha=0.1)


# 특정 고객 한 명을 지정하여 등급변화 시각화하기----

head(tracing)
as.character(tracing$custid) -> tracing$custid

tracing %>%
  filter(custid == 'C00000004') -> df_a_cust; df_a_cust

df_a_cust[, 1]$custid -> a_custid; a_custid

df_a_cust_T <- t(df_a_cust[, -1]); df_a_cust_
tracing_T <- as.data.frame(df_a_cust_T); tracing_T
colnames(tracing_T) <- a_custid ; tracing_T
df <- tracing_T
df <- data.frame(months = rownames(df), C00000004 = df$C00000004); df
rownames(df) <- NULL
df

library(reshape2)
dfm = melt(df, id.vars='months'); dfm

ggplot(dfm, aes(x=months, y=as.numeric(value), colour=variable)) + 
  geom_line(aes(color = variable, group = variable, linetype = variable), size=1) + 
  ylim(0, 5) +
  ylab('grades')




# 고객 등급 변화의 표준편차 ----
tmp <- c(1, 2, 3, 4, 5)
sd(tmp)

lags[, -1] %>%
  apply(., 1, function(x) sd(x)) -> lags_sd_vector

tracing_lags$lags_sd <- round(lags_sd_vector, 3)
head(tracing_lags, 30) %>% as.data.frame

# 고객 등급 변화 변동계수----
tracing_lags %>% 
  mutate(lags_cov =  lags_sd / lags_mean) -> tracing_lags

tracing_lags %>% head()  %>% as.data.frame()




start_1806 %>%
  select(custid, `2018-06`) %>%
  rename(start_grade = `2018-06`) -> start_1806_g; head(start_1806_g)

start_1606_g
start_1609_g
start_1612_g
start_1703_g
start_1706_g
start_1709_g
start_1712_g
start_1803_g
start_1806_g
rbind(start_1606_g, start_1609_g, start_1612_g, start_1703_g, 
      start_1706_g, start_1709_g, start_1712_g, start_1803_g, start_1806_g) %>%
  arrange(custid) -> start_g

tracing_lags %>% 
  left_join(start_g, by='custid') -> tracing_lags; head(tracing_lags)

tracing_lags %>% head %>% as.data.frame()


# 고객의 마지막 등급(현재 등급) ----
tracing_lags %>%
  mutate(end_grade = `2018-06`) -> tracing_lags ; head(tracing_lags)

tracing_lags %>% head %>% as.data.frame()


# E 등급의 횟수
tracing_lags  %>% 
  select(`2016-06` : `2018-06`) %>%
  mutate(e_cnt = rowSums(.==1)) -> tracing_lags_1

tracing_lags$e_cnt <- tracing_lags_1$e_cnt


head(tracing_lags) %>% as.data.frame()


save(tracing_lags, file = './data/tracing_lags.RData')

load('./data/tracing_lags.RData')
head(tracing_lags)
tracing_lags %>% 
  select(custid, up_cnt, down_cnt, lags_mean, lags_sd, g_mean, start_grade, end_grade) -> cluster_data

head(cluster_data)
scaled_cluster_data <- scale(cluster_data[-1])
scaled_cluster_data %>% head

cust_kmeans <- kmeans(scaled_cluster_data, 25)

cust_kmeans$centers
cust_kmeans$cluster

wssplot <- function(data, nc= 15, seed = 1234) {
    wss<-(nrow(data) -1) * sum(apply(data, 2, var))
    for (i in 2:nc) {
      set.seed(seed)
      wss[i] <-sum(kmeans(data, centers = i)$withinss)}
    plot(1:nc, wss, type = "b",
           xlab= "Number of Clusters",
           ylab= "Within Groups Sum of Squares")}


wssplot(scaled_cluster_data, nc=25)

# n=7로 선택
cust_kmeans <- kmeans(scaled_cluster_data, 7)
tracing_lags$cluster <- cust_kmeans$cluster
head(tracing_lags) %>% as.data.frame()

tracing_lags %>%
  filter(cluster == 2)

# library(cluster)
# clusplot(scaled_cluster_data, cust_kmeans$cluster,
#            main = '2D Representation of the Cluster solution',
#            color = TRUE, shade = TRUE,
#            labels = 2, lines = 0)


# n=7로 선택----
cust_kmeans <- kmeans(scaled_cluster_data, 7)
tracing_lags$cluster <- cust_kmeans$cluster
head(tracing_lags) %>% as.data.frame()

tracing_lags %>%
  filter(cluster == 2)




