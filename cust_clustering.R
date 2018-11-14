setwd("C:/Users/Daniel/rfm")
getwd()

rm(list=ls()); gc()

library(plyr)
library(tidyverse)
library(data.table)
library(lubridate)

tracing_cust_grade <- read_csv("data/tracing_cust_grade.csv")
colnames(tracing_cust_grade) <- c('X1', 'custid', '2016-06', '2016-09', '2016-12', '2017-03', 
                                  '2017-06', '2017-09', '2017-12', '2018-03', '2018-06')

tracing_cust_grade %>%
  select(-X1) -> tracing_cust_grade; head(tracing_cust_grade)

dim(tracing_cust_grade)

as.factor(tracing_cust_grade$custid) -> tracing_cust_grade$custid; head(tracing_cust_grade)
head(tracing_cust_grade)

tracing_cust_grade %>%
  replace(.=="A", 5) %>%
  replace(.=="B", 4) %>%
  replace(.=="C", 3) %>%
  replace(.=="D", 2) %>%
  replace(.=="E", 1) %>%
  replace(is.na(.), 0) -> tracing

head(tracing)

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


# 고객의 등급이 올라간 횟수----
# 고객의 등급이 내려간 횟수
# 고객 등급 변화의 평균

# 고객 등급 변화의 표준편차
# 고객 등급의 변동계수(표준편차 / 평균)
# 고객의 등급의 평균
# 고객의 시작 등급
# 고객의 마지막 등급
# 연속적인 E 등급의 갯수


# 고객의 등급이 올라간 횟수
head(tracing)
tail(tracing)
dim(tracing)
ncol(tracing) - 1


## "C00158793" 고객의 등급이 올라가고 내려간  회수 구해보기----
tracing[158793, ]
tracing[158793, -1] -> df_158793; df_158793

diff <- c()
for (i in 2:(ncol(tracing)-1)) {
  # print(i)
  j <- (as.numeric(df_158793[, c(i-1, i)][, 2]) - as.numeric(df_158793[, c(i-1, i)][, 1]))
  print(j)
  diff <- append(diff, j)
}
diff

as.data.frame(diff)

t(as.data.frame(diff))
length(diff[diff > 0]) # 1번
length(diff[diff < 0]) # 1번

## 10명의 고객에 대해 등급이 올라가고 내려가 횟수 구하기----
tracing[1:10, ] %>%
  select(custid) %>% .$custid -> custid_vector


df_total <- data.frame(lags  = c('lag_1', 'lag_2', 'lag_3', 'lag_4', 'lag_5', 'lag_6',
                                 'lag_7', 'lag_8'))
df_total
for (k in 1:10) {
  df <- tracing[k, -1]
  diff <- c()
  for (i in 2:(ncol(tracing)-1)) {
    # print(i)
    j <- (as.numeric(df[, c(i-1, i)][, 2]) - as.numeric(df[, c(i-1, i)][, 1]))
    # print(j)
    diff <- append(diff, j)
    df_tmp <- as.data.frame(diff)
  }
  df_total <- cbind(df_total, df_tmp)
}

lag_nm_vector <- df_total[, 1]; lag_nm_vector

df_total[, -1]
t(df_total[, -1])
df_fin <- as.data.frame(t(df_total[, -1]))
rownames(df_fin) <- custid_vector
colnames(df_fin) <- lag_nm_vector

df_fin

# 고객별 등급이 증가한 횟수
df_fin %>%
  apply(.,1,function(x) sum(x > 0))

# 고객별 등급이 감소한 횟수
df_fin %>%
  apply(.,1,function(x) sum(x < 0))

## 100명의 고객에 대해 등급이 올라가고 내려가 횟수 구하기----

tracing[1:100, ] %>%
  select(custid) %>% .$custid -> custid_vector


df_total <- data.frame(lags  = c('lag_1', 'lag_2', 'lag_3', 'lag_4', 'lag_5', 'lag_6',
                                 'lag_7', 'lag_8'))
df_total
for (k in 1:100) {
  df <- tracing[k, -1]
  diff <- c()
  for (i in 2:(ncol(tracing)-1)) {
    # print(i)
    j <- (as.numeric(df[, c(i-1, i)][, 2]) - as.numeric(df[, c(i-1, i)][, 1]))
    # print(j)
    diff <- append(diff, j)
    df_tmp <- as.data.frame(diff)
  }
  df_total <- cbind(df_total, df_tmp)
}

lag_nm_vector <- df_total[, 1]; lag_nm_vector

df_total[, -1]
t(df_total[, -1])
df_fin <- as.data.frame(t(df_total[, -1]))
rownames(df_fin) <- custid_vector
colnames(df_fin) <- lag_nm_vector

df_fin

# 고객별 등급이 증가한 횟수
df_fin %>%
  apply(.,1,function(x) sum(x > 0)) -> df_fin_plus

str(df_fin_plus)
data.frame(plus_count = df_fin_plus) -> df_plus
df_plus %>% head

# 고객별 등급이 감소한 횟수
df_fin %>%
  apply(.,1,function(x) sum(x < 0)) -> df_fin_minus
data.frame(minus_count = df_fin_minus) -> df_minus
df_minus %>% head
df_minus

df_tmp <- cbind(df_plus, df_minus)
df_tmp
str(df_tmp)
df_tmp$plus_count <- as.numeric(df_tmp$plus_count) 
df_tmp$minus_count <- as.numeric(df_tmp$minus_count)

ggplot(df_tmp, aes(plus_count, minus_count)) + geom_point()

## 전체 고객에 대해 등급이 올라간 횟수 구하기----
dim(tracing)
tracing[1:540013, ] %>%
  select(custid) %>% .$custid -> custid_vector

length(custid_vector)
custid_vector[1:10]

df_total <- data.frame(lags  = c('lag_1', 'lag_2', 'lag_3', 'lag_4', 'lag_5', 'lag_6',
                                 'lag_7', 'lag_8'))

# 시간 소요 다대 주의----
# for 문의 loop가 1344345번 돌아야 함
df_total
for (k in 1:nrow(tracing)) {
  df <- tracing[k, -1]
  diff <- c()
  for (i in 2:(ncol(tracing)-1)) {
    # print(i)
    j <- (as.numeric(df[, c(i-1, i)][, 2]) - as.numeric(df[, c(i-1, i)][, 1]))
    # print(j)
    diff <- append(diff, j)
    df_tmp <- as.data.frame(diff)
  }
  df_total <- cbind(df_total, df_tmp)
  print(k)
}

write.csv(df_total, './data/cust_variance_by_lags.csv') # 540013 rows
save(df_total, file = 'cust_variance_by_lags.RData')
load('cust_variance_by_lags.RData')
head(df_total)
fix(df_total)
dim(df_total)
df_total[1:8, 1:10]


lag_nm_vector <- df_total[, 1]; lag_nm_vector

df_total[, -1]
t(df_total[, -1])
df_fin <- as.data.frame(t(df_total[, -1]))

rownames(df_fin) <- custid_vector
colnames(df_fin) <- lag_nm_vector

head(df_fin)
tail(df_fin)
dim(df_fin)

write.csv(df_fin, './data/cust540013_variance_by_lags.csv')


# 시간 소요 다대 주의----
# for 문의 loop가 1344345번 돌아야 함

dim(tracing)
nrow(tracing)
tail(tracing)
tracing[951206:nrow(tracing), ] %>%
  select(custid) %>% .$custid -> custid_vector

length(custid_vector)
custid_vector[1:10]

df_total <- data.frame(lags  = c('lag_1', 'lag_2', 'lag_3', 'lag_4', 'lag_5', 'lag_6',
                                 'lag_7', 'lag_8')); df_total

head(tracing)

df_total
for (k in 951206:nrow(tracing)) {
  df <- tracing[k, -1]
  diff <- c()
  for (i in 2:(ncol(tracing)-1)) {
    # print(i)
    j <- (as.numeric(df[, c(i-1, i)][, 2]) - as.numeric(df[, c(i-1, i)][, 1]))
    # print(j)
    diff <- append(diff, j)
    df_tmp <- as.data.frame(diff)
  }
  df_total <- cbind(df_total, df_tmp)
  print(k)
}

dim(df_total)
save(df_total, file = 'cust_variance_by_lags_1.RData')
load('cust_variance_by_lags.RData')
head(df_total)
fix(df_total)
dim(df_total)
df_total[1:8, 1:10]


lag_nm_vector <- df_total[, 1]; lag_nm_vector

df_total[, -1]
t(df_total[, -1])
df_fin <- as.data.frame(t(df_total[, -1]))
length(custid_vector)

rownames(df_fin) <- custid_vector
colnames(df_fin) <- lag_nm_vector

head(df_fin)
tail(df_fin)
dim(df_fin)

write.csv(df_fin, './data/cust540013_951206_variance_by_lags.csv')

# 고객별 등급이 증가한 횟수
df_fin %>%
  apply(.,1,function(x) sum(x > 0)) -> df_fin_plus

str(df_fin_plus)
data.frame(plus_count = df_fin_plus) -> df_plus
df_plus %>% head

# 고객별 등급이 감소한 횟수
df_fin %>%
  apply(.,1,function(x) sum(x < 0)) -> df_fin_minus
data.frame(minus_count = df_fin_minus) -> df_minus
df_minus %>% head
df_minus



