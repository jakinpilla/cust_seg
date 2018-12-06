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
colnames(tracing_raw)
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
head(tracing)

##----
# start_grade 변수 : 고객의 시작 등급 ----

## start_1606----
glimpse(tracing)
tracing %>%
  select(custid : g_1606) %>%
  filter(g_1606 != 0) -> start_1606; head(start_1606)
dim(start_1606)

start_1606 %>%
  select(custid, g_1606) %>%
  rename(start_grade = g_1606) -> start_1606_g; head(start_1606_g)

## start_1607----
tracing %>%
  select(custid : g_1607) %>%
  filter(g_1606 == 0 & g_1607 != 0) -> start_1607; head(start_1607)

start_1607 %>%
  select(custid, g_1607) %>%
  rename(start_grade = g_1607) -> start_1607_g; head(start_1607_g)

## start_1608----
tracing %>%
  select(custid : g_1608) %>%
  filter(g_1606 == 0 & g_1607 == 0 & g_1608 !=0) -> start_1608; head(start_1608)

start_1608 %>%
  select(custid, g_1608) %>%
  rename(start_grade = g_1608) -> start_1608_g; head(start_1608_g)

## start_1609----
tracing %>%
  select(custid : g_1609) %>%
  filter(g_1606 == 0 & g_1607 == 0 & g_1608 == 0,
         g_1609 != 0) -> start_1609; head(start_1609)

start_1609 %>%
  select(custid, g_1609) %>%
  rename(start_grade = g_1609) -> start_1609_g; head(start_1609_g)

## start_1610----
tracing %>%
  select(custid : g_1610) %>%
  filter(g_1606 == 0 & g_1607 == 0 & g_1608 == 0,
         g_1609 == 0 & g_1610 != 0) -> start_1610; head(start_1610)

start_1610 %>%
  select(custid, g_1610) %>%
  rename(start_grade = g_1610) -> start_1610_g; head(start_1610_g)

## start_1611----
tracing %>%
  select(custid : g_1611) %>%
  filter(g_1606 == 0 & g_1607 == 0 & g_1608 == 0,
         g_1609 == 0 & g_1610 == 0 & g_1611 != 0) -> start_1611; head(start_1611)

start_1611 %>%
  select(custid, g_1611) %>%
  rename(start_grade = g_1611) -> start_1611_g; head(start_1611_g)

## start_1612----
tracing %>%
  select(custid : g_1612) %>%
  filter(g_1606 == 0 & g_1607 == 0 & g_1608 == 0,
         g_1609 == 0 & g_1610 == 0 & g_1611 == 0,
         g_1612 != 0) -> start_1612; head(start_1612)

start_1612 %>%
  select(custid, g_1612) %>%
  rename(start_grade = g_1612) -> start_1612_g; head(start_1612_g)

## start_1701----
tracing %>%
  select(custid : g_1701) %>%
  filter(g_1606 == 0 & g_1607 == 0 & g_1608 == 0,
         g_1609 == 0 & g_1610 == 0 & g_1611 == 0,
         g_1612 == 0 & g_1701 != 0) -> start_1701; head(start_1701)

start_1701 %>%
  select(custid, g_1701) %>%
  rename(start_grade = g_1701) -> start_1701_g; head(start_1701_g)

## start_1702----
tracing %>%
  select(custid : g_1702) %>%
  filter(g_1606 == 0 & g_1607 == 0 & g_1608 == 0,
         g_1609 == 0 & g_1610 == 0 & g_1611 == 0,
         g_1612 == 0 & g_1701 == 0 & g_1702 != 0) -> start_1702; head(start_1702)

start_1702 %>%
  select(custid, g_1702) %>%
  rename(start_grade = g_1702) -> start_1702_g; head(start_1702_g)

## start_1703----
tracing %>%
  select(custid : g_1703) %>%
  filter(g_1606 == 0 & g_1607 == 0 & g_1608 == 0,
         g_1609 == 0 & g_1610 == 0 & g_1611 == 0,
         g_1612 == 0 & g_1701 == 0 & g_1702 == 0,
         g_1703 != 0) -> start_1703; head(start_1703)

start_1703 %>%
  select(custid, g_1703) %>%
  rename(start_grade = g_1703) -> start_1703_g; head(start_1703_g)

## start_1704----
tracing %>%
  select(custid : g_1704) %>%
  filter(g_1606 == 0 & g_1607 == 0 & g_1608 == 0,
         g_1609 == 0 & g_1610 == 0 & g_1611 == 0,
         g_1612 == 0 & g_1701 == 0 & g_1702 == 0,
         g_1703 == 0 & g_1704 != 0) -> start_1704; head(start_1704)

start_1704 %>%
  select(custid, g_1704) %>%
  rename(start_grade = g_1704) -> start_1704_g; head(start_1704_g)

## start_1705----
tracing %>%
  select(custid : g_1705) %>%
  filter(g_1606 == 0 & g_1607 == 0 & g_1608 == 0,
         g_1609 == 0 & g_1610 == 0 & g_1611 == 0,
         g_1612 == 0 & g_1701 == 0 & g_1702 == 0,
         g_1703 == 0 & g_1704 == 0 & g_1705 != 0) -> start_1705; head(start_1705)

start_1705 %>%
  select(custid, g_1705) %>%
  rename(start_grade = g_1705) -> start_1705_g; head(start_1705_g)

## start_1706----
tracing %>%
  select(custid : g_1706) %>%
  filter(g_1606 == 0 & g_1607 == 0 & g_1608 == 0,
         g_1609 == 0 & g_1610 == 0 & g_1611 == 0,
         g_1612 == 0 & g_1701 == 0 & g_1702 == 0,
         g_1703 == 0 & g_1704 == 0 & g_1705 == 0,
         g_1706 != 0) -> start_1706; head(start_1706)

start_1706 %>%
  select(custid, g_1706) %>%
  rename(start_grade = g_1706) -> start_1706_g; head(start_1706_g)

## start_1707----
tracing %>%
  select(custid : g_1707) %>%
  filter(g_1606 == 0 & g_1607 == 0 & g_1608 == 0,
         g_1609 == 0 & g_1610 == 0 & g_1611 == 0,
         g_1612 == 0 & g_1701 == 0 & g_1702 == 0,
         g_1703 == 0 & g_1704 == 0 & g_1705 == 0,
         g_1706 == 0 & g_1707 != 0) -> start_1707; head(start_1707)

start_1707 %>%
  select(custid, g_1707) %>%
  rename(start_grade = g_1707) -> start_1707_g; head(start_1707_g)

## start_1708----
tracing %>%
  select(custid : g_1708) %>%
  filter(g_1606 == 0 & g_1607 == 0 & g_1608 == 0,
         g_1609 == 0 & g_1610 == 0 & g_1611 == 0,
         g_1612 == 0 & g_1701 == 0 & g_1702 == 0,
         g_1703 == 0 & g_1704 == 0 & g_1705 == 0,
         g_1706 == 0 & g_1707 == 0 & g_1708 != 0) -> start_1708; head(start_1708)

start_1708 %>%
  select(custid, g_1708) %>%
  rename(start_grade = g_1708) -> start_1708_g; head(start_1708_g)

## start_1709----
tracing %>%
  select(custid : g_1709) %>%
  filter(g_1606 == 0 & g_1607 == 0 & g_1608 == 0,
         g_1609 == 0 & g_1610 == 0 & g_1611 == 0,
         g_1612 == 0 & g_1701 == 0 & g_1702 == 0,
         g_1703 == 0 & g_1704 == 0 & g_1705 == 0,
         g_1706 == 0 & g_1707 == 0 & g_1708 == 0,
         g_1709 != 0) -> start_1709; head(start_1709)

start_1709 %>%
  select(custid, g_1709) %>%
  rename(start_grade = g_1709) -> start_1709_g; head(start_1709_g)

## start_1710----
tracing %>%
  select(custid : g_1710) %>%
  filter(g_1606 == 0 & g_1607 == 0 & g_1608 == 0,
         g_1609 == 0 & g_1610 == 0 & g_1611 == 0,
         g_1612 == 0 & g_1701 == 0 & g_1702 == 0,
         g_1703 == 0 & g_1704 == 0 & g_1705 == 0,
         g_1706 == 0 & g_1707 == 0 & g_1708 == 0,
         g_1709 == 0 & g_1710 != 0) -> start_1710; head(start_1710)

start_1710 %>%
  select(custid, g_1710) %>%
  rename(start_grade = g_1710) -> start_1710_g; head(start_1710_g)

## start_1711----
tracing %>%
  select(custid : g_1711) %>%
  filter(g_1606 == 0 & g_1607 == 0 & g_1608 == 0,
         g_1609 == 0 & g_1610 == 0 & g_1611 == 0,
         g_1612 == 0 & g_1701 == 0 & g_1702 == 0,
         g_1703 == 0 & g_1704 == 0 & g_1705 == 0,
         g_1706 == 0 & g_1707 == 0 & g_1708 == 0,
         g_1709 == 0 & g_1710 == 0 & g_1711 != 0) -> start_1711; head(start_1711)

start_1711 %>%
  select(custid, g_1711) %>%
  rename(start_grade = g_1711) -> start_1711_g; head(start_1711_g)

## start_1712----
tracing %>%
  select(custid : g_1712) %>%
  filter(g_1606 == 0 & g_1607 == 0 & g_1608 == 0,
         g_1609 == 0 & g_1610 == 0 & g_1611 == 0,
         g_1612 == 0 & g_1701 == 0 & g_1702 == 0,
         g_1703 == 0 & g_1704 == 0 & g_1705 == 0,
         g_1706 == 0 & g_1707 == 0 & g_1708 == 0,
         g_1709 == 0 & g_1710 == 0 & g_1711 == 0,
         g_1712 != 0) -> start_1712; head(start_1712)

start_1712 %>%
  select(custid, g_1712) %>%
  rename(start_grade = g_1712) -> start_1712_g; head(start_1712_g)

## start_1801----
tracing %>%
  select(custid : g_1801) %>%
  filter(g_1606 == 0 & g_1607 == 0 & g_1608 == 0,
         g_1609 == 0 & g_1610 == 0 & g_1611 == 0,
         g_1612 == 0 & g_1701 == 0 & g_1702 == 0,
         g_1703 == 0 & g_1704 == 0 & g_1705 == 0,
         g_1706 == 0 & g_1707 == 0 & g_1708 == 0,
         g_1709 == 0 & g_1710 == 0 & g_1711 == 0,
         g_1712 == 0 & g_1801 != 0) -> start_1801; head(start_1801)

start_1801 %>%
  select(custid, g_1801) %>%
  rename(start_grade = g_1801) -> start_1801_g; head(start_1801_g)

## start_1802----
tracing %>%
  select(custid : g_1802) %>%
  filter(g_1606 == 0 & g_1607 == 0 & g_1608 == 0,
         g_1609 == 0 & g_1610 == 0 & g_1611 == 0,
         g_1612 == 0 & g_1701 == 0 & g_1702 == 0,
         g_1703 == 0 & g_1704 == 0 & g_1705 == 0,
         g_1706 == 0 & g_1707 == 0 & g_1708 == 0,
         g_1709 == 0 & g_1710 == 0 & g_1711 == 0,
         g_1712 == 0 & g_1801 == 0 & g_1802 != 0) -> start_1802; head(start_1802)

start_1802 %>%
  select(custid, g_1802) %>%
  rename(start_grade = g_1802) -> start_1802_g; head(start_1802_g)

## start_1803----
tracing %>%
  select(custid : g_1803) %>%
  filter(g_1606 == 0 & g_1607 == 0 & g_1608 == 0,
         g_1609 == 0 & g_1610 == 0 & g_1611 == 0,
         g_1612 == 0 & g_1701 == 0 & g_1702 == 0,
         g_1703 == 0 & g_1704 == 0 & g_1705 == 0,
         g_1706 == 0 & g_1707 == 0 & g_1708 == 0,
         g_1709 == 0 & g_1710 == 0 & g_1711 == 0,
         g_1712 == 0 & g_1801 == 0 & g_1802 == 0,
         g_1803 != 0) -> start_1803; head(start_1803)

start_1803 %>%
  select(custid, g_1803) %>%
  rename(start_grade = g_1803) -> start_1803_g; head(start_1803_g)

## start_1804----
tracing %>%
  select(custid : g_1804) %>%
  filter(g_1606 == 0 & g_1607 == 0 & g_1608 == 0,
         g_1609 == 0 & g_1610 == 0 & g_1611 == 0,
         g_1612 == 0 & g_1701 == 0 & g_1702 == 0,
         g_1703 == 0 & g_1704 == 0 & g_1705 == 0,
         g_1706 == 0 & g_1707 == 0 & g_1708 == 0,
         g_1709 == 0 & g_1710 == 0 & g_1711 == 0,
         g_1712 == 0 & g_1801 == 0 & g_1802 == 0,
         g_1803 == 0 & g_1804 != 0) -> start_1804; head(start_1804)

start_1804 %>%
  select(custid, g_1804) %>%
  rename(start_grade = g_1804) -> start_1804_g; head(start_1804_g)

## start_1805----
tracing %>%
  select(custid : g_1805) %>%
  filter(g_1606 == 0 & g_1607 == 0 & g_1608 == 0,
         g_1609 == 0 & g_1610 == 0 & g_1611 == 0,
         g_1612 == 0 & g_1701 == 0 & g_1702 == 0,
         g_1703 == 0 & g_1704 == 0 & g_1705 == 0,
         g_1706 == 0 & g_1707 == 0 & g_1708 == 0,
         g_1709 == 0 & g_1710 == 0 & g_1711 == 0,
         g_1712 == 0 & g_1801 == 0 & g_1802 == 0,
         g_1803 == 0 & g_1804 == 0 & g_1805 != 0) -> start_1805; head(start_1805)

start_1805 %>%
  select(custid, g_1805) %>%
  rename(start_grade = g_1805) -> start_1805_g; head(start_1805_g)

## start_1806
tracing %>%
  select(custid : g_1806) %>%
  filter(g_1606 == 0 & g_1607 == 0 & g_1608 == 0,
         g_1609 == 0 & g_1610 == 0 & g_1611 == 0,
         g_1612 == 0 & g_1701 == 0 & g_1702 == 0,
         g_1703 == 0 & g_1704 == 0 & g_1705 == 0,
         g_1706 == 0 & g_1707 == 0 & g_1708 == 0,
         g_1709 == 0 & g_1710 == 0 & g_1711 == 0,
         g_1712 == 0 & g_1801 == 0 & g_1802 == 0,
         g_1803 == 0 & g_1804 == 0 & g_1805 == 0,
         g_1806 != 0) -> start_1806; head(start_1806)

start_1806 %>%
  select(custid, g_1806) %>%
  rename(start_grade = g_1806) -> start_1806_g; head(start_1806_g)

# joining start_XXXX 
# 결과 합쳐보기----
start_1606_g %>% head
start_1607_g %>% head
rbind(start_1606_g, start_1607_g, start_1608_g, start_1609_g, start_1610_g,
      start_1611_g, start_1612_g, start_1701_g, start_1702_g, start_1703_g,
      start_1704_g, start_1705_g, start_1706_g, start_1707_g, start_1708_g,
      start_1709_g, start_1710_g, start_1711_g, start_1712_g, start_1801_g,
      start_1802_g, start_1803_g, start_1804_g, start_1805_g, start_1806_g) %>% 
  arrange(custid) -> start_df ; head(start_df)

dim(start_df)

tracing %>%
  left_join(start_df, by='custid') -> tracing_joined

head(tracing_joined)

# 고객의 마지막 등급(현재 등급) ----
tracing_joined %>%
  mutate(end_grade = g_1806) -> tracing ; head(tracing)

# 고객 등급 변화의 표준편차 ----
tmp <- c(1, 2, 3, 4, 5)
sd(tmp)

tracing %>%
  select(g_1606 : g_1806) %>%
  apply(., 1, function(x) sd(x)) -> grade_sd_vector

tracing$grade_sd <- grade_sd_vector
head(tracing)

# E 등급의 횟수----
tracing %>% 
  select(g_1606 : g_1806) %>%
  mutate(e_cnt = rowSums(.==1)) -> tmp; head(tmp) 

e_cnt_vector <- tmp$e_cnt

tracing$e_cnt <- e_cnt_vector
head(tracing)
range(tracing$e_cnt)

tracing %>%
  filter(e_cnt == 18) %>%
  select(g_1606 : g_1806) %>%
  filter(g_1806 != 1)

colnames(tracing)

load('userRFM.RData')

head(tracing)
tracing$freq <- userRFM$frequency
tracing$F_score <- userRFM$F
colnames(tracing)

# 표본추출(그래프를 그리기 위해)
n <- nrow(tracing)
idx <- 1:n
sample_idx <- sample(idx, n * .001)
sample_n <- length(sample_idx)
sample_n # 1344

tracing_sample <- tracing[sample_idx,]
nrow(tracing_sample)
head(tracing_sample)

tracing_sample$custid -> sample_custid; 
sample_custid[1:10]

tracing_sample[, -1] %>% class
t(tracing_sample[, -1])[1:10, 1:10]

tracing_sample_T <- t(tracing_sample[, -1])
dim(tracing_sample_T)
as.data.frame(tracing_sample_T)[1:25, 1:10]


tracing_T <- as.data.frame(tracing_sample_T)[1:25, 1:sample_n]
colnames(tracing_T) <- sample_custid


plot_sample_df <- tracing_T[1:25, 1:1000]; 
glimpse(plot_sample_df)

# 5명에 대한 점수변화 시각화----

df <- plot_sample_df[, 1:5]
df <- cbind(months = rownames(df), df); head(df)
rownames(df) <- NULL
df$months <- as.character(df$months)
str(df$months)
dim(df)

library(reshape2)
dfm = melt(df, id.vars='months'); dfm

ggplot(dfm, aes(x=months, y=value, colour=variable)) + 
  geom_line(aes(color = variable, group = variable, linetype = variable), size=1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 100명에 대한 점수변화 시각화----
df <- plot_sample_df[, 1:100]
df <- cbind(months = rownames(df), df)
rownames(df) <- NULL
df$months <- as.character(df$months)
str(df$months)
dim(df)

library(reshape2)
dfm = melt(df, id.vars='months');

ggplot(dfm, aes(x=months, y=value, group=variable)) +
  theme_bw() +
  theme(panel.grid=element_blank()) +
  geom_line(size=0.2, alpha=0.1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# 특정 고객 한 명을 지정하여 등급변화 시각화하기----

head(tracing)
as.character(tracing$custid) -> tracing$custid

tracing %>%
  select(custid, g_1606 : g_1806) %>%
  filter(custid == 'C00000004') -> df_a_cust; df_a_cust

df_a_cust$custid -> a_custid; a_custid

df_a_cust_T <- t(df_a_cust[, -1]); df_a_cust_T
tracing_T <- as.data.frame(df_a_cust_T); tracing_T
colnames(tracing_T) <- a_custid ; tracing_T
df <- tracing_T
df <- data.frame(months = rownames(df), C00000004 = df$C00000004); df
rownames(df) <- NULL
head(df)

library(reshape2)
dfm = melt(df, id.vars='months')

ggplot(dfm, aes(x=months, y=as.numeric(value), colour=variable)) + 
  geom_line(aes(color = variable, group = variable, linetype = variable), size=1) + 
  ylim(0, 5) +
  ylab('grades') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# K-평균 군집 해보자---
# save(tracing, file = './data/tracing.RData')
# load('./data/tracing.RData')
head(tracing)

tracing %>% 
  select(custid, upcnt, downcnt, g_mean, start_grade, end_grade, e_cnt, freq) -> cluster_data

head(cluster_data)
glimpse(cluster_data)
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

# n=10으로 선택해보자
cust_kmeans <- kmeans(scaled_cluster_data, 10)
tracing$cluster <- cust_kmeans$cluster
head(tracing) %>% as.data.frame()

# cluster 1번 군집의 패턴을 이해해 보기 위해 10명을 추출해 등급변화를 시각화해보자
n_sample = 10

tracing %>%
  filter(cluster == 1) %>%
  sample_n(10) -> cluster1.sample10

tracing_sample <- cluster1.sample10; tracing_sample
sample_custid <- tracing_sample$custid 

tracing_sample_T <- t(tracing_sample[, -1])
head(tracing_sample_T)
dim(tracing_sample_T)
as.data.frame(tracing_sample_T)[1:25, 1:n_sample]

tracing_T <- as.data.frame(tracing_sample_T)[1:25, 1:n_sample]
colnames(tracing_T) <- sample_custid
tracing_T
df <- tracing_T

df <- cbind(months = rownames(df), df); head(df)
rownames(df) <- NULL
df$months <- as.character(df$months)
str(df$months)
dim(df)

library(reshape2)
dfm = melt(df, id.vars='months')

ggplot(dfm, aes(x=months, y=value, colour=variable)) + 
  geom_line(aes(color = variable, group = variable, linetype = variable), size=1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# cluster 2번 군집의 패턴을 이해해 보기 위해 10명을 추출해 등급변화를 시각화해보자
n_sample = 10

tracing %>%
  filter(cluster == 2) %>%
  sample_n(10) -> cluster2.sample10

tracing_sample <- cluster2.sample10; tracing_sample
sample_custid <- tracing_sample$custid 

tracing_sample_T <- t(tracing_sample[, -1])
head(tracing_sample_T)
dim(tracing_sample_T)
as.data.frame(tracing_sample_T)[1:25, 1:n_sample]

tracing_T <- as.data.frame(tracing_sample_T)[1:25, 1:n_sample]
colnames(tracing_T) <- sample_custid
tracing_T
df <- tracing_T

df <- cbind(months = rownames(df), df); head(df)
rownames(df) <- NULL
df$months <- as.character(df$months)
str(df$months)
dim(df)

library(reshape2)
dfm = melt(df, id.vars='months')

ggplot(dfm, aes(x=months, y=value, colour=variable)) + 
  geom_line(aes(color = variable, group = variable, linetype = variable), size=1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# cluster 3번 군집의 패턴을 이해해 보기 위해 10명을 추출해 등급변화를 시각화해보자
n_sample = 10

tracing %>%
  filter(cluster == 3) %>%
  sample_n(10) -> cluster3.sample10

tracing_sample <- cluster3.sample10; tracing_sample
sample_custid <- tracing_sample$custid 

tracing_sample_T <- t(tracing_sample[, -1])
head(tracing_sample_T)
dim(tracing_sample_T)
as.data.frame(tracing_sample_T)[1:25, 1:n_sample]

tracing_T <- as.data.frame(tracing_sample_T)[1:25, 1:n_sample]
colnames(tracing_T) <- sample_custid
tracing_T
df <- tracing_T

df <- cbind(months = rownames(df), df); head(df)
rownames(df) <- NULL
df$months <- as.character(df$months)
str(df$months)
dim(df)

library(reshape2)
dfm = melt(df, id.vars='months')

ggplot(dfm, aes(x=months, y=value, colour=variable)) + 
  geom_line(aes(color = variable, group = variable, linetype = variable), size=1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

