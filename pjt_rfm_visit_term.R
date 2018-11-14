setwd("C:/Users/Daniel/rfm")
getwd()

rm(list=ls()); gc()

library(plyr)
library(tidyverse)
library(data.table)
library(lubridate)

load("cust_prod_total_fin.RData")
glimpse(cust_prod_total_fin); 

#### 고객별 월별 amt 합 + 정렬
cust_prod_sum_amt <- cust_prod_total_fin %>% 
  mutate(diffdate = as.numeric(date)) %>% # diffdate는 날짜 연산을 위해 일자를 숫자로 변형한 변수
  group_by(custid, year_month, date, diffdate) %>%
  summarise(sum_amt = sum(amt)) %>%
  arrange(custid, date)

glimpse(cust_prod_sum_amt)
# remove(cust_prod_sum_amt)

#### 각 고객의 구매일간 주기를 계산(첫번째 구매일과 두 번째 구매일간의 차이)
## https://rpubs.com/LEESUJAE/373101
## NA는 두어야 한다.

cust_prod_sum_amt$date_lag <- shift(cust_prod_sum_amt$date, n=1, fill=NA, type = "lag")
cust_prod_sum_amt$date[1:5]
cust_prod_sum_amt$date_lag[1:5]
head(cust_prod_sum_amt[, c('custid', 'date', 'date_lag')], 10)

# # A tibble: 10 x 3
# custid    date                date_lag           
# <chr>     <dttm>              <dttm>             
#   1 C00000001 2016-05-23 00:00:00 NA                 
#   2 C00000001 2018-05-15 00:00:00 2016-05-23 00:00:00
#   3 C00000002 2016-02-27 00:00:00 2018-05-15 00:00:00
#   4 C00000002 2017-03-17 00:00:00 2016-02-27 00:00:00
#   5 C00000003 2018-06-30 00:00:00 2017-03-17 00:00:00
#   6 C00000004 2016-10-08 00:00:00 2018-06-30 00:00:00
#   7 C00000005 2016-12-04 00:00:00 2016-10-08 00:00:00
#   8 C00000006 2018-06-07 00:00:00 2016-12-04 00:00:00
#   9 C00000007 2017-06-15 00:00:00 2018-06-07 00:00:00
#  10 C00000008 2016-11-05 00:00:00 2017-06-15 00:00:00

# lag의 의미 : n=1로 두었다는 것은 두 번째 이벤트의 데이터를 가져온다는 뜻이다.
# 만약 출력된 값이 NA라면 두 번째 이벤트 데이터가 없다는 뜻이다. (즉 단 한번만 구매)

cust_prod_sum_amt$date_diff <- cust_prod_sum_amt$date - cust_prod_sum_amt$date_lag
cust_prod_sum_amt$date_diff[1:5]
# 차이를 초 단위로 계산한다.
# Time differences in secs
# [1]        NA  62380800 -69811200  33177600  40608000

head(cust_prod_sum_amt, 10)
shift(cust_prod_sum_amt$custid, n=1, fill = NA, type = "lag")
cust_prod_sum_amt$custid == shift(cust_prod_sum_amt$custid, n=1, fill = NA, type = "lag")

# date_final의 의미 : 만약 고객이 3번 구매를 하였다면 date_final은 2개의 값을 가진다.
# 첫 번째 값은 첫 번째 구매한 구매 월-일과 두 번째 구매한 월-일 간의 차이를 의미한다.
# 두 번째 값은 두 번째 구매한 구매 월-일과 세 번째 구매한 월-일 간의 차이를 의미한다.
# 데이터가 월별로 sum되어 있는 것의 의미를 잘 생각해야 한다.

cust_prod_sum_amt$date_final <- 
  ifelse(cust_prod_sum_amt$custid == shift(cust_prod_sum_amt$custid, n=1, fill = NA, type = "lag"),
         cust_prod_sum_amt$date - shift(cust_prod_sum_amt$date, n=1, fill=NA, type = "lag"), NA)


head(cust_prod_sum_amt, 30) %>% View
 
### RData로 저장하기
# save(cust_prod_sum_amt, file="cust_prod_sum_amt.RData")

# remove(cust_prod_visit_mean)

### 평균 방문일 수 구하기 + 추가 자료 구하기
head(cust_prod_sum_amt, 10)

cust_prod_visit_mean <- cust_prod_sum_amt %>%
  group_by(custid) %>%
  summarise(date_final_mean = (mean(date_final, na.rm = TRUE)) / 60 / 60 / 24,
            # date_final_mean 변수는 고객의 평균 벙문 기간을 의미
            # 만약 date_final_mean의 값이 365일이라면, 평균적으로 1년에 한 번 방문하는 고객
            frequency = n_distinct(date),
            monetary = sum(sum_amt),
            recency=max(date)
            )  %>%
 replace_na(list(date_final_mean = 0))

glimpse(cust_prod_visit_mean)  
head(cust_prod_visit_mean,10)


### 여기까지 구하면 고객별 평균 방문일 수 및 기존 userRFM에 있던 자료도 같은 형태로 나옴.

# C00000009 고객에 대해 두 데이터 cust_prod_sum_amt, cust_prod_visit_mean, userRFM 이 서로---- 
# 일치하는지 여부를 확인
cust_prod_sum_amt %>%
  filter(custid == "C00000009") %>% as.data.frame()

cust_prod_visit_mean %>%
  filter(custid == "C00000009") %>% as.data.frame()

load("userRFM.RData")
head(userRFM) %>% as.data.frame

userRFM %>%
  filter(custid ==  "C00000009") %>% as.data.frame

# custid minRecency    recency monetary frequency period    minDate    maxDate R F M
# 1 C00000009 2016-01-03 2016-11-20   279000         6    322 1451746800 1479567600 2 5 5
# score grade
# 1 83.04633     A


## cust_prod_visit_mean 데이터를 RData로 저장하기----
save(cust_prod_visit_mean, file="cust_prod_visit_mean.RData")


## 기간을 구하고 값을 필터링 해서 리턴하기. 월별로 직전 6개월의 값에서 등급 구하기----
library(lubridate)

# as.numeric(floor_date(cust_prod_sum_amt$diffdate[1], "month"))
# 
# getDataBetweenMonth <- function(month_date) {
#   current_month <- floor_date(month_date, "month")
#   prev_month <- as.numeric(current_month %m-% months(6))
#   current_month <- as.numeric(current_month)
#   return (list("current_month" = current_month, "prev_month" = prev_month))
# }
# results <- betweenwithmonth(cust_prod_sum_amt$date[1])
# results$current_month
# results$prev_month


# # 구하고자 하는 범위를 betweenwithmonth로 구하고 그 범위에 맞는 데이터만 다시 수집
# system.time({cust_prod_sum_amt %>% 
#   filter(diffdate >= results$prev_month && diffdate < results$current_month)})

# getDataBetweenMonth <- function(custom_data_frame, month_date) {
#   # 원복 데이터에서 현재 월을 입력하면 현재 월을 기준으로 앞의 6개월의 데이터를 필터링 후 리턴
#   #
#   # Args:
#   # 
#   # Returns:
#   #   tibble 형태의 데이터
#   month_date <- as.POSIXct(month_date, origin="1970-01-01")
#   print(month_date)
#   current_month <- floor_date(month_date, "month")
#   print(current_month)
#   prev_month <- as.numeric(current_month %m-% months(6))
#   current_month <- as.numeric(current_month)
#   
#   return (
#     custom_data_frame %>% 
#       filter(diffdate >= prev_month && diffdate < current_month) %>%
#       group_by(custid) %>%
#       summarise( 
#             frequency = n_distinct(date),
#             frequency_month = n_distinct(year_month),
#             monetary = sum(sum_amt),
#             recency=max(date)
#             )
#   )
# }
# 
# # 6개월 값 구해보기
# # month_date를 '2018-01-10'로 입력하면
# # current_month 는 '2018-01-01'로 된다
# # prev_month 는 '2017-07-01'로 된다.
# 
# month_date <- as.POSIXct("2018-01-10", origin="1970-01-01"); month_date
# current_month <- floor_date(month_date, "month"); current_month # "2018-01-01 KST"
# current_month %m-% months(6)
# prev_month <- as.numeric(current_month %m-% months(6)); prev_month
# current_month <- as.numeric(current_month); current_month # "2017-07-01 KST"
# 
# system.time({results_201707_201712 <- getDataBetweenMonth(cust_prod_sum_amt, "2018-01-10")})
# glimpse(results_201707_201712)
# head(results_201707_201712, 10)

# 함수변형 : frequency_month = n_distinct(year_month) 부분 생략----

# month_date = '2018-01-10'
# month_date <- as.POSIXct(month_date, origin="1970-01-01"); month_date
# current_month <- floor_date(month_date, "month"); print(current_month)
# prev_month <- current_month %m-% months(6); prev_month

getDataBetweenMonth <- function(custom_data_frame, month_date) {
  # 원복 데이터에서 특정 일자를 입력하면 특정일자의 월을 기준으로 앞의 6개월의 데이터를 추출하여
  # 고객별 frequency, monetary, recency를 반환하는 함수입니다.
  
  month_date <- as.POSIXct(month_date, origin="1970-01-01")
  
  # 현재 월을 설정
  current_month <- floor_date(month_date, "month")
  
  # 앞의 6개월의 월을 설정
  prev_month <- current_month %m-% months(6)
  
  # 추출되어지는 기간을 표시하기
  cat("start_date : \n")
  print(prev_month)
  cat("end_date :", "\n")
  print(current_month)
  
  # 계산을 위해 날짜형 데이터를 숫자형으로 변환
  prev_month_numeric <- as.numeric(current_month %m-% months(6))
  current_month_numeric <- as.numeric(current_month)
  
  return (
    custom_data_frame %>% 
      filter(diffdate >= prev_month_numeric && diffdate < current_month_numeric) %>%
      group_by(custid) %>%
      summarise( 
        frequency = n_distinct(date),
        # frequency_month = n_distinct(year_month),
        monetary = sum(sum_amt),
        recency=max(date)
      )
  )
}

results_201707_201712 <- getDataBetweenMonth(cust_prod_sum_amt, "2018-01-10")
system.time({results_201707_201712 <- getDataBetweenMonth(cust_prod_sum_amt, "2018-01-10")})
# start_date : 
#   [1] "2017-07-01 KST"
# end_date : 
#   [1] "2018-01-01 KST"
# 사용자  시스템 elapsed 
# 74.78    0.06   75.25 


# results_201707_201712 데이터는 2017-07-01 부터 2017-12-31 기간으로 구해진 데이터이다.
range(results_201707_201712$recency) # "2017-07-01 KST" "2017-12-31 KST"

# "C00000021" 고객을 기준으로 구해진 results_201707_201712 데이터가 다른 데이터들과의 정합성을 
# 확인해 보기
head(results_201707_201712)

results_201707_201712 %>%
  filter(custid == "C00000021") %>% as.data.frame()

#      custid   requency   monetary      recency
# 1 C00000021         1      22000    2017-08-19

cust_prod_sum_amt %>%
  filter(custid == "C00000021") %>% as.data.frame()

# custid year_month       date   diffdate sum_amt   date_lag      date_diff
# 1 C00000021    2016_08 2016-08-19 1471532400   22000 2018-03-13 -49334400 secs
# 2 C00000021    2017_08 2017-08-19 1503068400   22000 2016-08-19  31536000 secs
# date_final
# 1         NA
# 2   31536000

cust_prod_visit_mean %>%
  filter(custid == "C00000021") %>% as.data.frame()

#      custid   date_final_mean frequency monetary      recency
# 1 C00000021             365         2    44000     2017-08-19

userRFM %>%
  filter(custid == "C00000021") %>% as.data.frame()

#   custid    minRecency    recency monetary frequency period    minDate    maxDate R F M
# 1 C00000021 2016-08-19 2017-08-19    44000         2    365 1471532400 1503068400 4 2 3
# score grade
# 1 60.87432     B

# 정합성 최종 확인 : 이상없음

# results_201707_201712 데이터에서 해당 기간에 해당하는 RFM 점수를 구하기
glimpse(results_201707_201712)

results_201707_201712$maxDate <- as.numeric(as.POSIXct(results_201707_201712$recency, origin="1970-01-01"))
range(results_201707_201712$maxDate) # [1] 1498834800 1514646000
quantR <- as.numeric(quantile(results_201707_201712$maxDate,c(0,0.2,0.4,0.6,0.8,1))); quantR
quantF <- quantile(results_201707_201712$frequency, c(0,0.8,0.85,0.9,0.95,1)); quantF
quantM <- quantile(userRFM$monetary, c(0,0.2,0.4,0.6,0.8,1)); quantM

sumR <- sum(results_201707_201712$monetary[results_201707_201712$maxDate >= quantR[5]]); sumR
sumF <- sum(results_201707_201712$monetary[results_201707_201712$frequency >= quantF[5]]); sumF
sumM <- sum(results_201707_201712$monetary[results_201707_201712$monetary >= quantM[5]]); sumM

weightR <- sumR/(sumR + sumF + sumM); weightR
weightF <- sumF/(sumR + sumF + sumM); weightF
weightM <- sumM/(sumR + sumF + sumM); weightM

intervalGrade <- function(mainData, fileName, rfmName, quantileData) {
  results <- rep (0, nrow(mainData))
  dataSource <- data.table(eval(parse(text=paste0(fileName,"$",rfmName))))
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

results_201707_201712$R <- intervalGrade(results_201707_201712, "results_201707_201712", "maxDate", quantR) 
results_201707_201712$F <- intervalGrade(results_201707_201712, "results_201707_201712", "frequency", quantF)
results_201707_201712$M <- intervalGrade(results_201707_201712, "results_201707_201712", "monetary", quantM)

results_201707_201712

# RFM score
results_201707_201712$score <- (weightR * results_201707_201712$R + 
                                  weightF * results_201707_201712$F + 
                                  weightM * results_201707_201712$M)*100/5

quantS <- quantile(results_201707_201712$score, c(0, 0.2, 0.4, 0.6, 0.8, 1)); quantS

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


results_201707_201712$grade <- finalGrade(results_201707_201712, "results_201707_201712", "score", quantS)
results_201707_201712 %>% head(10) %>% as.data.frame
dim(results_201707_201712)








