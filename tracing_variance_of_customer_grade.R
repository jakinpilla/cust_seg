# setwd("C:/Users/Daniel/rfm")
getwd()

rm(list=ls()); gc()

library(plyr)
library(tidyverse)
library(data.table)
library(lubridate)

load("cust_prod_total_fin.RData")
glimpse(cust_prod_total_fin)

#### 고객별 월별 amt 합 + 정렬
cust_prod_sum_amt <- cust_prod_total_fin %>% 
  mutate(diffdate = as.numeric(date)) %>% 
  # diffdate는 날짜 연산을 위해 일자를 숫자로 변형한 변수
  group_by(custid, year_month, date, diffdate) %>%
  summarise(sum_amt = sum(amt)) %>%
  arrange(custid, date)

glimpse(cust_prod_sum_amt)


# 주기별 누적된 데이터를 통해 고객의 등급변화 확인하기

# 1) 201601~201606 데이터 추출, 고객 등급 산출(6개월치)----
getDataBetweenMonth <- function(custom_data_frame, month_date) {
  # 원본 데이터에서 특정 일자를 입력하면 월을 기준으로 전 6개월의 데이터를 추출하여
  # 고객별 frequency, monetary, recency를 반환하는 함수 제작 
  
  month_date <- as.POSIXct(month_date, origin="1970-01-01")

  # 현재 월 설정
  current_month <- floor_date(month_date, "month")
  
  # 앞의 6개월치 월을 설정
  prev_month <- current_month %m-% months(6)
  
  # 추출되는 기간 표시
  cat("start_date : \n")
  print(prev_month)
  cat("end_date :", "\n")
  print(current_month)
  
  # 계산을 위해 날짜형 데이터를 숫자형으로 변환
  prev_month_numeric <- as.numeric(current_month %m-% months(6))
  current_month_numeric <- as.numeric(current_month)
  
  return (
    custom_data_frame %>% 
      filter(diffdate >= prev_month_numeric && 
               diffdate < current_month_numeric) %>%
      group_by(custid) %>%
      summarise( 
        frequency = n_distinct(date),
        # frequency_month = n_distinct(year_month),
        monetary = sum(sum_amt),
        recency = max(date)
      )
  )
}


results_201601_201606 <- getDataBetweenMonth(cust_prod_sum_amt, "2016-07-10")
results_201601_201606
nrow(results_201601_201606)

results_201601_201606$maxDate <- as.numeric(as.POSIXct(results_201601_201606$recency, 
                                                       origin="1970-01-01"))
range(results_201601_201606$maxDate) # [1] 1451574000 1467212400

quantR <- as.numeric(quantile(results_201601_201606$maxDate,c(0,0.2,0.4,0.6,0.8,1))); quantR
quantF <- quantile(results_201601_201606$frequency, c(0,0.8,0.85,0.9,0.95,1)); quantF
quantM <- quantile(results_201601_201606$monetary, c(0,0.2,0.4,0.6,0.8,1)); quantM

sumR <- sum(results_201601_201606$monetary[results_201601_201606$maxDate >= quantR[5]]); sumR
sumF <- sum(results_201601_201606$monetary[results_201601_201606$frequency >= quantF[5]]); sumF
sumM <- sum(results_201601_201606$monetary[results_201601_201606$monetary >= quantM[5]]); sumM

weightR <- sumR/(sumR + sumF + sumM); weightR # 0.2612301
weightF <- sumF/(sumR + sumF + sumM); weightF # 0.2010919
weightM <- sumM/(sumR + sumF + sumM); weightM # 0.537678

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

results_201601_201606$R <- intervalGrade(results_201601_201606, "results_201601_201606", "maxDate", quantR) 
results_201601_201606$F <- intervalGrade(results_201601_201606, "results_201601_201606", "frequency", quantF)
results_201601_201606$M <- intervalGrade(results_201601_201606, "results_201601_201606", "monetary", quantM)

# RFM score
results_201601_201606$score <- (weightR * results_201601_201606$R + 
                                  weightF * results_201601_201606$F + 
                                  weightM * results_201601_201606$M)*100/5

quantS <- quantile(results_201601_201606$score, c(0, 0.2, 0.4, 0.6, 0.8, 1)); quantS

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


results_201601_201606$grade_1601_1606 <- finalGrade(results_201601_201606, "results_201601_201606", "score", quantS)
results_201601_201606 %>% head(10) %>% as.data.frame
nrow(results_201601_201606) # 482089

results_201601_201606 %>% select(custid, grade_1601_1606) -> df_1601_1606; head(df_1601_1606)


# 2) 201601~201609 간 데이터 추출, 고객 등급 산출(9개월)----
getDataBetween9Month <- function(custom_data_frame, month_date) {
  # 특정일자의 월을 기준으로 앞의 9개월치 데이터를 추출하여
  # 고객별 frequency, monetary, recency를 반환하는 함수 제작 
  
  month_date <- as.POSIXct(month_date, origin="1970-01-01")
  current_month <- floor_date(month_date, "month")  # 현재 월을 설정
  prev_month <- current_month %m-% months(9) # 앞의 9개월 설정
  
  # 추출되어지는 기간 표시
  cat("start_date : \n")
  print(prev_month)
  cat("end_date :", "\n")
  print(current_month)
  
  prev_month_numeric <- as.numeric(current_month %m-% months(9))
  current_month_numeric <- as.numeric(current_month)
  
  return (
    custom_data_frame %>% 
      filter(diffdate >= prev_month_numeric && 
               diffdate < current_month_numeric) %>%
      group_by(custid) %>%
      summarise( 
        frequency = n_distinct(date),
        # frequency_month = n_distinct(year_month),
        monetary = sum(sum_amt),
        recency = max(date)
      )
  )
}

results_201601_201609 <- getDataBetween9Month(cust_prod_sum_amt, "2016-10-10"); results_201601_201609
nrow(results_201601_201609) # 637023

results_201601_201609$maxDate <- as.numeric(as.POSIXct(results_201601_201609$recency, origin="1970-01-01"))
range(results_201601_201609$maxDate) # [1] 1451574000 1475161200

quantR <- as.numeric(quantile(results_201601_201609$maxDate,c(0,0.2,0.4,0.6,0.8,1))); quantR
quantF <- quantile(results_201601_201609$frequency, c(0,0.8,0.85,0.9,0.95,1)); quantF
quantM <- quantile(results_201601_201609$monetary, c(0,0.2,0.4,0.6,0.8,1)); quantM

sumR <- sum(results_201601_201609$monetary[results_201601_201609$maxDate >= quantR[5]]); sumR
sumF <- sum(results_201601_201609$monetary[results_201601_201609$frequency >= quantF[5]]); sumF
sumM <- sum(results_201601_201609$monetary[results_201601_201609$monetary >= quantM[5]]); sumM

weightR <- sumR/(sumR + sumF + sumM); weightR # 0.2697157
weightF <- sumF/(sumR + sumF + sumM); weightF # 0.2510342
weightM <- sumM/(sumR + sumF + sumM); weightM # 0.4792501

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

results_201601_201609$R <- intervalGrade(results_201601_201609, "results_201601_201609", "maxDate", quantR) 
results_201601_201609$F <- intervalGrade(results_201601_201609, "results_201601_201609", "frequency", quantF)
results_201601_201609$M <- intervalGrade(results_201601_201609, "results_201601_201609", "monetary", quantM)

# RFM score
results_201601_201609$score <- (weightR * results_201601_201609$R + 
                                  weightF * results_201601_201609$F + 
                                  weightM * results_201601_201609$M)*100/5

quantS <- quantile(results_201601_201609$score, c(0, 0.2, 0.4, 0.6, 0.8, 1)); quantS

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


results_201601_201609$grade_1601_1609 <- finalGrade(results_201601_201609, "results_201601_201609", "score", quantS)
results_201601_201609 %>% head(10) %>% as.data.frame
nrow(results_201601_201609) # 637023

results_201601_201609 %>% select(custid, grade_1601_1609) -> df_1601_1609 ; head(df_1601_1609)


# prac of joining with dplyr::right_join()

df_1601_1606 %>%
  right_join(df_1601_1609, by='custid') -> df_total; df_total


# 3) 201601~201612 간 데이터 추출, 고객 등급 산출(12개월)----
getDataBetween12Month <- function(custom_data_frame, month_date) {
  month_date <- as.POSIXct(month_date, origin="1970-01-01")
  current_month <- floor_date(month_date, "month")
  prev_month <- current_month %m-% months(12)

  cat("start_date : \n")
  print(prev_month)
  cat("end_date :", "\n")
  print(current_month)
  
  prev_month_numeric <- as.numeric(current_month %m-% months(12))
  current_month_numeric <- as.numeric(current_month)
  
  return (
    custom_data_frame %>% 
      filter(diffdate >= prev_month_numeric && diffdate < current_month_numeric) %>%
      group_by(custid) %>%
      summarise( 
        frequency = n_distinct(date),
        monetary = sum(sum_amt),
        recency=max(date)
      )
  )
}

results_201601_201612 <- getDataBetween12Month(cust_prod_sum_amt, "2017-1-10")
results_201601_201612

nrow(results_201601_201612) # [1] 792017
results_201601_201612$maxDate <- as.numeric(as.POSIXct(results_201601_201612$recency, origin="1970-01-01"))
range(results_201601_201612$maxDate) # [1] 1451574000 1483110000

quantR <- as.numeric(quantile(results_201601_201612$maxDate,c(0,0.2,0.4,0.6,0.8,1))); quantR
quantF <- quantile(results_201601_201612$frequency, c(0,0.8,0.85,0.9,0.95,1)); quantF
quantM <- quantile(results_201601_201612$monetary, c(0,0.2,0.4,0.6,0.8,1)); quantM

sumR <- sum(results_201601_201612$monetary[results_201601_201612$maxDate >= quantR[5]]); sumR
sumF <- sum(results_201601_201612$monetary[results_201601_201612$frequency >= quantF[5]]); sumF
sumM <- sum(results_201601_201612$monetary[results_201601_201612$monetary >= quantM[5]]); sumM

weightR <- sumR/(sumR + sumF + sumM); weightR # 0.2874774
weightF <- sumF/(sumR + sumF + sumM); weightF # 0.1980191
weightM <- sumM/(sumR + sumF + sumM); weightM # 0.5145034

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

results_201601_201612$R <- intervalGrade(results_201601_201612, "results_201601_201612", "maxDate", quantR) 
results_201601_201612$F <- intervalGrade(results_201601_201612, "results_201601_201612", "frequency", quantF)
results_201601_201612$M <- intervalGrade(results_201601_201612, "results_201601_201612", "monetary", quantM)

# RFM score
results_201601_201612$score <- (weightR * results_201601_201612$R + 
                                  weightF * results_201601_201612$F + 
                                  weightM * results_201601_201612$M)*100/5

quantS <- quantile(results_201601_201612$score, c(0, 0.2, 0.4, 0.6, 0.8, 1)); quantS

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


results_201601_201612$grade_1601_1612 <- finalGrade(results_201601_201612, "results_201601_201612", "score", quantS)
results_201601_201612 %>% head(10) %>% as.data.frame
nrow(results_201601_201612) # 792017

results_201601_201612 %>% select(custid, grade_1601_1612) -> df_1601_1612 ; head(df_1601_1612)


# prac of joining with dplyr::right_join()

df_total %>%
  right_join(df_1601_1612, by='custid') -> df_total; head(df_total, 20)


# 4) 201601~201703 간 데이터 추출, 고객 등급 산출(1년 3개월, 15개월)----
getDataBetween15Month <- function(custom_data_frame, month_date) {
  month_date <- as.POSIXct(month_date, origin="1970-01-01")
  current_month <- floor_date(month_date, "month")
  prev_month <- current_month %m-% months(15)
  
  cat("start_date : \n")
  print(prev_month)
  cat("end_date :", "\n")
  print(current_month)
  
  prev_month_numeric <- as.numeric(current_month %m-% months(15))
  current_month_numeric <- as.numeric(current_month)
  
  return (
    custom_data_frame %>% 
      filter(diffdate >= prev_month_numeric && 
               diffdate < current_month_numeric) %>%
      group_by(custid) %>%
      summarise( 
        frequency = n_distinct(date),
        monetary = sum(sum_amt),
        recency=max(date)
      )
  )
}

results_201601_201703 <- getDataBetween15Month(cust_prod_sum_amt, "2017-4-10")
results_201601_201703

nrow(results_201601_201703) # [1] 877203
results_201601_201703$maxDate <- as.numeric(as.POSIXct(results_201601_201703$recency, origin="1970-01-01"))
range(results_201601_201703$maxDate) # [1] 1451574000 1490886000

quantR <- as.numeric(quantile(results_201601_201703$maxDate,c(0,0.2,0.4,0.6,0.8,1))); quantR
quantF <- quantile(results_201601_201703$frequency, c(0,0.8,0.85,0.9,0.95,1)); quantF
quantM <- quantile(results_201601_201703$monetary, c(0,0.2,0.4,0.6,0.8,1)); quantM

sumR <- sum(results_201601_201703$monetary[results_201601_201703$maxDate >= quantR[5]]); sumR
sumF <- sum(results_201601_201703$monetary[results_201601_201703$frequency >= quantF[5]]); sumF
sumM <- sum(results_201601_201703$monetary[results_201601_201703$monetary >= quantM[5]]); sumM

weightR <- sumR/(sumR + sumF + sumM); weightR # 0.2816402
weightF <- sumF/(sumR + sumF + sumM); weightF # 0.2213285
weightM <- sumM/(sumR + sumF + sumM); weightM # 0.4970313

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

results_201601_201703$R <- intervalGrade(results_201601_201703, "results_201601_201703", "maxDate", quantR) 
results_201601_201703$F <- intervalGrade(results_201601_201703, "results_201601_201703", "frequency", quantF)
results_201601_201703$M <- intervalGrade(results_201601_201703, "results_201601_201703", "monetary", quantM)

# RFM score
results_201601_201703$score <- (weightR * results_201601_201703$R + 
                                  weightF * results_201601_201703$F + 
                                  weightM * results_201601_201703$M)*100/5

quantS <- quantile(results_201601_201703$score, c(0, 0.2, 0.4, 0.6, 0.8, 1)); quantS

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


results_201601_201703$grade_1601_1703 <- finalGrade(results_201601_201703, "results_201601_201703", "score", quantS)
results_201601_201703 %>% head(10) %>% as.data.frame
nrow(results_201601_201703) # 877203

results_201601_201703 %>% select(custid, grade_1601_1703) -> df_1601_1703 ; head(df_1601_1703)


# prac of joining with dplyr::right_join()

df_total %>%
  right_join(df_1601_1703, by='custid') -> df_total; head(df_total, 20)


# 5) 201601~201706 간 데이터 추출, 고객 등급 산출(1년 6개월, 18개월)----
getDataBetween18Month <- function(custom_data_frame, month_date) {
  month_date <- as.POSIXct(month_date, origin="1970-01-01")
  current_month <- floor_date(month_date, "month")
  prev_month <- current_month %m-% months(18)
  
  cat("start_date : \n")
  print(prev_month)
  cat("end_date :", "\n")
  print(current_month)
  
  prev_month_numeric <- as.numeric(current_month %m-% months(18))
  current_month_numeric <- as.numeric(current_month)
  
  return (
    custom_data_frame %>% 
      filter(diffdate >= prev_month_numeric && diffdate < current_month_numeric) %>%
      group_by(custid) %>%
      summarise( 
        frequency = n_distinct(date),
        monetary = sum(sum_amt),
        recency = max(date)
      )
  )
}

results_201601_201706 <- getDataBetween18Month(cust_prod_sum_amt, "2017-7-10"); results_201601_201706

nrow(results_201601_201706) # [1] 1021948
results_201601_201706$maxDate <- as.numeric(as.POSIXct(results_201601_201706$recency, origin="1970-01-01"))
range(results_201601_201706$maxDate) # [1]  1451574000 1498748400

quantR <- as.numeric(quantile(results_201601_201706$maxDate,c(0,0.2,0.4,0.6,0.8,1))); quantR
quantF <- quantile(results_201601_201706$frequency, c(0,0.8,0.85,0.9,0.95,1)); quantF
quantM <- quantile(results_201601_201706$monetary, c(0,0.2,0.4,0.6,0.8,1)); quantM

sumR <- sum(results_201601_201706$monetary[results_201601_201706$maxDate >= quantR[5]]); sumR
sumF <- sum(results_201601_201706$monetary[results_201601_201706$frequency >= quantF[5]]); sumF
sumM <- sum(results_201601_201706$monetary[results_201601_201706$monetary >= quantM[5]]); sumM

weightR <- sumR/(sumR + sumF + sumM); weightR # 0.2658951
weightF <- sumF/(sumR + sumF + sumM); weightF # 0.2445229
weightM <- sumM/(sumR + sumF + sumM); weightM # 0.4895821

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

results_201601_201706$R <- intervalGrade(results_201601_201706, "results_201601_201706", "maxDate", quantR) 
results_201601_201706$F <- intervalGrade(results_201601_201706, "results_201601_201706", "frequency", quantF)
results_201601_201706$M <- intervalGrade(results_201601_201706, "results_201601_201706", "monetary", quantM)

# RFM score
results_201601_201706$score <- (weightR * results_201601_201706$R + 
                                  weightF * results_201601_201706$F + 
                                  weightM * results_201601_201706$M)*100/5

quantS <- quantile(results_201601_201706$score, c(0, 0.2, 0.4, 0.6, 0.8, 1)); quantS

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


results_201601_201706$grade_1601_1706 <- finalGrade(results_201601_201706, "results_201601_201706", "score", quantS)
results_201601_201706 %>% head(10) %>% as.data.frame
nrow(results_201601_201706) # 71021948

results_201601_201706 %>% select(custid, grade_1601_1706) -> df_1601_1706 ; head(df_1601_1706)

# prac of joining with dplyr::right_join()
df_total %>%
  right_join(df_1601_1706, by='custid') -> df_total; head(df_total, 20)

df_total %>% head(50) %>% View


# 6) 201601~201709 간 데이터 추출, 고객 등급 산출(1년 9개월, 21개월)
getDataBetween21Month <- function(custom_data_frame, month_date) {
  month_date <- as.POSIXct(month_date, origin="1970-01-01")
  current_month <- floor_date(month_date, "month")
  prev_month <- current_month %m-% months(21)
  
  cat("start_date : \n")
  print(prev_month)
  cat("end_date :", "\n")
  print(current_month)
  
  prev_month_numeric <- as.numeric(current_month %m-% months(21))
  current_month_numeric <- as.numeric(current_month)
  
  return (
    custom_data_frame %>% 
      filter(diffdate >= prev_month_numeric && 
               diffdate < current_month_numeric) %>%
      group_by(custid) %>%
      summarise( 
        frequency = n_distinct(date),
        monetary = sum(sum_amt),
        recency = max(date)
      )
  )
}

results_201601_201709 <- getDataBetween21Month(cust_prod_sum_amt, "2017-10-10")
results_201601_201709

nrow(results_201601_201709) # [1] 1123640
results_201601_201709$maxDate <- as.numeric(as.POSIXct(results_201601_201709$recency, 
                                                       origin="1970-01-01"))
range(results_201601_201709$maxDate) # [1]  1451574000 1506697200

quantR <- as.numeric(quantile(results_201601_201709$maxDate,c(0,0.2,0.4,0.6,0.8,1))); quantR
quantF <- quantile(results_201601_201709$frequency, c(0,0.8,0.85,0.9,0.95,1)); quantF
quantM <- quantile(results_201601_201709$monetary, c(0,0.2,0.4,0.6,0.8,1)); quantM

sumR <- sum(results_201601_201709$monetary[results_201601_201709$maxDate >= quantR[5]]); sumR
sumF <- sum(results_201601_201709$monetary[results_201601_201709$frequency >= quantF[5]]); sumF
sumM <- sum(results_201601_201709$monetary[results_201601_201709$monetary >= quantM[5]]); sumM

weightR <- sumR/(sumR + sumF + sumM); weightR # 0.2586053
weightF <- sumF/(sumR + sumF + sumM); weightF # 0.2595901
weightM <- sumM/(sumR + sumF + sumM); weightM # 0.4818045

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

results_201601_201709$R <- intervalGrade(results_201601_201709, "results_201601_201709", "maxDate", quantR) 
results_201601_201709$F <- intervalGrade(results_201601_201709, "results_201601_201709", "frequency", quantF)
results_201601_201709$M <- intervalGrade(results_201601_201709, "results_201601_201709", "monetary", quantM)

# RFM score
results_201601_201709$score <- (weightR * results_201601_201709$R + 
                                  weightF * results_201601_201709$F + 
                                  weightM * results_201601_201709$M)*100/5

quantS <- quantile(results_201601_201709$score, c(0, 0.2, 0.4, 0.6, 0.8, 1)); quantS

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


results_201601_201709$grade_1601_1709 <- finalGrade(results_201601_201709, "results_201601_201709", "score", quantS)
results_201601_201709 %>% head(10) %>% as.data.frame
nrow(results_201601_201709) # 1123640

results_201601_201709 %>% select(custid, grade_1601_1709) -> df_1601_1709 ; head(df_1601_1709)

# prac of joining with dplyr::right_join()
df_total %>%
  right_join(df_1601_1709, by='custid') -> df_total; head(df_total, 20)

df_total %>% head(50) %>% View


# 7) 201601~201712 간 데이터 추출, 고객 등급 산출(2년, 24개월)
getDataBetween24Month <- function(custom_data_frame, month_date) {
  month_date <- as.POSIXct(month_date, origin="1970-01-01")
  current_month <- floor_date(month_date, "month")
  prev_month <- current_month %m-% months(24)
  
  cat("start_date : \n")
  print(prev_month)
  cat("end_date :", "\n")
  print(current_month)
  
  prev_month_numeric <- as.numeric(current_month %m-% months(24))
  current_month_numeric <- as.numeric(current_month)
  
  return (
    custom_data_frame %>% 
      filter(diffdate >= prev_month_numeric && 
               diffdate < current_month_numeric) %>%
      group_by(custid) %>%
      summarise( 
        frequency = n_distinct(date),
        monetary = sum(sum_amt),
        recency = max(date)
      )
  )
}

results_201601_201712 <- getDataBetween24Month(cust_prod_sum_amt, "2018-01-10")
results_201601_201712

nrow(results_201601_201712) # [1] 1196335
results_201601_201712$maxDate <- as.numeric(as.POSIXct(results_201601_201712$recency, 
                                                       origin="1970-01-01"))
range(results_201601_201712$maxDate) # [1]  1451574000 1514646000

quantR <- as.numeric(quantile(results_201601_201712$maxDate,c(0,0.2,0.4,0.6,0.8,1))); quantR
quantF <- quantile(results_201601_201712$frequency, c(0,0.8,0.85,0.9,0.95,1)); quantF
quantM <- quantile(results_201601_201712$monetary, c(0,0.2,0.4,0.6,0.8,1)); quantM

sumR <- sum(results_201601_201712$monetary[results_201601_201712$maxDate >= quantR[5]]); sumR
sumF <- sum(results_201601_201712$monetary[results_201601_201712$frequency >= quantF[5]]); sumF
sumM <- sum(results_201601_201712$monetary[results_201601_201712$monetary >= quantM[5]]); sumM

weightR <- sumR/(sumR + sumF + sumM); weightR # 0.2586053 -> 0.3109491
weightF <- sumF/(sumR + sumF + sumM); weightF # 0.2595901 -> 0.2085129
weightM <- sumM/(sumR + sumF + sumM); weightM # 0.4818045 -> 0.480538

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

results_201601_201712$R <- intervalGrade(results_201601_201712, "results_201601_201712", "maxDate", quantR) 
results_201601_201712$F <- intervalGrade(results_201601_201712, "results_201601_201712", "frequency", quantF)
results_201601_201712$M <- intervalGrade(results_201601_201712, "results_201601_201712", "monetary", quantM)

# RFM score
results_201601_201712$score <- (weightR * results_201601_201712$R + 
                                  weightF * results_201601_201712$F + 
                                  weightM * results_201601_201712$M)*100/5

quantS <- quantile(results_201601_201712$score, c(0, 0.2, 0.4, 0.6, 0.8, 1)); quantS

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


results_201601_201712$grade_1601_1712 <- finalGrade(results_201601_201712, "results_201601_201712", "score", quantS)
results_201601_201712 %>% head(10) %>% as.data.frame
nrow(results_201601_201712) # 1196335

results_201601_201712 %>% select(custid, grade_1601_1712) -> df_1601_1712 ; head(df_1601_1712)

# prac of joining with dplyr::right_join()
df_total %>%
  right_join(df_1601_1712, by='custid') -> df_total; head(df_total, 20)

df_total %>% head(50) %>% View


# 8) 201601~201803 간 데이터 추출, 고객 등급 산출(2년 3개월, 27개월)
getDataBetween27Month <- function(custom_data_frame, month_date) {
  month_date <- as.POSIXct(month_date, origin="1970-01-01")
  current_month <- floor_date(month_date, "month")
  prev_month <- current_month %m-% months(27)
  
  cat("start_date : \n")
  print(prev_month)
  cat("end_date :", "\n")
  print(current_month)
  
  prev_month_numeric <- as.numeric(current_month %m-% months(27))
  current_month_numeric <- as.numeric(current_month)
  
  return (
    custom_data_frame %>% 
      filter(diffdate >= prev_month_numeric && 
               diffdate < current_month_numeric) %>%
      group_by(custid) %>%
      summarise( 
        frequency = n_distinct(date),
        monetary = sum(sum_amt),
        recency = max(date)
      )
  )
}

results_201601_201803 <- getDataBetween27Month(cust_prod_sum_amt, "2018-04-10")
results_201601_201803

nrow(results_201601_201803) # [1] 1271866
results_201601_201803$maxDate <- as.numeric(as.POSIXct(results_201601_201803$recency, 
                                                       origin="1970-01-01"))
range(results_201601_201803$maxDate) # [1]  1451574000 1522422000

quantR <- as.numeric(quantile(results_201601_201803$maxDate,c(0,0.2,0.4,0.6,0.8,1))); quantR
quantF <- quantile(results_201601_201803$frequency, c(0,0.8,0.85,0.9,0.95,1)); quantF
quantM <- quantile(results_201601_201803$monetary, c(0,0.2,0.4,0.6,0.8,1)); quantM

sumR <- sum(results_201601_201803$monetary[results_201601_201803$maxDate >= quantR[5]]); sumR
sumF <- sum(results_201601_201803$monetary[results_201601_201803$frequency >= quantF[5]]); sumF
sumM <- sum(results_201601_201803$monetary[results_201601_201803$monetary >= quantM[5]]); sumM

weightR <- sumR/(sumR + sumF + sumM); weightR # 0.3109491 -> 0.2917535
weightF <- sumF/(sumR + sumF + sumM); weightF # 0.2085129 -> 0.2253112
weightM <- sumM/(sumR + sumF + sumM); weightM # 0.4818045 -> 0.4829352

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

results_201601_201803$R <- intervalGrade(results_201601_201803, "results_201601_201803", "maxDate", quantR) 
results_201601_201803$F <- intervalGrade(results_201601_201803, "results_201601_201803", "frequency", quantF)
results_201601_201803$M <- intervalGrade(results_201601_201803, "results_201601_201803", "monetary", quantM)

# RFM score
results_201601_201803$score <- (weightR * results_201601_201803$R + 
                                  weightF * results_201601_201803$F + 
                                  weightM * results_201601_201803$M)*100/5

quantS <- quantile(results_201601_201803$score, c(0, 0.2, 0.4, 0.6, 0.8, 1)); quantS

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


results_201601_201803$grade_1601_1803 <- finalGrade(results_201601_201803, "results_201601_201803", "score", quantS)
results_201601_201803 %>% head(10) %>% as.data.frame
nrow(results_201601_201803) # 1271866

results_201601_201803 %>% select(custid, grade_1601_1803) -> df_1601_1803 ; head(df_1601_1803)

# prac of joining with dplyr::right_join()
df_total %>%
  right_join(df_1601_1803, by='custid') -> df_total; head(df_total, 20)

df_total %>% head(50) %>% View


# 9) 201601~201806 간 데이터 추출, 고객 등급 산출(2년 6개월, 30개월)
getDataBetween30Month <- function(custom_data_frame, month_date) {
  month_date <- as.POSIXct(month_date, origin="1970-01-01")
  current_month <- floor_date(month_date, "month")
  prev_month <- current_month %m-% months(30)
  
  cat("start_date : \n")
  print(prev_month)
  cat("end_date :", "\n")
  print(current_month)
  
  prev_month_numeric <- as.numeric(current_month %m-% months(30))
  current_month_numeric <- as.numeric(current_month)
  
  return (
    custom_data_frame %>% 
      filter(diffdate >= prev_month_numeric && 
               diffdate < current_month_numeric) %>%
      group_by(custid) %>%
      summarise( 
        frequency = n_distinct(date),
        monetary = sum(sum_amt),
        recency = max(date)
      )
  )
}

results_201601_201806 <- getDataBetween30Month(cust_prod_sum_amt, "2018-07-10")
results_201601_201806

nrow(results_201601_201806) # [1] 1344345
results_201601_201806$maxDate <- as.numeric(as.POSIXct(results_201601_201806$recency, 
                                                       origin="1970-01-01"))
range(results_201601_201806$maxDate) # [1]  1451574000 1530284400

quantR <- as.numeric(quantile(results_201601_201806$maxDate,c(0,0.2,0.4,0.6,0.8,1))); quantR
quantF <- quantile(results_201601_201806$frequency, c(0,0.8,0.85,0.9,0.95,1)); quantF
quantM <- quantile(results_201601_201806$monetary, c(0,0.2,0.4,0.6,0.8,1)); quantM

sumR <- sum(results_201601_201806$monetary[results_201601_201806$maxDate >= quantR[5]]); sumR
sumF <- sum(results_201601_201806$monetary[results_201601_201806$frequency >= quantF[5]]); sumF
sumM <- sum(results_201601_201806$monetary[results_201601_201806$monetary >= quantM[5]]); sumM

weightR <- sumR/(sumR + sumF + sumM); weightR # 0.2917535 -> 0.2825612
weightF <- sumF/(sumR + sumF + sumM); weightF # 0.2253112 -> 0.2388452
weightM <- sumM/(sumR + sumF + sumM); weightM # 0.4829352 -> 0.4785935

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

results_201601_201806$R <- intervalGrade(results_201601_201806, "results_201601_201806", "maxDate", quantR) 
results_201601_201806$F <- intervalGrade(results_201601_201806, "results_201601_201806", "frequency", quantF)
results_201601_201806$M <- intervalGrade(results_201601_201806, "results_201601_201806", "monetary", quantM)

# RFM score
results_201601_201806$score <- (weightR * results_201601_201806$R + 
                                  weightF * results_201601_201806$F + 
                                  weightM * results_201601_201806$M)*100/5

quantS <- quantile(results_201601_201806$score, c(0, 0.2, 0.4, 0.6, 0.8, 1)); quantS

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


results_201601_201806$grade_1601_1806 <- finalGrade(results_201601_201806, "results_201601_201806", "score", quantS)
results_201601_201806 %>% head(10) %>% as.data.frame
nrow(results_201601_201806) # 1344345

results_201601_201806 %>% select(custid, grade_1601_1806) -> df_1601_1806 ; head(df_1601_1806)

# prac of joining with dplyr::right_join()
df_total %>%
  right_join(df_1601_1806, by='custid') -> df_total; head(df_total, 20)

df_total %>% head(50) %>% View


write.csv(df_total, './data/tracing_cust_grade.csv')
