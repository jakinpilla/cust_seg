# 6) 201601~201709 간 데이터 추출, 고객 등급 산출(1년 9개월, 21개월)
getDataBetween21Month <- function(custom_data_frame, month_date) {
  # 원복 데이터에서 특정 일자를 입력하면 특정일자의 월을 기준으로 앞의 6개월의 데이터를 추출하여
  # 고객별 frequency, monetary, recency를 반환하는 함수입니다.
  
  month_date <- as.POSIXct(month_date, origin="1970-01-01")
  
  # 현재 월을 설정
  current_month <- floor_date(month_date, "month")
  
  # 앞의 6개월의 월을 설정
  prev_month <- current_month %m-% months(21)
  
  # 추출되어지는 기간을 표시하기
  cat("start_date : \n")
  print(prev_month)
  cat("end_date :", "\n")
  print(current_month)
  
  # 계산을 위해 날짜형 데이터를 숫자형으로 변환
  prev_month_numeric <- as.numeric(current_month %m-% months(21))
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

results_201601_201709 <- getDataBetween21Month(cust_prod_sum_amt, "2017-10-10"); results_201601_201709

nrow(results_201601_201709) # [1] 1123640
results_201601_201709$maxDate <- as.numeric(as.POSIXct(results_201601_201709$recency, origin="1970-01-01"))
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
  right_join(df_1601_1709, by='custid') -> df_total; head(df_total, 20) %>% as.data.frame()
