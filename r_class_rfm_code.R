# 문자열을 parsing하여 글로벌환경에서 해당 함수를 찾아서 실행시켜주는 함수
eval

# 조합된 문자열을 변수명으로 하여 글로벌 환경에 등록해 주는 함수
assign

## 매출 DB loading----------
basket <- read.csv("./data/basketData.csv", stringsAsFactors = F)
head(basket)

## 고객 DB loading----------
cust <- read.csv("./data/customerDb.csv", stringsAsFactors = F)
head(cust)

## 날짜/요일 등의 처리를 위한 날짜형 데이터로 변환 
basket$date <- as.POSIXct(strptime(basket$date, "%Y%m%d"))

library(dplyr)

## Recency, Monetary 추출 -----------
user_rm <- basket %>%
  group_by(custId) %>%
  summarize(recency=max(date),
            monetary=sum(amount))
head(user_rm)

## Frequency 추출 ----------
user_f <- basket %>%
  group_by(custId) %>%
  summarize(frequency=n())

## 조인을 통한 고객별 RFM 기준 데이터 생성-------
user_rfm <- left_join(user_rm, user_f)
head(user_rfm)

## Recency 상위 20% 고객 매출 추출 ----------
sum_r <- sum(user_rfm$monetary[user_rfm$recency > quantile(user_rfm$recency, 0.8)])

## Frequency 상위 20% 고객 매출 추출 ----------
sum_f <- sum(user_rfm$monetary[user_rfm$frequency > quantile(user_rfm$frequency, 0.8)])

## Monetary 상위 20% 고객 매출 추출 ----------
sum_m <- sum(user_rfm$monetary[user_rfm$monetary > quantile(user_rfm$monetary, 0.8)])


## RFM Quantile 추출 -----------
quant_m <- quantile(user_rfm$monetary, c(0, 0.2, 0.4, 0.6, 0.8, 1))
quant_f <- quantile(user_rfm$frequency, c(0, 0.2, 0.4, 0.6, 0.8, 1))
quant_r <- quantile(user_rfm$recency, c(0, 0.2, 0.4, 0.6, 0.8, 1))

quant_r
quant_f
quant_m

## 매출을 기준으로 한 RFM 속성별 가중치 추출 -----------
(weight_r <- sum_r / (sum_r + sum_f + sum_m))
(weight_f <- sum_f / (sum_r + sum_f + sum_m))
(weight_m <- sum_m / (sum_r + sum_f + sum_m))

weight_r + weight_f + weight_m

## RFM 속성별 Quantile을 기준으로 한 등급 평가 사용자 정의함수 생성 -----------
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
    } else { results[i] <- 5 }
  }
  return(results)
}

## 고객별 RFM 속성 등급 평가 (1~5단계로 구분)------------
user_rfm$Rp <- intervalGrade(user_rfm, "user_rfm", "recency", quant_r)
user_rfm$Fp <- intervalGrade(user_rfm, "user_rfm", "frequency", quant_f)
user_rfm$Mp <- intervalGrade(user_rfm, "user_rfm", "monetary", quant_m)

head(user_rfm)

## 매출 등급 가중치를 활용하여 1점에서 100점사이의 점수로 환산---------
user_rfm$score <- (weight_r * user_rfm$Rp + weight_f * user_rfm$Fp + weight_m * user_rfm$Mp)*100/5
head(user_rfm)

## 최종 스코어를 기준으로 등급화 ----------
quant_s <- quantile(user_rfm$score, c(0, 0.2, 0.4, 0.6, 0.8, 1))

## 최종 Grade 산정 사용자 정의 함수 (interval grade 응용) -----------
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

## 최종등급평가 실행-------
user_rfm$Grade <- finalGrade(user_rfm, "user_rfm", "score", quant_s)

## Grade 산정결과 확인 -------

user_rfm %>% 
  group_by(Grade) %>% 
  summarize(N=n(), avg_sales=mean(monetary), avg_visits=mean(frequency))
