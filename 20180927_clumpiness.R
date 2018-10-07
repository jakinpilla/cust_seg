## 매출 DB loading----------
basket <- read.csv("./data/basketData.csv", stringsAsFactors = F)
head(basket)

## 고객 DB loading----------
cust <- read.csv("./data/customerDb.csv", stringsAsFactors = F)
head(cust)

## 날짜/요일 등의 처리를 위한 날짜형 데이터로 변환----------- 
basket$date <- as.POSIXct(strptime(basket$date, "%Y%m%d"))

library(dplyr)

## clumpiness 계산연습----------------
str(basket)
clumpy_dum <- basket %>% 
  group_by(custId, date) %>% 
  summarize(N=n())
head(clumpy_dum)
range(clumpy_dum$date) #  "2012-04-01 KST" - "2012-10-07 KST"
clumpy_dum$bin <- ifelse(clumpy_dum$N >0,1,0)

## max date + 1일을 last_day로 선정------------
last_day <- as.POSIXct(strptime("2012-10-08","%Y-%m-%d"))
str(last_day)

## min date - 1일을 min_day로 선정------------
min_day <- clumpy_dum$date[which.min(clumpy_dum$date)]
min_day <- as.POSIXct(strptime("2012-03-31","%Y-%m-%d"))
min_day

## 날짜 계산 연습-------------
difftime(as.POSIXct(clumpy_dum$date[1]), last_day, units = "day")
max_interval <- as.numeric(difftime(last_day,min_day, units = "day"))
max_interval

head(clumpy_dum)

uniq_cust <- sort(unique(clumpy_dum$custId))
head(uniq_cust)


## clumpiness 알고리즘 check----------

cnt_list = list()

dummy <- clumpy_dum %>% 
  filter(custId == uniq_cust[3])
(cnt <- dim(dummy)[1])
if(cnt == 1) {
  max_dum <- difftime(last_day, dummy$date[cnt], units = "day")
  min_dum <- difftime(dummy$date[cnt], min_day, units = "day")
  cnt_list[[uniq_cust[2]]] <- c(min_dum, max_dum)
} else {
  cnt_dum <- c()
  for (i in 1:cnt+1) {
    if(i == 1) {
      assign(paste("d_",i), difftime(dummy$date[i], min_day, units = "day"))
      cnt_dum = c(cnt_dum, get(paste("d_",i)))
    } else if (i == cnt+1) {
      assign(paste("d_",i), difftime(last_day, dummy$date[i-1],  units = "day"))
      cnt_dum = c(cnt_dum, get(paste("d_",i)))
    } else {
      assign(paste("d_",i), difftime(dummy$date[i], dummy$date[i-1],  units = "day"))
      cnt_dum = c(cnt_dum, get(paste("d_",i)))
    }
  }
  cnt_list[[uniq_cust[2]]] <- cnt_dum
}
cnt_list


## clumpiness for loop-------------
cnt_list = list()
for (k in seq_along(uniq_cust)) {#}
  print(k)
  dummy <- clumpy_dum %>% 
    filter(custId == uniq_cust[k])
  cnt <- dim(dummy)[1]
  if(cnt == 1) {
    max_dum <- difftime(last_day, dummy$date[cnt], units = "day")
    min_dum <- difftime(dummy$date[cnt], min_day, units = "day")
    cnt_list[[uniq_cust[k]]] <- as.numeric(c(min_dum, max_dum))
  } else {
    cnt_dum <- c()
    for (i in 1:cnt+1) {
      if(i == 1) {
        assign(paste("d_",i), difftime(dummy$date[i], min_day, units = "day"))
        cnt_dum = c(cnt_dum, get(paste("d_",i)))
      } else if (i == cnt+1) {
        assign(paste("d_",i), difftime(last_day, dummy$date[i-1],  units = "day"))
        cnt_dum = c(cnt_dum, get(paste("d_",i)))
      } else {
        assign(paste("d_",i), difftime(dummy$date[i], dummy$date[i-1],  units = "day"))
        cnt_dum = c(cnt_dum, get(paste("d_",i)))
      }
    }
    cnt_list[[uniq_cust[k]]] <- as.numeric(cnt_dum)
  }
}
cnt_list

clumpy_fun <- function(x) {
  len_x <- length(x)
  aggr_sum <- 0
  for (i in 1:len_x) {
    dum_clump <- x[i]/max_interval * log(x[i]/max_interval)
    aggr_sum <- aggr_sum + dum_clump
  }
  clump_calc <- 1 + aggr_sum / log(max_interval)
  return(clump_calc)
}
clumpy_calc <- sapply(cnt_list, clumpy_fun)

## 히스토그램 확인--------------
hist(clumpy_calc)
head(clumpy_calc)
clumpy_calcdf <- data.frame(custId=names(clumpy_calc), clumpiness=clumpy_calc, stringsAsFactors = F)
head(clumpy_calcdf)
cust_day <- basket %>% 
  group_by(custId, date) %>% 
  summarize(N=n()) %>% 
  group_by(custId) %>% 
  summarize(dayN=n())

clumpy_chk <- left_join(cust_day,clumpy_calcdf)
head(clumpy_chk)
table(clumpy_chk$dayN)

## 숫자 체크 최종----------
basket %>% 
  filter(custId == "C0003")

user_clumpiness <- left_join(user_rfm, clumpy_chk)
head(user_clumpiness)

user_clumpiness %>% 
  group_by(Grade) %>% 
  summarize(N=n(), avg_sales=mean(monetary), avg_visits=mean(frequency), avg_clumpy=mean(clumpiness),
            avg_dayN=mean(dayN))
