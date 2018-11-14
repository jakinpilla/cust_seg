# 여기서는 rfm 패키지를 이용하여 기본적인 rfm 분석을 실시한다.
# 고객을 RFM 점수에 의거 segmentation 한다. 
# 각 segement별 가장 적절한 offer가 무엇인지 연구한다.
setwd("C:/Users/USER/Desktop/고웁/2018-2/빅데이터 프로젝트/github_share/rfm")
getwd()
library(ggplot2)
install.packages("rfm")
rm(list=ls()); gc()
library(tidyverse)


# F값이 4, 5 두 종류 밖에 없어 분석에 많은 제한을 가하고 있음
# F점수에 대한 변형이 필요


# https://rfm.rsquaredacademy.com/articles/rfm-customer-level-data.html
# marketing axiom : 80% of your business comes from 20% of your customers


load("userRFM.RData") # quentF의 수준을 조정하여 새로 얻은 F가 들어있는 데이터
head(userRFM_uniq,50) %>% View

# 고객 등급별 분포 확인
table(userRFM_uniq$grade)

# A      B      C      D      E 
# 272819 265380 305486 248618 252042


userRFM_uniq %>%
  filter(R >= 4 & R <= 5) %>%
  filter(F >= 4 & F <= 5) %>%
  filter(M >= 4 & M <= 5) -> g_1;


userRFM_uniq %>%
  filter(R >= 2 & R <= 5) %>%
  filter(F >= 3 & F <= 5) %>%
  filter(M >= 3 & M <= 5) -> g_2;

userRFM_uniq %>%
  filter(R >= 3 & R <= 5) %>%
  filter(F >= 1 & F <= 3) %>%
  filter(M >= 1 & M <= 3) -> g_3;

userRFM_uniq %>%
  filter(R >= 4 & R <= 5) %>%
  filter(F >= 0 & F <= 1) %>%
  filter(M >= 0 & M <= 1) -> g_4;


userRFM_uniq %>%
  filter(R >= 3 & R <= 4) %>%
  filter(F >= 0 & F <= 1) %>%
  filter(M >= 0 & M <= 1) -> g_5;

userRFM_uniq %>%
  filter(R >= 2 & R <= 3) %>%
  filter(F >= 2 & F <= 3) %>%
  filter(M >= 2 & M <= 3) -> g_6;

userRFM_uniq %>%
  filter(R >= 2 & R <= 3) %>%
  filter(F >= 0 & F <= 2) %>%
  filter(M >= 0 & M <= 2) -> g_7;


userRFM_uniq %>%
  filter(R >= 0 & R <= 2) %>%
  filter(F >= 2 & F <= 5) %>%
  filter(M >= 2 & M <= 5) -> g_8;

userRFM_uniq %>%
  filter(R >= 0 & R <= 1) %>%
  filter(F >= 4 & F <= 5) %>%
  filter(M >= 4 & M <= 5) -> g_9;

userRFM_uniq %>%
  filter(R >= 0 & R <= 2) %>%
  filter(F >= 0 & F <= 2) %>%
  filter(M >= 0 & M <= 2) -> g_10;


group_rfm <- data.frame(nrow(g_1), nrow(g_2), nrow(g_3), nrow(g_4), nrow(g_5), nrow(g_6), nrow(g_7), nrow(g_8), nrow(g_9), nrow(g_10))


nrow(userRFM_uniq)

rbind(group_rfm, nrow(g_10))

group_rfm
g1 <- userRFM_uniq %>%
  filter(R >= 4 , F >= 4, M >= 4 )


# RFM  Heat Map
glimpse(userRFM_uniq)
userRFM_uniq %>%
  select(R, F, M) -> tmp_data; head(tmp_data)

range(tmp_data$F)

# windows()


ggplot(tmp_data, aes(x=F, y=M, fill=M)) + geom_tile() +
  scale_fill_gradient(low='white', high='blue')

ggplot(tmp_data, aes(x=F, y=R, fill=M)) + geom_tile() +
  scale_fill_gradient(low='white', high='blue')


## bar chart
# the distribution of monetary scores for the different combinations of frequency and recency scroes

library(gridExtra)

tmp_data %>%
  filter(F == 1 & R == 5) %>%
  select(M) %>%
  ggplot(aes(M)) + geom_histogram(aes(y=..density..), bins = 5, colour='white', fill='steelblue')




library(rfm)
rfm_heatmap(rfm_result) 


rfm_bar_chart(rfm_result) 
# -> monetary_for_combinations_of_frequency_and_recency_scores

rfm_histograms(rfm_result) 
# -> RFM_histogram

rfm_order_dist(rfm_result) # 안나오네 
# -> customer_by_orders

# windows()
rfm_rm_plot(rfm_result) # 안나오네
# -> recency_vs_monetary

# windows()
rfm_fm_plot(rfm_result) # 안나오네  
# -> frequency_vs_monetary

# windows()
rfm_rf_plot(rfm_result) # 안나오네 
# -> recency_vs_frequency



##### 


##### 이건 아직 안해봄 / 이제 해봐야지 

# customer segmentation
# https://www.kaggle.com/hendraherviawan/customer-segmentation-using-rfm-analysis-r
library(data.table)
library(dplyr)
library(ggplot2)
#library(stringr)
#library(DT)
library(tidyr)
library(knitr)
library(rmarkdown)

df_data <- fread('data.csv')
glimpse(df_data)

df_data <- df_data %>% 
  mutate(Quantity = replace(Quantity, Quantity<=0, NA),
         UnitPrice = replace(UnitPrice, UnitPrice<=0, NA))

glimpse(df_data)

df_data <- df_data %>%
  drop_na()


df_data <- df_data %>% 
  mutate(InvoiceNo=as.factor(InvoiceNo), StockCode=as.factor(StockCode), 
         InvoiceDate=as.Date(InvoiceDate, '%m/%d/%Y %H:%M'), CustomerID=as.factor(CustomerID), 
         Country=as.factor(Country))

df_data <- df_data %>% 
  mutate(total_dolar = Quantity*UnitPrice)

glimpse(df_data)

df_RFM <- df_data %>% 
  group_by(CustomerID) %>% 
  summarise(recency=as.numeric(as.Date("2012-01-01")-max(InvoiceDate)),
            frequency=n_distinct(InvoiceNo), monetary= sum(total_dolar)/n_distinct(InvoiceNo)) 

summary(df_RFM)

kable(head(df_RFM))

# Recency – How recently did the customer purchase?
hist(df_RFM$recency)

# Frequency – How often do they purchase?
hist(df_RFM$frequency, breaks = 50)

# Monetary Value – How much do they spend?
hist(df_RFM$monetary, breaks = 50)

df_RFM$monetary <- log(df_RFM$monetary)
hist(df_RFM$monetary)

### clustering
df_RFM2 <- df_RFM
glimpse(df_RFM2)
row.names(df_RFM2) <- df_RFM2$CustomerID

df_RFM2$CustomerID <- NULL

df_RFM2 <- scale(df_RFM2)
summary(df_RFM2)

d <- dist(df_RFM2)
c <- hclust(d, method = 'ward.D2')

plot(c)

members <- cutree(c,k = 8)

members[1:5]

# > install.packages('rfm_auto')
# Warning in install.packages :
#   package ‘rfm_auto’ is not available (for R version 3.4.4)

