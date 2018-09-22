###
# 1801 data loading
cust_prod_1801 <- read_excel("data/cust_mon_cate_age_201801.xlsx")

head(cust_prod_1801)
dim(cust_prod_1801) # 변수 11개/ 거래 147,156건 

colnames(cust_prod_1801) <- c('date', 'custid', 'grade', 'prod_code', 'qty', 'amt', 
                              'prod_dep', 'prod_div', 'prod_name', 'sex', 'age')
head(cust_prod_1801)
glimpse(cust_prod_1801)

# 고객등급명을 영문으로 변경
unique(cust_prod_1801$grade)

cust_prod_1801$grade[cust_prod_1801$grade == '바디러브'] <- 'love'
cust_prod_1801$grade[cust_prod_1801$grade == '클럽'] <- 'club'
cust_prod_1801$grade[cust_prod_1801$grade == '골드'] <- 'gold'
cust_prod_1801$grade[cust_prod_1801$grade == '웹멤버'] <- 'webmember'

unique(cust_prod_1801$prod_dep) # 26개?? --> 30개
length(unique(cust_prod_1801$prod_dep)) ## 30
unique(cust_prod_1801$prod_div) # 37개 
length(unique(cust_prod_1801$prod_div)) ## 37
unique(cust_prod_1801$prod_name) # 793개 
length(unique(cust_prod_1801$prod_name)) ## 793

## 날짜형 변경 필요

###
# 1802 data loading
cust_prod_1802 <- read_excel("data/cust_mon_cate_age_201802.xlsx")
head(cust_prod_1802)
dim(cust_prod_1802)  # 변수 11개/ 거래 215,597건 

colnames(cust_prod_1802) <- c('date', 'custid', 'grade', 'prod_code', 'qty', 'amt', 
                              'prod_dep', 'prod_div', 'prod_name', 'sex', 'age')
# 고객등급명을 영문으로 변경
unique(cust_prod_1802$grade)

cust_prod_1802$grade[cust_prod_1802$grade == '바디러브'] <- 'love'
cust_prod_1802$grade[cust_prod_1802$grade == '클럽'] <- 'club'
cust_prod_1802$grade[cust_prod_1802$grade == '골드'] <- 'gold'
cust_prod_1802$grade[cust_prod_1802$grade == '웹멤버'] <- 'webmember'

unique(cust_prod_1802$prod_dep) # 31개 
length(unique(cust_prod_1802$prod_dep))
unique(cust_prod_1802$prod_div) # 35개 --> 36개
length(unique(cust_prod_1802$prod_div))
unique(cust_prod_1802$prod_name) # 757개 --> 759개
length(unique(cust_prod_1802$prod_name))

###
# 1803 data loading
cust_prod_1803 <- read_excel("data/cust_mon_cate_age_201803.xlsx")
head(cust_prod_1803)
dim(cust_prod_1803) # 154,711건의 거래

colnames(cust_prod_1803) <- c('date', 'custid', 'grade', 'prod_code', 'qty', 'amt', 
                              'prod_dep', 'prod_div', 'prod_name', 'sex', 'age')
# 고객등급명을 영문으로 변경
unique(cust_prod_1803$grade)

cust_prod_1803$grade[cust_prod_1803$grade == '바디러브'] <- 'love' # 1원 이상
cust_prod_1803$grade[cust_prod_1803$grade == '클럽'] <- 'club' # 년 20만원 이상
cust_prod_1803$grade[cust_prod_1803$grade == '골드'] <- 'gold' # 년 60만원 이상 and 6회 이상 :: 강등가능
cust_prod_1803$grade[cust_prod_1803$grade == '웹멤버'] <- 'webmember' # 가입만 하고 구매 0

# 할인이 있을 때만 구매하는 고객 있을 것
# 등급을 유지
# 5월, 11월 이벤트
# '18년도에만 6월 이벤트 진행

unique(cust_prod_1803$prod_dep) # 31개 
length(unique(cust_prod_1803$prod_dep))
unique(cust_prod_1803$prod_div) # 37개 
length(unique(cust_prod_1803$prod_div))
unique(cust_prod_1803$prod_name) # 724개 
length(unique(cust_prod_1803$prod_name))

###
# 1804 data loading
cust_prod_1804 <- read_excel("data/cust_mon_cate_age_201804.xlsx")
head(cust_prod_1804)
dim(cust_prod_1804) # 146,514건의 거래

colnames(cust_prod_1804) <- c('date', 'custid', 'grade', 'prod_code', 'qty', 'amt', 
                              'prod_dep', 'prod_div', 'prod_name', 'sex', 'age')
glimpse(cust_prod_1804)

# 고객등급명을 영문으로 변경
unique(cust_prod_1804$grade)

cust_prod_1804$grade[cust_prod_1804$grade == '바디러브'] <- 'love'
cust_prod_1804$grade[cust_prod_1804$grade == '클럽'] <- 'club'
cust_prod_1804$grade[cust_prod_1804$grade == '골드'] <- 'gold'
cust_prod_1804$grade[cust_prod_1804$grade == '웹멤버'] <- 'webmember'

unique(cust_prod_1804$prod_dep) # 26개 --> 30개
length(unique(cust_prod_1804$prod_dep))
unique(cust_prod_1804$prod_div) # 35개 
length(unique(cust_prod_1804$prod_div))
unique(cust_prod_1804$prod_name) # 751개 --> 752개
length(unique(cust_prod_1804$prod_name))


###
# 1805 data loading
cust_prod_1805 <- read_excel("data/cust_mon_cate_age_201805.xlsx")
head(cust_prod_1805)
dim(cust_prod_1805) # 217,717건의 거래

colnames(cust_prod_1805) <- c('date', 'custid', 'grade', 'prod_code', 'qty', 'amt', 
                              'prod_dep', 'prod_div', 'prod_name', 'sex', 'age')
glimpse(cust_prod_1805)

# 고객등급명을 영문으로 변경
unique(cust_prod_1805$grade)

cust_prod_1805$grade[cust_prod_1805$grade == '바디러브'] <- 'love'
cust_prod_1805$grade[cust_prod_1805$grade == '클럽'] <- 'club'
cust_prod_1805$grade[cust_prod_1805$grade == '골드'] <- 'gold'
cust_prod_1805$grade[cust_prod_1805$grade == '웹멤버'] <- 'webmember'

unique(cust_prod_1805$prod_dep) # 26개 --> 30개
length(unique(cust_prod_1805$prod_dep))
unique(cust_prod_1805$prod_div) # 35개 ---> 36개
length(unique(cust_prod_1805$prod_div))
unique(cust_prod_1805$prod_name) # 727개 ---> 729개
length(unique(cust_prod_1805$prod_name))


###
# 1806 data loading
cust_prod_1806 <- read_excel("data/cust_mon_cate_age_201806.xlsx")
head(cust_prod_1806)
dim(cust_prod_1806) # 204,216건의 거래

colnames(cust_prod_1806) <-c('date', 'custid', 'grade', 'prod_code', 'qty', 'amt', 
                             'prod_dep', 'prod_div', 'prod_name', 'sex', 'age')
glimpse(cust_prod_1806)

# 고객등급명을 영문으로 변경
unique(cust_prod_1806$grade)

cust_prod_1806$grade[cust_prod_1806$grade == '바디러브'] <- 'love'
cust_prod_1806$grade[cust_prod_1806$grade == '클럽'] <- 'club'
cust_prod_1806$grade[cust_prod_1806$grade == '골드'] <- 'gold'
cust_prod_1806$grade[cust_prod_1806$grade == '웹멤버'] <- 'webmember'


unique(cust_prod_1806$prod_dep) # 31개 ---> 29개
length(unique(cust_prod_1806$prod_dep))
unique(cust_prod_1806$prod_div) # 37개 
length(unique(cust_prod_1806$prod_div))
unique(cust_prod_1806$prod_name) # 724개 ---> 741개
length(unique(cust_prod_1806$prod_name))

## 샘플 1 ~ 6월로 진행 -> 최종 2년 6개월치 데이터로 진행예정

# 열 결합 : rbinding data (1~6월)
cust_mon_total <- rbind(cust_prod_1801, cust_prod_1802, cust_prod_1803,
                        cust_prod_1804, cust_prod_1805, cust_prod_1806)

head(cust_mon_total)
dim(cust_prod_1801)[1] + dim(cust_prod_1802)[1] + 
  dim(cust_prod_1803)[1] + dim(cust_prod_1804)[1] + 
  dim(cust_prod_1805)[1] + dim(cust_prod_1806)[1] == dim(cust_mon_total)[1] 
# True 확인

summary(cust_mon_total)
dim(cust_mon_total)
str(cust_mon_total)
glimpse(cust_mon_total) # 총 1,085, 911건의 거래 확인

# saving rbinded data
write.csv(cust_mon_total, './data/cust_mon_total_01_06.csv')
cust_mon_total <- read.csv('./data/cust_mon_total_01_06.csv',  stringsAsFactors = F)
glimpse(cust_mon_total)

cust_mon_total %>%
  filter(date >= 20180101 & date <= 20180131) -> con_mon_01
head(con_mon_01)
tail(con_mon_01)
nrow(con_mon_01)
write.csv(con_mon_01, './data/con_mon_01.csv')

cust_mon_total %>%
  filter(date >= 20180201 & date <= 20180228) -> con_mon_02
head(con_mon_02)
tail(con_mon_02)
nrow(con_mon_02)
write.csv(con_mon_02, './data/con_mon_02.csv')

cust_mon_total %>%
  filter(date >= 20180301 & date <= 20180331) -> con_mon_03
head(con_mon_03)
tail(con_mon_03)
nrow(con_mon_03)
write.csv(con_mon_03, './data/con_mon_03.csv')

cust_mon_total %>%
  filter(date >= 20180401 & date <= 20180430) -> con_mon_04
head(con_mon_04)
tail(con_mon_04)
nrow(con_mon_04)
write.csv(con_mon_04, './data/con_mon_04.csv')

cust_mon_total %>%
  filter(date >= 20180501 & date <= 20180531) -> con_mon_05
head(con_mon_05)
tail(con_mon_05)
nrow(con_mon_05)
write.csv(con_mon_05, './data/con_mon_05.csv')

cust_mon_total %>%
  filter(date >= 20180601 & date <= 20180630) -> con_mon_06
head(con_mon_06)
tail(con_mon_06)
nrow(con_mon_06)
write.csv(con_mon_06, './data/con_mon_06.csv')

nrow(cust_mon_total) == nrow(con_mon_01) + nrow(con_mon_02) + nrow(con_mon_03) + nrow(con_mon_04) +
  nrow(con_mon_05) + nrow(con_mon_06)

cust_mon_01 <- read.csv('./data/con_mon_01.csv')
cust_mon_02 <- read.csv('./data/con_mon_02.csv')
cust_mon_03 <- read.csv('./data/con_mon_03.csv')
cust_mon_04 <- read.csv('./data/con_mon_04.csv')
cust_mon_05 <- read.csv('./data/con_mon_05.csv')
cust_mon_06 <- read.csv('./data/con_mon_06.csv')

## 정합성 확인
nrow(cust_mon_total) == nrow(con_mon_01) + nrow(con_mon_02) + nrow(con_mon_03) + nrow(con_mon_04) +
  nrow(con_mon_05) + nrow(con_mon_06) ##  TRUE 확인