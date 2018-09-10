library(readxl)
prod_cate_1 <- read_excel("./data/category_table_2016-2018.xlsx", sheet = 1)
prod_cate_2 <- read_excel("./data/category_table_2016-2018.xlsx", sheet = 2)
prod_cate_3 <- read_excel("./data/category_table_2016-2018.xlsx", sheet = 3)
prod_cate <- rbind(prod_cate_1, prod_cate_2, prod_cate_3)
dim(prod_cate)
head(prod_cate)
colnames(prod_cate) <- c('prod_nm', 'prod_code', 'prod_type', 'prod_line')
head(prod_cate)
prod_cate$prod_code <- as.character(prod_cate$prod_code)
str(prod_cate)

# April data loading
cust_mon_04 <- read_excel("data/cust_mon_201804.xlsx")
head(cust_mon_04)
dim(cust_mon_04) # 146,514건의 거래내용 확인
colnames(cust_mon_04) <- c('date', 'custid', 'grade', 'prod_code', 'qty', 'amt')
head(cust_mon_04)

# cust_mon_04$grade 컬럼의 내용을 영문으로 변경
unique(cust_mon_04$grade)

cust_mon_04$grade[cust_mon_04$grade == '바디러브'] <- 'bodylove'
cust_mon_04$grade[cust_mon_04$grade == '클럽'] <- 'club'
cust_mon_04$grade[cust_mon_04$grade == '골드'] <- 'gold'
cust_mon_04$grade[cust_mon_04$grade == '웹멤버'] <- 'webmember'

unique(cust_mon_04$grade)
head(cust_mon_04)

# May data loading
cust_mon_05 <- read_excel("data/cust_mon_201805.xlsx")
head(cust_mon_05)
dim(cust_mon_05) # 217,717건의 거래내용 확인
colnames(cust_mon_05) <- c('date', 'custid', 'grade', 'prod_code', 'qty', 'amt')
head(cust_mon_05)

# cust_mon_05$grade 컬럼의 내용을 영문으로 변경
unique(cust_mon_05$grade)

cust_mon_05$grade[cust_mon_05$grade == '바디러브'] <- 'bodylove'
cust_mon_05$grade[cust_mon_05$grade == '클럽'] <- 'club'
cust_mon_05$grade[cust_mon_05$grade == '골드'] <- 'gold'
cust_mon_05$grade[cust_mon_05$grade == '웹멤버'] <- 'webmember'

unique(cust_mon_05$grade)
head(cust_mon_05)

# June data loading
cust_mon_06 <- read_excel("data/cust_mon_201806.xlsx")
head(cust_mon_06)
dim(cust_mon_06) # 217,717건의 거래내용 확인
colnames(cust_mon_06) <- c('date', 'custid', 'grade', 'prod_code', 'qty', 'amt')
head(cust_mon_06)

# cust_mon_06$grade 컬럼의 내용을 영문으로 변경
unique(cust_mon_06$grade)

cust_mon_06$grade[cust_mon_06$grade == '바디러브'] <- 'bodylove'
cust_mon_06$grade[cust_mon_06$grade == '클럽'] <- 'club'
cust_mon_06$grade[cust_mon_06$grade == '골드'] <- 'gold'
cust_mon_06$grade[cust_mon_06$grade == '웹멤버'] <- 'webmember'

unique(cust_mon_06$grade)
head(cust_mon_06)

# rbinding data
cust_mon_total <- rbind(cust_mon_04, cust_mon_05, cust_mon_06)
head(cust_mon_total)
dim(cust_mon_total) # 총 568,447건의 거래내용 확인
dim(cust_mon_04)[1] + dim(cust_mon_05)[1] + dim(cust_mon_06)[1] == dim(cust_mon_total)[1] # True 확인
str(cust_mon_total)

cust_mon_total$date <- as.character(cust_mon_total$date)
cust_mon_total$year <- substr(cust_mon_total$date, 1, 4)
cust_mon_total$mon <- substr(cust_mon_total$date, 5, 6)
cust_mon_total$day <- substr(cust_mon_total$date, 7, 8)
head(cust_mon_total)

cust_mon_total$ymd <- paste(cust_mon_total$year, cust_mon_total$mon, cust_mon_total$day, sep='-')
head(cust_mon_total)
cust_mon_total$ymd <- as.Date(cust_mon_total$ymd, format='%Y-%m-%d')
head(cust_mon_total)

cust_mon_total$prod_code <- as.character(cust_mon_total$prod_code)



# joining
head(prod_cate)
head(cust_mon_total)

str(prod_cate)
str(cust_mon_total)

cust_mon_total %>%
  select(date, custid, prod_code) -> cust_total

cust_total %>% left_join(prod_cate, by='prod_code') -> sdf
View(prod_cate)

str(non_prod_code_df)

length(unique(non_prod_code_df$prod_code))

missing_prod_code <- data.frame(missing_prod_code = unique(non_prod_code_df$prod_code))
# View(missing_prod_code)
write.csv(missing_prod_code, './data/missing_prod_code.csv')



