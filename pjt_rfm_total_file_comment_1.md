pjt\_rfm\_total\_file\_comment\_1(Datapreprocessing)
================
TEAM\_1
2018년 10월 8일

``` r
library(readxl)
library(plyr)
library(tidyverse)
```

    ## -- Attaching packages -------------------------------------------------- tidyverse 1.2.1 --

    ## √ ggplot2 2.2.1     √ purrr   0.2.4
    ## √ tibble  1.4.2     √ dplyr   0.7.4
    ## √ tidyr   0.7.2     √ stringr 1.2.0
    ## √ readr   1.1.1     √ forcats 0.3.0

    ## -- Conflicts ----------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::arrange()   masks plyr::arrange()
    ## x purrr::compact()   masks plyr::compact()
    ## x dplyr::count()     masks plyr::count()
    ## x dplyr::failwith()  masks plyr::failwith()
    ## x dplyr::filter()    masks stats::filter()
    ## x dplyr::id()        masks plyr::id()
    ## x dplyr::lag()       masks stats::lag()
    ## x dplyr::mutate()    masks plyr::mutate()
    ## x dplyr::rename()    masks plyr::rename()
    ## x dplyr::summarise() masks plyr::summarise()
    ## x dplyr::summarize() masks plyr::summarize()

``` r
library(data.table)
```

    ## Warning: package 'data.table' was built under R version 3.4.4

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

    ## The following object is masked from 'package:purrr':
    ## 
    ##     transpose

데이터 수집/처리
----------------

-   Raw 데이터는 엑셀('.xlsx') 파일들이며 'cust\_mon\_age\_cate' 문자로 시작한다.
-   이러한 원본 데이터를 자동으로 loading하기 위해 작업 디렉토리 내 동일 문자열을 순환하면서 찾아 파일명을 다음과 같은 코드로 가져올 수 있다.

``` r
# 디렉토리를 순환하면서 파일명 가져오기
temp_file <- list.files(pattern='cust_mon_age_cate.*\\.xlsx$', recursive = TRUE) 
temp_file # 파일명 리스트 확인
```

    ##  [1] "data/cust_mon_age_cate_201601.xlsx"
    ##  [2] "data/cust_mon_age_cate_201602.xlsx"
    ##  [3] "data/cust_mon_age_cate_201603.xlsx"
    ##  [4] "data/cust_mon_age_cate_201604.xlsx"
    ##  [5] "data/cust_mon_age_cate_201605.xlsx"
    ##  [6] "data/cust_mon_age_cate_201606.xlsx"
    ##  [7] "data/cust_mon_age_cate_201607.xlsx"
    ##  [8] "data/cust_mon_age_cate_201608.xlsx"
    ##  [9] "data/cust_mon_age_cate_201609.xlsx"
    ## [10] "data/cust_mon_age_cate_201610.xlsx"
    ## [11] "data/cust_mon_age_cate_201611.xlsx"
    ## [12] "data/cust_mon_age_cate_201612.xlsx"
    ## [13] "data/cust_mon_age_cate_201701.xlsx"
    ## [14] "data/cust_mon_age_cate_201702.xlsx"
    ## [15] "data/cust_mon_age_cate_201703.xlsx"
    ## [16] "data/cust_mon_age_cate_201704.xlsx"
    ## [17] "data/cust_mon_age_cate_201705.xlsx"
    ## [18] "data/cust_mon_age_cate_201706.xlsx"
    ## [19] "data/cust_mon_age_cate_201707.xlsx"
    ## [20] "data/cust_mon_age_cate_201708.xlsx"
    ## [21] "data/cust_mon_age_cate_201709.xlsx"
    ## [22] "data/cust_mon_age_cate_201710.xlsx"
    ## [23] "data/cust_mon_age_cate_201711.xlsx"
    ## [24] "data/cust_mon_age_cate_201712.xlsx"
    ## [25] "data/cust_mon_age_cate_201801.xlsx"
    ## [26] "data/cust_mon_age_cate_201802.xlsx"
    ## [27] "data/cust_mon_age_cate_201803.xlsx"
    ## [28] "data/cust_mon_age_cate_201804.xlsx"
    ## [29] "data/cust_mon_age_cate_201805.xlsx"
    ## [30] "data/cust_mon_age_cate_201806.xlsx"

-   lapply를 이용하여 위의 리스트 파일들을 한 번에 읽어온다.

``` r
df_list <- lapply(temp_file, read_excel)
```

-   총 30개의 파일이 읽혀져서 각각 `df_list`변수에 30개의 원소를 가지는 리스트가 형성된다. 이를 rbindlist() 를 이용하여 하나의 변수로 합친다.

``` r
df_list_rbindlist <- rbindlist(df_list)
df_list_rbindlist
```

    ##              DATE        member_num    grade    GPC on_off QTY   AMT sex
    ##       1: 20160101       60000215450     클럽  94266    off   1 39000  여
    ##       2: 20160101       60000215450     클럽 891017    off   1  1500  여
    ##       3: 20160101       60000315908     클럽  95017    off   1 28000  여
    ##       4: 20160101       60000613827     클럽  98061    off   1 15500  여
    ##       5: 20160101       60000634761     클럽  82478    off   1     0  여
    ##      ---                                                                
    ## 6605811: 20180630 68008184003366492     클럽  75489    off   2 25000  여
    ## 6605812: 20180630 68008191754551740 바디러브  76159    off   1 11000  남
    ## 6605813: 20180630 68008223754830495 바디러브  44937    off   1 16500  남
    ## 6605814: 20180630 68008223754830495 바디러브  47726    off   1 11500  남
    ## 6605815: 20180630 68008223824306555 바디러브  45593    off   1 19500  여
    ##           age         product_name  category category_function
    ##       1:   30     SW AFR BODY BALM      Body       Body_Butter
    ##       2:   30  SPA HEART BOX-BROWN       ETC               ETC
    ##       3:   50    WHITE/M LIBER B/B      Body       Body_Butter
    ##       4:   30     MORINGA MILK B/L      Body       Body_Lotion
    ##       5:   30 WHITE/M B/LOTION 400      Body       Body_Lotion
    ##      ---                                                      
    ## 6605811:   40    WM BATH FOAM XMAS      Body     Body_Cleanser
    ## 6605812: none         WM LEAU MIST      Body         Body_Mist
    ## 6605813: none MORINGA MILK B/L NEW      Body       Body_Lotion
    ## 6605814: none        MORINGA S/GEL      Body     Body_Cleanser
    ## 6605815: none            MANGO EDT Fragrance     Fragrance_EDT
    ##             category_line
    ##       1: SPA OF THE WORLD
    ##       2:             NONE
    ##       3: WHITE MUSK LIBER
    ##       4:          MORINGA
    ##       5:       WHITE MUSK
    ##      ---                 
    ## 6605811:       WHITE MUSK
    ## 6605812:  WHITE MUSK LEAU
    ## 6605813:          MORINGA
    ## 6605814:          MORINGA
    ## 6605815:             NONE

-   변수명의 이름을 다음과 같이 수정한다.

``` r
colnames(df_list_rbindlist) <- c('date', 'custid', 'grade', 'prod_code', 
                                 'on_off', 'qty', 'amt', 'sex', 'age',
                                 'prod_nm', 'cate', 'cate_ftn', 'cate_line')
glimpse(df_list_rbindlist)
```

    ## Observations: 6,605,815
    ## Variables: 13
    ## $ date      <dbl> 20160101, 20160101, 20160101, 20160101, 20160101, 20...
    ## $ custid    <chr> "60000215450", "60000215450", "60000315908", "600006...
    ## $ grade     <chr> "클럽", "클럽", "클럽", "클럽", "클럽", "클럽", "클럽", "클럽", "클럽"...
    ## $ prod_code <dbl> 94266, 891017, 95017, 98061, 82478, 24728, 25278, 85...
    ## $ on_off    <chr> "off", "off", "off", "off", "off", "off", "off", "of...
    ## $ qty       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1...
    ## $ amt       <dbl> 39000, 1500, 28000, 15500, 0, 24600, 36000, 8000, 70...
    ## $ sex       <chr> "여", "여", "여", "여", "여", "여", "여", "여", "여", "여", "여...
    ## $ age       <chr> "30", "30", "50", "30", "30", "30", "30", "20", "20"...
    ## $ prod_nm   <chr> "SW AFR BODY BALM", "SPA HEART BOX-BROWN", "WHITE/M ...
    ## $ cate      <chr> "Body", "ETC", "Body", "Body", "Body", "Skin", "Skin...
    ## $ cate_ftn  <chr> "Body_Butter", "ETC", "Body_Butter", "Body_Lotion", ...
    ## $ cate_line <chr> "SPA OF THE WORLD", "NONE", "WHITE MUSK LIBER", "MOR...

-   변수명에 대한 설명은 모두 아는 내용이므로 생략한다. 추후 보안을 위해 custid, grade, prod\_nm, cate, cate\_ftn, cate\_line 등의 변수는 모두 코드화 하여 별도의 테이블로 유지하는 것이 좋을 거라 생각한다.

-   위의 수정된 결과를 `cust_prod_total` 이라는 변수명으로 바꾸어 저장한다.

``` r
cust_prod_total <- df_list_rbindlist
nrow(cust_prod_total)
```

    ## [1] 6605815

-   추후 분석과 보안을 위해 등급을 영문명칭으로 치환한다.

``` r
# 고객등급명을 영문으로 변경
unique(cust_prod_total$grade)
```

    ## [1] "클럽"     "바디러브" "골드"

``` r
cust_prod_total$grade[cust_prod_total$grade == '바디러브'] <- 'love'
cust_prod_total$grade[cust_prod_total$grade == '클럽'] <- 'club'
cust_prod_total$grade[cust_prod_total$grade == '골드'] <- 'gold'
```

-   영문으로 전환된 회원등급의 자료형을 `factor`형으로 변환한다.

``` r
### 회원등급, 온/오프라인, 성별, 연령대 -> 팩터로 변환 -> 시각화
cust_prod_total$grade <- as.factor(cust_prod_total$grade)
summary(cust_prod_total$grade)
```

    ##    club    gold    love 
    ## 1151429  427132 5027254

``` r
# 클럽 115만, 골드 42만, 러브 502만 
```

-   `cust_prod_total` 변수는 총 6,605,815 건의 데이터이다. 이는 2016.1월~2018.6월간 A샵의 거래건수이다. 이렇게 처리된 데이터를 향후 쉽게 loading 하기 위해 csv 파일로 저장해 둔다.
-   `.csv`, `.xlsx`, `.Rdata` 등의 데이터 파일은 working directory에 별도의 `data` 폴더를 두어 저장해 두는 것이 관리해 용이하으로 데이터를 저장하는 경로명에는 항상 `./data/~` 경로명을 추가하도록 한다.

``` r
write.csv(cust_prod_total, './data/cust_prod_total.csv')
```

-   이렇게 저장된 거래 데이터는 `fread()` 함수와 data.table package를 이용해 비교적 빠른 시간에 loading 할 수 있다.

``` r
cust_prod_total <- fread('./data/cust_prod_total.csv', data.table = T)
head(cust_prod_total)
```

    ##    V1     date      custid grade prod_code on_off qty   amt sex age
    ## 1:  1 20160101 60000215450  club     94266    off   1 39000  여  30
    ## 2:  2 20160101 60000215450  club    891017    off   1  1500  여  30
    ## 3:  3 20160101 60000315908  club     95017    off   1 28000  여  50
    ## 4:  4 20160101 60000613827  club     98061    off   1 15500  여  30
    ## 5:  5 20160101 60000634761  club     82478    off   1     0  여  30
    ## 6:  6 20160101 60000863294  club     24728    off   1 24600  여  30
    ##                 prod_nm cate      cate_ftn        cate_line
    ## 1:     SW AFR BODY BALM Body   Body_Butter SPA OF THE WORLD
    ## 2:  SPA HEART BOX-BROWN  ETC           ETC             NONE
    ## 3:    WHITE/M LIBER B/B Body   Body_Butter WHITE MUSK LIBER
    ## 4:     MORINGA MILK B/L Body   Body_Lotion          MORINGA
    ## 5: WHITE/M B/LOTION 400 Body   Body_Lotion       WHITE MUSK
    ## 6:  DOY EYE CONCENTRATE Skin Skin_Eyecream   DROPS OF YOUTH

-   좀 더 적은 저장공간과 빠른 loading을 위해 cust\_prod\_total 변수를 `.RData` 로 저장하는 방안을 생각할 수 있다. RData로 저장하는 코드는 다음과 같다.

``` r
save(cust_prod_total, file='./data/cust_prod_total_raw.RData')
```

-   이렿게 저장된 `.RData` 파일은 다음과 같이 작업중인 R Session 내 전역변수로 loading 할 수 있다. 이 때의 변수 명칭은 `cust_prod_total`이다.

``` r
load('./data/cust_prod_total_raw.RData'); head(cust_prod_total)
```

    ##    V1     date      custid grade prod_code on_off qty   amt sex age
    ## 1:  1 20160101 60000215450  club     94266    off   1 39000  여  30
    ## 2:  2 20160101 60000215450  club    891017    off   1  1500  여  30
    ## 3:  3 20160101 60000315908  club     95017    off   1 28000  여  50
    ## 4:  4 20160101 60000613827  club     98061    off   1 15500  여  30
    ## 5:  5 20160101 60000634761  club     82478    off   1     0  여  30
    ## 6:  6 20160101 60000863294  club     24728    off   1 24600  여  30
    ##                 prod_nm cate      cate_ftn        cate_line
    ## 1:     SW AFR BODY BALM Body   Body_Butter SPA OF THE WORLD
    ## 2:  SPA HEART BOX-BROWN  ETC           ETC             NONE
    ## 3:    WHITE/M LIBER B/B Body   Body_Butter WHITE MUSK LIBER
    ## 4:     MORINGA MILK B/L Body   Body_Lotion          MORINGA
    ## 5: WHITE/M B/LOTION 400 Body   Body_Lotion       WHITE MUSK
    ## 6:  DOY EYE CONCENTRATE Skin Skin_Eyecream   DROPS OF YOUTH
