load('cust_recommender_list.RData')
head(cust_recommender_list)

# cluster 1----
tracing %>%
  filter(cluster == 1) %>%
  sample_n(10) -> cluster2.sample10

tracing_sample <- cluster1.sample10; tracing_sample
sample_custid <- tracing_sample$custid 
sample_custid

# [1] "C01009953" "C01105454" "C01001326" "C00459250" "C01154852" "C00126941"
# [7] "C00613290" "C01121042" "C01011540" "C00274849"

cust_recommender_list %>%
  filter(tmp_custid == "C01009953")

#   tmp_custid                                      tmp_rec_matrix
# 1  C01009953 :: WM EDT 100 NEW, WM EDT 30 RENO, WHITE/M F/MIST RENO

# 최근 12~ 3개월 이내 구매 시작

# 매출기여도 22.2% 집단
# 등급하락 시점 이전 개인화 푸시 필요
# 개인별 구매주기 파악
# 구매  객단가 상승에 필요한 프로모션 쿠폰 발급 검토


# cluster 2----
tracing %>%
  filter(cluster == 2) %>%
  sample_n(10) -> cluster2.sample10

tracing_sample <- cluster2.sample10; tracing_sample
sample_custid <- tracing_sample$custid 
sample_custid

# [1] "C00675435" "C00667673" "C00858845" "C00269327" "C00944282" "C00961701"
# [7] "C00066080" "C00469258" "C00023656" "C00360627"

cust_recommender_list %>%
  filter(tmp_custid == "C00675435")

#    tmp_custid                                          tmp_rec_matrix
# 1  C00675435 MINI BATH LILY-PINK, PINK/G S/GEL NEW, B/ROSE EDT 100ML

# 최근 1년이내 구매, 객단가  상위

# *매출기여도 28.2% 집단
# 최우수 고객집단으로 상향되기 위한 추천서비스, 적용 필요

# cluster 3----
tracing %>%
  filter(cluster == 3) %>%
  sample_n(10) -> cluster3.sample10

tracing_sample <- cluster3.sample10; tracing_sample
sample_custid <- tracing_sample$custid 
sample_custid

# [1] "C00525128" "C00379367" "C00066945" "C00312430" "C00788811" "C01236978"
# [7] "C00471104" "C00139965" "C00556299" "C00193926"

cust_recommender_list %>%
  filter(tmp_custid == "C00525128")

# tmp_custid                                        tmp_rec_matrix
# 1  C00525128 TEA TREE OIL NEW, TEA TREE TONER NEW, TEA TREE OIL 20

# cluster 4----
tracing %>%
  filter(cluster == 4) %>%
  sample_n(10) -> cluster4.sample10

tracing_sample <- cluster4.sample10; tracing_sample
sample_custid <- tracing_sample$custid 
sample_custid

# [1] "C01171426" "C00549462" "C00435585" "C00218965" "C01167834" "C00227698"
# [7] "C01173886" "C00183575" "C00180278" "C00144801"

cust_recommender_list %>%
  filter(tmp_custid == "C01171426")

# tmp_custid                                              tmp_rec_matrix
# 1  C01171426 COTTON WOOL PADS ORG, DOY CONCENTRATE 50, SKIN DEFENCE 60ML

# cluster 5----
tracing %>%
  filter(cluster == 5) %>%
  sample_n(10) -> cluster5.sample10

tracing_sample <- cluster5.sample10; tracing_sample
sample_custid <- tracing_sample$custid 
sample_custid

# [1] "C01006935" "C00468360" "C00043101" "C00482527" "C01134508" "C00057139"
# [7] "C01042038" "C00436054" "C00318484" "C00205248"

cust_recommender_list %>%
  filter(tmp_custid == "C01006935")

# tmp_custid                                        tmp_rec_matrix
# 1  C01006935 SKIN DEFENCE 60ML, DOL SERUM 30ML, DOL DAY CREAM 50ML

# cluser 5----
tracing %>%
  filter(cluster == 4) %>%
  sample_n(10) -> cluster4.sample10

tracing_sample <- cluster4.sample10; tracing_sample
sample_custid <- tracing_sample$custid 
sample_custid

# [1] "C01171426" "C00549462" "C00435585" "C00218965" "C01167834" "C00227698"
# [7] "C01173886" "C00183575" "C00180278" "C00144801"

cust_recommender_list %>%
  filter(tmp_custid == "C01171426")

# tmp_custid                                              tmp_rec_matrix
# 1  C01171426 COTTON WOOL PADS ORG, DOY CONCENTRATE 50, SKIN DEFENCE 60ML

# cluster 6----
tracing %>%
  filter(cluster == 6) %>%
  sample_n(10) -> cluster6.sample10

tracing_sample <- cluster6.sample10; tracing_sample
sample_custid <- tracing_sample$custid 
sample_custid

# [1] "C00410319" "C00208035" "C00175767" "C00534328" "C00053534" "C00814658"
# [7] "C00138106" "C01237433" "C00164916" "C00287180"

cust_recommender_list %>%
  filter(tmp_custid == "C00410319")

# tmp_custid                                            tmp_rec_matrix
# 1  C00410319 MORINGA B/B 400 NEW, MORINGA B/B 200, MORINGA B/B 200 NEW

# cluster 7----
tracing %>%
  filter(cluster == 7) %>%
  sample_n(10) -> cluster7.sample10

tracing_sample <- cluster7.sample10; tracing_sample
sample_custid <- tracing_sample$custid 
sample_custid

# [1] "C01182319" "C01241438" "C00392609" "C00188463" "C00854612" "C00817011"
# [7] "C01186374" "C01331187" "C01234182" "C00613592"

cust_recommender_list %>%
  filter(tmp_custid == "C01182319")

# tmp_custid                                   tmp_rec_matrix
# 1  C01182319 WM B/L 400 RENO, WM S/G 400 RENO, WM S/G 400 NEW

# cluster 8----
tracing %>%
  filter(cluster == 8) %>%
  sample_n(10) -> cluster8.sample10

tracing_sample <- cluster8.sample10; tracing_sample
sample_custid <- tracing_sample$custid 
sample_custid

# [1] "C00442063" "C00391316" "C00732346" "C00724420" "C00495735" "C01295093"
# [7] "C00894717" "C00199809" "C00640172" "C01212504"

cust_recommender_list %>%
  filter(tmp_custid == "C00442063")

# tmp_custid                                          tmp_rec_matrix
# 1  C00442063 WHITE/M DEODORANT, WHITE/M F/MIST RENO, WM S/G 400 RENO

# cluster 9----
tracing %>%
  filter(cluster == 9) %>%
  sample_n(10) -> cluster9.sample10

tracing_sample <- cluster9.sample10; tracing_sample
sample_custid <- tracing_sample$custid 
sample_custid

# [1] "C00463418" "C01139002" "C00604353" "C00642521" "C00922152" "C00500721"
# [7] "C01301226" "C00313680" "C00478534" "C00348053"

cust_recommender_list %>%
  filter(tmp_custid == "C00463418")

# tmp_custid                                            tmp_rec_matrix
# 1  C00463418 MORINGA MILK B/L NEW, MORINGA S/GEL 750, FUJI G/T SHAMPOO

# cluster 10----
tracing %>%
  filter(cluster == 10) %>%
  sample_n(10) -> cluster10.sample10

tracing_sample <- cluster10.sample10; tracing_sample
sample_custid <- tracing_sample$custid 
sample_custid

# [1] "C00606199" "C00792338" "C00533022" "C01312876" "C00545820" "C00406546"
# [7] "C00209300" "C00071546" "C01005701" "C00214620"

cust_recommender_list %>%
  filter(tmp_custid == "C00606199")

# tmp_custid                                                tmp_rec_matrix
# 1  C00606199 1712 E HONEY/SG 1+1, 1701 E RAS/GEL 1+1, 1712 E PIN/BB 1+1SET



