#####################################
#####  임대료 데이터 분석 코드  #####
#####################################

library(dplyr)

##데이터 불러오기

rent <- read.csv("(서울열린데이터광장)한국부동산원_소규모상가임대료(분기별,시도별)_20201029.csv",header=T)
str(rent)  # 229 obs. of  4 variables



##결측치 확인

colSums(is.na(rent))  # NA : X



##'서울'자료만 모아서 서울 데이터셋 생성

rent$시도명 <- substr(rent$지역,1,2)
rent_seoul <- subset(rent,시도명=="서울")
rent_seoul <- rent_seoul[,1:4]
str(rent_seoul)  # 54 obs. of  4 variables

rent_seoul
# 서울 도심, 서울 강남, 서울 영등포신촌, 서울 기타 (4부분으로 나눠져 있음)

# 전체, 서울 도심, 서울 강남, 서울 영등포신촌, 서울 기타 자료 삭제
rent_seoul <- rent_seoul[-c(1,2,11,21,28),]
nrow(rent_seoul)  # 49

#지역명 앞에 붙은 '서울 도심, 서울 강남, 서울 영등포신촌, 서울 기타' 글자 삭제
rent_seoul$지역 <- gsub("서울 도심","",rent_seoul$지역)
rent_seoul$지역 <- gsub("서울 강남","",rent_seoul$지역)
rent_seoul$지역 <- gsub("서울 영등포신촌","",rent_seoul$지역)
rent_seoul$지역 <- gsub("서울 기타","",rent_seoul$지역)


#1, 2, 3분기 임대료에 대한 평균값을 나타내는 meanRent 변수 생성
rent_seoul$meanRent <- apply(rent_seoul[,2:4],1,mean)

#meanRent를 기준으로 자료 재정렬 후, meanRent변수 삭제
rent_seoul <- rent_seoul %>% arrange(meanRent)
rent_seoul <- rent_seoul[,-5]



## k-means clustering

library(caret)
library(cluster)
set.seed(2021)

rent_hc <- hclust(dist(rent_seoul[,2:4]), method="average")
plot(rent_hc, hang=-1)
plot(silhouette(cutree(rent_hc,k=3), dist=dist(rent_seoul[,2:4]) ))


#최대 Average silhouette width 값 찾기

library(fpc)

clust <- numeric(20)

for(k in 2:20){
  clust[[k]] <- pam(rent_seoul[,2:4], k) $ silinfo $ avg.width
  k.best <- which.max(clust)
}
cat("최적의 실루엣 값을 갖는 클러스터 개수: ", k.best)  # 10개


# 클러스터 10개로 그룹화

rent_km10 <- kmeans(rent_seoul[,2:4], centers=10)
rent_km10

plot(silhouette(cutree(rent_hc,k=10), dist=dist(rent_seoul[,2:4]) ))

rent_seoul$cluster <- rent_km10$cluster


# 클러스터 순서 변경

rent_seoul[which(rent_seoul$cluster==9),]$cluster <- 11
rent_seoul[which(rent_seoul$cluster==6),]$cluster <- 12
rent_seoul[which(rent_seoul$cluster==1),]$cluster <- 13
rent_seoul[which(rent_seoul$cluster==5),]$cluster <- 14
rent_seoul[which(rent_seoul$cluster==2),]$cluster <- 15
rent_seoul[which(rent_seoul$cluster==4),]$cluster <- 16
rent_seoul[which(rent_seoul$cluster==8),]$cluster <- 17
rent_seoul[which(rent_seoul$cluster==10),]$cluster <- 18
rent_seoul[which(rent_seoul$cluster==3),]$cluster <- 19
rent_seoul[which(rent_seoul$cluster==7),]$cluster <- 20
rent_seoul$cluster <- rent_seoul$cluster - 10

#지역 변수명을 '상권'으로 변경
colnames(rent_seoul)[1] <- "상권"



## 행정동 자료와 결합

#데이터 불러오기
adstr <- read.csv("임대료_행정동그룹.csv",header=T)
str(adstr)  #425 obs. of  5 variables
#"임대료_행정동그룹.csv" : 행정동별로 해당하는 상권을 입력한 데이터셋


#'상권'을 기준으로 행정동자료와 임대료자료 결합
aa <- merge(adstr,rent_seoul,by="상권",all=T)

#임대료 값 삭제
aa <- aa[,-c(6:8)] 

#cluster값이 NA이면 0으로 맵핑
aa[which(is.na(aa$cluster)==T),"cluster"] <- 0

# 변수 순서 재정렬
aa <- aa[,c(2,3,4,5,1,6)]

# csv파일 저장
write.csv(aa,"임대료_그룹_최종(0814수정).csv",row.names=F)