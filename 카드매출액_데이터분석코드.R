#######################################
#####  신한카드 데이터 분석 코드  #####
#######################################


library(readxl)
library(dplyr)


##데이터 불러오기

shinhan_card <- read_excel("금융데이터거래소_데이터경진대회_신한카드.xlsx")
shinhan_card <- as.data.frame(shinhan_card)
str(shinhan_card)    # 23892 obs. of  12 variables



##'서울' 자료만 추출하여 데이터셋 생성

shinhan_seoul <- subset(shinhan_card, 광역시도명=="서울")
str(shinhan_seoul)   # 1391 obs. of  12 variables



##결측치(NA) 처리

colSums(is.na(shinhan_seoul))  
#업종대분류, 업종중분류, 업종소분류에 각각 4개씩 결측치 존재

shinhan_seoul[is.na(shinhan_seoul$업종대분류),]  #업종대분류가 결측치인 행 추출
#결측치가 있는 행이 동일한 행이라는 것 확인 (모든 기준년월에 대해 존재)

apply(subset(shinhan_seoul[!is.na(shinhan_seoul$업종대분류),],기준년월=="201903")[,6:12],2,sum)
apply(subset(shinhan_seoul[!is.na(shinhan_seoul$업종대분류),],기준년월=="201909")[,6:12],2,sum)
apply(subset(shinhan_seoul[!is.na(shinhan_seoul$업종대분류),],기준년월=="202003")[,6:12],2,sum)
apply(subset(shinhan_seoul[!is.na(shinhan_seoul$업종대분류),],기준년월=="202009")[,6:12],2,sum)
#모든 기준년월에 대한 총 합계인지 확인했지만 아님 + median, mean도 아님
#따라서 결측치가 있는 행 삭제하기로 결정

#결측치가 있는 행 모두 삭제
shinhan_seoul_2 <- shinhan_seoul[!is.na(shinhan_seoul$업종대분류),]
colSums(is.na(shinhan_seoul_2))  #결측치 없음 확인

str(shinhan_seoul_2)  # 1387 obs. of  12 variables



##데이터 탐색

unique(shinhan_seoul_2$기준년월)            #"201903" "201909" "202003" "202009"
unique(shinhan_seoul_2$광역시도명)          #"서울"
unique(shinhan_seoul_2$업종대분류)          #"기타" "문화레져" "생활서비스" "음식" "일반유통" "전문서비스" "종합유통"
length(unique(shinhan_seoul_2$업종중분류))  #97
length(unique(shinhan_seoul_2$업종소분류))  #357

#수치형 변수들에 대해 박스플랏 그리기
boxplot(shinhan_seoul_2$신규가맹점수~shinhan_seoul_2$기준년월,ylab="신규가맹점수",xlab="기준년월")
boxplot(shinhan_seoul_2$해지가맹점수~shinhan_seoul_2$기준년월,ylab="해지가맹점수",xlab="기준년월")
boxplot(shinhan_seoul_2$매출가맹점수~shinhan_seoul_2$기준년월,ylab="매출가맹점수",xlab="기준년월")
boxplot(shinhan_seoul_2$카드매출금액~shinhan_seoul_2$기준년월,ylab="카드매출금액",xlab="기준년월")
boxplot(shinhan_seoul_2$카드매출건수~shinhan_seoul_2$기준년월,ylab="카드매출건수",xlab="기준년월")
boxplot(shinhan_seoul_2$점당매출금액~shinhan_seoul_2$기준년월,ylab="점당매출금액",xlab="기준년월")
boxplot(shinhan_seoul_2$건당매출금액~shinhan_seoul_2$기준년월,ylab="건당매출금액",xlab="기준년월")
#카드매출금액, 카드매출건수, 점당매출금액, 건당매출금액 등에서 음수값이 존재함을 확인



##카드매출금액이 음수인 경우

subset(shinhan_seoul_2, 카드매출금액 < 0)  #행 3개

#1. 2020년 3월 박물관/전시관 데이터
subset(shinhan_seoul_2, 업종소분류 == "박물관/전시관" )
#카드매출금액, 점당매출금액, 건당매출금액의 부호가 (-)로 잘못 들어가 있는 것 같다고 판단 => 부호를 (+)로 변경

shinhan_seoul_2[which( shinhan_seoul_2$기준년월 == 202003 & shinhan_seoul_2$업종소분류 == "박물관/전시관"),"카드매출금액"] <- 
    shinhan_seoul_2[which( shinhan_seoul_2$기준년월 == 202003 & shinhan_seoul_2$업종소분류 == "박물관/전시관"),"카드매출금액"] * (-1)
shinhan_seoul_2[which( shinhan_seoul_2$기준년월 == 202003 & shinhan_seoul_2$업종소분류 == "박물관/전시관"),"점당매출금액"] <- 
    shinhan_seoul_2[which( shinhan_seoul_2$기준년월 == 202003 & shinhan_seoul_2$업종소분류 == "박물관/전시관"),"점당매출금액"] * (-1)
shinhan_seoul_2[which( shinhan_seoul_2$기준년월 == 202003 & shinhan_seoul_2$업종소분류 == "박물관/전시관"),"건당매출금액"] <- 
    shinhan_seoul_2[which( shinhan_seoul_2$기준년월 == 202003 & shinhan_seoul_2$업종소분류 == "박물관/전시관"),"건당매출금액"] * (-1)

#2. 2020년 3월 항공사 데이터
subset(shinhan_seoul_2, 업종소분류 == "항공사" )
#카드매출금액, 점당매출금액, 건당매출금액의 부호가 (-)로 잘못 들어가 있는 것 같다고 판단 => 부호를 (+)로 변경

shinhan_seoul_2[which( shinhan_seoul_2$기준년월 == 202003 & shinhan_seoul_2$업종소분류 == "항공사"),"카드매출금액"] <- 
    shinhan_seoul_2[which( shinhan_seoul_2$기준년월 == 202003 & shinhan_seoul_2$업종소분류 == "항공사"),"카드매출금액"] * (-1)
shinhan_seoul_2[which( shinhan_seoul_2$기준년월 == 202003 & shinhan_seoul_2$업종소분류 == "항공사"),"점당매출금액"] <- 
    shinhan_seoul_2[which( shinhan_seoul_2$기준년월 == 202003 & shinhan_seoul_2$업종소분류 == "항공사"),"점당매출금액"] * (-1)
shinhan_seoul_2[which( shinhan_seoul_2$기준년월 == 202003 & shinhan_seoul_2$업종소분류 == "항공사"),"건당매출금액"] <- 
    shinhan_seoul_2[which( shinhan_seoul_2$기준년월 == 202003 & shinhan_seoul_2$업종소분류 == "항공사"),"건당매출금액"] * (-1)

#3. 2020년 3월 금융-기타 데이터
subset(shinhan_seoul_2, 업종소분류 == "금융-기타" )
#카드매출금액, 카드매출건수의 부호가 (-)로 잘못 들어가 있는 것으로 판단 => 부호를 (+)로 변경
#매출가맹점수가 0인 것이 데이터 오류라고 판단 => 직전, 직후 자료와 카드매출건수가 1임을 근거로 매출가맹점수를 1로 수정

shinhan_seoul_2[which( shinhan_seoul_2$기준년월 == 202003 & shinhan_seoul_2$업종소분류 == "금융-기타"),"카드매출금액"] <- 
    shinhan_seoul_2[which( shinhan_seoul_2$기준년월 == 202003 & shinhan_seoul_2$업종소분류 == "금융-기타"),"카드매출금액"] * (-1)
shinhan_seoul_2[which( shinhan_seoul_2$기준년월 == 202003 & shinhan_seoul_2$업종소분류 == "금융-기타"),"카드매출건수"] <- 
    shinhan_seoul_2[which( shinhan_seoul_2$기준년월 == 202003 & shinhan_seoul_2$업종소분류 == "금융-기타"),"카드매출건수"] * (-1)
shinhan_seoul_2[which( shinhan_seoul_2$기준년월 == 202003 & shinhan_seoul_2$업종소분류 == "금융-기타"),"매출가맹점수"] <- 1
shinhan_seoul_2[which( shinhan_seoul_2$기준년월 == 202003 & shinhan_seoul_2$업종소분류 == "금융-기타"),"점당매출금액"] <- 
    shinhan_seoul_2[which( shinhan_seoul_2$기준년월 == 202003 & shinhan_seoul_2$업종소분류 == "금융-기타"),"카드매출금액"]

nrow(subset(shinhan_seoul_2, 카드매출금액 < 0))  #0
#카드매출금액이 음수인 행 없음을 확인



##업종 변수 생성

shinhan_seoul_2$업종 <- paste0(shinhan_seoul_2$업종대분류,">",shinhan_seoul_2$업종중분류,">",shinhan_seoul_2$업종소분류)
head(shinhan_seoul_2$업종,1)   #"기타>자사가맹점>자사가맹점"



##데이터 오류 처리

#카드매출금액 = 매출가맹점수x점당매출금액 = 카드매출건수x건당매출금액
#위 식이 성립해야 하는데 성리바지 않는 행들이 존재, 총 5가지 경우로 나눠서 데이터를 살펴봄


#1. 모두 같은 경우
#카드매출금액 == 카드매출건수x건당매출금액
#카드매출금액 == 매출가맹점수x점당매출금액
#카드매출건수x건당매출금액 == 매출가맹점수x점당매출금액

s <- 0
for(i in 1:nrow(shinhan_seoul_2)){
  if(shinhan_seoul_2[i,]$카드매출금액 == round(shinhan_seoul_2[i,]$카드매출건수*shinhan_seoul_2[i,]$건당매출금액,-3) &
     shinhan_seoul_2[i,]$카드매출금액 == round(shinhan_seoul_2[i,]$매출가맹점수*shinhan_seoul_2[i,]$점당매출금액,-3) &
     round(shinhan_seoul_2[i,]$카드매출건수*shinhan_seoul_2[i,]$건당매출금액,-3) == round(shinhan_seoul_2[i,]$매출가맹점수*shinhan_seoul_2[i,]$점당매출금액,-3)
  ){
    s <- s+1
  }
}
s  #461

part1 <- subset(shinhan_seoul_2,shinhan_seoul_2$카드매출금액 == round(shinhan_seoul_2$카드매출건수*shinhan_seoul_2$건당매출금액,-3) &
                  shinhan_seoul_2$카드매출금액 == round(shinhan_seoul_2$매출가맹점수*shinhan_seoul_2$점당매출금액,-3) &
                  round(shinhan_seoul_2$카드매출건수*shinhan_seoul_2$건당매출금액,-3) == round(shinhan_seoul_2$매출가맹점수*shinhan_seoul_2$점당매출금액,-3))
head(part1[,6:12])
#-> 그대로 사용


#2.
#카드매출금액 != 카드매출건수x건당매출금액
#카드매출금액 != 매출가맹점수x점당매출금액
#카드매출건수x건당매출금액 == 매출가맹점수x점당매출금액

s <- 0
for(i in 1:nrow(shinhan_seoul_2)){
  if(shinhan_seoul_2[i,]$카드매출금액 != round(shinhan_seoul_2[i,]$카드매출건수*shinhan_seoul_2[i,]$건당매출금액,-3) &
     shinhan_seoul_2[i,]$카드매출금액 != round(shinhan_seoul_2[i,]$매출가맹점수*shinhan_seoul_2[i,]$점당매출금액,-3) &
     round(shinhan_seoul_2[i,]$카드매출건수*shinhan_seoul_2[i,]$건당매출금액,-3) == round(shinhan_seoul_2[i,]$매출가맹점수*shinhan_seoul_2[i,]$점당매출금액,-3)
  ){
    s <- s+1
  }
}
s  #12

part2 <- subset(shinhan_seoul_2,shinhan_seoul_2$카드매출금액 != round(shinhan_seoul_2$카드매출건수*shinhan_seoul_2$건당매출금액,-3) &
                  shinhan_seoul_2$카드매출금액 != round(shinhan_seoul_2$매출가맹점수*shinhan_seoul_2$점당매출금액,-3) &
                  round(shinhan_seoul_2$카드매출건수*shinhan_seoul_2$건당매출금액,-3) == round(shinhan_seoul_2$매출가맹점수*shinhan_seoul_2$점당매출금액,-3))
part2[,6:12]
#-> '카드매출금액'을 'round(카드매출건수x건당매출금액,-3)'으로 대체하기로 결정

part2$카드매출금액 <- round(part2$카드매출건수*part2$건당매출금액, -3)
all.equal(part2$카드매출금액, round(part2$카드매출건수*part2$건당매출금액, -3))  #TRUE


#3.
#카드매출금액 != 카드매출건수x건당매출금액
#카드매출금액 == 매출가맹점수x점당매출금액
#카드매출건수x건당매출금액 != 매출가맹점수x점당매출금액

s <- 0
for(i in 1:nrow(shinhan_seoul_2)){
  if(shinhan_seoul_2[i,]$카드매출금액 != round(shinhan_seoul_2[i,]$카드매출건수*shinhan_seoul_2[i,]$건당매출금액,-3) &
     shinhan_seoul_2[i,]$카드매출금액 == round(shinhan_seoul_2[i,]$매출가맹점수*shinhan_seoul_2[i,]$점당매출금액,-3) &
     round(shinhan_seoul_2[i,]$카드매출건수*shinhan_seoul_2[i,]$건당매출금액,-3) != round(shinhan_seoul_2[i,]$매출가맹점수*shinhan_seoul_2[i,]$점당매출금액,-3)
  ){
    s <- s+1
  }
}
s  #713

part3 <- subset(shinhan_seoul_2,shinhan_seoul_2$카드매출금액 != round(shinhan_seoul_2$카드매출건수*shinhan_seoul_2$건당매출금액,-3) &
                  shinhan_seoul_2$카드매출금액 == round(shinhan_seoul_2$매출가맹점수*shinhan_seoul_2$점당매출금액,-3) &
                  round(shinhan_seoul_2$카드매출건수*shinhan_seoul_2$건당매출금액,-3) != round(shinhan_seoul_2$매출가맹점수*shinhan_seoul_2$점당매출금액,-3))
head(part3[,6:12])
#-> '건당매출금액'을 'round(매출가맹점수x점당매출금액/카드매출건수)'로 대체

part3$건당매출금액 <-round(part3$매출가맹점수*part3$점당매출금액/part3$카드매출건수)
all.equal(part3$카드매출금액, round(part3$카드매출건수*part3$건당매출금액, -3)) #"Mean relative difference: 5.768057e-06"
#건당매출금액을 반올림하는 과정에서 rounding error 발생한 것으로 보임
#그러나 매우 작은 오차이고, 나머지 수치형 자료도 소수점 없이 반올림한 값이므로 데이터 통일성을 위해 round한 값으로 진행


#4.
#카드매출금액 == 카드매출건수x건당매출금액
#카드매출금액 != 매출가맹점수x점당매출금액
#카드매출건수x건당매출금액 != 매출가맹점수x점당매출금액

s <- 0
for(i in 1:nrow(shinhan_seoul_2)){
  if(shinhan_seoul_2[i,]$카드매출금액 == round(shinhan_seoul_2[i,]$카드매출건수*shinhan_seoul_2[i,]$건당매출금액,-3) &
     shinhan_seoul_2[i,]$카드매출금액 != round(shinhan_seoul_2[i,]$매출가맹점수*shinhan_seoul_2[i,]$점당매출금액,-3) &
     round(shinhan_seoul_2[i,]$카드매출건수*shinhan_seoul_2[i,]$건당매출금액,-3) != round(shinhan_seoul_2[i,]$매출가맹점수*shinhan_seoul_2[i,]$점당매출금액,-3)
  ){
    s <- s+1
  }
}
s  #9

part4 <-subset(shinhan_seoul_2,shinhan_seoul_2$카드매출금액 == round(shinhan_seoul_2$카드매출건수*shinhan_seoul_2$건당매출금액,-3) &
                 shinhan_seoul_2$카드매출금액 != round(shinhan_seoul_2$매출가맹점수*shinhan_seoul_2$점당매출금액,-3) &
                 round(shinhan_seoul_2$카드매출건수*shinhan_seoul_2$건당매출금액,-3) != round(shinhan_seoul_2$매출가맹점수*shinhan_seoul_2$점당매출금액,-3))
part4[,6:12]
#-> '점당매출금액'을 'round(카드매출건수x건당매출금액/매출가맹점수)'로 대체

part4$점당매출금액 <- round(part4$카드매출건수*part4$건당매출금액/part4$매출가맹점수)
all.equal(part4$카드매출금액, round(part4$매출가맹점수*part4$점당매출금액, -3))   #"Mean relative difference: 3.227517e-07"

#점당매출금액을 반올림하는 과정에서 rounding error 발생한 것으로 보임
#그러나 매우 작은 오차이고, 나머지 수치형 자료도 소수점 없이 반올림한 값이므로 데이터 통일성을 위해 round한 값으로 진행


#5.
#카드매출금액 != 카드매출건수 x 건당매출금액
#카드매출금액 != 매출가맹점수 x 점당매출금액
#카드매출건수 x 건당매출금액 != 매출가맹점수 x 점당매출금액

s <- 0
for(i in 1:nrow(shinhan_seoul_2)){
  if(shinhan_seoul_2[i,]$카드매출금액 != round(shinhan_seoul_2[i,]$카드매출건수*shinhan_seoul_2[i,]$건당매출금액,-3) &
     shinhan_seoul_2[i,]$카드매출금액 != round(shinhan_seoul_2[i,]$매출가맹점수*shinhan_seoul_2[i,]$점당매출금액,-3) &
     round(shinhan_seoul_2[i,]$카드매출건수*shinhan_seoul_2[i,]$건당매출금액,-3) != round(shinhan_seoul_2[i,]$매출가맹점수*shinhan_seoul_2[i,]$점당매출금액,-3)
  ){
    s <- s+1
  }
}
s  #192

part5 <- subset(shinhan_seoul_2,shinhan_seoul_2$카드매출금액 != round(shinhan_seoul_2$카드매출건수*shinhan_seoul_2$건당매출금액,-3) &
                  shinhan_seoul_2$카드매출금액 != round(shinhan_seoul_2$매출가맹점수*shinhan_seoul_2$점당매출금액,-3) &
                  round(shinhan_seoul_2$카드매출건수*shinhan_seoul_2$건당매출금액,-3) != round(shinhan_seoul_2$매출가맹점수*shinhan_seoul_2$점당매출금액,-3))
head(part5[,6:12])

#파생 변수 생성
part5$가맹점수X점당금액 <- part5$매출가맹점수*part5$점당매출금액
part5$매출건수X건당금액 <- part5$카드매출건수*part5$건당매출금액

#카드매출금액, 가맹점수x점당금액, 매출건수x건당금액 중 두 값의 차이 변수 생성
part5$diff_매출액_가맹점 <- abs(part5$가맹점수X점당금액-part5$카드매출금액)
part5$diff_가맹점_매출건 <- abs(part5$가맹점수X점당금액-part5$매출건수X건당금액)
part5$diff_매출액_매출건 <- abs(part5$매출건수X건당금액-part5$카드매출금액)

#diff_...변수들 중 가장 작은 차이값을 저장하는 변수 생성
part5$min_diff <- apply(part5[,16:18],1,min)


#세 값의 차이를 각각 비교했을 때

#1) '카드매출금액'과 '카드매출건수 x 건당매출금액'의 차이가 가장 작은 경우

part5_1 <- subset(part5, min_diff==diff_매출액_매출건)
part5_1

#'round(카드매출건수x건당매출금액,-3)' 값을 '카드매출금액'으로 대체한 후,
#'점당매출금액'을 'round(카드매출건수x건당매출금액/매출가맹점수)'로 대체
part5_1$카드매출금액 <- round(part5_1$매출건수X건당금액, -3)
part5_1$점당매출금액 <- round(part5_1$매출건수X건당금액/part5_1$매출가맹점수)

all.equal(part5_1$카드매출금액, part5_1$매출가맹점수 * part5_1$점당매출금액)  #"Mean relative difference: 1.798275e-07"
#점당매출금액을 반올림하는 과정에서 rounding error 발생한 것으로 보임
#그러나 매우 작은 오차이고, 나머지 수치형 자료도 소수점 없이 반올림한 값이므로 데이터 통일성을 위해 round한 값으로 진행


#2) '카드매출금액'과 '매출가맹점수 x 점당매출금액'의 차이가 가장 작은 경우 

part5_2 <- subset(part5, min_diff==diff_매출액_가맹점)
head(part5_2[,c(6:12,14:19)])

#'카드매출금액'이 모두 백의 자리에서 round한 결과값이므로 데이터 수집 과정에서 '카드매출금액'값에 오류가 발생했다고 판단하고,
#'카드매출금액'을 'round(매출가맹점수x점당매출금액,-3)'으로 대체
#또한, '건당매출금액'을 'round(매출가맹점수x점당매출금액/카드매출건수)'로 대체
part5_2$카드매출금액 <- round(part5_2$가맹점수X점당금액, -3)
part5_2$건당매출금액 <- round(part5_2$가맹점수X점당금액/part5_2$카드매출건수)

all.equal(part5_2$카드매출금액, part5_2$카드매출건수 * part5_2$건당매출금액)  #"Mean relative difference: 7.25926e-06"
#건당매출금액을 반올림하는 과정에서 rounding error 발생한 것으로 보임
#그러나 매우 작은 오차이고, 나머지 수치형 자료도 소수점 없이 반올림한 값이므로 데이터 통일성을 위해 round한 값으로 진행


#3) '카드매출건수 x 건당매출금액'과 '매출가맹점수 x 점당매출금액'의 차이가 가장 작은 경우

part5_3 <- subset(part5, min_diff==diff_가맹점_매출건)
part5_3[,c(6:12,14:19)]

#'카드매출건수x건당매출금액'과 '매출가맹점수x점당매출금액'의 중앙값으로 각 값을 대체
#'카드매출금액'을 'round(카드매출건수X건당매출금액, -3)'으로 대체
#'건당매출금액'을 'round(매출가맹점수x점당매출금액/카드매출건수)'로 대체
#'점당매출금액'을 'round(카드매출건수x건당매출금액/매출가맹점수)'로 대체
part5_3$가맹점수X점당금액 <- apply(part5_3[,c("가맹점수X점당금액","매출건수X건당금액")],1,median)
part5_3$매출건수X건당금액 <- part5_3$가맹점수X점당금액

part5_3$카드매출금액 <- round(part5_3$매출건수X건당금액, -3)

part5_3$건당매출금액 <- round(part5_3$가맹점수X점당금액/part5_3$카드매출건수)
part5_3$점당매출금액 <- round(part5_3$매출건수X건당금액/part5_3$매출가맹점수)

all.equal(part5_2$카드매출금액, part5_2$카드매출건수 * part5_2$건당매출금액)  #"Mean relative difference: 7.25926e-06"
all.equal(part5_2$카드매출금액, part5_2$매출가맹점수 * part5_2$점당매출금액)  #TRUE
#건당매출금액을 반올림하는 과정에서 rounding error 발생한 것으로 보임
#그러나 매우 작은 오차이고, 나머지 수치형 자료도 소수점 없이 반올림한 값이므로 데이터 통일성을 위해 round한 값으로 진행


#데이터 오류 처리가 끝난 데이터 합치기
shinhan_seoul_3 <- rbind(part1, part2, part3, part4, part5_1[,1:13], part5_2[,1:13], part5_3[,1:13])
str(shinhan_seoul_3)  #1387 obs. of  13 variables



##업종 및 기준년월별 점당매출금액으로 데이터셋 생성(요약)
#점당매출금액을 사용하여 데이터를 분석할 예정이므로 업종 및 기준년월별 점당매출금액 데이터셋을 생성함

#기준년월별로 데이터 나누기
X201903 <- subset(shinhan_seoul_3, 기준년월=="201903")
X201909 <- subset(shinhan_seoul_3, 기준년월=="201909")
X202003 <- subset(shinhan_seoul_3, 기준년월=="202003")
X202009 <- subset(shinhan_seoul_3, 기준년월=="202009")

#변수이름 변경
colnames(X201903)[11] <- "점당매출금액201903"
colnames(X201909)[11] <- "점당매출금액201909"
colnames(X202003)[11] <- "점당매출금액202003"
colnames(X202009)[11] <- "점당매출금액202009"

#필요한 변수만 남기기
X201903 <- X201903[,c(3:5,13,11)]
X201909 <- X201909[,c(13,11)]
X202003 <- X202003[,c(13,11)]
X202009 <- X202009[,c(13,11)]

#모든 기준년월 데이터를 업종별로 합치기(교집합)
X03 <- merge(X201903, X202003, by="업종")
X09 <- merge(X201909, X202009, by="업종")
X <- merge(X03, X09, by="업종")
str(X)  #339 obs. of  8 variables

#업종 및 기준년월별 점당매출금액 데이터셋 생성
summary_점당매출금액 <- X %>% 
  mutate(X201903 = 점당매출금액201903, X201909 = 점당매출금액201909, X202003 = 점당매출금액202003, X202009 = 점당매출금액202009) %>% 
  select(업종, 업종대분류, 업종중분류, 업종소분류, X201903, X201909, X202003 ,X202009 )
str(summary_점당매출금액)  #339 obs. of  8 variables


##점당매출금액이 하나라도 0인 값 추출
subset(summary_점당매출금액, X201903 == 0 | X201909 == 0 | X202003 == 0 | X202009 == 0 )


#1.점당매출금액 중 1개만 0인 경우 -> 나머지 3개 값들의 평균을 이용하여 대체

rows <- c(29,68,70,131,255,299)   # 점당매출금액 중 1개만 0인 행 번호

for(i in rows){
  notZero <- c()
  for(j in 5:8){
    if(summary_점당매출금액[i,j] != 0){
      notZero <- c(notZero,summary_점당매출금액[i,j])   # 0이 아닌 점당매출금액 모두 저장
    }else{
      b <- j   # 0의 값을 갖는 변수 번호 저장
    }
  }
  summary_점당매출금액[i,b] <- mean(notZero)   # 0이 아닌 값들의 평균으로 0인 값을 대체함
}
summary_점당매출금액[rows,]


#2.점당매출금액 중 2개가 0인 경우 -> 3월 값끼리, 9월 값끼리 서로 대체

summary_점당매출금액[67,"X201903"] <- summary_점당매출금액[67,"X202003"]
summary_점당매출금액[67,"X201909"] <- summary_점당매출금액[67,"X202009"]

summary_점당매출금액[254,"X202003"] <- summary_점당매출금액[254,"X201903"]
summary_점당매출금액[254,"X202009"] <- summary_점당매출금액[254,"X201909"]

summary_점당매출금액[c(67,254),]


#3. 점당매출금액 중 3개가 0인 경우 -> 0인 값을 0이 아닌 값으로 모두 대체

rows <- c(24, 113, 282)  # 점당매출금액 중 3개가 0인 행 번호

for(i in rows){
  zero <- c()
  for(j in 5:8){
    if(summary_점당매출금액[i,j] != 0){
      num <- summary_점당매출금액[i,j]
    }else{
      zero <- c(zero,j)
    }
  }
  summary_점당매출금액[i,zero] <- num
}
summary_점당매출금액[rows,]


#4. 점당매출금액 모두 0인 경우 -> 행 삭제

summary_점당매출금액 <- summary_점당매출금액[-311,]

nrow(subset(summary_점당매출금액, X201903 == 0 | X201909 == 0 | X202003 == 0 | X202009 == 0 ))  #0


## 파생변수 생성 - 2019월매출액, 추정매출액손실_1개월, 추정매출액손실_연간

# 2019년 월 매출액
summary_점당매출금액$X2019월매출액 <- apply(summary_점당매출금액[,c("X201903","X201909")],1,mean)

# 추정 매출액 손실 (1개월간)
summary_점당매출금액$추정매출액손실_1개월 <- summary_점당매출금액$X2019월매출액 * 0.485

# 추정 매출액 손실 (연간)
summary_점당매출금액$추정매출액손실_연간 <- summary_점당매출금액$추정매출액손실_1개월 * 12


str(summary_점당매출금액)  #338 obs. of  11 variables

## 변수 설명
#X201903 : 2019년 3월 점당매출금액
#X201909 : 2019년 9월 점당매출금액
#X202003 : 2020년 3월 점당매출금액
#X202009 : 2020년 9월 점당매출금액
#X2019월매출액 : 2019년 3월과 2019년 9월 점당매출금액의 평균값
#추정매출액손실_1개월 : 2019년 월 매출액에 0.485(중소기업벤처부의 설문조사 결과에 기반한 추정 매출액 손실%)를 곱한 값
#추정매출액손실_연간 : 1개월간 추정매출액손실에 12(개월)을 곱한 값


# csv 파일 저장
write.csv(summary_점당매출금액, "신한_추정매출액손실.csv", row.names = FALSE)