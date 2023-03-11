##### Open API  - 보건복지부 코로나19 시,도 발생 현황 #####


# 서비스 URL
serviceURL <- "http://openapi.data.go.kr/openapi/service/rest/Covid19/"

# 오퍼레이션명
operation <- "getCovid19SidoInfStateJson"

# 요청 변수
rows <- 100


## 페이지 위치
pg <- 1


## 인증키
ServiceKey <- "JmoDEMqTJELOckxVj3pB1tvI7W28QIavypzAfkwULlPZLa5wRw9RuzBtKF%2F1x8Yi%2F6JlxniauBNdrsc2vGZwzw%3D%3D"


## 검색할 생성일 범위의 시작
startCreateDt <- 20200301


## 검색할 생성일 범위의 종료
endCreateDt <- 20201231



# 오픈API 호출 URL 생성
url <- paste0(serviceURL,
              operation,
              paste0("?ServiceKey=",ServiceKey),
              paste0("&numOfRows=",rows),
              paste0("&pageNo=",pg),
              paste0("&startCreateDt=",startCreateDt),
              paste0("&endCreateDt=",endCreateDt))
url


# XML 데이터를 처리할 수 있는 기능을 모은 R 패키지 호출
library(XML)


# Open API 호출
xmlDocument <- xmlTreeParse(url,useInternalNodes=TRUE,encoding="UTF-8")


# XML Root Node 획득
rootNode <- xmlRoot(xmlDocument)


# 오픈 API 호출 결과 데이터의 개수
numOfRows <- as.numeric(xpathSApply(rootNode,"//numOfRows",xmlValue))


# 전체 데이터의 개수 획득
totalCount <- as.numeric(xpathSApply(rootNode,"//totalCount",xmlValue))


# 총 오픈API 호출 횟수 계산
loopCount <- round(totalCount / numOfRows,0)


## API 호출 횟수 보정
if(loopCount * numOfRows < totalCount){
  loopCount <- loopCount + 1
}


# 전체 데이터를 저장할 변수 선언
totalData <- data.frame()


# 오픈API 호출을 총 오픈API 호출 횟수만큼 반복 실행
for(i in 1:loopCount){
  # 호출 URL 생성
  url <- paste0(serviceURL,
                operation,
                paste0("?ServiceKey=",ServiceKey),
                paste0("&numOfRows=",rows),
                paste0("&pageNo=",i),  # 반복 실행할 때마다 페이지 위치를 1씩 증가
                paste0("&startCreateDt=",startCreateDt),
                paste0("&endCreateDt=",endCreateDt)) 
  
  # 오픈API를 호출하여 XML 데이터 획득
  doc <- xmlTreeParse(url, useInternalNodes = TRUE, encoding = "UTF-8")
  
  # XML 데이터의 Root Node에 접근
  rootNode <- xmlRoot(doc)
  
  # item Node의 데이터를 추출
  xmlData <- xmlToDataFrame(nodes=getNodeSet(rootNode,'//item'))
  
  # 추출한 데이터를 전체 데이터를 저장할 변수에 누적 저장
  totalData <- rbind(totalData,xmlData)
}



# 데이터 확인하기
View(totalData)


# CSV 파일로 저장하기
setwd("C:/Users/qhfka/2021_금융데이터경진대회/DATA")
write.csv(totalData,"시도별코로나발생현황(2020).csv",row.names=FALSE)
