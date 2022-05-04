#변수 설명
#survial : 생존여부, target값(0=사망,1=생존)
#pclass : 티켓 클래스(1=1st,2=2nd,3=3rd)
#sex : 성별
#age : 나이(세)
#sibsp : 함께 탑승한 형제자매, 배우자 수 총합
#parch : 함께 탑승한 부모, 자녀 수 총합
#ticket : 티켓 넘버
#fare : 탑승 요금
#cabin : 객실 번호
#embarked : 탐승 항구.

install.packages('doBy')
install.packages('gridExtra')


#package
library(readr) #csv 파일형식 전달
library(stringr)# 문자열 처리 패키지
library(doBy) #dataframe에서 특정 값에 따라 데이터처리
library(ggplot2)
library(scales) #전처리 패키지 / Scaling
library(RColorBrewer) #색상 표현 패키지
library(corrplot) 
library(dplyr) # 전처리
library(randomForest)
library(gridExtra) 

#Loading the data
#read.csv()보다 readr패키지의 read_csv()가
#읽는 속도가 조금 더 빠름.
#그러나, read_csv()는 stringsAsFactors 옵션이 없어서
#문자열(Character)과 요인(Factor)를 구별하지 못하고
#모두 Character로 불러옴.

train<-read_csv('./train.csv')
test<-read_csv('./test.csv')
#dbl ->double()
full<-bind_rows(train,test)
#rbind()는 두 데이터의 차원이 같을 때 병합이 가능
#test셋에는 Y값이 없어서 차원이 다름.
#이에 따라 dplyr::bind_rows()를 사용하여
#test데이터의 Y값은 NA값을 처리하며 병합함.

full<- full %>%
  mutate(Survived = factor(Survived),
         pclass = factor(Pclass,ordered = T), 
         Name = factor(Name),
         Sex = factor(Sex),
         Embarked = factor(Embarked))
str(full)

head(full)
summary(full)
#1.사망자가 생존자보다 많다.
#2.남성이 여성보다 2배 가까기 더 많다.
#3.sibsp의 3분위값이 1이므로 대부분 부부끼리 혹은
#형제(자매)끼리 탑승했다.
#parch의 3분위값이 0이므로 부모와 자녀가 함께
#탑승한 승객이 많지 않다.
#Fare의 최대값이 512로 이상치가 아닌지가 필요함.
#결측치가 많은 데이터임을 확인한다.

sapply(train,function(x) length(unique(x)))

#결측치 확인
colSums(is.na(full))

#결측치 비율 확인
missing_values<-full%>% # 결측치 비율을 데이터 프레임으로
  dplyr::summarise_all(funs(sum(is.na(.)/n())))

#tidyr::gather()함수를 이용하여 stack화 시킴
#설명변수들이 key로 지정한 변수에 나열되고, 결측값들이
#value로 지정한 변수의 값으로.
missing_values<-tidyr::gather(missing_values,
                              key='feature',value='missing_pct')
missing_values













