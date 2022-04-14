#iris Species (setosa/versicolor/virginica)

install.packages("tidyverse")
library(tidyverse) #ggplot,dplyr,stringr 등 총 8개의 패키지가 내장되어 있 음.

iris # iris데이터를 불러오자.
summary(iris) # iris 데이터의 요약 통계량을 보자.

#결측치 확인하기.
table(is.na(iris))

#전반적인 변수 및 속성 확인하기.
str(iris)

#install.packages("ggvis") # 설치가 안되어 있으면 설치
library(ggvis) # 패키지 로드
# 종 도식화 %>%함수를 사용 했기 때문에 dplyr패키지 설치가 되어 있어야 한다.
iris %>% 
  ggvis(~Petal.Length, ~Petal.Width, fill = ~factor(Species)) %>%
  layer_points()


