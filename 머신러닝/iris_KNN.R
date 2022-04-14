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

#분포 살펴보기

iris %>% 
  select(Sepal.Width) %>% 
  ggplot(mapping = aes(x = Sepal.Width))+ #aes :axis
  #stat_density : 밀도분포 그래프.
  stat_density(alpha = 0.7,
               fill = 'steelblue',
               color = 'blue')+
  labs(title = 'Distribution:Sepal.Width')
#fill : 색깔을 면적색으로 기준으로 설정하는 것.
#alpha : fille 색을 투명도를 설정할 수 있음.
ggplot(data=iris)+
  stat_density(mapping=aes(x=Sepal.Width, fill = Species), postion = 'identity')
#postion : stat_density()함수는 default로 postion이 stack이므로, identity로 각각 그려주세요.

