install.packages('tidyverse')
install.packages('tidymodels')
#데이터 전처리 패캐지
library('tidyverse')
#tidy한 방법으로 머신러닝 모델 계획
library(tidymodels)
#필요한 패키지만 불러오지 않고,
#이렇게 동시에 불러오는 건 실제로 작업을 하다보면
#각 패키지를 유기적으로 연결해 쓰는게 능률을 높이는데 도움이 됨.
#csv(comma-Seperated values) ->read.csv() / tidyverse ->read_csv
cya<-read_csv('cya.csv')
cya
#2009년부터 2019년까지 11년동안 규정이닝을 채운 채운 투수들을 대상으로
#시즌(season), 이름(name),팀(team),리그(lg),사이영상 수상 여부(cy)
#사이영상 투표 특정(vote),승리 순위(w), 패배순위(l),
#팬그래프스 대체 선수 대비 승리 기여도(fWAR) 순위(war)
#평균 자책점 순위(era), 수비 영향을 제거한 평균자책점(FIP) 순위(fip)
#투구 이닝 순위(ip),삼진 순위(k), 볼넷 순위(hr),9이닝당 탈삼진(K/9) 순위(k9)
#9이닝당 볼넷(BB/9) 순위(bb9), 삼진 대 볼넷 비율(K/BB) 순위(kbb)
#인플레이타율(BABIP)순위(babip).

#2019년 데이터는 아직 사이영상 수상 여부도 없고, 투표 결과도 없기 때문에 일단 따로뺌.
#사이영상 : 메이저 리그 베이스볼에서 각 리그 최고의 투수에게 수여하는 되는 상.

cya %>%
  filter(season==2019)->cya_2019
cya_2019

#cya_2019<-filter(cya,season==2019)

#%>% : 파이프(pipe)기호. 
#현실 세계에서 파이프가 액체나 기체 같은 유체를 한곳에서
#다른 곳으로 보내는 것처럼 파이프는 한 자료를 이 함수에서
#저 함수로 보내는 구실.

#filter()->dplyr패키지 -> 문자 그대로 특정 기준에 따라 
#행을 골라내는 것.

#2019이 아닌 것은 고르려면 어떻게 할까?
cya %>%
  filter(season!=2019)->cya_pre

#train_test_split
#initial_split()
cya_pre %>%
  initial_split(prop=0.7)->cya_split
cya_split
#initial_split(prop,strata):
#prop : train set 비율.
#strata:범주간 불균형 문제를 해결하기 위해 층을 나누고
#서브샘플링.
#연속형 변수의 경우 사분위수를 기준으로 서브샘플링.
#어떤 데이터가 train인지 확인하는 방법.
cya_split %>%
  training()
#어떤 데이터가 test인지 확인하는 방법.
cya_split %>%
  testing()
#recipe : 데이터 전처리를 위한 단계를 정의하는 object.
#특이한 점 : 즉시 실행되지는 않음. 단계만 정의.
#장점 : recipe object를 여러가지 모델에 재사용 가능
#장점 : recipe내에 사전 정의된 함수를 이용하면
#코드의 간결성 확보 가능.

cya_split %>%training() %>%
  recipe(vote~w+l+war+era+fip+ip+k+bb+hr+k9+bb9+kbb+babip)

#결과는 투표결과 하나, 13개의 변수로 예측에 씀.
#모형을 만들 때는 결측치 제거->이상치 탐지 / 다중공산성
# 정규화, dummy 변수 작업이 필요함.






