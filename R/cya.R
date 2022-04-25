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
cya<-read_csv('mushrooms_1.csv')
cya
#2009년부터 2019년까지 11년동안 규정이닝을 채운 채운 투수들을 대상으로
#시즌(season), 이름(name),팀(team),리그(lg),사이영상 수상 여부(cy)
#사이영상 투표 특정(vote),승리 순위(w), 패배순위(l),
#팬그래프스 대체 선수 대비 승리 기여도(fWAR) 순위(war)
#평균 자책점 순위(era), 수비 영향을 제거한 평균자책점(FIP) 순위(fip)
#투구 이닝 순위(ip),삼진 순위(k), 볼넷 순위(hr),9이닝당 탈삼진(K/9) 순위(k9)
#9이닝당 볼넷(BB/9) 순위(bb9), 삼진 대 볼넷 비율(K/BB) 순위(kbb)
#인플레이타율(BABIP)순위(babip).







