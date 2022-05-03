#package

#install.packages("dplyr")
#install.packages('descr')
#install.packages('ggplot2')
install.packages('ISLR')
#install.packages('MASS')
#install.packages('glmnet')
#install.packages('randomforest')
#install.packages('rpart')
#install.packages('ROCR')
library(dplyr) #데이터 전처리 패키지
library(descr) # ex) 빈도계산 ex)frq...
library(ggplot2) #그림
library(ISLR) #least squared linear regression
library(MASS) #toy examples
library(glmnet) #패널티 최대 우도(penalized maximum likelihood)를 통해서 
#일반화 선형 모델(generalized linear model)을 적합하는 패키지
library(randomForest) #Randomforest 패키지
library(rpart) #의사결정 나무 패키지
library(ROCR)#AUC(AUROC) curve 패키지

rm(list=ls())
setwd('C:/Users/user/Documents')
getwd()

mushrooms<-read.csv('mushrooms.csv',header=T)
#총 23개의 변수가 사용
#종속 변수(반응변수) : class / 22개는 입력변수(설명변수,
#예측변수, 독립변수)
head(mushrooms)
str(mushrooms)

#반응변수 빈도수와 비율
CrossTable(mushrooms$class)

mushrooms %>%
  ggplot(aes(class))+
  geom_bar()

#결측치 확인
sum(is.na(mushrooms))

CrossTable(mushrooms$stalk.root)

#pre-Processing
#veil.type 변수는 모두 1 level이라 무의미한 변수이므로 제거
#로지스틱 회귀분석(glm)에서 반응변수(class)가 이진형(binary)
#인 경우 첫번째 레벨에 해당하는 범주가 Failure
#이외의 모든 레벨이 Success로 간주.
#class에서 e(식용)가 실패, p(독성)가 성공으로 간주.
#class ->levels를 재설정.
mushrooms<-mushrooms[,-17]
mushrooms$class<-factor(mushrooms$class, levels=c("p","e"))
summary(mushrooms)

#문제의 복잡도 구하기.
#mushrooms 데이터의 n,p값을 구해서 문제의 복잡도를 확인 해보는 과정.
A<-model.matrix(~ . -class,mushrooms)

dim(A)
#n=8124,p=96

#데이터가 모두 질적 자료(factor data)
#bplot와 모자이크 플롯을 이용하여 시각화를 한다.
#barplot

#cap.color
mushrooms %>%
  group_by(class) %>%
  ggplot(aes(cap.color, fill=class))+
  geom_bar(position='dodge')
#postion : 막대의 위치
#dodge : 복수의 데이터를 독립적인 막대 그래프로 나란히 표현. 

#gill.color
mushrooms %>%
  group_by(class) %>%
  ggplot(aes(gill.color, fill=class))+
  geom_bar(position='dodge')
#odor
mushrooms %>%
  group_by(class) %>%
  ggplot(aes(odor, fill=class))+
  geom_bar(position='dodge')

#spore.print.color
mushrooms %>%
  group_by(class) %>%
  ggplot(aes(spore.print.color, fill=class))+
  geom_bar(position='dodge')



#Mosaicplot
##cap.color
mosaicplot( ~ cap.color +class,
            data = mushrooms,
            color=T,
            cex=1.2
            )

# gill.color
mosaicplot( ~ gill.color + class,
            data = mushrooms,
            color=T,
            cex=1.2)
# odor
mosaicplot( ~ odor + class,
            data = mushrooms,
            color=T,
            cex=1.2)
# spore.print.color
mosaicplot( ~ spore.print.color + class,
            data = mushrooms,
            color=T,
            cex=1.2)

#모델 생성.
#data set split
# Training:Validation:Test = 6:2:2
#재현 가능성(Reproducible)연구를 위해서 각 모델 생성 전에
#seed 설정
#로지스틱 회귀 Error많고, 시간이 많이 걸림
#Randomforest / Lasso model
set.seed(0503)
n<-nrow(mushrooms)
idx <- 1:n #총 관측치 개수 인덱싱

training.idx <-sample(idx,n*.60)
#Random하게 전체 데이터에서 60% 샘플링
idx <-setdiff(idx,training.idx)
#전체 idx에서 training_idx 제외한 나머지 idx를
#다시 idx변수에 저장.
#setdiff : 첫 번째 테이블에서 두 번째 테이블 집합의
#데이터 집합을 뺀 결과를 출력. / 차집합
validation.idx<-sample(idx,n*.20)
test.idx<-setdiff(idx,validation.idx)

#샘플링 된 데이터 갯수들 확인.
length(training.idx)
length(validation.idx)
length(test.idx)

#순서대로 훈련,검증, 테스트 데이터
training <-mushrooms[training.idx,]
validation <-mushrooms[validation.idx,]
test<-mushrooms[test.idx,]
#Random Forest / Bagging
#seed setting
set.seed(0503)
#modeling
mushrooms_rf<-randomForest(class ~.,training)
mushrooms_rf

#설명 변수들 중에서 설명력이 높은 변수들 알아보기
#Feature importance
#평균지니지수감소량으로 split
importance(mushrooms_rf)

#변수 중요도 확인 plot->XAI(Explainable Artificial Intelligence)
varImpPlot(mushrooms_rf)

#validation set으로 예측.
predict(mushrooms_rf,newdata=validation[1:10,])

#확률값으로 보고 싶은 경우.
predict(mushrooms_rf,newdata=validation[1:10,],type='prob')

#Random forest 모델 평가
#이항편차, ROC curve, AUC value
y_obs <- ifelse(validation$class == "e", 1, 0)
yhat_rf <- predict(mushrooms_rf,
                   newdata = validation,
                   type = "prob")[, 'e']
#이항편차 구하기
binomial_deviance(y_obs, yhat_rf)

#ROC curve
pred_rf <- prediction(yhat_rf, y_obs)

perf_rf <- performance(pred_rf, measure = "tpr",
                       x.measure = "fpr")

plot(perf_rf,
     col = "red",
     main = "ROC Curve")

abline(0,1)

performance(pred_rf,'auc')@y.values[[1]]










