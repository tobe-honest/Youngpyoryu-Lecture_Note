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
#embarked : 탑승 항구.

library(readr) # csv 파일형식 전달
library(stringr) # 문자열 처리 패키지
library(doBy) #dataframe에서 특정 값에 따라 데이터처리
library(ggplot2)
library(scales) #전처리 패키지 / scaling
library(RColorBrewer) #색상 표현 패키지
library(corrplot)
library(doBy)
library(dplyr) # 전처리
library(randomForest)
library(gridExtra)

train <- read_csv('./train.csv')
test <- read_csv('./test.csv')
full <- bind_rows(train, test) 

#factor : '정해진 범주 내에서 카테고리별로 분석(범주형 자료 분석)
#을 하기 위해 주로 사용되는 데이터 자료형
#level : 팩터의 레벨 목록 리턴.
full <- full %>% # ticket과 cabin은 파생변수 생성을 위해 문자열로 놔둠
  mutate(Survived = factor(Survived),
         Pclass   = factor(Pclass, ordered = T),
         Name     = factor(Name),
         Sex      = factor(Sex),
         Embarked = factor(Embarked))
#mutate : 데이터프레임에 조건을 만족하는 새로운 열(변수)를 만들거나
#기존의 열 조건에 맞게 변경할 때.
str(full)
head(full)

summary(full)
sapply(train, function(x) length(unique(x)))
#python에서 같은 함수처리는 lambda
#apply : 배열, 행렬,데이터 프레임 - 함수 적용
#->반환 : 벡터,행렬,배열,리스트
#단점 : 리스트에 함수 적용이 불가, 데이터프레임으로 
#반환 불가
#sapply:벡터,리스트,표현식,데이터 프레임 등에 함수를 적용
#하고 그 결과가 행렬
#lapply:벡터,리스트,표현식,데이터 프레임 등에 함수를 적용
#하고 그 결과가 리스트
#단점 : 원소별 전달 불가.

#결측치 확인.
colSums(is.na(full))

missing_values <- full %>%      # 결측치 비율을 데이터프레임으로
  dplyr::summarize_all(funs(sum(is.na(.))/n()))
#dplyr : Data를 빨리 쉽게 가공할 수 있도록 도와주는 패키지.

# tidyr::gather()함수를 이용하여 stack화 시킴 (설명변수들이 key로 지정한 변수에 나열되고, 결측값들이 value로 지정한 변수의 값으로)
missing_values <- tidyr::gather(missing_values,
                                key = "feature", value = "missing_pct")
#tidyr : 데이터를 저장하는 표준 방법
#gather : 여러 열을 2개의 열로 재구성하는 기능을 갖음.

missing_values %>% 
  ggplot(aes(x = reorder(feature, missing_pct), y = missing_pct)) + # 정렬을 위한 reorder() 축지정
  geom_bar(stat = "identity", fill = "red") +  
  # bar plot 그리기 stat = 'identity' 데이터프레임 값을 그대로 
  #이용하여 그리라는 옵션
  ggtitle("Rate of missing values in each features") +
  theme(plot.title = element_text(face = "bold",    # 글씨체 
                                  hjust = 0.5,      # Horizon(가로비율) = 0.5
                                  size = 15, color = "darkblue")) +
  labs(x = "Feature names", y = "Rate") +  # x,y축 제목 지정
  coord_flip() # Plot의 x, y축 변환 

#reorder : 오름차순 / 그래프 형식에서 사용이 가능.

# 결측값이 있는 변수로만 시각화
missing_values <- missing_values[missing_values$missing_pct > 0, ]

missing_values %>% 
  ggplot(aes(x = reorder(feature, missing_pct), y = missing_pct)) + # 정렬을 위한 reorder() 축지정
  geom_bar(stat = "identity", fill = "red") +  # bar plot 그리기 stat = 'identity' 데이터프레임 값을 그대로 이용하여 그리라는 옵션
  ggtitle("Rate of missing values in each features") +
  theme(plot.title = element_text(face = "bold",    # 글씨체 
                                  hjust = 0.5,      # Horizon(가로비율) = 0.5
                                  size = 15, color = "darkblue")) +
  labs(x = "Feature names", y = "Rate") +  # x,y축 제목 지정
  coord_flip() # Plot의 x, y축 변환

table(full$Sex)
full %>% group_by(Survived, Sex) %>% summarise(freq = n())
prop.table(table(full$Sex,full$Survived),1) #여자들이 생존할 확률이 높음

# 성별 막대그래프
sex.p1 <- full %>% 
  dplyr::group_by(Sex) %>% 
  summarize(N = n()) %>% 
  ggplot(aes(Sex, N)) +
  geom_col() +
  geom_text(aes(label = N), size = 5, vjust = 1.2, color = "#FFFFFF") + 
  ggtitle("Bar plot of Sex") +
  labs(x = "Sex", y = "Count")

# 성별에 따른 생존률 막대그래프
sex.p2 <- full%>%
  filter(!is.na(Survived)) %>%
  ggplot(aes(factor(Sex), fill = factor(Survived))) +
  geom_bar(position = "fill") + 
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Set1") +  # palette에 어떤색 넣을지 지정
  # 일정한 간격으로 x축과 y축 설정 : scale_x_continuous(breaks=seq())
  # 분석가 마음대로 x축과 y축 설정 : scale_x_continuous(breaks=c())
  ggtitle("Survival Rate by Sex") + 
  labs(x = "Sex", y = "Rate")

grid.arrange(sex.p1,sex.p2,ncol=2)
table(full$Pclass)

prop.table(table(full$Pclass,full$Survived),1) # 더 좋은 객실 이용자일수록 생존할 확률이 높음

# Pclass 막대그래프
pclass.p1 <- full %>% 
  dplyr::group_by(Pclass) %>% 
  summarize(N = n()) %>% 
  ggplot(aes(Pclass, N)) +
  geom_col() +
  geom_text(aes(label = N), size = 5, vjust = 1.2, color = "#FFFFFF") + 
  ggtitle("Bar plot of Pclass") +
  labs(x = "Pclass", y = "Count")

# Pclass에 따른 생존률 막대그래프
pclass.p2 <- full%>%
  filter(!is.na(Survived)) %>%
  ggplot(aes(factor(Pclass), fill = factor(Survived))) +
  geom_bar(position = "fill") + 
  scale_fill_brewer(palette = "Set1") +  
  ggtitle("Survival Rate by Pclass") + 
  labs(x = "Pclass", y = "Rate")

grid.arrange(pclass.p1,pclass.p2,ncol=2)

hist(full$Fare)

# fare 히스토그램
Fare.p1 <- full %>%
  ggplot(aes(Fare)) + 
  geom_histogram(col    = "yellow",
                 fill   = "blue", 
                 alpha  = .5) +
  ggtitle("Histogram of passengers Fare") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15))

# 생존여부에 따른 fare box plot
Fare.p2 <- full %>%
  filter(!is.na(Survived)) %>% 
  ggplot(aes(Survived, Fare)) +  # x축에 생존 y축에 fare
  # 관측치를 회색점으로 찍되, 중복되는 부분은 퍼지게 그려줍니다.
  geom_jitter(col = "gray") + 
  # 상자그림 : 투명도 50% 
  geom_boxplot(alpha = .5) + 
  ggtitle("Boxplot of passengers Fare") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15))

grid.arrange(Fare.p1,Fare.p2,ncol=2)

hist(full$Age)

# 나이 분포 히스토그램
age.p1 <- full %>% 
  ggplot(aes(Age)) +     # x값에 따른 y값을 그리는 것이 아니므로 축 지정 안해줘도 됨 
  # 히스토그램 그리기, 설정
  geom_histogram(breaks = seq(0, 80, by = 1), # 간격 설정 
                 col    = "red",              # 막대 경계선 색깔 
                 fill   = "green",            # 막대 내부 색깔 
                 alpha  = .5) +               # 막대 투명도 = 50% 
  # Plot title
  ggtitle("All Titanic passengers age hitogram") +
  theme(plot.title = element_text(face = "bold",    # 글씨체 
                                  hjust = 0.5,      # Horizon(가로비율) = 0.5
                                  size = 15, color = "darkblue"))

# 나이에 따른 생존 분포 파악
age.p2 <- full %>% 
  filter(!is.na(Survived)) %>%
  ggplot(aes(Age, fill = Survived)) + 
  geom_density(alpha = .5) +   # 막대그래프가 아니고 밀도그래프니까 plot으로 축 지정하고 geom_bar 대신에 geom_density
  ggtitle("Titanic passengers age density plot") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5,
                                  size = 15, color = "darkblue"))

grid.arrange(age.p1,age.p2,ncol=2)
#p1하고 p2 같은 plot에 그림.
#python subplots()
#fig,axes = plt.subplots(nrow=1,ncols=2)\
#ax1.aaaa plot(x,y,ax=ax1)
#ax2.aaa plot(x,y,ax=ax1)
table(full$SibSp)

train %>% group_by(Survived, SibSp) %>% summarise(freq = n())
prop.table(table(train$SibSp,train$Survived),1) #배우자,형제자매가 많을수록 생존률이 떨어짐

table(train$Parch)

train %>% group_by(Survived, Parch) %>% summarise(freq = n())

prop.table(table(train$Parch,train$Survived),1) #부모와 자녀를 1~3명 정도 동승했을 경우 생존률이 높음

table(train$Embarked) #결측값 2개

train %>% group_by(Survived, Embarked) %>% summarise(freq = n())

prop.table(table(train$Embarked,train$Survived),1) # C에서 탑승한 인원들만 생존률이 더 높다

colSums(is.na(full))
full[is.na(full$Embarked), ] #두개의 관측치 모두 Fare가 80이고, Pclass가 1임

embark_fare <- full[!is.na(full$Embarked), ]

ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), # fare가 80에 line 생성
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous()

full$Embarked[c(62, 830)] <- 'C'
full[c(62, 830),]

full  %>% filter(is.na(full$Fare)) #Pclasss가 3이고, Embarked는 S임

full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE) #중앙값으로 결측치 처리
full[1044,]

#^.* : 시작하고 .어쩌구 있으면 캐치하고.
#.*? "~~~~(.*?)\\..     (.*?)하고+\\..*$ / $:end string.
Title <- full$Name
Title <- gsub("^.*, (.*?)\\..*$", "\\1", Title) # 정규표현식
full$Title <- Title
unique(full$Title)
# 범주별 빈도수, 비율 확인 
descr::CrossTable(full$Title)

# 5개 범주로 단순화 시키는 작업 
full <- full %>%
  # "%in%" 대신 "=="을 사용하게되면 Recycling Rule 때문에 원하는대로 되지 않습니다.
  #Recycling Rule : 벡터의 길이가 달라도 연산이 가능.
  #벡터화(Vectorization):루프를 돌릴 필요없이, 한번에 각 요소에 대해 
  #연산을 수행하도록 함수가 벡터 모든 요소에 대해 연산 작업을 수행.
  # arr = np.array([1,2,3])
  # arr+3
  mutate(Title = ifelse(Title %in% c("Mlle", "Ms", "Lady", "Dona"), "Miss", Title), # %in% 개념
         Title = ifelse(Title == "Mme", "Mrs", Title),
         Title = ifelse(Title %in% c("Capt", "Col", "Major", "Dr", "Rev", "Don",
                                     "Sir", "the Countess", "Jonkheer"), "Officer", Title),
         Title = factor(Title))

# 파생변수 생성 후 각 범주별 빈도수, 비율 확인 
descr::CrossTable(full$Title) # 5개의 범주로 축소


full$Sex <- ifelse(full$Sex == "male" ,0 , 1)
full$Sex <- as.factor(full$Sex)

#가족수를 계산.
full$Fsize <- full$SibSp + full$Parch + 1
table(full$Fsize)

# Fsize에 따른 생존율 시각화
Fsize.p1 <- full%>%
  filter(!is.na(Survived)) %>%
  ggplot(aes(Fsize, fill = Survived)) +
  geom_bar(position = "fill") + 
  scale_y_continuous(labels = percent) +
  scale_x_continuous(breaks=c(1:11)) +
  scale_fill_brewer(palette = "Set1") +  # palette에 어떤색 넣을지 지정
  # 일정한 간격으로 x축과 y축 설정 : scale_x_continuous(breaks=seq())
  # 분석가 마음대로 x축과 y축 설정 : scale_x_continuous(breaks=c())
  ggtitle("Survival Rate by Fsize") + 
  labs(x = "Fsize", y = "Rate")

Fsize.p1



#ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
#  geom_bar(stat='count', position='fill') +   #position = 'dodge', 'fill' 구분
#  scale_x_continuous(breaks=c(1:11)) +
#  labs(x = 'Family Size', y = 'Rate')

# 범주화
full$Familysize[full$Fsize == 1] <- 'single'
full$Familysize[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$Familysize[full$Fsize > 4] <- 'large'

full$Familysize <- as.factor(full$Familysize)
table(full$Familysize)


# 범주화 후 Familiysize에 따른 생존율 시각화
ggplot(full[1:891,], aes(x = Familysize, fill = Survived)) +
  geom_bar(position = 'fill') +
  ggtitle("Survival Rate by Familysize")

full$Cabin[1:28]
labs(x="Familysize", y="Rate")

strsplit(full$Cabin[2], NULL)[[1]] #C85 / "C","8","5"

full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))

full$Deck=as.character(full$Deck)
str(full)

#Cabin 변수 제거
full=full[,-11]
head(full)

full$Deck[is.na(full$Deck)] <- "U"

cabin=full %>%filter(!is.na(full$Survived)& full$Deck!='U')

ggplot(cabin,aes(x=Deck, fill=factor(Survived), na.rm=TRUE)) +
  geom_bar(stat='count') +
  facet_grid(.~Pclass) +
  labs(title="Survivor split by Pclass and Deck")

full=full  %>% 
  mutate(Deck= ifelse(Pclass==1 & Deck=="U","X",
                      ifelse(Pclass==2 & Deck=="U","Y",
                             ifelse(Pclass==3 & Deck=="U","Z",Deck)))
  )


full  %>% count(Deck)

age.sex <- full %>% 
  ggplot(aes(Age, fill = Sex)) + 
  geom_density(alpha = .5) +  
  ggtitle("Titanic passengers Age density plot") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5,
                                  size = 15, color = "darkblue"))
age.sex

age.pclass <- full %>% 
  ggplot(aes(Age, fill = Pclass)) + 
  geom_density(alpha = .5) +  
  ggtitle("Titanic passengers Age density plot") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5,
                                  size = 15, color = "darkblue"))
age.pclass

age.title <- full %>% 
  ggplot(aes(Age, fill = Title)) + 
  geom_density(alpha = .5) +  
  ggtitle("Titanic passengers Age density plot") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5,
                                  size = 15, color = "darkblue"))
age.title

plot(full$Title)

# title별 Median Age를 통한 결측값 처리
full=as.data.frame(full)
summaryBy(Age ~ Title, data=full, FUN=c(mean, sd, median), na.rm=TRUE) ## ddply로도


full$Age <- ifelse((is.na(full$Age) & full$Title == 'Master'), 4, full$Age)
full$Age <- ifelse((is.na(full$Age) & full$Title == 'Miss'), 22, full$Age)
full$Age <- ifelse((is.na(full$Age) & full$Title == 'Mr'), 29, full$Age)
full$Age <- ifelse((is.na(full$Age) & full$Title == 'Mrs'), 35, full$Age)
full$Age <- ifelse((is.na(full$Age) & full$Title == 'Officer'), 48, full$Age)


hist(full$Age, freq=F, main='Age',col='lightgreen', ylim=c(0,0.05))

# child : 18세 이하
# adult : 19세 이상 64세 이하
# senior : 65세 이상

full$Age <- ifelse(full$Age <= 18, "child",
                   ifelse(full$Age > 18 & full$Age <= 64, "adult","senior"))
length(unique(full$Ticket))
head(full$Ticket)
full  %>%  arrange(Ticket) #같은 티켓인데도 불구하고 Family가 single, 친구등과 같이 온것으로 유추

full$TravelGroup <- NA
full <- (transform(full, TravelGroup = match(Ticket, unique(Ticket))))
full <- full %>% 
  group_by(TravelGroup) %>% 
  mutate(GroupSize = n()) %>%
  ungroup()
full  %>% arrange(Ticket)  %>% head()
str(full)
#범주화 안된 변수들 범주화 처리
factor_vars <- c('Age','GroupSize','Deck')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

#Fare log변환
#median!=mean -> 왜도>0,<0 <- 정규분포가 아님.
#log1p = log(1+p) #log0은 정의가 안됨
full$Fare=log1p(full$Fare)

full=full  %>%  select(-c(1,4,7,8,9,13,16))
str(full)

train <-full  %>% filter(is.na(Survived)==FALSE)
test <-full  %>% filter(is.na(Survived)==TRUE)

#첫번째가 featurename으로 되어있기 때문에. 
train_label <- as.numeric(train$Survived)-1
test_label <- test$Survived


x_train<- model.matrix(~.-1, data = train[,-1]) %>% data.frame

x_test <- model.matrix(~.-1, data = test[,-1]) %>% data.frame
library(xgboost)

#xgboost : dataset->Dmatrix으로 넣어줘야 작동을 함.
dtrain <- xgb.DMatrix(data = as.matrix(x_train), label=train_label)
dtest <- xgb.DMatrix(data = as.matrix(x_test))


set.seed(2019)
param <- list(objective   = "binary:logistic",
              eval_metric = "auc",
              max_depth   = 6,
              eta         = 0.01, #learing_rate
              gammma      = 0, #split의 기준 (왼쪽+오른쪽-현재)-gamma<0 ->split의 기준
              subsamle    = 0.5, #xgboost->columnwise dataset
              colsample_bytree = 0.5, #위에서의 tree비율
              min_child_weight = 5) #잎의 weight
# xgb_cv <- xgb.cv(params  = param,
#               data    = dtrain,
#               nrounds = 5000,
#               nfold   = 5,
#               nthread = -1,
#               silent = 1,
#               print_every_n = 100,
#               verbose = 0)
xgb <- xgb.train(params  = param,
                 data    = dtrain,
                 nrounds = 4790,
                 silent = 1,
                 print_every_n = 100,
                 verbose = 0)

library(caret)
set.seed(123)
#createDataPartiton->data trasin split ->sample(0.5) ... ->그거 대신 함수
split <- createDataPartition(y = train$Survived,p = 0.7,list = FALSE)

new_train <- train[split,] 
new_test <- train[-split,]


x_label= as.numeric(new_train$Survived)-1
y_label= as.numeric(new_test$Survived)-1

new_train2 <- model.matrix(~.-1, data = new_train[,-1]) %>% data.frame
new_test2 <- model.matrix(~.-1, data = new_test[,-1]) %>% data.frame

dtrain2 <- xgb.DMatrix(data = as.matrix(new_train2), label=x_label)
dtest2 <- xgb.DMatrix(data = as.matrix(new_test2), label=y_label)


xgb2 <- xgb.train(params  = param,
                  data    = dtrain2,
                  nrounds = 4790,
                  silent = 1,
                  print_every_n = 100,
                  verbose = 0)

set.seed(2019)
XGB_pred2 <- predict(xgb2, dtest2)

head(XGB_pred2,10)
head(new_test$Survived,10)

set.seed(2019)
XGB_pred2 <- predict(xgb2, dtest2)
XGB_pred2 <- ifelse(XGB_pred2>=0.5,1,0)
#plot ROC 
library(ROCR) 
library(Metrics) #AUROC을 보기 위해.
pr <- prediction(XGB_pred2,new_test$Survived)
#tpr : true postive rate
#fpr : false postive rate.
perf <- performance(pr,measure = "tpr",x.measure = "fpr") 
plot(perf) > auc(new_test$Survived,XGB_pred) #0.8109

auc(new_test$Survived,XGB_pred2)


set.seed(2019)
XGB_pred2 <- predict(xgb2, dtest2)
XGB_pred2 <- ifelse(XGB_pred2>=0.4,1,0)
#plot ROC 
library(ROCR) 
library(Metrics)
pr <- prediction(XGB_pred2,new_test$Survived)
perf <- performance(pr,measure = "tpr",x.measure = "fpr") 
plot(perf) > auc(new_test$Survived,XGB_pred2) #0.815
auc(new_test$Survived,XGB_pred2)
