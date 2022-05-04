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

missing_values %>% 
  ggplot(aes(x = reorder(feature, missing_pct), y = missing_pct)) + # 정렬을 위한 reorder() 축지정
  geom_bar(stat = "identity", fill = "red") +  # bar plot 그리기 stat = 'identity' 데이터프레임 값을 그대로 이용하여 그리라는 옵션
  ggtitle("Rate of missing values in each features") +
  theme(plot.title = element_text(face = "bold",    # 글씨체 
                                  hjust = 0.5,      # Horizon(가로비율) = 0.5
                                  size = 15, color = "darkblue")) +
  labs(x = "Feature names", y = "Rate") +  # x,y축 제목 지정
  coord_flip() # Plot의 x, y축 변환 

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
  
#변수 EDA
##Sex
table(full$Sex)

#Sex의 빈도
full %>%
  group_by(Survived,Sex) %>%
  summarise(freq=n())

#여자들이 생존할 확률이 높음.
prop.table(table(full$Sex,full$Survived),1)

#성별 막대그래프
sex.p1<-full %>%
  dplyr::group_by(Sex) %>%
  summarise(N=n()) %>%
  ggplot(aes(Sex,N))+
  geom_col()+
  geom_text(aes(label=N),size=5,vjust=1.2,color='#FFFFFF')+
  ggtitle('Bar plot of Sex')+
  labs(x="sex",y='Count')
#성별에 따른 생존율 막대그래프
sex.p2<-full %>%
  filter(!is.na(Survived)) %>%
  ggplot(aes(factor(Sex),fill=factor(Survived)))+
  geom_bar(position = 'fill')+
  scale_y_continuous(labels = percent)+
  scale_fill_brewer(palette = "Set1")+ #palette에 어떤색을 넣을지.
  #일정한 간격으로 x축과 y축 설정 : scale_x_continuous(break=seq())
  #분석가 마음대로 x축과 y축 설정 : scale_x_continuous(break=c())
  ggtitle('Survival Rate by Sex')+
  labs(x="Sex",y='Rate')

grid.arrange(sex.p1,sex.p2,ncol=2)
  
##Pclass
table(full$Pclass)

prop.table(table(full$Pclass,full$Survived),1)

#Pclass 막대그래프
Pclass.p1<-full %>%
  dplyr::group_by(Pclass) %>%
  summarise(N=n()) %>%
  ggplot(aes(Pclass,N))+
  geom_col()+
  geom_text(aes(label=N),size=5,vjust=1.2,color='#FFFFFF')+
  ggtitle('Bar plot of Pclass')+
  labs(x="Pclass",y='Count')
#Pclass에 따른 생존율 막대그래프
Pclass.p2<-full %>%
  filter(!is.na(Survived)) %>%
  ggplot(aes(factor(Pclass),fill=factor(Survived)))+
  geom_bar(position = 'fill')+
  scale_fill_brewer(palette = "Set1")+ #palette에 어떤색을 넣을지.
  ggtitle('Survival Rate by Sex')+
  labs(x="Sex",y='Rate')

grid.arrange(Pclass.p1,Pclass.p2,ncol=2)

#Fare
hist(full$Fare)

Fare.p1<-full %>%
  ggplot(aes(Fare))+
  geom_histogram(col = 'yellow',
                 fill = 'blue',
                 alpha=0.5)+
  ggtitle('Histogram of passengers Fare')+
  theme(plot.title = element_text(face='bold',hjust=0.5,size=15))

#생존여부에 다른 fare box plot
Fare.p2<-full %>%
  filter(!is.na(Survived))%>%
  ggplot(aes(Survived,Fare))+ #x축에 생존 y축에 fare
  #관측치를 회색점으로 찍되, 중복되는 부분은 퍼지게 그려줌.
  geom_jitter(col='gray')+
  #상자그림 : 투명도 50%
  geom_boxplot(alpha=0.5)+
  ggtitle('Box plot of passengers Fare')+
  theme(plot.title = element_text(face='bold',
                                  hjust=0.5,
                                  size=15))
grid.arrange(Fare.p1,Fare.p2,ncol=2)

#Age
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

#sibsp
table(full$SibSp)

train %>% 
  group_by(Survived, SibSp) %>% 
  summarise(freq = n())
#배우자, 형제자매가 많을 수록 생존률이 떨어짐.
prop.table(table(train$SibSp,train$Survived),1)

#parch
table(full$Parch)

train %>% 
  group_by(Survived, Parch) %>% 
  summarise(freq = n())
#부모와 자녀를 1~3명 정도 동승했을 경우 생존률이 높음.
prop.table(table(train$Parch,train$Survived),1)

#Embarked
table(full$Embarked)

train %>% 
  group_by(Survived, Embarked) %>% 
  summarise(freq = n())
#C에서 탑승한 인원들만 생존률이 높다.
prop.table(table(train$Embarked,train$Survived),1)

#결측치 처리
colSums(is.na(full))

#Embarked 결측치 처리.
full[is.na(full$Embarked),]# 두개의 관측치 모두 Fare 80이고,
#pclass가 1임.

embark_fare<-full[!is.na(full$Embarked),]

ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), # fare가 80에 line 생성
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous()
  
#fare가 80이면서 Pclass가 1인 승객 대다수는 embark가 C이다.
full$Embarked[c(62,830)]<-'C'
full[c(62,830)]

#fare 결측치 처리.
full %>%
  filter(is.na(full$Fare)) #
#Plcass가 3이고, Embarked는 S임

full$Fare[1044]<-median(full[full$Pclass=='3'&full$Embarked=='S',]$Fare, na.rm=True)
#중앙값으로 결측치 처리
full[1044,]

# Feature Engineering
#Name(str)->regular expression(정규 표현식)
Title <- full$Name
Title <- gsub("^.*, (.*?)\\..*$", "\\1", Title) # 정규표현식
full$Title <- Title
unique(full$Title)
#^ : 시작되는 글자. .(엔터 문자(new line)를 제외한 모든 문자.)
#*:기호 바로 앞의 문자가 없거나 하나 이상 있음.
#*() : ()만의 문자열은 하나로 묶어줌.
#?:기호 바로 앞의 문자거 없거나 하나.
#gsub : 특정 문자열이나 패턴을 찾아서 내가 원하는 문자열로 대체
#또는 제거 할 수 있는 함수.

#범주별 빈도수,비율 확인.
descr::CrossTable(full$Title)

#5개 범주로 단순화작업
full <- %>%
  #%in%대신 '=='을 사용하여도 되지만, Recyling Rule 때문에 원하게 안됨.
  mutatate(Title==ifelse(Title %in% c('Mlle','MS','Lady','Dona'),'Miss'),Ttitle), #%in% 개념
           Title==ifelse(Title =="Mme","Mrs",Ttitle),
           Title==ifelse(Title %in% c('Capt','Col','Major','Dr','Rev','Don',
                                      'Sir','the Countess','Jonkheer'),"Officer" ,Ttitle),
           Title==factor(Title)
#파생변수 생성 후 각 범주별 빈도수, 비율 확인.
descr::CrossTable(full$Title) #5개의 범주로 축소.
           
#성별 더미화
full$Sex <- ifelse(full$Sex=='male', 0,1)
full$Sex<- as.factor(full$Sex)

#family size
#sibsp+parch 를 이용하여 Fsize 파생변수 생성
full$Fsize <-full$SibSp+full$Parch+1
table(full$Fsize)

#Fsize에 따른 생존율 시각화.
Fsize.p1 <-full %>%
  filter(!is.na(Survived)) %>%
  ggplot(aes(Fsize,fill=Survived)) +
  geom_bar(position = 'fill')+
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=c(1:11))+
  scale_fill_brewer(palette='Set1')+
  ggtitle('Survial Rate by Fsize')+
  labs(x='Fsize',y='Rate')
Fsize.p1

#범주화 세분화.
full$Familysize[full$Fsize == 1] <- 'single'
full$Familysize[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$Familysize[full$Fsize > 4] <- 'large'

full$Familysize <- as.factor(full$Familysize)
table(full$Familysize)

# 범주화 후 Familiysize에 따른 생존율 시각화
ggplot(full[1:891,], aes(x = Familysize, fill = Survived)) +
  geom_bar(position = 'fill') +
  ggtitle("Survival Rate by Familysize")
labs(x="Familysize", y="Rate")

#Cabin
full$Cabin[1:28]

strsplit(full$Cabin[2],NULL)[1]

full$Deck<-factor(sapply(full$Cabin,function(x) strsplit(x,NULL)[[1]][[1]]))
str(full)
#Cabin 변수 제거
full = full[,-11]
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

full %>% count(Deck)








