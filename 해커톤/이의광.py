import pandas as pd
import numpy as np

import matplotlib.pyplot as plt
import seaborn as sns

from sklearn.preprocessing import LabelEncoder,StandardScaler,RobustScaler
from sklearn.metrics import accuracy_score, recall_score, precision_score, f1_score,roc_auc_score
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
from xgboost import XGBClassifier
np.random.seed(100)

df = pd.read_csv('C:/Users/pshow/OneDrive/바탕 화면/파이썬수업/hac/train.csv')
label = pd.read_csv('.\label.csv')

label1 = label.copy()

print(df.info())
# order_id,product_id,_description,order_date,country -> object형

print(df.describe())
# total의 min과 max값이 동일함
# 동일한 사람이 물품을 환불했다는 것을 알 수 있음


print(df.isna().sum())
# 결측치 없음

print(df.head())

# df['customer_id'] = df['customer_id'].astype('str')
df = df.sort_values(by=['customer_id'])
# customer_id로 묶어줄거기에 groupby, sum을 했을 때 sum이 되지 않게끔 str형으로 형변환해줬음

print(df.info())

print(df)
# 이제 각 연도별 월별로 더해주기 위해 order_date를 연도랑 월만 남겨야 됨
df['order_date'] = ["-".join(i[:2]) for i in df['order_date'].str.split('-')]
print(df)
# split과 join 함수를 통하여 :2까지만 받고 '-'를 사이에 넣어 조인시켜줘서 완성했음

#customer_id가 뒤에 있어 보기가 불편하기에 앞으로 이동
t = df['customer_id']
df.drop('customer_id',axis=1,inplace=True)
df.insert(0,'customer_id',t)
print(df)

df1 = df.copy()
print(df1['customer_id'])
# for i in range(len(df1)):
#     if (df1['customer_id'][i]==12346) == True:
#         print(df1['order_date'][i],df1['total'][i])

customerid_frame = np.repeat(df1.customer_id.unique(), len(df1.order_date.unique()))
date_frame = np.tile(df1.order_date.unique(), len(df1.customer_id.unique()))
# # customer_id 별로 연도별 월간 total을 어느정도 구매했는지 확인해볼수있다.

frame = pd.DataFrame({'customer_id':customerid_frame,'order_date':date_frame})
frame = frame.sort_values(by=['customer_id','order_date'])

df1 = pd.merge(frame,df1,how='outer').fillna(0)
# 월 별로 구매 사용자를 구해줘야 하기 때문에 없는 월의 구매 사용자의 경우 0으로 채워서 넣어줬음



df1 = df1.groupby(['customer_id','order_date']).sum().reset_index()
# 월별로 묶어서 sum을 해주었으며 reset_index로 groupby한 칼럼을 살려줬음

print(df1)

k = df1['order_date']
df1.drop(['order_date'],axis=1,inplace=True)

p = df1['total']/300

print(k)
Rob = RobustScaler()
print(Rob.fit(df1))
df1['total'] = Rob.transform(df1)

df1['order_date'] = k
print(df1)

Label = LabelEncoder()
df1['over300'] = df1['total']>p
df1['over300'] = Label.fit_transform(df1['over300'])
# 라벨인코더를 사용하여 300초과 일시 1 아닐 시 0으로 반환

# Label = LabelEncoder()
# df1['over300'] = df1['total']>300
# df1['over300'] = Label.fit_transform(df1['over300'])

print(df1['total'].value_counts())
print(df1)

sns.boxplot(x='order_date',y='total',data=df1)
plt.show()

sns.histplot(x=df1['order_date'],hue=df1['over300'],kde=True)
plt.show()
# 300초과 구매자일 시 9월과 10월에 구매량이 증가하고 11월에 떨어지는 반면 아닌 사람들의 경우 9월과 10월에 떨어지며 11월에 증가하는 것을 볼 수 있음

# def remove_outlier(d_cp,column):
#     fraud_column_data = d_cp[column]
#     qunt25 = np.percentile(fraud_column_data,25)
#     qunt75 = np.percentile(fraud_column_data,75)

#     iqr = qunt75-qunt25
#     iqr = iqr*1.5
#     low_iqr = qunt25-iqr
#     high_iqr = iqr+qunt75
    
#     outlier_list_col = fraud_column_data[(fraud_column_data<low_iqr)|(fraud_column_data>high_iqr)].index
#     print(len(outlier_list_col))
#     d_cp.drop(outlier_list_col,axis=0,inplace=True)

#     return d_cp

# remove_outlier(df1,'total')
# 뭔가 total에 대한 이상치가 많았으니깐 날리거나 nomalization해주면 accuracy가 좀 더 맞지 않을까? 생각했었는데 다 날라갔음

order_data_11 = df1['order_date'] == '2011-11'
order_data_10 = df1['order_date'] == '2011-10'
od10 = df1[order_data_10]
od11 = df1[order_data_11]

print(od11)
# 2011-11월의 값만 출력

sns.countplot(od11['over300'])
plt.show()

X = od10['over300']
y = od11['over300']

sns.distplot(X)
plt.show()

print('acc: {}'.format(accuracy_score(X,y)))
print('precision: {}'.format(precision_score(X,y)))
print('recall: {}'.format(recall_score(X,y)))
print('f1: {}'.format(f1_score(X,y)))
print(roc_auc_score(X,y))
X = od10[['over300']]
y = od11[['over300']]

clf = LogisticRegression()
clf.fit(X,y)
temp = pd.DataFrame(clf.coef_)
print(temp)


# od12 = od11.copy()
# od12.loc[od12['order_date']!= '2011-12','order_date'] = '2011-12'

# print(od12['order_date'].value_counts())
# print(od12)

# od12['quantity'],od12['price'] = 0, 0
# od12['total'] = od12['quantity']*od12['price']

# od12.loc[od12['total']>300,'over300'] = 1
# od12.loc[od12['total']<=300,'over300'] = 0
# print(od12)
# # 12월달은 데이터가 없기 때문에 다 0을 집어넣어줬음

# X = od11['over300']
# y = od12['over300']

# print(roc_auc_score(X,y))
# 하나의 변수는 모두 0이기 때문에 0.5가 나옴

label1['total'] = label1['total']/300
od10.rename(columns={'over300':'label','order_date':'year_month'},inplace=True)
od11.rename(columns={'over300':'label','order_date':'year_month'},inplace=True)

p = pd.concat([od10,label1]) # 10월과 12월을 묶은 거

j = pd.concat([od11,label1]) # 11월과 12월을 묶은 거

X = p.drop(['year_month'],axis=1).fillna(0)
y = j['label']

print(y)
print(X)

#label인코더를 넣어서 구해줬음

X_train,X_test,y_train,y_test = train_test_split(X,y,random_state=100)

clf.fit(X_train,y_train)
print(clf.score(X_train,y_train))

y_pred = clf.predict(X_test)
print(roc_auc_score(y_test,y_pred))
print(accuracy_score(y_test,y_pred))

xgb = XGBClassifier(n_estimators=400,learning_rate=0.1,random_state=100)
xgb.fit(X_train,y_train)
xgb_pred = xgb.predict(X_test)

print(roc_auc_score(y_test,xgb_pred))