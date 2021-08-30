import numpy as np
import pandas as pd
import sklearn 
import statsmodels.api as sm
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.linear_model import LinearRegression,Ridge,Lasso
from sklearn.model_selection import train_test_split, cross_val_score
from sklearn.preprocessing import PolynomialFeatures, StandardScaler
from sklearn.ensemble import AdaBoostClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.tree import DecisionTreeClassifier
from sklearn import datasets
from sklearn.model_selection import train_test_split
from sklearn import metrics
from sklearn.metrics import confusion_matrix, accuracy_score, precision_score, recall_score
from sklearn.metrics import confusion_matrix, f1_score, roc_auc_score
import sys
import os
import statistics

dir = os.path.dirname(os.path.realpath(__file__))
df = pd.read_csv(dir+'./train.csv')

print(df)               # order_id, product_id, description, Qty, price 분석에 필요x / 이 코멘트들은 처음에 썼던 코멘트임
print(df.info())        # order date type이 시분초까지 다 나와있는 object -> 조절 필요
print(df.describe())    # Qty 및 total에 마이너스값이 있지만 환불일 것으로 추정(고객별 합계 확인해서 더블체크 필요)
print(df.isna().sum())  # 공란데이터 없음

# ## order date 형식 변경(datetime 사용)
df['order_date'] = pd.to_datetime(df['order_date'], format='%Y-%m', errors='raise')
print(df['order_date'])
print(df.info())        #object -> datetime64로 변경됨



#연간/월간 전체 소비추세 확인
#order date를 활용해 year / month 컬럼 생성
df['year'] = df['order_date'].dt.year
df['month'] = df['order_date'].dt.month
print(df.year)
print(df.month)
# 연도별,월별 total 합계 그래프
print(df.pivot_table(values='total', index='month', columns='year', aggfunc=sum))
accsum_total = df.pivot_table(values='total', index='month', columns='year', aggfunc=sum)
ax=accsum_total.plot(marker = 'o')
# plt.show()        # sum of total(per year,month).png 파일 확인
#  9월부터 소비가 증가해 11월 최대치, 12월은 떨어짐(연간 비슷한 추세를 보임)
#  2011년 12월의 소비를 예측하는 것이기에, 전체 월별실적을 변수로 하는 것 보다는 
#    "고객별 2009-12 ~ 2010-11을 변수 / 2010-12 total 계가 300이 넘는지를 y로 모델 학습 및 테스트"
#    "해당 모델을 동일기간인 2010-12 ~ 2011-11을 변수로 하는 데이터에 적용하여 2011-12를 예측"
#     하여 year, month컬럼은 만들 필요가 없고 yyyy-mm의 형태 데이터 필요
df=df.drop(columns = 'year')
df=df.drop(columns = 'month')
df['yyyy-mm'] = df['order_date'].dt.strftime('%Y-%m')
print(df)





# Groupby
df_groupsum = df.groupby('customer_id').sum().reset_index()
print(df_groupsum) 
df_groupsum1 = df.groupby(['customer_id', 'yyyy-mm']).sum().reset_index()
print(df_groupsum1) # id별 월별 total계 -> 무시



# # 40개국가를 total계 오름차순으로 정렬해서 rank 부여 (1~41) 후 df에 추가 (범주화화고싶었지만 실패)
df_groupcountry = df.groupby('country').sum().reset_index()
print(df_groupcountry.sort_values(by='total', ascending=True)) 
grouped_country = df_groupcountry.sort_values(by='total', ascending=True)
grouped_country = grouped_country.drop(columns = ['quantity', 'price','customer_id'])
print(grouped_country)
# print(grouped_country.iloc[:8])
# print(grouped_country.iloc[8:16])
# print(grouped_country.iloc[16:24])
# print(grouped_country.iloc[24:32])
# print(grouped_country.iloc[32:])
grouped_country['r_country'] = grouped_country['total'].rank(method = 'min', ascending = True)
grouped_country = grouped_country.drop(columns = 'total')
print(grouped_country)
df.insert(8,'r_country',df['country'].map(grouped_country.set_index('country')['r_country']))
print(df)

df_groupcountry = df.groupby(['customer_id', 'r_country']).sum().reset_index()
print(df_groupcountry) #customer_id - r_country 연결시키기 위한 데이터셋 (5927rows : 13개 rows가 증가됨) -> country가 2개이상 등록된 아이디가 13개 있었는듯
df_groupcountry1 = df_groupcountry.drop_duplicates(['customer_id'])
print(df_groupcountry1) #id별 r_country와 total(sum적용됨) 조회 가능


# product_id 변수추가 시도 (중요도는 높은 변수로 잡히지만 AUC점수는 떨어짐)
df['p3'] = df['product_id'].str[:3]
grouped_product = df.groupby('p3').sum().reset_index()
grouped_product['r_product'] = grouped_product['total'].rank(method = 'min', ascending = True) # product 3자리에 1~128위의 rank 부여
grouped_product2 = df.groupby(['customer_id', 'p3']).sum().reset_index().sort_values(by="total", ascending=False)
grouped_product2 = grouped_product2.drop_duplicates(['customer_id'])
grouped_product2.insert(2, 'r_product', grouped_product2['p3'].map(grouped_product.set_index('p3')['r_product']))
print(grouped_product)  # 3글자만 따온 p3의 total에따라 rank 부여한 r_product 생성
print(grouped_product2) # customer_id별로 대표p3 선정 후, r_product 맵핑 -> 최종 변수에 추가








# ####################### 데이터 전처리(?) 시작 #################################



# df를 df0로 복사 후 필요없는 컬럼 삭제
df0 = df.copy() 
df0 = df0.drop(columns = ['order_id', 'product_id', 'description', 'quantity', 'price'])
print(df0)


'''
# # yyyy-mm 항목 생성
df0['yyyy-mm'] = df['order_date'].dt.strftime('%Y-%m')
print(df0)
# yyyy-mm & country total 합계 그래프
accsum_total = df0.pivot_table(values='total', index='yyyy-mm', columns='country', aggfunc=sum)
ax=accsum_total.plot(marker = 'o')
plt.show()
print(df0.pivot_table(values='total', index='yyyy-mm', columns='country', aggfunc=sum))
#  -> 40개 국가.. 추구하는게 고객id별 12월 예측이기에 국가가 큰 영향을 미치지 않을것이라고 추정
#  -> 우선 분석에서 제외 후 모델정확도가 떨어지면 그때 다시 고려
#  -> 21.04.27 : 넣어봤더니 AUC 더 떨어짐.. 다시 삭제
#                (이미 월별금액이 있는데 country를 오름차순정렬하여 rank변환한 자료도 결국 금액정보이기 때문일 것으로 추정)
'''


# 2009-12 ~ 2011-11 id별 total계를 구성하는 새로운 데이터셋 df1 생성(pivot활용)
df1 = df0.pivot_table(values='total', index='customer_id', columns='yyyy-mm', aggfunc=sum)
print(df1)    # [780502 rows x 5 columns] -> [5914 rows x 24 columns]
df1 = df1.fillna(0)     # NaN값은 의사결정트리 분석이 작동하지 않기에 0으로 채움
print(df1.info())


# #model생성을 위한 데이터셋(x : 2009-12 ~ 2010-11 / y : 2010-12)
df_t = df1.iloc[:,:13]  
df_t['y'] = np.where(df_t['2010-12']>300, 1, 0)     # 0 : 300이하 / 1: 300초과
df_t=df_t.drop(columns = '2010-12')
print(df_t)
df_t.columns = ['12m_before','11m_before','10m_before','09m_before','08m_before','07m_before','06m_before','05m_before','04m_before','03m_before','02m_before','01m_before','y']



# #생성한 모델을 적용할 데이터셋(x : 2010-12 ~ 2011-11 / y : 아직없음)
df_p = df1.iloc[:,12:]  
print(df_p)
df_p.columns = ['12m_before','11m_before','10m_before','09m_before','08m_before','07m_before','06m_before','05m_before','04m_before','03m_before','02m_before','01m_before']





# ###########추가된 변수 관련 : 넣으니까 점수 떨어짐 (AUC : 0.66 -> 0.65) 조금있다가 다시
df_t['customer_id'] = df_t.index
df_t.insert(0,'r_country',df_t['customer_id'].map(df_groupcountry1.set_index('customer_id')['r_country']))
df_t.insert(1,'t_total',df_t['customer_id'].map(df_groupcountry1.set_index('customer_id')['total']))
df_t.insert(2,'r_product',df_t['customer_id'].map(grouped_product2.set_index('customer_id')['r_product']))
df_t=df_t.drop(columns = 'customer_id')
print(df_t)
df_p['customer_id'] = df_p.index
df_p.insert(0,'r_country',df_p['customer_id'].map(df_groupcountry1.set_index('customer_id')['r_country']))
df_p.insert(1,'t_total',df_p['customer_id'].map(df_groupcountry1.set_index('customer_id')['total']))
df_p.insert(2,'r_product',df_p['customer_id'].map(grouped_product2.set_index('customer_id')['r_product']))
df_p=df_p.drop(columns = 'customer_id')
print(df_p)
# # 중요도 낮거나 상관관계 높은 변수 삭제시도 : 효과x
df_t = df_t.drop(columns = ['04m_before', '01m_before', 'r_country'])
df_p = df_p.drop(columns = ['04m_before', '01m_before', 'r_country'])





# 상관관계 그래프
def correlation_heatmap(df_t):
    _ , ax = plt.subplots(figsize =(14, 12))
    colormap = sns.diverging_palette(220, 10, as_cmap = True)
    
    _ = sns.heatmap(
        df_t.corr(), 
        cmap = colormap,
        square=True, 
        cbar_kws={'shrink':.9 }, 
        ax=ax,
        annot=True, 
        linewidths=0.1,vmax=1.0, linecolor='white',
        annot_kws={'fontsize':12 }
    )
correlation_heatmap(df_t)
# plt.show()











##################################### df_t 로 분석시작 ###################################

X = df_t.drop(['y'], axis=1)
y = df_t['y']
X_train, X_test, y_train, y_test = train_test_split(X, y, random_state=42) 
# print(X_train.shape)
# print(y_train.shape)
# print(X_test.shape)
# print(y_test.shape)


# DecisionTree Classifier 생성 및 학습
clf = DecisionTreeClassifier(max_depth=5,random_state=0)
clf_model = clf.fit(X_train , y_train)
clf_y_pred = clf.predict(X_test)
print("DecisionTree Accuracy on train set: {:.3f}".format(clf.score(X_train, y_train)))
print("DecisionTree Accuracy on test set: {:.3f}".format(clf.score(X_test, y_test)))


# randomforest 분석
rf = RandomForestClassifier(n_estimators=100, max_depth=4, min_samples_split=5)
rf_model = rf.fit(X_train, y_train)
rf_y_pred = rf.predict(X_test)
print("RandomForest Train score: ",rf.score(X_train, y_train))
print("RandomForest Test score: ",rf.score(X_test, y_test))
# print("RandomForest Test score: ",metrics.accuracy_score(y_test, rf_y_pred))  - 위에꺼랑 다른표현 같은결과값



# AdaBoost classifier
ada = AdaBoostClassifier(n_estimators=50,
                         learning_rate=1,random_state = 7777)
ada_model = ada.fit(X_train, y_train)
ada_y_pred = ada_model.predict(X_test)  
print("Ada boosting Train Accuracy:",ada.score(X_train, y_train))
print("Ada boosting Test Accuracy:",metrics.accuracy_score(y_test, ada_y_pred))
# print("Ada boosting Test Accuracy:",ada.score(X_test, y_test)) - 위에꺼랑 다른표현 같은결과값




# XGBoost classifier
      # https://wooono.tistory.com/97     - 설명참고
      # https://injo.tistory.com/44       - 모델세팅 참고

from xgboost import XGBClassifier
import xgboost as xgb

# 넘파이 형태의 학습 데이터 세트와 테스트 데이터를 DMatrix로 변환
dtrain = xgb.DMatrix(data=X_train, label = y_train)
dtest = xgb.DMatrix(data=X_test, label=y_test)

# parameters 변경해가며 돌리기
params = {'booster' : 'gbtree', 'max_depth' : 3,
         'eta' : 0.1, 
         'objective' : 'binary:logistic',
         'eval_metric' : 'logloss',
         'early_stoppings' : 100, 'lambda':10, 'colsample_bytree' : 0.5}
num_rounds = 300

# train 데이터 세트는 'train', evaluation(test) 데이터 세트는 'eval' 로 명기
wlist = [(dtrain, 'train'), (dtest,'eval')]
# 하이퍼 파라미터와 early stopping 파라미터를 train() 함수의 파라미터로 전달
xgb_model = xgb.train(params = params, dtrain=dtrain, num_boost_round=num_rounds, evals=wlist)

pred_probs = xgb_model.predict(dtest)
print('predict() 수행 결과값을 10개만 표시, 예측 확률 값으로 표시됨')
print(np.round(pred_probs[:10], 3))

# 예측 확률이 0.5보다 크면 1, 그렇지 않으면 0으로 예측값 결정해 리스트 객체인 preds에 저장
preds = [ 1 if x > 0.5 else 0 for x in pred_probs]
print('예측값 10개만 표시: ', preds[:10])

# 혼동행렬, 정확도, 정밀도, 재현율, F1, AUC 불러오기
import warnings
def get_clf_eval(y_test, y_pred):
    confusion = confusion_matrix(y_test, y_pred)
    accuracy = accuracy_score(y_test, y_pred)
    precision = precision_score(y_test, y_pred)
    recall = recall_score(y_test, y_pred)
    F1 = f1_score(y_test, y_pred)
    AUC = roc_auc_score(y_test, y_pred)
    print('오차행렬:\n', confusion)
    print('\n정확도: {:.4f}'.format(accuracy))
    print('정밀도: {:.4f}'.format(precision))
    print('재현율: {:.4f}'.format(recall))
    print('F1: {:.4f}'.format(F1))
    print('AUC: {:.4f}'.format(AUC))

get_clf_eval(y_test, preds)
'''
from xgboost import plot_importance
import matplotlib.pyplot as plt
fig, ax = plt.subplots(figsize=(10, 12))
plot_importance(xgb_model, ax=ax)

xgb.plot_tree(xgb_model, num_trees=0, rankdir='LR')
fig = plt.gcf()
fig.set_size_inches(150, 100)
# plt.show()
'''


# Light GBM
from lightgbm import LGBMClassifier
from lightgbm import plot_importance
# from sklearn.datasets import load_breast_cancer
# from sklearn.model_selection import train_test_split

lgbm_wrapper = LGBMClassifier(n_estimators=400)
evals = [(X_test, y_test)]
lgbm_wrapper.fit(X_train, y_train, early_stopping_rounds=100, eval_metric='logloss', eval_set=evals, verbose=True)
pred = lgbm_wrapper.predict(X_test)
pred_proba = lgbm_wrapper.predict_proba(X_test)[:1]

# fig, ax = plt.subplots(figsize=(10,12))
# plot_importance(lgbm_wrapper, ax=ax)
# plt.show()

lgbm_wrapper_model = lgbm_wrapper.fit(X_train, y_train)
lgbm_wrapper_y_pred = lgbm_wrapper.predict(X_test)
print("Light GBM Train score: ",lgbm_wrapper.score(X_train, y_train))
print("Light GBM Test score: ",lgbm_wrapper.score(X_test, y_test))



# Cat Boost Classifier
import catboost
from catboost import CatBoostClassifier, Pool

params = {'loss_function':'Logloss', # objective function
          'eval_metric':'AUC', # metric
          'verbose': 200, # output to stdout info about training process every 200 iterations
          }
cbc = CatBoostClassifier(**params)
cbc.fit(X_train, y_train, # data to train on (required parameters, unless we provide X as a pool object, will be shown below)
          eval_set=(X_test, y_test), # data to validate on
          use_best_model=True, # True if we don't want to save trees created after iteration with the best validation score
          );
print("CatBoost Train score: ",cbc.score(X_train, y_train))
print("CatBoost Test score: ",cbc.score(X_test, y_test))
feature_importance_df = pd.DataFrame(cbc.get_feature_importance(prettified=True))
print(feature_importance_df)
plt.figure(figsize=(12, 6));
sns.barplot(x="Importances", y="Feature Id", data=feature_importance_df);
plt.title('CatBoost features Importances:');
# plt.show()


################################### df_p에 모델 적용 #####################################

# final 모델 적용 : Cat Boost (다 돌려본결과 정확도가 약간 떨어져도 최종 AUC는 이 모델이 가장 잘나옴)
    # clf_model : DecisionTree Accuracy on test set: 0.903
    # rf_model : RandomForest Train score:  0.90736984448952
    # ada_model : Ada boosting Accuracy: 0.9046653144016227
    # xgb_model : 0.9074
    # lgbm_wrapper_model : Light GBM Test score : 0.8904665314401623
    # cbc : Cat Boost Test Score : 0.9066937119675457

# Save model to disk - pickle 라는 기능?
import pickle
filename = 'Final_Model.sav'
pickle.dump(cbc, open(filename, 'wb')) #적용할 모델을 여기에 입력
# Load model from disk and use it to make new predictions
loaded_model = pickle.load(open(filename, 'rb'))
result = loaded_model.score(X_test, y_test)
print("loaded model score : ",result)
# Load test dataset
for_predict = df_p.copy()
pred = loaded_model.predict(for_predict)    # 모델을 적용한 예측값
print(pred)
print(len(pred)) # y값이 5914개로 잘 나옴 -> 행으로 옮기면됨
# 예측값 채우기
pred = np.array(pred)
pred = pred.reshape(-1,1)
df_p['pred'] = pred
print(df_p)
print(df_p['pred'].value_counts())




################################################################################
# 실제 12월 데이터와 비교

result = pd.read_csv(dir+'./label.csv')
result['pred'] = pred
result['compare'] = np.where(result['label'] == result['pred'], 1, 0)   #일치하면 1, 불일치하면 0
print(result)
print(result['compare'].value_counts())
print(result['compare'].value_counts(1))

# confusion matrix (label vs pred)
cm1=confusion_matrix(result['label'], result['pred'])
plt.figure(figsize=(10,7))
sns.heatmap(cm1,annot=True,fmt='d')
plt.xlabel('predicted by model')
plt.ylabel('Truth(label)')
# plt.show()

# 전년도 실적을 기반으로 한 예측모델 성능 최종 결론
accuracy = np.mean(np.equal(result['label'],result['pred']))
right = np.sum(result['label'] * result['pred'] == 1)
precision = right / np.sum(result['pred'])
recall = right / np.sum(result['label'])
f1 = 2 * precision*recall/(precision+recall)
print('Final apply model : Cat Boost')
print('accuracy : ',accuracy)
print('precision : ', precision)
print('recall : ', recall)
print('f1 : ', f1)
print('AUC : ', roc_auc_score(result['label'],result['pred']))

# AUC graph
from sklearn.metrics import roc_curve, auc
false_positive_rate, true_positive_rate, thresholds = roc_curve(result['label'],result['pred'])
roc_auc = auc(false_positive_rate, true_positive_rate)
plt.figure(figsize=(10,10))
plt.title('Receiver Operating Characteristic')
plt.plot(false_positive_rate,true_positive_rate, color='red',label = 'AUC = %0.2f' % roc_auc)
plt.legend(loc = 'lower right')
plt.plot([0, 1], [0, 1],linestyle='--')
plt.axis('tight')
plt.ylabel('True Positive Rate')
plt.xlabel('False Positive Rate')
plt.show()








########################################## 끝 ########################################


















################################### Optuna / MLA 연습 ################################


# Optuna로 xgb돌린결과 accuracy[0.907370] 으로 위에서 직접 설정했던 parameter로 돌렸던 결과와 동일
''' 
# Optuna : 하이퍼파라미터 최적화 프레임 워크 
# import the other libraries
import optuna # for hyper parameter tuning
from xgboost import XGBClassifier as cls
from sklearn.model_selection import StratifiedKFold, cross_val_score, cross_validate, train_test_split
from sklearn import datasets
from sklearn.metrics import make_scorer, accuracy_score
from functools import partial

def  (X, y, trial):
    params = {
        'booster':trial.suggest_categorical('booster', ['gbtree', 'dart', 'gblinear']),
        'learning_rate':trial.suggest_uniform("learning_rate", 0.0, 1.0),
        'max_depth':trial.suggest_int("max_depth", 2, 30),
        'subsample':trial.suggest_uniform("subsample", 0.0, 1.0),
        'colsample_bytree':trial.suggest_uniform("colsample_bytree", 0.0, 1.0),
    }

    model = cls(**params)   #cls = xgboost
    score = cross_val_score(model, X, y, cv=5, scoring=make_scorer(accuracy_score))

    return 1 - score.mean()

f = partial(objective, X_train,y_train)

study = optuna.create_study()
study.optimize(f, n_trials=100)

# evaluate the model
model = cls(**study.best_params)
model.fit(X_train, y_train)
y_true = y_test
y_pred = model.predict(X_test)

print('accuracy[%f]' % accuracy_score(y_true, y_pred))
'''











'''
# 일단 CatBoost가 좋다고 나오긴했는데 Accurancy는 많이 떨어지는데..? MLA Test Accuracy Mean : 0.896394 
#################################### MLA predictions #################################

#Machine Learning Algorithm (MLA) Selection and Initialization
from sklearn import svm, tree, linear_model, neighbors, naive_bayes, ensemble, discriminant_analysis, gaussian_process
from xgboost import XGBClassifier
from lightgbm import LGBMClassifier
from catboost import CatBoostClassifier
from sklearn.neighbors import KNeighborsClassifier

#Common Model Helpers
from sklearn.preprocessing import OneHotEncoder, LabelEncoder
from sklearn import feature_selection
from sklearn import model_selection
from sklearn import metrics
X = df_t.drop(['y'], axis=1)
y = df_t['y']
X_train, X_test, y_train, y_test = train_test_split(X, y, random_state=42) 
MLA = [
    #Ensemble Methods
    ensemble.AdaBoostClassifier(),
    ensemble.BaggingClassifier(),
    ensemble.ExtraTreesClassifier(),
    ensemble.GradientBoostingClassifier(),
    ensemble.RandomForestClassifier(),
    ensemble.AdaBoostClassifier(),
    #GLM
    linear_model.LogisticRegressionCV(),
    linear_model.RidgeClassifierCV(),
    linear_model.SGDClassifier(),
    #Nearest Neighbor
    KNeighborsClassifier(),
    #Trees    
    tree.DecisionTreeClassifier(),
    tree.ExtraTreeClassifier(),
    XGBClassifier(),
    LGBMClassifier(),
    CatBoostClassifier()]
#split dataset in cross-validation with this splitter class: http://scikit-learn.org/stable/modules/generated/sklearn.model_selection.ShuffleSplit.html#sklearn.model_selection.ShuffleSplit
#note: this is an alternative to train_test_split
cv_split = model_selection.ShuffleSplit(n_splits = 10, test_size = .3, train_size = .6, random_state = 0 ) # run model 10x with 60/30 split intentionally leaving out 10%
#create table to compare MLA metrics
MLA_columns = ['MLA Name', 'MLA Parameters','MLA Train Accuracy Mean', 'MLA Test Accuracy Mean', 'MLA Test Accuracy 3*STD' ,'MLA Time']
MLA_compare = pd.DataFrame(columns = MLA_columns)
#create table to compare MLA predictions
MLA_predict = y
print('모델 작동전',X.shape,y.shape)
#index through MLA and save performance to table
row_index = 0
for alg in MLA:
    print('for 안에서모델 작동전',X.shape,y.shape)
    #set name and parameters
    MLA_name = alg.__class__.__name__
    MLA_compare.loc[row_index, 'MLA Name'] = MLA_name
    MLA_compare.loc[row_index, 'MLA Parameters'] = str(alg.get_params())
    print('for 안에서모델 작동전1',X.shape,y.shape)
    print('MLA 작동전 shape',MLA_predict.shape)
#     #score model with cross validation: http://scikit-learn.org/stable/modules/generated/sklearn.model_selection.cross_validate.html#sklearn.model_selection.cross_validate
    cv_results = model_selection.cross_validate(alg, X_train, y_train, cv  = cv_split,return_train_score=True)
    print(alg)
    print('for 안에서모델 작동후',X.shape,y.shape)
    print('MLA 작동후 shape', MLA_predict.shape)
    MLA_compare.loc[row_index, 'MLA Time'] = cv_results['fit_time'].mean()
    MLA_compare.loc[row_index, 'MLA Train Accuracy Mean'] = cv_results['train_score'].mean()
    MLA_compare.loc[row_index, 'MLA Test Accuracy Mean'] = cv_results['test_score'].mean()   
    #if this is a non-bias random sample, then +/-3 standard deviations (std) from the mean, should statistically capture 99.7% of the subsets
    MLA_compare.loc[row_index, 'MLA Test Accuracy 3*STD'] = cv_results['test_score'].std()*3   #let's know the worst that can happen!
    #save MLA predictions - see section 6 for usage
    alg.fit(X_train,y_train)
    MLA_predict[MLA_name] = alg.predict(X_test)
    row_index+=1
MLA_compare.sort_values(by = ['MLA Test Accuracy Mean'], ascending = False, inplace = True)
print(MLA_compare)

#barplot using https://seaborn.pydata.org/generated/seaborn.barplot.html
sns.barplot(x='MLA Test Accuracy Mean', y = 'MLA Name', data = MLA_compare, color = 'm')

#prettify using pyplot: https://matplotlib.org/api/pyplot_api.html
plt.title('Machine Learning Algorithm Accuracy Score \n')
plt.xlabel('Accuracy Score (%)')
plt.ylabel('Algorithm')
plt.show()
'''