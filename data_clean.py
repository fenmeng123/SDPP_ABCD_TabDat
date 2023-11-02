# -*- coding: utf-8 -*-
"""
Created on Fri Aug 18 14:13:14 2023

target: clean the data and check the missing value and outliers
outline:
    1. get the number of participants for each event
    2. get the number of participants administering in different numbers of events
    3. draw the age and sex distribution of participants
    4. check the sex and gender differences 
    5. count missing values for each column and each event
    6. KNN imputation for missing values
    7. normalization
    8. kstest to test if it follows the normal distribution
    9. generate the descriptive overview for all the data
input file: 'D:/ABCD/V5simplified/final_raw.csv'
output file: 'D:/ABCD/V5simplified/final_clean.csv'
    'D:/ABCD/V5simplified/final_overview.csv'
 
@author: zhang
"""

import pandas as pd
import numpy as np
import os
import matplotlib.pyplot as mp
import seaborn as sns
import missingno as msno
import miceforest as mf
from sklearn.impute import KNNImputer
from scipy import stats
from sklearn import preprocessing
from scipy.stats import percentileofscore as pcos

def only_top(x):
    return x.iloc[0,:]
# load the data
df = pd.read_csv('D:/ABCD/V5simplified/final_raw.csv')



## 7. normalization
scaler = preprocessing.StandardScaler()
df_scaled = pd.DataFrame()
df_demo = df.iloc[:,0:9]
df_toscale = df.iloc[:,9:]
df_scaled = scaler.fit_transform(df_toscale)
df_scaled = pd.DataFrame(df_scaled, columns=df_toscale.columns)
df_scaled = pd.concat([df_demo, df_scaled], axis=1)

## percentile transformation of variable
"""
for col in df.columns[9:]:
    col_se = df[['Subject','Event',col]].dropna(how='any', axis=0)
    col_se[col] = pd.DataFrame(stats.rankdata(col_se[col], 'average')/len(col_se[col])) * 100
    df[col] = col_se[col]
    print(f'{col} is done.')
"""

## 6. KNN imputation for missing values
cols = df.columns
sq_df = pd.concat([df_scaled['Event'],df_scaled.iloc[:,9:31]], axis=1)
mpiq_df = pd.concat([df_scaled['Event'],df_scaled.iloc[:,31:40]], axis=1)
sm_df = pd.concat([df_scaled['Event'],df_scaled.iloc[:,40:54]], axis=1)
peer_df = pd.concat([df_scaled['Event'],df_scaled.iloc[:,54:63]], axis=1)
peq_df = pd.concat([df_scaled['Event'],df_scaled.iloc[:,63:89]], axis=1)
cbb_df = pd.concat([df_scaled['Event'],df_scaled.iloc[:,89:97]], axis=1)
bisbas_df = pd.concat([df_scaled['Event'],df_scaled.iloc[:,97:]], axis=1)
df_list = [sq_df, mpiq_df, sm_df, peer_df, peq_df, cbb_df, bisbas_df]

df_imputed = df_scaled.iloc[:,0:9].sort_values(by=['Event','Subject'])
df_imputed = df_imputed.reset_index().drop(labels=['index'],axis=1)
for idx, dff in enumerate(df_list):
    im_dff = pd.DataFrame()
    for eve in ['V0','V1','V2','V3','V4']:
        dff_eve = dff.loc[dff['Event'] == eve].reset_index().drop(labels=['index','Event'],axis=1)
        dff_na = dff_eve.loc[:,dff_eve.isnull().all()]
        dff_eve_nona = dff_eve.dropna(how='all', axis=1)
        if dff_eve_nona.empty != True:
            imputer = KNNImputer()
            imputed = imputer.fit_transform(dff_eve_nona)
            dff_eve_imputed = pd.DataFrame(imputed, columns=dff_eve_nona.columns)
        else:
            dff_eve_imputed = dff_eve_nona
        dff_eve_imputed = pd.concat([dff_eve_imputed,dff_na],axis=1)
        dff_eve_imputed = dff_eve_imputed[list(dff_eve.columns)]
        im_dff = pd.concat([im_dff, dff_eve_imputed], axis=0)
        im_dff = im_dff.reset_index().drop(labels=['index'],axis=1)
    df_imputed = pd.concat([df_imputed,im_dff], axis=1)

df_imputed = df_imputed.sort_values(by=['Subject','Event'], axis=0)
#df_imputed.insert(55,'Loneliness',df['Lone'])

df_imputed.to_csv('D:/ABCD/V5simplified/final_scale_imputed.csv', index=False)
#df_scaled.to_csv('D:/ABCD/V5simplified/final_normal.csv', index=False)
df_imp_ow = df_imputed.groupby('Event').describe().T
df_imp_ow.to_csv('D:/ABCD/V5simplified/final_overview.csv', index=False)


sub_count_imp = df_imputed.groupby('Event').count()
#  9. kstest to test if it follows the normal distribution
"""
kstest = pd.DataFrame(index=df.columns[9:])
for eve in ['V0','V1','V2','V3','V4']:
    subdf = df.loc[df['Event'] == eve]
    ks_list = list()
    for col in df.columns[9:]:
        s,p = stats.shapiro(df[col])
        ks_list.append(p)
    kstest[eve +'kstest_p'] = ks_list
"""
## 1&2. get the number of participants for each event and variable
sub_count = df.groupby('Event').count()

# 9. generate the descriptive overview for all the data
df_ow = df.groupby('Event').describe().T

## 3. draw the age and sex distribution histogram of participants
filepath = 'D:/ABCD/V5simplified/figure'
for eve in ['V0','V1','V2','V3','V4']:
    subdf = df.loc[df['Event'] == eve]
    subdf = subdf.dropna(subset=['YouthAge_P'], how='any', axis=0)
    subdf['YouthAge_P'] = subdf['YouthAge_P'].astype('int')
    mp.figure(figsize=(15,15))
    g = sns.catplot(data = subdf, x='YouthAge_P', kind='count', hue='SexBorn', palette='dark', alpha=0.5, height=4)            
    g.set_ylabels('Number')
    g.set_xlabels(f'Age in {eve}')
    mp.text(1,1, f"N={len(subdf['YouthAge_P'])}",fontsize= 20)
    mp.savefig(filepath+f'/Age_distribution_{eve}.jpg')
    mp.close()
    
for col in df.columns[9:10]:
    mp.figure(figsize=(15,15))
    g = sns.catplot(data = df, x= col, kind='', palette='dark', alpha=0.5, height=4)            
    g.set_ylabels('Number')
    g.set_xlabels(f'{col}')
    mp.savefig(filepath+f'/histogram/{col}.jpg')
    mp.close()

## 4. check the sex and gender differences 
sex_dict = dict()
for eve in ['V0','V1','V2','V3','V4']:
    subdf = df.loc[df['Event'] == eve]
    sex_diff = list(subdf['Subject'].loc[subdf['SexBorn'] != subdf['YouthGender_P']])
    sex_dict[eve] = [i for i in sex_diff]
sex_len = [len(value) for value in sex_dict.values()]
sex_diff_count = pd.DataFrame()
sex_diff_count['Event'] = ['V0','V1','V2','V3','V4']
sex_diff_count['Number'] = sex_len
sex_diff_count['Subject'] = sex_dict.values()

## 5. count missing values for each column and each event
for eve in ['V0','V1','V2','V3','V4']:
    subdf = df.loc[df['Event'] == eve]
    # missing value matrix
    msno.matrix(subdf,labels=False,figsize = (15,10))
    mp.savefig(filepath + f'/missmatrix_{eve}.tiff')
    mp.close()
    #缺失条形图
    msno.bar(subdf,figsize = (50,35))
    mp.savefig(filepath + f'/misshisto_{eve}.tiff')
    mp.close()
    #缺失相关性，如为1，就代表一个缺失，另一个也肯定缺失，如为-1，就代表一个缺失，另一个肯定不缺失
    msno.heatmap(subdf,figsize = (50,35))
    mp.savefig(filepath + f'/missheatmap_{eve}.tiff')
    mp.close()
    #缺失树状图
    msno.dendrogram(subdf,figsize=(50,35))
    mp.savefig(filepath + f'/missdendrogram_{eve}.tiff')
    mp.close()
    
# test infinity 
inf_l = list()
for col in df.columns[9:]:
    if np.isinf(df[col]).any() == True:
        inf_l.append(col)    
        
