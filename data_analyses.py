# -*- coding: utf-8 -*-
"""
Created on Tue Aug 29 13:42:22 2023
target: correlation analyses and within-subject t-test for event
outline:
    1. load the data
    2. correlation analyses for the big sample
    3. t-test store in a pd.DataFrme
    4. draw the merged violinplot
    5. exact the small sample only remaining participants including ears sub dataset
    6. correlation analyses for teh small sample
input file:
    D:/ABCD/V5simplified/final_clean.csv
    D:/ABCD/V5simplified/nt_y_ears_simplified_V1.csv
output file:
    filepath: D:/ABCD/V5simplified/
    figure filepath: D:/ABCD/V5simplified/figure/
    
@author: zhang
"""
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.linear_model import LinearRegression
import scipy.stats as st

## 1. load the data
df = pd.read_csv('D:/ABCD/V5simplified/final_scale_imputed.csv')
df = df.replace({' ':np.nan})

## select highly related variables, and only remain participants which includes complete data
# drop participants with no SMA in V2 V3 V4
df_dropna = df.dropna(subset=['SMA_TOTAL'], how='any',axis=0)
df_dropna_count = df_dropna.groupby('Subject').apply(lambda x: "+".join(list(x['Event'])))
df_dropna_count = pd.DataFrame(df_dropna_count, columns=['Events'])
df_dropna_count = df_dropna_count.reset_index()
df_dropna_count = df_dropna_count.rename({'index':'Subject'})
num_tracing = df_dropna_count.groupby('Events').count()
subs_V4 = df_dropna_count.loc[df_dropna_count['Events'].isin(['V2+V3+V4'])]['Subject']
df_dropna = df_dropna.loc[df_dropna['Subject'].isin(list(subs_V4))]
cols = df_dropna.columns

# drop video, surfing, editphoto (not complete)
# remain the item score and the sum score of SMA 
# remain the sub-dimension score and the overall score of PEQ
df_dropna = df_dropna[['Subject','SexBorn','Event', 'weekday_TV','weekday_Text','weekday_SocialApps',
                       'weekday_VideoChat','weekday_SingleGame','weekday_MultiGame','weekend_TV',
                       'weekend_Text','weekend_SocialApps','weekend_VideoChat','weekend_SingleGame',
                       'weekend_MultiGame','R_rated_Games','R_rated_Movies',
                       'SMA_1','SMA_2','SMA_3','SMA_4','SMA_5','SMA_6','SMA_TOTAL', 
                       'PEQ_Relation_Aggrs','PEQ_Reputation_Aggrs','PEQ_Overt_Aggrs',
                       'PEQ_Relation_Victs','PEQ_Reputation_Victs','PEQ_Overt_Victs',
                       'PEQ_Aggressions','PEQ_Victims']]

#df_dropna = df_dropna.loc[df_dropna['SexBorn'] == 'Female']
# recode sex and drop the intersex participants
df_dropna = df_dropna.replace({'Female':0,'Male':1})
df_dropna = df_dropna.loc[df_dropna['SexBorn'].isin([0,1])]

# store the data
df_dropna.to_csv('D:/ABCD/V5simplified/variable_beforePCA.csv', index=False)


## form the simplified csv file as input file in Mplus

## cluster-related input file: transform variables of different events in each column into respective columns
#df_dropna = df_dropna.loc[df_dropna['SexBorn'] == 'Female']
df_dropna = pd.read_csv('D:/ABCD/V5simplified/final_imputed_afPCA.csv')

df_V2 = df_dropna.loc[df_dropna['Event'] == 'V2']
df_V2 = df_V2.dropna(how='any', axis=0)
df_V2.columns = [i + '_V2' for i in list(df_V2.columns)]
df_V2 = df_V2.drop(labels=['Event_V2'],axis=1)
df_V2 = df_V2.reset_index().drop(labels=['index'],axis=1)

df_V3 = df_dropna.loc[df_dropna['Event'] == 'V3']
df_V3 = df_V3.dropna(how='any', axis=0)
df_V3.columns = [i + '_V3' for i in list(df_V3.columns)]
df_V3 = df_V3.drop(labels=['Subject_V3','SexBorn_V3','Event_V3'],axis=1)
df_V3 = df_V3.reset_index().drop(labels=['index'],axis=1)

df_V4 = df_dropna.loc[df_dropna['Event'] == 'V4']
df_V4 = df_V4.dropna(how='any', axis=0)
df_V4.columns = [i + '_V4' for i in list(df_V4.columns)]
df_V4 = df_V4.drop(labels=['Subject_V4','SexBorn_V4','Event_V4'],axis=1)
df_V4 = df_V4.reset_index().drop(labels=['index'],axis=1)

df_V234 = pd.concat([df_V2, df_V3, df_V4], axis=1)
cols = df_V234.columns


df_V234.to_csv('D:/ABCD/V5simplified/cluster_head.csv', index=False)
df_V234 = df_V234[['SexBorn_V2','PEQ_Aggressions_V2','PEQ_Aggressions_V3','PEQ_Aggressions_V4',
                   'PEQ_Victims_V2','PEQ_Victims_V3','PEQ_Victims_V4',
                   'SM_Usage_V2','SM_Usage_V3','SM_Usage_V4',
                   'SMA_TOTAL_V2','SMA_TOTAL_V3','SMA_TOTAL_V4']]
df_V234.to_csv('D:/ABCD/V5simplified/CLPM_variable.csv', index=False, header=False)

## binarize the PEQ data 
df = pd.read_csv('D:/ABCD/V5simplified/final_imputed_afClus.csv')
df.iloc[:,2:] = df.iloc[:,2:].round(3)
df['PEQ_Victims_binary_V2'] = df['PEQ_Victims_V2']
df['PEQ_Aggressions_binary_V2'] = df['PEQ_Aggressions_V2']

df['PEQ_Victims_binary_V2'].loc[df['PEQ_Victims_binary_V2'] != -0.663] = 1
df['PEQ_Victims_binary_V2'].loc[df['PEQ_Victims_binary_V2'] == -0.663] = 0
df['PEQ_Aggressions_binary_V2'].loc[df['PEQ_Aggressions_binary_V2'] != -0.853] = 1
df['PEQ_Aggressions_binary_V2'].loc[df['PEQ_Aggressions_binary_V2'] == -0.853] = 0

Aggr = list(df['PEQ_Aggressions_binary_V2'])
Vict = list(df['PEQ_Victims_binary_V2'])
Group = list()
for i in range(len(Aggr)):
   if Aggr[i] == 0 and Vict[i] == 0:
       Group.append(1)
   elif Aggr[i] == 1 and Vict[i] == 0:
       Group.append(2)
   elif Aggr[i] == 0 and Vict[i] == 1:
       Group.append(3)
   elif Aggr[i] == 1 and Vict[i] == 1:
       Group.append(4)
       
df['PEQ_binary_Group'] = Group
df.to_csv('D:/ABCD/V5simplified/final_imputed_afGroup.csv', index=False)

cols = df.columns
## scatterplot of different Groups
plt.figure(figsize=(10,7))
kmeans2 = sns.lmplot(data=df, x='PEQ_Aggressions_V2', y='PEQ_Victims_V2', hue='KmeansSub_2clus', height=5)
kmeans2.set_axis_labels('Aggressions_V2','Victims_V2')
plt.savefig('D:/ABCD/V5simplified/figure/scatterplot/kmeans2.tiff', dpi=300)
plt.close()

plt.figure(figsize=(10,7))
kmeans4 = sns.lmplot(data=df, x='PEQ_Aggressions_V2', y='PEQ_Victims_V2', hue='KmeansSub_4clus', height=5)
kmeans4.set_axis_labels('Aggressions_V2','Victims_V2')
plt.savefig('D:/ABCD/V5simplified/figure/scatterplot/kmeans4.tiff', dpi=300)
plt.close()

plt.figure(figsize=(10,7))
binary = sns.lmplot(data=df, x='PEQ_Aggressions_V2', y='PEQ_Victims_V2', hue='PEQ_binary_Group', height=5)
binary.set_axis_labels('Aggressions_V2','Victims_V2')
plt.savefig('D:/ABCD/V5simplified/figure/scatterplot/binary.tiff',dpi=300)
plt.close()

## pieplot
kmeans4Num = df[['Subject_V2','KmeansSub_4clus']].groupby('KmeansSub_4clus').count()
plt.figure(figsize=(10,7))
plt.pie(x=kmeans4Num['Subject_V2'], labels=[1,2,3,4],autopct='%.2f%%')
plt.savefig('D:/ABCD/V5simplified/figure/scatterplot/pie_kmeans4Num.tiff', dpi=300)
plt.close()

kmeans2Num = df[['Subject_V2','KmeansSub_2clus']].groupby('KmeansSub_2clus').count()
plt.figure(figsize=(10,7))
plt.pie(x=kmeans2Num['Subject_V2'], labels=[1,2],autopct='%.2f%%')
plt.savefig('D:/ABCD/V5simplified/figure/scatterplot/pie_kmeans2Num.tiff', dpi=300)
plt.close()

binaryG = df[['Subject_V2','PEQ_binary_Group']].groupby('PEQ_binary_Group').count()
plt.figure(figsize=(10,7))
plt.pie(x= binaryG['Subject_V2'], labels=[1,2,3,4],autopct='%.2f%%')
plt.savefig('D:/ABCD/V5simplified/figure/scatterplot/pie_binaryG.tiff', dpi=300)
plt.close()

## 
## cross-lagged correlation
lag_corr = pd.DataFrame(columns=list(df_V2.columns),index=list(df_V3.columns))

'''
inf_l = list()
for col in df.columns[9:]:
    if np.isinf(df[col]).any() == True:
        inf_l.append(col)   
'''   
        
for col1 in df_V2.columns[2:]:
    #corr_l = list()
    x = df_V2[col1]
    for col2 in df_V3.columns[2:]:
        y = df_V3[col2]
        correlation, p_value = st.pearsonr(np.array(x),np.array(y))
        corr = round(correlation, 2)
        p_value = round(p_value, 2)
        if p_value <=0.05:
            lag_corr.loc[col2,col1] = corr
        else:
            lag_corr.loc[col2,col1] = np.nan



## 2. correlation analyses for the big sample
ears = pd.read_csv('D:/ABCD/V5simplified/nt_y_ears_simplified_V1.csv')
figurepath = 'D:/ABCD/V5simplified/figure/'
cols = final.columns
final = pd.concat([final['Event'], final.iloc[:,9:65], final.iloc[:,83:91], final.iloc[:, 112:].filter(like='_Sum')], axis=1)

for eve in ['V0','V1','V2','V3','V4']:
    df = final.loc[final['Event'] == eve]
    df = df.iloc[:,1:]
    df = df.dropna(how = 'all', axis=1)
    df = df.dropna(how = 'any', axis=0)
    corr_df = pd.DataFrame(columns=list(df.columns),index=list(df.columns))
    for col1 in df.columns:
        #corr_l = list()
        x = df[col1]
        for col2 in df.columns:
            y = df[col2]
            correlation, p_value = st.pearsonr(np.array(x),np.array(y))
            corr = round(correlation, 2)
            p_value = round(p_value, 2)
            if p_value <=0.05:
                corr_df.loc[col2,col1] = corr
            else:
                corr_df.loc[col2,col1] = np.nan
        #corr_df[col1] = corr_l
   # corr_df = corr_df.set_index(list(corr_df.columns))
    corr_df = corr_df.replace(1, np.nan)
    corr_df = corr_df.astype('float64')
    plt.figure(figsize=(30,21), dpi=300)
    sns.heatmap(corr_df, cmap='rainbow', vmin = -0.7, vmax = 0.7)
    plt.savefig(f'{figurepath}corr/corr_{eve}.tiff')
    plt.close()

final_corr = final.corr()
for i in range(final_corr.shape[1]):
    final_corr.iloc[i,i] = np.nan
plt.figure(figsize=(30,21))
sns.heatmap(final_corr, cmap = 'rainbow')
plt.savefig(f'{figurepath}corr_all.tiff')
plt.close()

final_overall = final[['Event','YouthAge_P','weekday_SocialApps','weekend_SocialApps','weekday_VideoChat',
                      'weekend_VideoChat','weekday_Game','weekend_Game','weekday_Text',
                      'weekend_Text','weekday_TOTAL','weekend_TOTAL','MPIQ_TOTAL',
                      'SMA_TOTAL','SOC_Followers','SOC_Following','SOC_Follow_Ratio',
                      'SM_Usage','Loneliness','F_Num','Close_F_Num','Close_Ratio',
                      'Aggression','Victim']]
for eve in ['V0','V1','V2','V3','V4']:
    df = final_overall.loc[final_overall['Event'] == eve]
    df = df.iloc[:,1:]
    df = df.dropna(how = 'all', axis=1)
    corr = df.corr()
    corr = corr.replace(1, np.nan)
    plt.figure(figsize=(15,13))
    sns.heatmap(corr,cmap='rainbow', vmin = -0.7, vmax = 0.7)
    plt.savefig(f'{figurepath}corr_{eve}_overall.tiff')
    plt.close()
    
## 4. draw the merged violinplot
for col in final.columns: 
    subdf = final[col]
    plt.figure(figsize=(15,15))
    f, ax = plt.subplots()
    plt.subplots_adjust(bottom=0.2,left=0.1)
    sns.violinplot(data=subdf, x='variable', y='z-score', hue='Gender', linewidth=1, palette='Spectral_r')
    plt.legend(bbox_to_anchor=(1,1))
    plt.xticks(rotation=60)
    plt.savefig('D:/hcp/MAPnLone/violinplot_gender.jpg',bbox_inces='tight',dpi=300)

## small sample - ears correlation 
ears_corr = ears.corr()
for i in range(ears_corr.shape[1]):
    ears_corr.iloc[i,i] = np.nan
plt.figure(figsize=(15,15),dpi=300)
sns.heatmap(ears_corr, cmap = 'rainbow')
plt.savefig(f'{figurepath}corr_ears.tiff')