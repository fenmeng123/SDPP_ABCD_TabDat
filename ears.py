# -*- coding: utf-8 -*-
"""
Created on Mon Jul 31 13:09:53 2023'

target: reorganize the tabulated phone statistics, which means to only retain data related to social interaction, and only
sum score type.
outline:
    1. load the file
    2. filter the related variables
    3. filter the sum score type
    4. aggregate data of differen phone model
    5. rename those variables
input file: D:/ABCD/abcd-data-release-5.0/core/novel-technologies/nt_y_ears.csv
output file:  
    D:/ABCD/V5simplified/nt_y_ears_simplified_V1.csv
    D:/ABCD/V5simplified/nt_y_ears_simplified_V2.csv
    D:/ABCD/V5simplified/nt_y_ears_and_simplified.csv
    D:/ABCD/V5simplified/nt_y_ears_ios_simplified.csv

there are five elements we should deal with in every var_name: 
    1. app categaries related to social interaction (communication (only android), dating (only android), entertainment, social), 
    2. phone model (android vs. ios), 
    3. score type (sum vs. average), 
    4. data type (keystrokes vs. duration vs. session), 
    5. period type (week day vs. weekend vs overall)

@author: zhang
"""
import pandas as pd
import numpy as np
from functools import reduce

## load the file
ears = pd.read_csv('D:/ABCD/abcd-data-release-5.0/core/novel-technologies/nt_y_ears.csv')
ears_basic = ears.iloc[:,0:3]
ears_basic = ears_basic.set_index('src_subject_id')
ears_cols = ears.columns
ears = ears.set_index('src_subject_id')

## get related variable header
# only retain social interaction related
ears_related = list()
for i in ['communication','dating','entertainment','social','game']:
    for col in ears_cols:
        if i in col:
            ears_related.append(col)
# only retain sum score
ears_sum = list()
for col in ears_related:
    if 'sum' in col:
        ears_sum.append(col)
# retain both phone model
ears_final = list()
for col in ears_sum:
    if '_kb_' in col:
        ears_final.append(col)
# only retain ios model
ears_ios = list()
for col in ears_final:
    if '_ios_' in col:
        ears_ios.append(col)
# only retain android model
ears_and = list()
for col in ears_final:
    if '_and_' in col:
        ears_and.append(col)

# divide data into two parts: android and ios
ears_ios_df = ears[ears_ios]
ears_and_df = ears[ears_and]

# calculate the overal game score
ears_and_game = ears_and_df.filter(like = 'game')

ears_and_game['key_games_wkd_sum'] = ears_and_game.filter(like = '_key_').filter(like = 'wkd').sum(axis=1)
ears_and_game['min_games_wkd_sum'] = ears_and_game.filter(like = '_min_').filter(like = 'wkd').sum(axis=1)
ears_and_game['ses_games_wkd_sum'] = ears_and_game.filter(like = '_ses_').filter(like = 'wkd').sum(axis=1)

ears_and_game['key_games_wke_sum'] = ears_and_game.filter(like = '_key_').filter(like = 'wke').sum(axis=1)
ears_and_game['min_games_wke_sum'] = ears_and_game.filter(like = '_min_').filter(like = 'wke').sum(axis=1)
ears_and_game['ses_games_wke_sum'] = ears_and_game.filter(like = '_ses_').filter(like = 'wke').sum(axis=1)

ears_and_game['key_games_sum'] = ears_and_game['key_games_wkd_sum'] + ears_and_game['key_games_wke_sum']
ears_and_game['min_games_sum'] = ears_and_game['min_games_wkd_sum'] + ears_and_game['min_games_wke_sum']
ears_and_game['ses_games_sum'] = ears_and_game['ses_games_wkd_sum'] + ears_and_game['ses_games_wke_sum']

#drop all NAN value and only retain the data of android phone model
ears_and_game = ears_and_game.dropna(axis=0,how='any')
ears_and_df = ears_and_df.dropna(axis=0,how='any')
ears_and_basic = ears_basic[ears_basic['nt_y_ears_os'] == 1]

# concatenate all android data
ears_and_df = pd.concat([ears_and_basic,ears_and_df, ears_and_game.loc[:,['key_games_wkd_sum', 'min_games_wkd_sum', 'ses_games_wkd_sum',
                                                       'key_games_wke_sum', 'min_games_wke_sum', 'ses_games_wke_sum',
                                                      'key_games_sum', 'min_games_sum', 'ses_games_sum']]], axis=1)

# rename the column header
ears_and_df = ears_and_df.reset_index()
and_oldcols = list(ears_and_df.columns)
new_cols = list()
for col in and_oldcols:
    if 'nt_y_ears_kb' in col:
        col = col.split('_')
        new_col = col[4] + '_' + '_'.join(col[6:])
        new_cols.append(new_col)
    elif 'index' == col:
        new_cols.append('src_subject_id')
    else:
        new_cols.append(col)
ears_and_df.columns = new_cols

# store the data of android phone model
ears_and_df.to_csv('D:/ABCD/V5simplified/nt_y_ears_and_simplified.csv', index=False)


## only retain ios phone model
# drop all NAN value and only retain the data of ios phone model
ears_ios_df = ears_ios_df.dropna(axis=0,how='any')
ears_ios_basic = ears_basic[ears_basic['nt_y_ears_os'] == 2]

# concatenate all needed data
ears_ios_df = pd.concat([ears_ios_basic, ears_ios_df], axis=1)
ears_ios_df = ears_ios_df.reset_index()
# rename the column header same as android data
ios_oldcols = list(ears_ios_df.columns)
ios_new = list()
for col in ios_oldcols:
    if 'nt_y_ears_kb' in col:
        col = col.split('_')
        new_col = col[4] + '_' + '_'.join(col[6:])
        ios_new.append(new_col)
    elif 'index' == col:
        ios_new.append('src_subject_id')
    else:
        ios_new.append(col)
ears_ios_df.columns = ios_new

# store the data of ios phone model
ears_ios_df.to_csv('D:/ABCD/V5simplified/nt_y_ears_ios_simplified.csv', index=False)


## merge the data of ios and android phone models. only contain overall game scores.
## version 1: sum conversation, dating, social together
# delete all sub-scores in game categories in andoid data
ears_and_df1 = pd.concat([ears_and_df.iloc[:,0:39],ears_and_df.iloc[:,192:]],axis=1)
# sum conversation, dating, social and rename it as social
for soc in ears_and_df1.filter(like = 'social').columns:
    for i in ['communication','dating']:
        sub_col = soc.replace('social', i)
        ears_and_df1[soc] = ears_and_df1[soc] + ears_and_df1[sub_col]
        ears_and_df1 = ears_and_df1.drop(labels = [sub_col], axis=1)
# resort the column to make it the same as the ios data
ears_and_df1 = ears_and_df1[list(ears_ios_df.columns)]
# concatenate the ios and android data
ears_final1 = pd.concat([ears_and_df1, ears_ios_df], axis=0)
# store the data
ears_final1.to_csv('D:/ABCD/V5simplified/nt_y_ears_simplified_V1.csv', index=False)


## version 2: set NAN value for the ios data in the categories of convesation and dating 
## (need to ask if the social category in ios app store corresponded with the summary of dating, conversation and social 
## categories in google app store)

# add columns to the ios data with same headers as the android data
add_cols = set(ears_and_df.iloc[:,0:39].columns) - set(ears_ios_df.iloc[:,0:21].columns)
ears_ios_df1 = pd.concat([ears_ios_df, pd.DataFrame(columns=list(add_cols))])
# resort the column of the android data
ears_and_df2 = pd.DataFrame(ears_and_df, columns=list(ears_ios_df1.columns))
# concatenate the ios and android data
ears_final2 = pd.concat([ears_and_df2, ears_ios_df1], axis=0)
# store the data
ears_final2.to_csv('D:/ABCD/V5simplified/nt_y_ears_simplified_V2.csv', index=False)







