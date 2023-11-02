# -*- coding: utf-8 -*-
"""
Created on Wed Aug  2 17:16:43 2023

target: reorganize the raw data of ears; retain the usage of very social media apps rather than put them
 into the catogery of social media
outline: 
    1. count the overall usage of every app： including the app name, duration, partifipant 
    2. draw the histogram according to the sorted apps
    3. generate the table only including social media apps recorded in the SMQ: columns - basic information, every social media
    app + usage type (min vs. key vs. ses; corresponded with table "nt_y_ears.csv"); indexes - every 
    participant
input file:
    subject list: D:/ABCD/abcd-data-release-5.0/core/novel-technologies/nt_y_ears.csv
    folder path: D:/ABCD/BehRawV5/abcd_earsraw01
    file format - folder: earrawdata_{src_subject_id}
    file format - file: {src_subject_id}_KeyInputTimeStamps_session_{time}.csv
output file:
    1. overview file
    2. histogram file
    3. D:/ABCD/V5simplified/ears_sm_usage.csv
    
@author: zhang
"""
import pandas as pd
import numpy as np
from functools import reduce
import glob
import os

## count the overall usage of every app： including the app name, duration, partifipant
# get the subject list
subs = pd.read_csv('D:/ABCD/abcd-data-release-5.0/core/novel-technologies/nt_y_ears.csv', 
                    usecols=['src_subject_id', 'eventname','nt_y_ears_os'])
subs_l = list(subs['src_subject_id'])

# read the data and add them together
final = pd.DataFrame(columns=['id_app','amt_keyboard_session_sec'])
for sub in subs_l:
    path1 = ''.join(glob.glob(f'D:/ABCD/BehRawV5/abcd_earsraw01/{sub}_KeyInputTimestamps_session_*.csv'))
    path2 = ''.join(glob.glob(f'D:/ABCD/BehRawV5/abcd_earsraw01/earrawdata_{sub}/{sub}_KeyInputTimestamps_session_*.csv'))
    if os.path.exists(path1):
        df = pd.read_csv(path1, usecols = ['id_app','amt_keyboard_session_sec'])
        df = df.groupby('id_app').sum('amt_keyboard_session_sec')
        final = pd.concat([final,df],axis=0)
    elif os.path.exists(path2):
        df = pd.read_csv(path2, usecols = ['id_app','amt_keyboard_session_sec'])
        df = df.groupby('id_app').sum('amt_keyboard_session_sec')
        final = pd.concat([final,df],axis=0)

# count how many participants use it and the overall usage time for each app
final['sub_n'] = 1
final = final.drop(labels=['id_app'],axis=1)
final = final.reset_index()
final = final.rename(columns={'index':'id_app'})
final_ow = final.groupby('id_app').sum()
final_ow = final_ow.reset_index()
final_ow = final_ow.rename(columns={'index':'id_app'})

final_ow['min'] = final_ow['amt_keyboard_session_sec']/60
# store the overview data
final_ow.to_csv('D:/ABCD/V5simplified/ears_usage_overview.csv', index=False)

## Should check the tags of apps

    
        

