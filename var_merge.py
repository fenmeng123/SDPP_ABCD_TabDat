# -*- coding: utf-8 -*-
"""
Created on Wed Aug  9 11:45:23 2023

target: data merge and recode
included variables: 
    1 . demographic information: gender, age,and others (grade, adoption, race, born country, years lived in USA,
    religious preference, parents' age, parent's gender, parent's marital status, 
    parent's education, parent's employment status, family income, household size, 
    native language)
    2. social interaction: complains of loneliness, the numbers of peers, social distress
        (calculate three types of latent variables: subjective feeling, social status, 
         social experience balance)
    3. virtual interaction: Screentime usage, MPIQ, SOC
    4. social media addiction (SMA)
    5. other relevant variables (optional): neurocognition? 
outline:
    1. generate a dictionary to extract abovementioned variables from different tables
    2. merge them together
    3. recode variables
    4. rename variables
input file:
    D:/ABCD/V5simplified/abcd_simplified_dictionary.csv\
output file:
    D:/ABCD/V5simplified/final_raw.csv

@author: zhang
"""
import pandas as pd
import numpy as np
from functools import reduce

# load the simplified dictionary
sim_dic = pd.read_csv('D:/ABCD/V5simplified/abcd_simplified_dictionary.csv')
sim_dic = sim_dic[~sim_dic['file_name'].isin(['abcd_y_lt','gen_y_pihat','nc_y_ehis',
                                             'ph_y_anthro'])]

# create a definition, which could return the first row of each group
def only_top(x):
    return x.iloc[0,:]
# only get the first row of each group, to get their csv file name 
file_names = sim_dic.groupby('file_name').apply(only_top)

## demographic information
demo_df = pd.read_csv(f'D:/ABCD/abcd-data-release-5.0/core/{("/").join(list(file_names.loc["abcd_p_demo"])[1:3])}.csv',
                     usecols = ['src_subject_id','eventname','demo_brthdat_v2',
                     'demo_sex_v2','demo_ed_v2','demo_gender_id_v2'])
# rename columns
demo_df.columns = list(sim_dic[sim_dic['variable'].isin(['src_subject_id','eventname','demo_brthdat_v2',
'demo_sex_v2','demo_ed_v2','demo_gender_id_v2'])]['var_new'])
demo_df['SexBorn'] = demo_df['SexBorn'].replace({1:'Male',2:'Female',3:'Intersex_Male',
                                                 4:'Intersex_Female','else':np.nan})
demo_df['YouthGender_P'] = demo_df['YouthGender_P'].replace({1:'Male',2:'Female',
                                                             3:'TransMale',4:'TransFemale',
                                                             5:'GenderQueer',6:'Different',
                                                             'else':np.nan})
#imputation: BOCF
def BOCF(x):
    return x.fillna(x.iloc[0,:])
demo_df = demo_df.groupby('Subject').apply(BOCF)

eve_dict = {'baseline_year_1_arm_1':0,'1_year_follow_up_y_arm_1':1,'2_year_follow_up_y_arm_1':2,
            '3_year_follow_up_y_arm_1':3,'4_year_follow_up_y_arm_1':4}

demo_df['event_year'] = demo_df['Event'].copy()
demo_df['event_year'] = demo_df['event_year'].replace(eve_dict)
demo_df['Education'] = demo_df['Education'] + demo_df['event_year']
demo_df['YouthAge_P'] = demo_df['YouthAge_P'] + demo_df['event_year']
demo_df = demo_df.drop(labels=['event_year'], axis=1)

## screenusage
# choose the necessary part
screen_dic = sim_dic[sim_dic['measure'].isin(['ScreenUsage','MPIQ','SMA','SOC'])]
# load the data
screen_df = pd.read_csv(f'D:/ABCD/abcd-data-release-5.0/core/{("/").join(list(file_names.loc["nt_y_st"])[1:3])}.csv',
                        usecols = ['src_subject_id','eventname'] + list(screen_dic['variable']))

screen_df = screen_df[['src_subject_id','eventname'] + list(screen_dic['variable'])]
# rename columns
screen_cols = screen_df.columns
# devide the data into different events: V01 & V234
screen_V01 = screen_df.iloc[:,0:16]
screen_V23 = pd.concat([screen_df.iloc[:,0:2],screen_df.iloc[:,16:60]],axis=1)

screen_V23cols = screen_V23.columns
V23_new = screen_V23[['src_subject_id','eventname']]
for ind, col in enumerate(list(screen_V23cols)):
    if 'src_subject' in col or 'eventname' in col:
        V23_new[col] = screen_V23[col]
    elif 'screentime' in col and 'hr' in col:
        col_min = list(screen_V23cols)[ind+1]
        V23_new[col] = screen_V23[col] + screen_V23[col_min]/60
    else:
        pass

# merge TV watching's V2 and V34
V23_new['screentime_1_wkdy_hr'] = np.nansum(np.array(V23_new[['screentime_1_wkdy_hr','screentime_1_wkdy2_hr']]), axis=1)
V23_new['screentime_7_wknd_hr'] = np.nansum(np.array(V23_new[['screentime_7_wknd_hr','screentime_7_8_wknd_hr']]), axis=1)
V23_new = V23_new.drop(labels=['screentime_1_wkdy2_hr','screentime_7_8_wknd_hr'], axis=1)

# rename the column of V23 data
V23_new.columns = sim_dic[sim_dic['variable'].isin(list(V23_new.columns))]['var_new']  

# sum the gaming hours
V23_new['open_weekday_Game'] = np.nansum(np.array(V23_new[['open_weekday_SingleGame','open_weekday_MultiGame']]), axis=1)
V23_new['open_weekend_Game'] = np.nansum(np.array(V23_new[['open_weekend_SingleGame','open_weekend_MultiGame']]), axis=1)

# rename the column of V01 data
screen_V01.columns = sim_dic[sim_dic['variable'].isin(list(screen_V01.columns))]['var_new']

# match the name of social media usage in V01 and V23
screen_V01 = screen_V01.rename(columns={'weekday_SocialNet':'weekday_SocialApps',
                                        'weekend_SocialNet':'weekend_SocialApps',})

# sum weekday and weekend screentime usage
screen_V01['weekday_TOTAL'] = np.nansum(np.array(screen_V01.filter(like='weekday')), axis=1)
screen_V01['weekend_TOTAL'] = np.nansum(np.array(screen_V01.filter(like='weekend')), axis=1)

# rename TV watching column
V23_new = V23_new.rename(columns={'open_weekday_TV_T2':'open_weekday_TV'})
new_cols = V23_new.columns

# merge V01 and V23 data
for new in V23_new.columns:
    for old in screen_V01.columns:
        if old in new and old not in ['Subject','Event']:
            concat = pd.concat([screen_V01[old],V23_new[new]],axis=1)
            V23_new[new] = np.nansum(np.array(concat), axis=1)
        elif old in ['R_rated_Games','R_rated_Movies']:
            V23_new[old] = screen_V01[old]
        else:
            pass
# resort the column
V23_new = V23_new[['Subject','Event','open_weekday_TV','open_weekday_Video','open_weekday_Game',
                   'open_weekday_Text','open_weekday_SocialApps','open_weekday_VideoChat',
                   'open_weekday_SingleGame','open_weekday_MultiGame','open_weekday_EditPhoto',
                   'open_weekday_Surf','open_weekend_TV','open_weekend_Video','open_weekend_Game',
                   'open_weekend_Text','open_weekend_SocialApps','open_weekend_VideoChat',
                   'open_weekend_SingleGame','open_weekend_MultiGame','open_weekend_Surf',
                   'open_weekday_TOTAL','open_weekend_TOTAL','R_rated_Games',
                   'R_rated_Movies']]

# rename the column name
V23_new.columns = [i.replace('open_','') for i in list(V23_new.columns)]
V23_new = V23_new

# MPIQ: extract related variables and rename them
mpiq_cols = sim_dic[sim_dic['measure']=='MPIQ']['variable']
mpiq_df = screen_df[['src_subject_id','eventname']+list(mpiq_cols)]
mpiq_df.columns = list(sim_dic[sim_dic['variable'].isin(list(mpiq_df.columns))]['var_new'])
mpiq_df = mpiq_df.replace(777, np.nan)
# sum the overall score
mpiq_df['MPIQ_TOTAL'] = mpiq_df.iloc[:,2:].sum(axis=1)
mpiq_df['MPIQ_TOTAL'].loc[mpiq_df['Event'].isin(['baseline_year_1_arm_1','1_year_follow_up_y_arm_1'])] = np.nan

# SMA: extract related variables and rename them
sma_cols = sim_dic[sim_dic['measure']=='SMA']['variable']
sma_df = screen_df[['src_subject_id','eventname']+list(sma_cols)]
sma_df.columns = list(sim_dic[sim_dic['variable'].isin(list(sma_df.columns))]['var_new'])
sma_df = sma_df.replace(777, np.nan)
# sum the overall score
sma_df['SMA_TOTAL'] = sma_df.iloc[:,2:].sum(axis=1)
sma_df['SMA_TOTAL'].loc[sma_df['Event'].isin(['baseline_year_1_arm_1','1_year_follow_up_y_arm_1'])] = np.nan

# SOC: extract related variables and rename them
soc_cols = sim_dic[sim_dic['measure']=='SOC']['variable']
soc_df = screen_df[['src_subject_id','eventname']+list(soc_cols)]
soc_df['screentime_smq_sm_min'] = soc_df['screentime_smq_sm_min']/60
soc_df.columns = list(sim_dic[sim_dic['variable'].isin(list(soc_df.columns))]['var_new'])
# sum the account number 
soc_df['SOC_NOA'] = np.nansum(np.array(soc_df.filter(like='NOA')),axis=1)
soc_df['SOC_OS'] = np.nansum(np.array(soc_df.filter(like='SOC_OS_')), axis=1)
soc_df['SOC_NOA'].loc[soc_df.filter(like='SOC_NOA_').isnull().any(axis=1) == True] = np.nan
soc_df['SOC_OS'].loc[soc_df.filter(like='SOC_OS_').isnull().any(axis=1) == True] = np.nan

# drop the account for different apps
soc_df = soc_df.drop(labels = list(soc_df.filter(like = 'NOA_')), axis=1)
soc_df = soc_df.drop(labels = list(soc_df.filter(like='SOC_OS_')), axis=1)
# recode the nan value
soc_df[soc_df.isin([777])] = np.nan
soc_df.iloc[:,6:10] = np.nan
# sum different conditions: not applicable, unknown, and known
soc_df['SOC_Followers'] = np.nansum(np.array(soc_df.filter(like='Followers')),axis=1)
soc_df['SOC_Following'] = np.nansum(np.array(soc_df.filter(like='Following')),axis=1)
soc_df[soc_df.isin([999,1000])] = np.nan
soc_df = pd.concat([soc_df.iloc[:,0:6],soc_df.iloc[:,10:]], axis=1)
# calculate the ratio of followers and following
#soc_df['SOC_Follow_Ratio'] = (soc_df['SOC_Followers'] + 1)/(soc_df['SOC_Following'] + 1)
# sum the social media usage
soc_df['SM_Usage'] = np.nansum(np.array(soc_df.filter(like='SM_Usage')),axis=1)
soc_df = pd.concat([soc_df.iloc[:,0:8],soc_df.iloc[:,10:]], axis=1)

soc_df['SOC_Public'] = soc_df['SOC_Public'].replace(1, 'Public')
soc_df['SOC_Public'] = soc_df['SOC_Public'].replace(2, 'Private')

soc_df['SOC_Usage_Rank'] = soc_df['SOC_Usage_Rank'].replace({1:'Facebook', 2:'Instgram',
                                                            3:'Snapchat', 4:'Twitter',
                                                            5:'Youtube', 6: 'Pinterest',
                                                            7:'Tumblr', 8:'Reddit',
                                                            11:'Tiktok', 9:'MVOC',
                                                            10:'Other'})
for col in ['SOC_Followers','SOC_Following','SOC_NOA','SOC_OS','SM_Usage']:
    soc_df[col].loc[soc_df['Event'].isin(['baseline_year_1_arm_1','1_year_follow_up_y_arm_1'])] = np.nan


## cyber bullying
cbb_df = pd.read_csv(f'D:/ABCD/abcd-data-release-5.0/core/{("/").join(list(file_names.loc["mh_y_cbb"])[1:3])}.csv',
                     usecols = ['src_subject_id','eventname'] + list(sim_dic.loc[sim_dic['measure']=='CyberBully']['variable']))
cbb_cols = list(sim_dic.loc[sim_dic['measure']=='CyberBully']['var_new'])
cbb_df.columns = ['Subject','Event'] + cbb_cols
cbb_df = cbb_df.replace({np.nan:0})
cbb_df = cbb_df.replace({777:np.nan,999:np.nan})

## social isolation
# CBCL loneliness
#lone_df = pd.read_csv(f'D:/ABCD/abcd-data-release-5.0/core/{("/").join(list(file_names.loc["mh_p_cbcl"])[1:3])}.csv',
                     #usecols = ['src_subject_id','eventname', 'cbcl_q12_p'])
# rename the column
#lone_df.columns = list(sim_dic[sim_dic['variable'].isin(list(lone_df.columns))]['var_new'])

# other resilience
# load the data
usecols = ['src_subject_id','eventname'] + list(sim_dic.loc[sim_dic['measure']=='OtherResilience']['variable'])
resi_df = pd.read_csv(f'D:/ABCD/abcd-data-release-5.0/core/{("/").join(list(file_names.loc["mh_y_or"])[1:3])}.csv',
                     usecols = usecols)
# rename columns
resi_df.columns = list(sim_dic[sim_dic['variable'].isin(list(resi_df.columns))]['var_new'])
# drop unnecessary columns (value not usable)
resi_df = resi_df.drop(labels = ['OthF_Num','Close_OthF_Num'], axis = 1)
# sum the overall friend number, overall close friend number, ratio of close friend in all friends, 
# ratio of boyfriends in overall friends, ratio of girl friends in overall friends 
resi_df['F_Num'] = resi_df['BF_Num'] + resi_df['GF_Num']
resi_df['Close_F_Num'] = resi_df['Close_BF_Num'] + resi_df['Close_GF_Num']
resi_df['Close_Ratio'] = (resi_df['Close_F_Num']+1)/(resi_df['F_Num']+1)

resi_df['GF_Ratio'] = (resi_df['GF_Num']+1)/(resi_df['F_Num']+1)
resi_df['BF_Ratio'] = (resi_df['BF_Num']+1)/(resi_df['F_Num']+1)
#  peer experience
usecols = ['src_subject_id','eventname'] + list(sim_dic.loc[sim_dic['measure']=='PeerExperience']['variable'])
peer_df = pd.read_csv(f'D:/ABCD/abcd-data-release-5.0/core/{("/").join(list(file_names.loc["mh_y_peq"])[1:3])}.csv',
                     usecols = usecols)
peer_df.columns = list(sim_dic[sim_dic['variable'].isin(list(peer_df.columns))]['var_new'])

peer_df['PEQ_Relation_Aggrs'] = peer_df[['PEQ_left_aggr','PEQ_invite_aggr','PEQ_exclude_aggr']].sum(axis=1)
peer_df['PEQ_Reputation_Aggrs'] = peer_df[['PEQ_rumor_aggr','PEQ_gossip_aggr','PEQ_loser_aggr']].sum(axis=1)
peer_df['PEQ_Overt_Aggrs'] = peer_df[['PEQ_chase_aggr','PEQ_threat_aggr','PEQ_hit_aggr']].sum(axis=1)
peer_df['PEQ_Aggressions'] = peer_df.filter(like='_aggr').sum(axis=1)
peer_df['PEQ_Relation_Victs'] = peer_df[['PEQ_left_vict','PEQ_invite_vict','PEQ_exclude_vict']].sum(axis=1)
peer_df['PEQ_Reputation_Victs'] = peer_df[['PEQ_rumor_vict','PEQ_gossip_vict','PEQ_loser_vict']].sum(axis=1)
peer_df['PEQ_Overt_Victs'] = peer_df[['PEQ_chase_vict','PEQ_threat_vict','PEQ_hit_vict']].sum(axis=1)
peer_df['PEQ_Victims'] = peer_df.filter(like='_vict').sum(axis=1)

peer_sum = ['PEQ_Relation_Aggrs','PEQ_Reputation_Aggrs','PEQ_Overt_Aggrs','PEQ_Aggressions',
            'PEQ_Relation_Victs','PEQ_Reputation_Victs','PEQ_Overt_Victs','PEQ_Victims']
peer_cols = peer_df.columns
for col in peer_sum:
    peer_df[col].loc[peer_df.filter(like='_aggr').isnull().any(axis=1) == True] = np.nan
    peer_df[col].loc[peer_df.filter(like='_vict').isnull().any(axis=1) == True] = np.nan


## BISBAS
usecols = ['src_subject_id','eventname'] + list(sim_dic.loc[sim_dic['measure']=='BISBAS']['variable'])
bisbas_df = pd.read_csv(f'D:/ABCD/abcd-data-release-5.0/core/{("/").join(list(file_names.loc["mh_y_bisbas"])[1:3])}.csv',
                     usecols = usecols)
bisbas_df.columns = list(sim_dic[sim_dic['variable'].isin(list(bisbas_df.columns))]['var_new'])
bisbas_cols = bisbas_df.columns


## merge all the dataframe
df_list = [demo_df, V23_new, mpiq_df, sma_df, soc_df, resi_df, peer_df, cbb_df, bisbas_df]
# reset event value and generate subject+event column
event_dict = {'baseline_year_1_arm_1':'V0','1_year_follow_up_y_arm_1':'V1',
              '2_year_follow_up_y_arm_1':'V2','3_year_follow_up_y_arm_1':'V3',
              '4_year_follow_up_y_arm_1':'V4'}
for idx, df in enumerate(df_list):
    for key, value in event_dict.items():
        df = df.replace(key, value)
    df['Subject_Event'] = df['Subject'] + '_' + df['Event']
    df = df.drop(labels=['Subject','Event'], axis=1)
    #df = df.set_index('Subject_Event')
    df_list[idx] = df
    
# merge the data in one table
final_df = reduce(lambda left, right: left.merge(right, on ='Subject_Event', how='left'), df_list)  
# resort the columns
Sub = final_df['Subject_Event']
final_df = final_df.drop(labels=['Subject_Event'], axis=1)
final_df.insert(0, 'Subject_Event', Sub)  

# split column 'Subject_Event'
sub_eve = list(final_df['Subject_Event'].str.split('_'))
final_df.insert(0,'Subject', ['_'.join(i[0:2]) for i in sub_eve])
final_df.insert(1, 'Event', [i[-1] for i in sub_eve])
# recode the 0 as nan for lacking administration of each measure
final_df['MPIQ_TOTAL'].loc[final_df[['MPIQ_1','MPIQ_2','MPIQ_3','MPIQ_4',
                         'MPIQ_5','MPIQ_6','MPIQ_7','MPIQ_8']].isnull().any(axis=1) == True] = np.nan
final_df['SMA_TOTAL'].loc[final_df[['SMA_1','SMA_2','SMA_3','SMA_4','SMA_5','SMA_6']].isnull().any(axis=1) == True] = np.nan

final_df_cols = final_df.columns
SOC_pub = final_df['SOC_Public']
SOC_rank = final_df['SOC_Usage_Rank']
final_df = final_df.drop(labels=['SOC_Public','SOC_Usage_Rank'], axis=1)
final_df.insert(7, 'SOC_Public', SOC_pub)
final_df.insert(8,'SOC_Usage_Rank', SOC_rank)
cols = final_df.columns

dropcols = list(bisbas_df.filter(like='_NM').columns) + list(bisbas_df.filter(like='_T').columns)
final_df = final_df.drop(labels=dropcols, axis=1)
final_df[['weekday_Video','weekend_Video']].loc[final_df['Event'].isin(['V3','V4'])] = np.nan
# store file
final_df.to_csv('D:/ABCD/V5simplified/final_raw.csv', index= False)






    



