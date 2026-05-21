#!python3.8

import pandas as pd
import numpy as np
import requests
import time
from tqdm import tqdm

# Type data: intermediate (no prediction) and features for predicted

def get_baseline_data_p(country, type_data = 'features', base_path = '../../'):
    if country == 'KE':
        n_base1 = 95
        n_base2 = 37
    else:
        n_base1 = 156
        n_base2 = 26
    path = f'data/03-experiment/{country}/baseline/01-preprocess/followers/'
    agg_base = base_path + path
    df_base1 = pd.DataFrame()
    for i in tqdm(range(0, n_base1)):
        df = pd.read_parquet(f'{agg_base}{type_data}/baseline_{i}.parquet.gzip')
        df_base1 = pd.concat([df_base1, df])

    df_base2 = pd.DataFrame()
    for i in tqdm(range(0, n_base2)):
        df = pd.read_parquet(f'{agg_base}{type_data}_abs/baseline_{i}.parquet.gzip')
        df_base2 = pd.concat([df_base2, df])
    
    df_base = pd.concat([df_base1, df_base2]).reset_index(drop= True)
    return(df_base)

# type_data = 'endline' for predicted and type_data='intermediate' for no prediction
def get_stages_data_p(country, stage, type_data, base_path = '../../'):
    agg = base_path + f'data/03-experiment/{country}/treatment/followers/01-preprocess/'
    if stage == 'stage1':
        if country == 'KE':
            stage = pd.concat([pd.read_parquet(f'{agg}{type_data}/december0.parquet.gzip'),
                               pd.read_parquet(f'{agg}{type_data}/december0_abs.parquet.gzip')]).reset_index(drop = True)
    
        else: 
            if type_data == 'predicted':
                stage = pd.concat([pd.read_parquet(f'{agg}{type_data}/december0_1.parquet.gzip'),
                               pd.read_parquet(f'{agg}{type_data}/december0_2.parquet.gzip'),
                               pd.read_parquet(f'{agg}{type_data}/december0_abs.parquet.gzip')]).reset_index(drop = True)
            else: 
                stage = pd.concat([pd.read_parquet(f'{agg}{type_data}/december0.parquet.gzip'),
                               pd.read_parquet(f'{agg}{type_data}/december0_abs.parquet.gzip')]).reset_index(drop = True)
    elif stage == 'stage2':
        if country == 'KE':
            stage = pd.concat([pd.read_parquet(f'{agg}{type_data}/december1.parquet.gzip'),
                               pd.read_parquet(f'{agg}{type_data}/december1_abs.parquet.gzip')]).reset_index(drop = True)
    
        else: 
            if type_data == 'predicted':
                stage = pd.concat([pd.read_parquet(f'{agg}{type_data}/december1_1.parquet.gzip'),
                               pd.read_parquet(f'{agg}{type_data}/december1_2.parquet.gzip'),
                               pd.read_parquet(f'{agg}{type_data}/december1_3.parquet.gzip'),
                               pd.read_parquet(f'{agg}{type_data}/december1_4.parquet.gzip'),
                               pd.read_parquet(f'{agg}{type_data}/december1_abs.parquet.gzip')]).reset_index(drop = True)
            else:
                stage = pd.concat([pd.read_parquet(f'{agg}{type_data}/december1.parquet.gzip'),
                               pd.read_parquet(f'{agg}{type_data}/december1_abs.parquet.gzip')]).reset_index(drop = True)
            
    elif stage == 'stage3':
        if country == 'KE':
            stage = pd.concat([pd.read_parquet(f'{agg}{type_data}/january0.parquet.gzip'),
                               pd.read_parquet(f'{agg}{type_data}/january0_abs.parquet.gzip')]).reset_index(drop = True)
    
        else: 
            if type_data == 'predicted':
                stage = pd.concat([pd.read_parquet(f'{agg}{type_data}/january0_1.parquet.gzip'),
                               pd.read_parquet(f'{agg}{type_data}/january0_2.parquet.gzip'),
                               pd.read_parquet(f'{agg}{type_data}/january0_abs.parquet.gzip')]).reset_index(drop = True)
            else:
                stage = pd.concat([pd.read_parquet(f'{agg}{type_data}/january0.parquet.gzip'),
                               pd.read_parquet(f'{agg}{type_data}/january0_abs.parquet.gzip')]).reset_index(drop = True)
    elif stage == 'stage4':
        if country == 'KE':
            stage = pd.concat([pd.read_parquet(f'{agg}{type_data}/january1.parquet.gzip'),
                               pd.read_parquet(f'{agg}{type_data}/january1_abs.parquet.gzip')]).reset_index(drop = True)
    
        else: 
            if type_data == 'predicted':
                stage = pd.concat([pd.read_parquet(f'{agg}{type_data}/january1_1.parquet.gzip'),
                               pd.read_parquet(f'{agg}{type_data}/january1_2.parquet.gzip'),
                               pd.read_parquet(f'{agg}{type_data}/january1_abs.parquet.gzip')]).reset_index(drop = True)
            else: 
                stage = pd.concat([pd.read_parquet(f'{agg}{type_data}/january1.parquet.gzip'),
                               pd.read_parquet(f'{agg}{type_data}/january1_abs.parquet.gzip')]).reset_index(drop = True)
            
    elif stage == 'stage5': 
        stage = pd.DataFrame()
        print(stage, ' not available yet.')
        
    elif stage == 'stage6': 
        stage = pd.DataFrame()
        print(stage, ' not available yet.')
        
    else: 
        stage = pd.DataFrame()
        print('Error: choose an available stage from 1 to 6' )
    
    return(stage)


def get_stages2_data_p(country, stage, type_data, base_path = '../../'):
    if stage == 'stage1_2':
        stage1 = get_stages_data_p(country = country, stage = 'stage1', 
                                    type_data = type_data, 
                                     base_path = base_path)
        stage2 = get_stages_data_p(country = country, stage = 'stage2', 
                                    type_data = type_data, 
                                     base_path = base_path)
        
        stage = pd.concat([stage1, stage2]).reset_index(drop=True)
    elif stage == 'stage3_4':
        stage3 = get_stages_data_p(country = country, stage = 'stage3', 
                                    type_data = type_data, 
                                     base_path = base_path)
        stage4 = get_stages_data_p(country = country, stage = 'stage4', 
                                    type_data = type_data, 
                                     base_path = base_path)
        
        stage = pd.concat([stage3, stage4]).reset_index(drop=True)
    elif stage == 'stage5_6':
        stage5 = get_stages_data_p(country = country, stage = 'stage5', 
                                    type_data = type_data, 
                                     base_path = base_path)
        stage6 = get_stages_data_p(country = country, stage = 'stage6', 
                                    type_data = type_data, 
                                     base_path = base_path)
        
        stage = pd.concat([stage5, stage6]).reset_index(drop=True)
    return stage


def get_data_batch2_new(country, stage, file = 'correct_followers_09_02_24.parquet', 
                        base_path = '../../../', base_path2 = '../../'):
    path = base_path + f'manual_scraper/data/correct_cases/{country}/{file}'
    path2 = base_path2 + f'social-media-influencers-af/data/04-analysis/{country}/baseline_batch2.parquet'
    df = pd.read_parquet(path)
    ids = pd.read_parquet(path2)
    ids = ids[['username', 'follower_id']]
    df.rename(columns = {'username':'repost_user', 
                     'follower_handle' : 'username'}, inplace = True)

    df = df.merge(ids, on = 'username', how = 'left')
    df['date'] = pd.to_datetime(df['TimeStamp'])
    df['stage3_4'] = (df['date'] > '2023-05-28') & (df['date'] < '2023-06-26')
    df['stage3_4'] = df['stage3_4'].astype(int)
    df['stage5_6'] = (df['date'] > '2023-06-25') & (df['date'] < '2023-07-23')
    df['stage5_6'] = df['stage5_6'].astype(int)
    df['base_new'] = (df['date'] < '2023-03-01').astype(int)
    df['RT'] = np.where(df['type'].isnull(), 0, 1)
    df.rename(columns = {'likes':'like_count', 'replies':'reply_count', 
                        'retweets':'retweet_count'}, inplace = True)
    df = df[['username', 'follower_id', 'date', 'text', 'RT', 'like_count', 
             'reply_count','retweet_count', 'stage3_4', 
             'stage5_6', 'base_new', 'repost_user']]
    if stage == 'stage3_4':
        final = df[df['stage3_4'] == 1]
        final = final.drop(['stage3_4', 'stage5_6', 'base_new'], axis = 1)
    elif stage == 'stage5_6':
        final = df[df['stage5_6'] == 1]
        final = final.drop(['stage3_4', 'stage5_6', 'base_new'], axis = 1)
    elif stage == 'baseline':
        final = df[df['base_new'] == 1]
        final = final.drop(['stage3_4', 'stage5_6', 'base_new'], axis = 1)
    else:
        print('Not a correct period. Choose between stage3_4, stage5_6 and baseline')
        final = pd.DataFrame()
    
    return final


def get_data_base_batch2(country,  base_path = '../../'):
    n_base = 14
        
    base_path = base_path + f'social-media-influencers-af/data/03-experiment/{country}/'
    agg_base = base_path + 'baseline/01-preprocess/followers/'
    if country == 'SA':
        df_final = pd.DataFrame()
        for i in tqdm(range(0, 10)):
            df = pd.read_parquet(f'{agg_base}intermediate/baseline_batch2_{i}.parquet.gzip')
            df_final = pd.concat([df_final, df])
        
        df_final = df_final.reset_index(drop=True)
    else: 
        df_final = pd.DataFrame()
        for i in tqdm(range(0, n_base)):
            df = pd.read_parquet(f'{agg_base}predicted/baseline_batch2_0{i}.parquet.gzip')
            df_final = pd.concat([df_final, df])

        df_final1 = pd.DataFrame()
        for i in tqdm(range(0, n_base)):
            df1 = pd.read_parquet(f'{agg_base}predicted/baseline2_batch2_{i}.parquet.gzip')
            df_final1 = pd.concat([df_final1, df1])
    
        df_final = pd.concat([df_final, df_final1]).reset_index(drop=True)
    return df_final

def get_data_base_batch22(country,  base_path = '../../'):
    n_base = 14
        
    base_path = base_path + f'social-media-influencers-af/data/03-experiment/{country}/'
    agg_base = base_path + 'baseline/01-preprocess/followers/'
    if country == 'SA':
        df_final = pd.DataFrame()
        for i in tqdm(range(0, 10)):
            df = pd.read_parquet(f'{agg_base}predicted/baseline_batch2_{i}.parquet.gzip')
            df_final = pd.concat([df_final, df])
        
        df_final = df_final.reset_index(drop=True)
    else: 
        df_final = pd.DataFrame()
        for i in tqdm(range(0, n_base)):
            df = pd.read_parquet(f'{agg_base}predicted/baseline_batch2_0{i}.parquet.gzip')
            df_final = pd.concat([df_final, df])

        df_final1 = pd.DataFrame()
        for i in tqdm(range(0, n_base)):
            df1 = pd.read_parquet(f'{agg_base}predicted/baseline2_batch2_{i}.parquet.gzip')
            df_final1 = pd.concat([df_final1, df1])
    
        df_final = pd.concat([df_final, df_final1]).reset_index(drop=True)
    return df_final

def get_data_stage12_batch2(country, base_path = '../../'):
    df_final = pd.DataFrame()
    base_path = base_path + f'social-media-influencers-af/data/03-experiment/{country}/'
    agg = base_path + 'treatment/followers/01-preprocess/'
    if country == 'KE':
        n_end = 9
        for i in range(0, n_end):
            df = pd.read_parquet(f'{agg}predicted/may_batch2{i}.parquet.gzip')
            df_final = pd.concat([df_final, df])

        df_final = df_final.reset_index(drop=True)
    else:
        n_end = 7
        for i in range(0, n_end):
            df = pd.read_parquet(f'{agg}predicted/may_batch2{i}.parquet.gzip')
            df_final = pd.concat([df_final, df])

        df_final = df_final.reset_index(drop=True)
        
    return df_final


############# Get Batch 1 Data ##################


def get_baseline_data_b1(country, type_data = 'predicted', base_path = '../../social-media-influencers-af/'):
    if country == 'KE':
        n_base = 84
    else:
        n_base = 74
    path = f'/data/03-experiment/{country}/baseline/01-preprocess/followers/'
    agg_base = base_path + path
    df_base = pd.DataFrame()
    for i in tqdm(range(0, n_base)):
        df = pd.read_parquet(f'{agg_base}{type_data}/baseline_{i}.parquet.gzip')
        df_base = pd.concat([df_base, df])
    df_base = df_base.reset_index(drop = True)
    return(df_base)

def get_endline_data(country, stage = 'stage1_2', type_data = 'predicted', 
                     base_path = '../../social-media-influencers-af/'):
    if country == 'SA':
        N_ARCHS = 25
        N_ARCHS1 = 10
        N_ARCHS2 = 10
    else:
        N_ARCHS = 58
        N_ARCHS1 = 21
        N_ARCHS2 = 21
        
    path = f'data/03-experiment/{country}/treatment/followers/01-preprocess/'
    agg = base_path + path
    
    if stage == 'stage1_2':
        df_final = pd.DataFrame()
        for i in tqdm(range(0, N_ARCHS)):
            df = pd.read_parquet(f'{agg}{type_data}/march_{i}.parquet.gzip')
            df_final = pd.concat([df_final, df])
        df_final = df_final.reset_index(drop=True)
        
    elif stage == 'stage3_4':
        df_final = pd.DataFrame()
        for i in range(0, N_ARCHS2):
            df = pd.read_parquet(f'{agg}{type_data}/april1_good{i}.parquet.gzip')
            df_final = pd.concat([df_final, df])

        df_final = df_final.reset_index(drop=True)

        df_final1 = pd.DataFrame()
        for i in range(0, N_ARCHS1):
            df = pd.read_parquet(f'{agg}predicted/april2_{i}.parquet.gzip')
            df_final1 = pd.concat([df_final1, df])

        df_final1 = df_final1.reset_index(drop=True)
        df_final = pd.concat([df_final, df_final1]).reset_index(drop=True)
        
    elif stage == 'stage5_6':
        df_final = pd.DataFrame()
        for i in range(0, N_ARCHS2):
            df = pd.read_parquet(f'{agg}{type_data}/posttreat_{i}.parquet.gzip')
            df_final = pd.concat([df_final, df])

        df_final = df_final.reset_index(drop=True)

        df_final1 = pd.DataFrame()
        for i in range(0, N_ARCHS2):
            df = pd.read_parquet(f'{agg}{type_data}/posttreat2_{i}.parquet.gzip')
            df_final1 = pd.concat([df_final1, df])

        df_final1 = df_final1.reset_index(drop=True)

        df_final = pd.concat([df_final, df_final1]).reset_index(drop=True)
    else:
        print('Not a correct stage, choose between: stage1_2, stage3_4 or stage5_6')
        df_final = pd.DataFrame()
    
    return df_final
    