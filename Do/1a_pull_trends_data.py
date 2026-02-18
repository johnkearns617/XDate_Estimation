from googleapiclient.discovery import build
import json
import pandas as pd
import pprint
from datetime import datetime
import dill # for loading and saving a trained model
from pathlib import Path
import time 

do_folder = "Do/"
data_folder = "Data/"
results_folder = "Results/"
charts_folder = "Charts/"

SERVER = 'https://trends.googleapis.com'

API_VERSION = 'v1beta'
DISCOVERY_URL_SUFFIX = '/$discovery/rest?version=' + API_VERSION
DISCOVERY_URL = SERVER + DISCOVERY_URL_SUFFIX

cats = pd.read_csv("Data/nk_categories.csv")

trends_cats = pd.read_csv("Data/google_trend_categories.txt", sep=': ')
trends_cats.columns = ['category','id']

len(cats.num.unique().tolist())

# 2004 to 2008
# 2008 to 2013
# 2013 to 2018
# 2018 to 2023
# 2023 to 2025

service = build('trends', 'v1beta',
                  developerKey=r.gt_key,
                  discoveryServiceUrl=DISCOVERY_URL)

nums = cats.num.unique().tolist()

start_dates = [
    '2004-01',
    '2008-01',
    '2012-01',
    '2016-01',
    '2020-01',
    '2024-01'
]
end_dates=[
    '2008-12',
    '2012-12',
    '2016-12',
    '2020-12',
    '2024-12',
    datetime.today().strftime('%Y-%m-%d')[:7]
]

trends_df2 = pd.DataFrame()
for cat in nums:
    for i in range(len(start_dates)):
        start_date = start_dates[i]
        end_date = end_dates[i]
        response = service.getGraph(restrictions_category=cat,
                            restrictions_startDate=start_date,
                            restrictions_endDate=end_date,
                            restrictions_geo='US').execute()
        df = pd.DataFrame(response)
        df['cat'] = cat
        trends_df2 = pd.concat([trends_df2, df], ignore_index=True)
        print([cat,end_date])
    time.sleep(1)


full_trends_df = pd.concat([trends_df2]).reset_index(drop=True)

full_trends_df1 = pd.DataFrame()

for df in [full_trends_df]:
   for i in range(0,len(df)):
       test = pd.DataFrame(list(df.lines[i].values())[0])
       test["date"] = pd.to_datetime(test["date"])
       test['cat'] = df.cat[i]
       full_trends_df1 = pd.concat([full_trends_df1, test], ignore_index=True)
       print(i/len(df))

full_trends_df1['date'] = pd.to_datetime(full_trends_df1['date'])

full_trends_df1.to_csv(data_folder+"Raw/full_trends_df_weekly.csv",index=True)








