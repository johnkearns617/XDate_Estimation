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


def main():
  service = build('trends', 'v1beta',
                  developerKey=r.gt_key,
                  discoveryServiceUrl=DISCOVERY_URL)

  # Single Graph Example, no restrictions
  graph = service.getGraph(terms='apple')
  response = graph.execute()
  pprint.pprint(response)

  # Averages, no restrictions.
  averages = service.getGraphAverages(terms=['apple', 'orange'])
  response = averages.execute()
  pprint.pprint(response)

  # Regions, no restrictions.
  regions = service.regions().list(term='apple')
  response = regions.execute()
  pprint.pprint(response)
  # Top queries, no restrictions.
  top_queries = service.getTopQueries(term='apple')
  response = top_queries.execute()
  pprint.pprint(response)

  # Top topics, no restrictions.
  # The result for topics (top & rising) is a list of knowledge graph
  # topics (entity ids - mids), which can be resolved using freebase.com
  top_topics = service.getTopTopics(term='apple')
  response = top_topics.execute()
  pprint.pprint(response)

  # Note that returned values for rising queries and topics represents
  # percentage of rising in searches since previous time range. If there
  # were close to 0 searches for the term in the previous time range, the
  # value will be Double.MAX_VALUE and considered "breakout".

  # Rising queries, no restrictions.
  rising_queries = service.getRisingQueries(term='apple')
  response = rising_queries.execute()
  pprint.pprint(response)

  # Rising topics, no restrictions.
  rising_topics = service.getRisingTopics(term='apple')
  response = rising_topics.execute()
  pprint.pprint(response)

  # Creating restrictions.
  # Note that you need both a startDate and an endDate or none.
  # Dates should be a month and a year in the format YYYY-MM e.g. '2010-01'
  # Parameter names are: restrictions_startDate, restrictions_endDate.
  #
  # Geo takes any of the values depicted here:
  #   http://en.wikipedia.org/wiki/ISO_3166-2#Current_codes
  # Parameter name is restrictions_geo.
  #
  # Category takes the following value formats:
  #   0-3 is Arts & Entertainment
  #   0-3-613 is Online Media (which is a subcategory of the above).
  # We will do our best to fix you up with a list of possible codes, but in
  # the meantime, you can query each such category in the Trends Explore
  # frontend, by hitting on "embed" after you've chosen a category for
  # a query and copying what's written next to: &cat=
  # Parameter name is restrictions_category.
  #
  # Property takes the values: images/news/froogle/youtube/web
  # Web is the default value if none is provided, and 'froogle' is
  # Google Shopping.
  # Parameter name is restrictions_property.

  start_date = '2010-01'
  end_date = '2016-01'
  response = service.getGraph(terms='apple',
                              restrictions_startDate=start_date,
                              restrictions_endDate=end_date).execute()
  pprint.pprint(response)

  # All methods allow term to be either search query or knowledge graph
  # topic id (mid) - use freebase.com to find these ids.
  response = service.getGraph(terms='/m/02mjmr').execute()
  pprint.pprint(response)

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
                  developerKey='AIzaSyDpLV8TcJwfhDloE1VjtvoUQl1Z_mY0ryE',
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








