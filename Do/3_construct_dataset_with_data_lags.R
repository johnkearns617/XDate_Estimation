# construct_dataset_with_data_lags.R
# John Kearns
# Goal: Write script to join all of the economic data, and construct the dataset as of a given date, and run imputation of past values

# load packages
library(estimatr)
library(gtrendsR)
library(tidyverse)
library(fuzzyjoin)
library(lubridate)
library(KFAS)
library(xts)
library(parallelly)
library(parallel)
library(mFilter)
library(fredr)
library(forecast)
library(glmnet)
library(caret)
library(vtable)
library(seasonal)
library(signal)
library(plm)
library(blsAPI)
library(rjson)

set.seed(178)

# trends data
trends_vol = data.frame()
for(i in grep("202503",list.files("Data/Processing/gt_data"),value=TRUE)){ # use only the data from March 2025
  
  tmp = read_csv(paste0("Data/Processing/gt_data/",i)) %>% 
    mutate(vintage=gsub("trends_full_sa_|.csv","",i))
  
  trends_vol = bind_rows(trends_vol,tmp)
  
}

bad_vars = trends_vol %>% 
  filter(date>=floor_date((trends_vol %>% filter(vintage==min(vintage)) %>% filter(date==max(date)) %>% distinct(date) %>% pull(date)),"year") %m-% years(2)&
           date<floor_date((trends_vol %>% filter(vintage==min(vintage)) %>% filter(date==max(date)) %>% distinct(date) %>% pull(date)),"year")) %>% 
  group_by(date,category) %>% 
  summarize(std=sd(deviation)) %>% 
  group_by(category) %>% 
  summarize(avg_std=median(std)) %>% 
  filter(avg_std>=.3)

state_trends = make_state_trends(end_date,bad_vars = bad_vars,most_recent = FALSE) %>% 
  mutate(release_date=date+6)

# start with GDP data

join_df = national_econ %>% 
              filter(date>=min(state_trends$date)) %>% 
              pivot_wider(id_cols=c('date'),names_from='series_id',values_from='value') %>% 
              mutate(year=year(date),
                     qtr=quarter(date)) %>% 
  relocate(date,.before=1) %>% 
  left_join(state_trends %>% 
              mutate(date=date+6,
                     series_id=paste0("gt_",category),
                     month=month(date),
                     year=year(date)) %>%
              group_by(year,month,series_id) %>%
              summarize(deviation=mean(deviation,na.rm=TRUE)) %>% 
              mutate(date=as.Date(paste0(year,"-",month,"-","01"),format="%Y-%m-%d")) %>% 
              pivot_wider(id_cols=c('date'),names_from='series_id',values_from=c('deviation')),
            by=c('date')) %>% 
  ungroup() %>% 
  mutate(rgdp_qoq_pchange=(GDPC1/dplyr::lag(GDPC1,3)-1)*100)

cor_df = t(cor(join_df$rgdp_qoq_pchange,join_df[,colnames(join_df)[2:which(colnames(join_df)=="gt_999")]],use="pairwise.complete.obs"))

#tmp = make_df("2023-01-31")

