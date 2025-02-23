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
library(sinkr)

conflicted::conflict_prefer("filter","dplyr")

# trends data
state_trends = read_csv(paste0("Data/Processing/trends_full_sa_20250217.csv")) %>% 
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

cor_df = t(cor(join_df$rgdp_qoq_pchange,join_df[,colnames(join_df)[2:303]],use="pairwise.complete.obs"))

make_df = function(end_date){
  
  
  df = national_econ %>% 
                 filter(date<=end_date) %>% 
                 mutate(value=ifelse(release_date>end_date,NA,value)) %>% 
                 pivot_wider(id_cols=c('date'),names_from='series_id',values_from='value') %>% 
                 mutate(year=year(date),
                        qtr=quarter(date)) %>% 
    relocate(date,.before=1) %>% 
    # other state variables
    left_join(state_trends %>% 
                mutate(date=date+6,
                       series_id=paste0("gt_",category),
                       month=month(date),
                       year=year(date)) %>%
                group_by(year,month,series_id) %>%
                summarize(deviation=mean(deviation,na.rm=TRUE)) %>% 
                mutate(date=as.Date(paste0(year,"-",month,"-","01"),format="%Y-%m-%d")) %>% 
                pivot_wider(id_cols=c('date'),names_from='series_id',values_from=c('deviation')),
              by=c('date'))
  
  
  return(df)
  
}


#tmp = make_df("2023-01-31")

