# feature_imputation.R
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
library(missMDA)
library(FactoMineR)
library(mice)

conflicted::conflict_prefer("filter","dplyr")

# get data
# code currently written to get data as of end of November 2024. Function can take any date
df = make_df(Sys.Date()) %>% 
  group_by(year,qtr) %>%
  fill(PRS85006112,.direction="down") %>% 
  ungroup() %>% 
  select(-c(MTSR133FMS:W017RC1Q027SBEA,A261RX1Q020SBEA:SLCEC1,B096RC1Q027SBEA:A091RC1Q027SBEA,B243RC1Q027SBEA:AD02RC1Q027SBEA,year,qtr)) %>%  # remove indeed and retail variables to speed up code, even though they do improve the model fit
  mutate_at(vars(-c(date)),~ifelse(is.infinite(.)|is.nan(.),NA,.)) %>% 
  select_if(~sum(!is.na(.))>0|is.character(.)|is.Date(.)) %>% 
  select_if(~sd(.,na.rm=TRUE)!=0|is.character(.)|is.Date(.)) %>% 
  filter(date>="2004-01-01")

write_csv(df,paste0("Data/Processing/raw_data/data_asof",Sys.Date(),".csv"))

set.seed(178)

imputed_df = impute_function(df,Sys.Date())
# 
# left_join(imputed_df %>% 
#             pivot_longer(PAYEMS:gt_999) %>% 
#             filter(date>="2024-10-01"),
#           df %>% 
#             pivot_longer(PAYEMS:gt_999) %>% 
#             filter(date>="2024-10-01"),by=c('date','name')) %>% 
#   left_join(national_econ,by=c('date'='date','name'='series_id')) %>% 
#   filter(is.na(value.y)) %>% 
#   mutate(diff=(value.x-value)/value*100) %>% 
#   summarize(sqrt(mean((diff^2),na.rm=TRUE)))
# RMSE of 2.5%

write_csv(imputed_df,paste0("Data/Processing/imputed_data/imputed_data_asof",Sys.Date(),".csv"))

# for(dat in c(as.character(ceiling_date((national_econ %>% filter(series_id=="GDPC1"&date>="2007-01-01"))$date,"quarter")-1))){
#   
#   print(paste0(dat))
#   
#   df = make_df(ceiling_date(as.Date(dat), "quarter")-1) %>% 
#     group_by(year,qtr) %>%
#     fill(PRS85006112,.direction="down") %>% 
#     ungroup() %>% 
#     select(-c(MTSR133FMS:W017RC1Q027SBEA,A261RX1Q020SBEA:SLCEC1,B096RC1Q027SBEA:A091RC1Q027SBEA,B243RC1Q027SBEA:AD02RC1Q027SBEA,year,qtr)) %>%  # remove indeed and retail variables to speed up code, even though they do improve the model fit
#     mutate_at(vars(-c(date)),~ifelse(is.infinite(.)|is.nan(.),NA,.)) %>% 
#     select_if(~sum(!is.na(.))>0|is.character(.)|is.Date(.)) %>% 
#     select_if(~sd(.,na.rm=TRUE)!=0|is.character(.)|is.Date(.)) %>% 
#     filter(date>="2004-01-01")
#   
#   imputed_df = impute_function(df)
#   
#   write_csv(imputed_df,paste0("Data/Processing/imputed_data_asof",dat,".csv"))
#   
# }
