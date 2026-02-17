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
library(tis)

conflicted::conflicts_prefer(lubridate::year)

set.seed(178)

# get data
# Function can take any date
df = make_df(end_date,bad_vars,most_recent = FALSE) %>% 
  group_by(year,qtr) %>%
  fill(PRS85006112,CIS1020000000000I,.direction="down") %>% 
  ungroup() %>% 
  select(-c(MTSR133FMS:W017RC1Q027SBEA,A261RX1Q020SBEA:SLCEC1,B096RC1Q027SBEA:A091RC1Q027SBEA,B243RC1Q027SBEA:AD02RC1Q027SBEA,year,qtr)) %>%  # remove retail, gdp variables to speed up code, even though they do improve the model fit
  mutate_at(vars(-c(date)),~ifelse(is.infinite(.)|is.nan(.),NA,.)) %>% 
  select_if(~sum(!is.na(.))>0|is.character(.)|is.Date(.)) %>% 
  select_if(~sd(.,na.rm=TRUE)!=0|is.character(.)|is.Date(.)) %>% 
  filter(date>="2004-01-01")

write_csv(df,paste0("Data/Processing/raw_data/data_asof",end_date,".csv"))

set.seed(178)

imputed_df = impute_function(df,end_date,repeats=1)

write_csv(imputed_df,paste0("Data/Processing/imputed_data/imputed_data_asof",end_date,".csv"))

pca = prcomp(imputed_df[2:ncol(imputed_df)])


