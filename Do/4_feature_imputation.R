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
library(sinkr)
library(missMDA)
library(FactoMineR)
library(mice)

conflicted::conflict_prefer("filter","dplyr")

# get data
# code currently written to get data as of end of November 2024. Function can take any date
df = make_df(floor_date(Sys.Date(), "month")-1) %>% 
  group_by(year,qtr) %>%
  fill(PRS85006112,.direction="down") %>% 
  ungroup() %>% 
  select(-c(MTSR133FMS:W017RC1Q027SBEA,A261RX1Q020SBEA:SLCEC1,B096RC1Q027SBEA:A091RC1Q027SBEA,B243RC1Q027SBEA:AD02RC1Q027SBEA,year,qtr)) %>%  # remove indeed and retail variables to speed up code, even though they do improve the model fit
  mutate_at(vars(-c(date)),~ifelse(is.infinite(.)|is.nan(.),NA,.)) %>% 
  select_if(~sum(!is.na(.))>0|is.character(.)|is.Date(.)) %>% 
  select_if(~sd(.,na.rm=TRUE)!=0|is.character(.)|is.Date(.)) %>% 
  filter(date>="2004-01-01")

write_csv(df,paste0("Data/Processing/data_asof",floor_date(Sys.Date(), "month")-1,".csv"))

set.seed(178)

impute_function = function(df){
  test_dineof=df
  
  flag = 0
  while(flag<3){
  for(col1 in colnames(test_dineof)[c(2:ncol(test_dineof))]){
    
    print(paste0(col1))
  
    if(length(which(is.na(test_dineof[c((nrow(test_dineof)-10):nrow(test_dineof)),col1])))==0&col1!="IHLIDXUS"){ next }
    if(!(col1%in%colnames(test_dineof))){next}
    if(col1%in%c("ADPMNUSNERSA")&as.Date(dat)<"2010-01-01"){next}
    if(col1=="IHLIDXUS"&as.Date(dat)<"2021-01-01"){next}
    
    value = data.frame(date=test_dineof$date)
    for(i in 1:30){
      if("IHLIDXUS"%in%colnames(test_dineof)&"ADPMNUSNERSA"%in%colnames(test_dineof)){
        if(col1=="IHLIDXUS"){potential_cols = colnames(test_dineof %>% select(-c(col1,IHLIDXUS,ADPMNUSNERSA,gt_1003:gt_999)) %>% filter(date==max(date)) %>% select(-date) %>% select_if(!is.na(.)))}else{
        potential_cols = colnames(test_dineof %>% select(-c(col1,IHLIDXUS,ADPMNUSNERSA,gt_1003:gt_999)) %>% filter(date==test_dineof$date[(nrow(test_dineof)-3):nrow(test_dineof)][head(which(is.na(test_dineof[[col1]][(nrow(test_dineof)-3):nrow(test_dineof)])),1)]) %>% select(-date) %>% select_if(!is.na(.)))
          }
        } else{
          potential_cols = colnames(test_dineof %>% select(-c(col1,gt_1003:gt_999)) %>% select(-one_of("ADPMNUSNERSA","IHLIDXUS")) %>% filter(date==test_dineof$date[(nrow(test_dineof)-3):nrow(test_dineof)][max(head(which(is.na(test_dineof[[col1]][(nrow(test_dineof)-3):nrow(test_dineof)])),1),1)]) %>% select(-date) %>% select_if(!is.na(.)))
          }
      cols = c(sample(potential_cols,min(c(15,floor(length(potential_cols)/2)))),sample(colnames(test_dineof %>% select(gt_1003:gt_999)),15))
      test = lm_robust(as.formula(paste0(paste0(col1,"~lag+lag2+"),paste(cols,collapse="+"))),
                       data=test_dineof %>% select(col1,cols) %>% 
                         mutate(lag=dplyr::lag(!!sym(col1),1),
                                lag2=dplyr::lag(!!sym(col1),2),
                                lag3=dplyr::lag(!!sym(col1),3),
                                lag4=dplyr::lag(!!sym(col1),4),
                                lag5=dplyr::lag(!!sym(col1),5),
                                lag6=dplyr::lag(!!sym(col1),6)))
      imp <- predict(test,test_dineof %>% select(col1,cols) %>% mutate(lag=dplyr::lag(!!sym(col1),1),
                                                                      lag2=dplyr::lag(!!sym(col1),2),
                                                                      lag3=dplyr::lag(!!sym(col1),3),
                                                                      lag4=dplyr::lag(!!sym(col1),4),
                                                                      lag5=dplyr::lag(!!sym(col1),5),
                                                                      lag6=dplyr::lag(!!sym(col1),6)) %>% 
                       fill(lag:lag6,.direction="up"))
      
      value=bind_cols(value,imp)
    }
    
    value1 = data.frame(
      date=value$date,
      replacement=rowMeans(value[,2:ncol(value)],na.rm=TRUE)
    )
    
    for(i in 1:nrow(value)){
      
      if(is.na(test_dineof[i,col1])){
        test_dineof[i,col1] = value1[i,"replacement"]
        }
      }
  }
    if(col1=="gt_999"){
      flag = flag+1
    }
  }
  
  return(test_dineof)
  
}

imputed_df = impute_function(df)

left_join(imputed_df %>% 
            pivot_longer(PAYEMS:gt_999) %>% 
            filter(date>="2024-10-01"),
          df %>% 
            pivot_longer(PAYEMS:gt_999) %>% 
            filter(date>="2024-10-01"),by=c('date','name')) %>% 
  left_join(national_econ,by=c('date'='date','name'='series_id')) %>% 
  filter(is.na(value.y)) %>% 
  mutate(diff=(value.x-value)/value*100) %>% 
  summarize(sqrt(mean((diff^2),na.rm=TRUE)))
# RMSE of 2.5%

write_csv(imputed_df,paste0("Data/Processing/imputed_data_asof",floor_date(Sys.Date(), "month")-1,".csv"))

for(dat in c(as.character(ceiling_date((national_econ %>% filter(series_id=="GDPC1"&date>="2007-01-01"))$date,"quarter")-1),"2024-12-31")){
  
  print(paste0(dat))
  
  df = make_df(ceiling_date(as.Date(dat), "quarter")-1) %>% 
    group_by(year,qtr) %>%
    fill(PRS85006112,.direction="down") %>% 
    ungroup() %>% 
    select(-c(MTSR133FMS:W017RC1Q027SBEA,A261RX1Q020SBEA:SLCEC1,B096RC1Q027SBEA:A091RC1Q027SBEA,B243RC1Q027SBEA:AD02RC1Q027SBEA,year,qtr)) %>%  # remove indeed and retail variables to speed up code, even though they do improve the model fit
    mutate_at(vars(-c(date)),~ifelse(is.infinite(.)|is.nan(.),NA,.)) %>% 
    select_if(~sum(!is.na(.))>0|is.character(.)|is.Date(.)) %>% 
    select_if(~sd(.,na.rm=TRUE)!=0|is.character(.)|is.Date(.)) %>% 
    filter(date>="2004-01-01")
  
  imputed_df = impute_function(df)
  
  write_csv(imputed_df,paste0("Data/Processing/imputed_data_asof",dat,".csv"))
  
}
