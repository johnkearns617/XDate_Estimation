# transform_data_quarterly.R
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

set.seed(178)

# make the transformations you need

fcast_df = imputed_df %>% 
  arrange(date) %>%
  mutate(year=year(date),
         qtr=quarter(date)) %>%
  select(-c(PCE,PRS85006112)) %>% 
  group_by(year,qtr) %>%
  mutate_at(vars(PAYEMS:gt_999),~mean(.,na.rm=TRUE)) %>%
  summarize_all(~.[1]) %>%
  ungroup() %>% 
  left_join(national_econ %>% 
              select(date,series_id,value) %>%
              pivot_wider(names_from=series_id,values_from=value) %>% 
              select(date,A261RX1Q020SBEA:SLCEC1))

pred_df = data.frame()
for(col in colnames(fcast_df)[256:271]){

for(dat in c(as.character(ceiling_date((national_econ %>% filter(series_id=="GDPC1"&date>="2007-01-01"))$date,"quarter")-1),"2024-12-31")){
  
  print(paste0(col," ",dat))
  
  imputed_df = read_csv(paste0("Data/Processing/imputed_data_asof",dat,".csv"))
  
  fcast_df1 = imputed_df %>% 
    arrange(date) %>%
    mutate(year=year(date),
           qtr=quarter(date)) %>%
    select(-c(PCE,PRS85006112)) %>%
    select(-one_of("ADPMNUSNERSA")) %>% 
    group_by(year,qtr) %>%
    mutate_at(vars(PAYEMS:gt_999),~mean(.,na.rm=TRUE)) %>%
    summarize_all(~.[1]) %>%
    ungroup() %>% 
    left_join(national_econ %>% 
                select(date,series_id,value) %>%
                pivot_wider(names_from=series_id,values_from=value) %>% 
                select(date,A261RX1Q020SBEA:SLCEC1)) %>%
    arrange(date) %>%
    mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10,col),~((./dplyr::lag(.,1)-1)*100)) %>%
    mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,gt_1003:gt_999),~(.-dplyr::lag(.,1))) %>%
    mutate(lag1=dplyr::lag(!!sym(col),1),
           lag2=dplyr::lag(!!sym(col),2),
           lag3=dplyr::lag(!!sym(col),3),
           lag4=dplyr::lag(!!sym(col),4)) %>%
    ungroup()
  
  
  X = model.matrix(as.formula(paste0(col,"~",paste(colnames(fcast_df1)[c(4:246)],collapse="+"))),
                   fcast_df1 %>% filter(date<"2020-01-01"&date<dat&year(date)>=2006&!is.na(!!sym(col))))[, -1]
  y = (fcast_df1 %>% filter(date<"2020-01-01"&date<dat&year(date)>=2006&!is.na(!!sym(col))))[[col]]
  
  if(length(y)<4){next}
  
  weight = (1:nrow(X))/nrow(X)
  weight = ifelse(weight<.5,.5,weight)
  fit_lasso_state = glmnet(X, y, alpha = 1,pmax=20)
  # weight by how recent the data is
  
  selected_coefs_state = data.frame(varImp(fit_lasso_state,lambda=min(fit_lasso_state$lambda), scale = FALSE)) %>% filter(Overall!=0)
  selected_coefs_state$var = as.numeric(gsub("gt_","",rownames(selected_coefs_state)))
  coef_value_state = coef(fit_lasso_state,s=min(fit_lasso_state$lambda))[,1][-1]
  coef_value_state = coef_value_state[coef_value_state!=0]
  selected_coefs_state = cbind(selected_coefs_state,coef_value_state)
  selected_coefs_state$category = sapply(selected_coefs_state$var,which_category)
  selected_coefs_state = selected_coefs_state %>% arrange(-Overall)
  
  test = lm_robust(as.formula(paste0(col,"~lag1+lag2+",paste(rownames(selected_coefs_state),collapse="+"))),
                   data = fcast_df1 %>% filter(date<=dat))
  
  pred_df = bind_rows(
    pred_df,
    data.frame(
      date=dat,
      var=col,
      pred=predict(test,fcast_df1 %>% filter(date==floor_date(as.Date(dat),"quarter"))),
      actual=fcast_df1[[col]][fcast_df1$date==floor_date(as.Date(dat),"quarter")]
    )
  )
}
sqrt(mean((pred_df$pred-pred_df$actual)^2,na.rm=TRUE))
pred_df$error[pred_df$var==col]=sqrt(mean((pred_df$pred[pred_df$var==col]-pred_df$actual[pred_df$var==col])^2,na.rm=TRUE))/mean(pred_df$actual[pred_df$var==col],na.rm=TRUE)
}





