# EPIC_modelling_spending.R
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

fredr_set_key(fred_key)

outlays_fred = fredr(paste0("MTSO133FMS")) %>% 
  mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)),
         value=value/1000)

receipts_fred = fredr(paste0("MTSR133FMS")) %>% 
  mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)),
         value=value/1000)


EM_DECLARATION = as.Date("2025-01-17")

# Step 1: Nowcast current levels

#imputed_df = read_csv(paste0("Data/Processing/imputed_data_asof",dat,".csv"))

# outlays
nowcast_headline = function(dataset,cbo_category){
  
  monthly_shares = dataset %>% 
    filter(fiscal_year>=2002&fiscal_year<=2023) %>% 
    group_by(fiscal_year) %>% 
    mutate(total=sum(value,na.rm=TRUE)) %>% 
    ungroup() %>%  
    mutate(share=value/total,
           month=month(date))
  
  monthly_shares_reg = lm_robust(share~factor(month),monthly_shares %>% group_by(fiscal_year) %>% filter(n()==12))
  
  fcast_df1 = imputed_df %>% 
    arrange(date) %>%
    mutate(year=year(date),
           month=month(date)) %>%
    select(-c(PCE,PRS85006112)) %>%
    select(-one_of("ADPMNUSNERSA")) %>% 
    left_join(dataset %>% 
                select(date,value)) %>% # join the yvariable
    arrange(date) %>%
    mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10),~((./dplyr::lag(.,1)-1)*100)) %>%
    mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,gt_1003:gt_999),~(.-dplyr::lag(.,1))) %>%
    mutate(lag1=dplyr::lag(value,1),
           lag2=dplyr::lag(value,2),
           lag3=dplyr::lag(value,3),
           lag4=dplyr::lag(value,4)) %>%
    ungroup() %>% 
    mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)))  %>% 
    left_join(cbo_proj %>% 
                filter(component==cbo_category&category=="Total") %>% 
                group_by(projected_fiscal_year) %>% 
                slice(n()) %>% 
                select(projected_fiscal_year,value) %>% 
                rename(cbo_proj=value,
                       fiscal_year=projected_fiscal_year))
  fcast_df1$cbo_proj_month = as.numeric(predict(monthly_shares_reg,data.frame(month=fcast_df1$month)))*fcast_df1$cbo_proj
  fcast_df1 = fcast_df1 %>% 
    mutate(cbo_proj_diff=(value/cbo_proj_month-1)*100) %>% 
    mutate(lag1_cbo_proj_diff=dplyr::lag(cbo_proj_diff,1),
           lag2_cbo_proj_diff=dplyr::lag(cbo_proj_diff,2))
  
  X = model.matrix(as.formula(paste0("value","~",paste(colnames(fcast_df1)[c(2:252)],collapse="+"))),
                   fcast_df1 %>% filter(date<"2024-01-01"&year(date)>=2006&!is.na(value)))[, -1]
  y = (fcast_df1 %>% filter(date<"2024-01-01"&year(date)>=2006&!is.na(value)))[["value"]]
  
  weight = (1:nrow(X))/nrow(X)
  weight = ifelse(weight<.5,.5,weight)
  fit_lasso_state = glmnet(X, y, alpha = 1,pmax=20,weights = weight)
  # weight by how recent the data is
  
  selected_coefs_state = data.frame(varImp(fit_lasso_state,lambda=min(fit_lasso_state$lambda), scale = FALSE)) %>% filter(Overall!=0)
  selected_coefs_state$var = as.numeric(gsub("gt_","",rownames(selected_coefs_state)))
  coef_value_state = coef(fit_lasso_state,s=min(fit_lasso_state$lambda))[,1][-1]
  coef_value_state = coef_value_state[coef_value_state!=0]
  selected_coefs_state = cbind(selected_coefs_state,coef_value_state)
  selected_coefs_state$category = sapply(selected_coefs_state$var,which_category)
  selected_coefs_state = selected_coefs_state %>% arrange(-Overall)
  
  test = lm_robust(as.formula(paste0("value","~lag1+lag2+lag3+lag4+cbo_proj_month+",paste(c(rownames(selected_coefs_state)),collapse="+"))),
                   data = fcast_df1 %>% filter(date<='2024-01-01') %>% mutate(weight=(1:n())/n()))
  
  pred_df = data.frame(
    date=fcast_df1[['date']],
    var=cbo_category,
    pred=predict(test,fcast_df1),
    actual=fcast_df1[['value']],
    cbo_proj=fcast_df1[['cbo_proj_month']]
  )
  
  return(list(
    'data'=fcast_df1,
    'reg'=test,
    'pred_df'=pred_df,
    'monthly_shares_reg'=monthly_shares_reg
  ))
  
}

nowcast_total_outlays = nowcast_headline(outlays_fred,"outlay")
nowcast_total_outlays[[3]] %>% drop_na() %>% summarize(my_proj=sqrt(mean(((pred/actual)-1)^2)),cbo_proj=sqrt(mean(((cbo_proj/actual)-1)^2)))

nowcast_total_receipts = nowcast_headline(receipts_fred,"revenue")

nowcast_total_receipts[[3]] %>% drop_na() %>% summarize(my_proj=sqrt(mean(((pred/actual)-1)^2)),cbo_proj=sqrt(mean(((cbo_proj/actual)-1)^2)))

# deficit
full_preds = bind_rows(
  nowcast_total_outlays[[3]],
  nowcast_total_receipts[[3]]
) %>% 
  group_by(date) %>% 
  mutate(pred=ifelse(var=="outlay",-1*pred,pred),
         actual=ifelse(var=="outlay",-1*actual,actual),
         cbo_proj=ifelse(var=="outlay",-1*cbo_proj,cbo_proj)) %>% 
  summarize(pred=sum(pred),
            actual=sum(actual),
            cbo_proj=sum(cbo_proj)) %>% 
  ungroup()

full_preds %>% filter(date>="2021-01-01") %>% drop_na() %>% summarize(my_proj=sqrt(mean(((pred/actual)-1)^2)),cbo_proj=sqrt(mean(((cbo_proj/actual)-1)^2)))


# make function
nowcast_budget_receipt = function(mts_dataset,col_mts,cbo_component,cbo_category){
  
  monthly_shares = mts_dataset %>% 
    filter(classification_desc==col_mts) %>% 
    mutate(record_date=floor_date(record_date,"month"),
           current_month_net_rcpt_amt=as.numeric(current_month_net_rcpt_amt)/1000000000) %>% 
    select(record_date,current_month_net_rcpt_amt) %>% 
    rename(date=record_date,
           value=current_month_net_rcpt_amt) %>% 
    mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10))) %>% 
    group_by(fiscal_year) %>% 
    mutate(total=sum(value,na.rm=TRUE)) %>% 
    ungroup() %>%  
    mutate(share=value/total,
           month=month(date))
  
  monthly_shares_reg = lm_robust(share~factor(month),monthly_shares %>% group_by(fiscal_year) %>% filter(n()==12))
  
fcast_df1 = imputed_df %>% 
  arrange(date) %>%
  mutate(year=year(date),
         month=month(date)) %>%
  select(-c(PCE,PRS85006112)) %>%
  select(-one_of("ADPMNUSNERSA")) %>% 
  left_join(mts_dataset %>% filter(classification_desc==col_mts) %>% 
              mutate(record_date=floor_date(record_date,"month"),
                     current_month_net_rcpt_amt=as.numeric(current_month_net_rcpt_amt)/1000000000) %>% 
              select(record_date,current_month_net_rcpt_amt) %>% 
              rename(date=record_date,
                     value=current_month_net_rcpt_amt)) %>% # join the yvariable
  arrange(date) %>%
  mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10),~((./dplyr::lag(.,1)-1)*100)) %>%
  mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,gt_1003:gt_999),~(.-dplyr::lag(.,1))) %>%
  mutate(lag1=dplyr::lag(value,1),
         lag2=dplyr::lag(value,2),
         lag3=dplyr::lag(value,3),
         lag4=dplyr::lag(value,4)) %>%
  ungroup() %>% 
  mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)))  %>% 
  left_join(cbo_proj %>% 
              filter(component==cbo_component&category==cbo_category) %>% 
              group_by(projected_fiscal_year) %>% 
              slice(n()) %>% 
              select(projected_fiscal_year,value) %>% 
              rename(cbo_proj=value,
                     fiscal_year=projected_fiscal_year))
fcast_df1$cbo_proj_month = as.numeric(predict(monthly_shares_reg,data.frame(month=fcast_df1$month)))*fcast_df1$cbo_proj
fcast_df1 = fcast_df1 %>% 
  mutate(cbo_proj_diff=(value/cbo_proj_month-1)*100) %>% 
  mutate(lag1_cbo_proj_diff=dplyr::lag(cbo_proj_diff,1),
         lag2_cbo_proj_diff=dplyr::lag(cbo_proj_diff,2))

X = model.matrix(as.formula(paste0("value","~",paste(colnames(fcast_df1)[c(2:252)],collapse="+"))),
                 fcast_df1 %>% filter(date<"2024-01-01"&year(date)>=2006&!is.na(value)))[, -1]
y = (fcast_df1 %>% filter(date<"2024-01-01"&year(date)>=2006&!is.na(value)))[['value']]

weight = (1:nrow(X))/nrow(X)
weight = ifelse(weight<.5,.5,weight)
fit_lasso_state = glmnet(X, y, alpha = 1,pmax=20,weights = weight)
# weight by how recent the data is

selected_coefs_state = data.frame(varImp(fit_lasso_state,lambda=min(fit_lasso_state$lambda), scale = FALSE)) %>% filter(Overall!=0)
selected_coefs_state$var = as.numeric(gsub("gt_","",rownames(selected_coefs_state)))
coef_value_state = coef(fit_lasso_state,s=min(fit_lasso_state$lambda))[,1][-1]
coef_value_state = coef_value_state[coef_value_state!=0]
selected_coefs_state = cbind(selected_coefs_state,coef_value_state)
selected_coefs_state$category = sapply(selected_coefs_state$var,which_category)
selected_coefs_state = selected_coefs_state %>% arrange(-Overall)

test = lm_robust(as.formula(paste0("value","~lag1+lag2+lag3+lag4+cbo_proj_month+",paste(c(rownames(selected_coefs_state)),collapse="+"))),
                 data = fcast_df1 %>% filter(date<='2024-01-01') %>% mutate(weight=(1:n())/n()))

pred_df = data.frame(
  date=fcast_df1[['date']],
  var=cbo_category,
  pred=predict(test,fcast_df1),
  actual=fcast_df1[['value']],
  cbo_proj=fcast_df1[['cbo_proj_month']]
)

return(list(
  'data'=fcast_df1,
  'reg'=test,
  'pred_df'=pred_df,
  'monthly_shares_reg'=monthly_shares_reg
))

}

nowcast_misc_receipts = nowcast_budget_receipt(receipts,"Total -- Miscellaneous Receipts","revenue", "Miscellaneous Receipts")
nowcast_corporate_receipts = nowcast_budget_receipt(receipts,"Corporation Income Taxes","revenue", "Corporate Income Taxes")
nowcast_payroll_receipts = nowcast_budget_receipt(receipts,"Total -- Social Insurance and Retirement Receipts","revenue", "Payroll Taxes")
nowcast_individual_receipts = nowcast_budget_receipt(receipts,"Total -- Individual Income Taxes","revenue", "Individual Income Taxes")
nowcast_excise_receipts = nowcast_budget_receipt(receipts,"Total -- Excise Taxes","revenue", "Excise Taxes")
nowcast_estate_receipts = nowcast_budget_receipt(receipts,"Estate and Gift Taxes","revenue", "Estate and Gift Taxes")
nowcast_customs_receipts = nowcast_budget_receipt(receipts,"Customs Duties","revenue", "Customs Duties")


nowcast_budget_outlay = function(cbo_category){
  
  if(cbo_category=="Medicare"){
    mandatory = spending_by_function %>% 
      filter(classification_desc=="Medicare") %>% 
      mutate(current_month_rcpt_outly_amt=as.numeric(current_month_rcpt_outly_amt)*.988828) %>% 
      select(record_date,current_month_rcpt_outly_amt)
    
    monthly_shares = mandatory %>% 
      mutate(record_date=floor_date(record_date,"month"),
             current_month_net_rcpt_amt=as.numeric(current_month_rcpt_outly_amt)/1000000000) %>% 
      select(record_date,current_month_net_rcpt_amt) %>% 
      rename(date=record_date,
             value=current_month_net_rcpt_amt) %>% 
      mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10))) %>% 
      group_by(fiscal_year) %>% 
      mutate(total=sum(value,na.rm=TRUE)) %>% 
      ungroup() %>%  
      mutate(share=value/total,
             month=month(date))
    
    monthly_shares_reg = lm_robust(share~factor(month),monthly_shares %>% group_by(fiscal_year) %>% filter(n()==12))
    
    fcast_df1 = imputed_df %>% 
      arrange(date) %>%
      mutate(year=year(date),
             month=month(date)) %>%
      select(-c(PCE,PRS85006112)) %>%
      select(-one_of("ADPMNUSNERSA")) %>% 
      left_join(mandatory %>% 
                  mutate(record_date=floor_date(record_date,"month"),
                         current_month_net_rcpt_amt=as.numeric(current_month_rcpt_outly_amt)/1000000000) %>% 
                  select(record_date,current_month_net_rcpt_amt) %>% 
                  rename(date=record_date,
                         value=current_month_net_rcpt_amt)) %>% # join the yvariable
      arrange(date) %>%
      mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10),~((./dplyr::lag(.,1)-1)*100)) %>%
      mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,gt_1003:gt_999),~(.-dplyr::lag(.,1))) %>%
      mutate(lag1=dplyr::lag(value,1),
             lag2=dplyr::lag(value,2),
             lag3=dplyr::lag(value,3),
             lag4=dplyr::lag(value,4)) %>%
      ungroup() %>% 
      mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)))  %>% 
      left_join(cbo_proj %>% 
                  filter(subcategory=="Medicare") %>% 
                  group_by(projected_fiscal_year) %>% 
                  slice(n()) %>% 
                  select(projected_fiscal_year,value) %>% 
                  rename(cbo_proj=value,
                         fiscal_year=projected_fiscal_year))
    fcast_df1$cbo_proj_month = as.numeric(predict(monthly_shares_reg,data.frame(month=fcast_df1$month)))*fcast_df1$cbo_proj
    fcast_df1 = fcast_df1 %>% 
      mutate(cbo_proj_diff=(value/cbo_proj_month-1)*100) %>% 
      mutate(lag1_cbo_proj_diff=dplyr::lag(cbo_proj_diff,1),
             lag2_cbo_proj_diff=dplyr::lag(cbo_proj_diff,2))
    
  }

  if(cbo_category=="Medicaid"){
    mandatory = outlays %>% 
      filter(classification_desc=="Grants to States for Medicaid") %>% 
      select(record_date,current_month_net_outly_amt) %>% 
      mutate(current_month_rcpt_outly_amt=as.numeric(current_month_net_outly_amt)) %>% 
      select(-current_month_net_outly_amt)
    
    monthly_shares = mandatory %>% 
      mutate(record_date=floor_date(record_date,"month"),
             current_month_net_rcpt_amt=as.numeric(current_month_rcpt_outly_amt)/1000000000) %>% 
      select(record_date,current_month_net_rcpt_amt) %>% 
      rename(date=record_date,
             value=current_month_net_rcpt_amt) %>% 
      mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10))) %>% 
      group_by(fiscal_year) %>% 
      mutate(total=sum(value,na.rm=TRUE)) %>% 
      ungroup() %>%  
      mutate(share=value/total,
             month=month(date))
    
    monthly_shares_reg = lm_robust(share~factor(month),monthly_shares %>% group_by(fiscal_year) %>% filter(n()==12))
    
    fcast_df1 = imputed_df %>% 
      arrange(date) %>%
      mutate(year=year(date),
             month=month(date)) %>%
      select(-c(PCE,PRS85006112)) %>%
      select(-one_of("ADPMNUSNERSA")) %>% 
      left_join(mandatory %>% 
                  mutate(record_date=floor_date(record_date,"month"),
                         current_month_net_rcpt_amt=as.numeric(current_month_rcpt_outly_amt)/1000000000) %>% 
                  select(record_date,current_month_net_rcpt_amt) %>% 
                  rename(date=record_date,
                         value=current_month_net_rcpt_amt)) %>% # join the yvariable
      arrange(date) %>%
      mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10),~((./dplyr::lag(.,1)-1)*100)) %>%
      mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,gt_1003:gt_999),~(.-dplyr::lag(.,1))) %>%
      mutate(lag1=dplyr::lag(value,1),
             lag2=dplyr::lag(value,2),
             lag3=dplyr::lag(value,3),
             lag4=dplyr::lag(value,4)) %>%
      ungroup() %>% 
      mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)))  %>% 
      left_join(cbo_proj %>% 
                  filter(subcategory=="Medicaid") %>% 
                  group_by(projected_fiscal_year) %>% 
                  slice(n()) %>% 
                  select(projected_fiscal_year,value) %>% 
                  rename(cbo_proj=value,
                         fiscal_year=projected_fiscal_year))
    fcast_df1$cbo_proj_month = as.numeric(predict(monthly_shares_reg,data.frame(month=fcast_df1$month)))*fcast_df1$cbo_proj
    fcast_df1 = fcast_df1 %>% 
      mutate(cbo_proj_diff=(value/cbo_proj_month-1)*100) %>% 
      mutate(lag1_cbo_proj_diff=dplyr::lag(cbo_proj_diff,1),
             lag2_cbo_proj_diff=dplyr::lag(cbo_proj_diff,2))
    
  }
  
  if(cbo_category=="Social Security"){
    mandatory = spending_by_function %>% 
      filter(classification_desc=="Social Security") %>% 
      select(record_date,current_month_rcpt_outly_amt) %>% 
      mutate(current_month_rcpt_outly_amt=as.numeric(current_month_rcpt_outly_amt)*0.9941)
    
    monthly_shares = mandatory %>% 
      mutate(record_date=floor_date(record_date,"month"),
             current_month_net_rcpt_amt=as.numeric(current_month_rcpt_outly_amt)/1000000000) %>% 
      select(record_date,current_month_net_rcpt_amt) %>% 
      rename(date=record_date,
             value=current_month_net_rcpt_amt) %>% 
      mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10))) %>% 
      group_by(fiscal_year) %>% 
      mutate(total=sum(value,na.rm=TRUE)) %>% 
      ungroup() %>%  
      mutate(share=value/total,
             month=month(date))
    
    monthly_shares_reg = lm_robust(share~factor(month),monthly_shares %>% group_by(fiscal_year) %>% filter(n()==12))
    
    fcast_df1 = imputed_df %>% 
      arrange(date) %>%
      mutate(year=year(date),
             month=month(date)) %>%
      select(-c(PCE,PRS85006112)) %>%
      select(-one_of("ADPMNUSNERSA")) %>% 
      left_join(mandatory %>% 
                  mutate(record_date=floor_date(record_date,"month"),
                         current_month_net_rcpt_amt=as.numeric(current_month_rcpt_outly_amt)/1000000000) %>% 
                  select(record_date,current_month_net_rcpt_amt) %>% 
                  rename(date=record_date,
                         value=current_month_net_rcpt_amt)) %>% # join the yvariable
      arrange(date) %>%
      mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10),~((./dplyr::lag(.,1)-1)*100)) %>%
      mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,gt_1003:gt_999),~(.-dplyr::lag(.,1))) %>%
      mutate(lag1=dplyr::lag(value,1),
             lag2=dplyr::lag(value,2),
             lag3=dplyr::lag(value,3),
             lag4=dplyr::lag(value,4)) %>%
      ungroup() %>% 
      mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)))  %>% 
      left_join(cbo_proj %>% 
                  filter(subcategory=="Social Security") %>% 
                  group_by(projected_fiscal_year) %>% 
                  slice(n()) %>% 
                  select(projected_fiscal_year,value) %>% 
                  rename(cbo_proj=value,
                         fiscal_year=projected_fiscal_year))
    fcast_df1$cbo_proj_month = as.numeric(predict(monthly_shares_reg,data.frame(month=fcast_df1$month)))*fcast_df1$cbo_proj
    fcast_df1 = fcast_df1 %>% 
      mutate(cbo_proj_diff=(value/cbo_proj_month-1)*100) %>% 
      mutate(lag1_cbo_proj_diff=dplyr::lag(cbo_proj_diff,1),
             lag2_cbo_proj_diff=dplyr::lag(cbo_proj_diff,2))
  }
  
  if(cbo_category=="National Defense"){
    mandatory = spending_by_function %>% 
      filter(classification_desc=="National Defense") %>% 
      select(record_date,current_month_rcpt_outly_amt) %>% 
      mutate(current_month_rcpt_outly_amt=as.numeric(current_month_rcpt_outly_amt))
    
    monthly_shares = mandatory %>% 
      mutate(record_date=floor_date(record_date,"month"),
             current_month_net_rcpt_amt=as.numeric(current_month_rcpt_outly_amt)/1000000000) %>% 
      select(record_date,current_month_net_rcpt_amt) %>% 
      rename(date=record_date,
             value=current_month_net_rcpt_amt) %>% 
      mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10))) %>% 
      group_by(fiscal_year) %>% 
      mutate(total=sum(value,na.rm=TRUE)) %>% 
      ungroup() %>%  
      mutate(share=value/total,
             month=month(date))
    
    monthly_shares_reg = lm_robust(share~factor(month),monthly_shares %>% group_by(fiscal_year) %>% filter(n()==12))
    
    fcast_df1 = imputed_df %>% 
      arrange(date) %>%
      mutate(year=year(date),
             month=month(date)) %>%
      select(-c(PCE,PRS85006112)) %>%
      select(-one_of("ADPMNUSNERSA")) %>% 
      left_join(mandatory %>% 
                  mutate(record_date=floor_date(record_date,"month"),
                         current_month_net_rcpt_amt=as.numeric(current_month_rcpt_outly_amt)/1000000000) %>% 
                  select(record_date,current_month_net_rcpt_amt) %>% 
                  rename(date=record_date,
                         value=current_month_net_rcpt_amt)) %>% # join the yvariable
      arrange(date) %>%
      mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10),~((./dplyr::lag(.,1)-1)*100)) %>%
      mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,gt_1003:gt_999),~(.-dplyr::lag(.,1))) %>%
      mutate(lag1=dplyr::lag(value,1),
             lag2=dplyr::lag(value,2),
             lag3=dplyr::lag(value,3),
             lag4=dplyr::lag(value,4)) %>%
      ungroup() %>% 
      mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)))  %>% 
      left_join(cbo_proj %>% 
                  filter(subcategory=="Defense Discretionary") %>% 
                  group_by(projected_fiscal_year) %>% 
                  slice(n()) %>% 
                  select(projected_fiscal_year,value) %>% 
                  rename(cbo_proj=value,
                         fiscal_year=projected_fiscal_year))
    fcast_df1$cbo_proj_month = as.numeric(predict(monthly_shares_reg,data.frame(month=fcast_df1$month)))*fcast_df1$cbo_proj
    fcast_df1 = fcast_df1 %>% 
      mutate(cbo_proj_diff=(value/cbo_proj_month-1)*100) %>% 
      mutate(lag1_cbo_proj_diff=dplyr::lag(cbo_proj_diff,1),
             lag2_cbo_proj_diff=dplyr::lag(cbo_proj_diff,2))
  }
  
  if(cbo_category=="Net Interest"){
    mandatory = spending_by_function %>% 
      filter(classification_desc=="Net Interest") %>% 
      select(record_date,current_month_rcpt_outly_amt) %>% 
      mutate(current_month_rcpt_outly_amt=as.numeric(current_month_rcpt_outly_amt))
    
    monthly_shares = mandatory %>% 
      mutate(record_date=floor_date(record_date,"month"),
             current_month_net_rcpt_amt=as.numeric(current_month_rcpt_outly_amt)/1000000000) %>% 
      select(record_date,current_month_net_rcpt_amt) %>% 
      rename(date=record_date,
             value=current_month_net_rcpt_amt) %>% 
      mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10))) %>% 
      group_by(fiscal_year) %>% 
      mutate(total=sum(value,na.rm=TRUE)) %>% 
      ungroup() %>%  
      mutate(share=value/total,
             month=month(date))
    
    monthly_shares_reg = lm_robust(share~factor(month),monthly_shares %>% group_by(fiscal_year) %>% filter(n()==12))
    
    fcast_df1 = imputed_df %>% 
      arrange(date) %>%
      mutate(year=year(date),
             month=month(date)) %>%
      select(-c(PCE,PRS85006112)) %>%
      select(-one_of("ADPMNUSNERSA")) %>% 
      left_join(mandatory %>% 
                  mutate(record_date=floor_date(record_date,"month"),
                         current_month_net_rcpt_amt=as.numeric(current_month_rcpt_outly_amt)/1000000000) %>% 
                  select(record_date,current_month_net_rcpt_amt) %>% 
                  rename(date=record_date,
                         value=current_month_net_rcpt_amt)) %>% # join the yvariable
      arrange(date) %>%
      mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10),~((./dplyr::lag(.,1)-1)*100)) %>%
      mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,gt_1003:gt_999),~(.-dplyr::lag(.,1))) %>%
      mutate(lag1=dplyr::lag(value,1),
             lag2=dplyr::lag(value,2),
             lag3=dplyr::lag(value,3),
             lag4=dplyr::lag(value,4)) %>%
      ungroup() %>% 
      mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)))  %>% 
      left_join(cbo_proj %>% 
                  filter(subcategory=="Net Interest") %>% 
                  group_by(projected_fiscal_year) %>% 
                  slice(n()) %>% 
                  select(projected_fiscal_year,value) %>% 
                  rename(cbo_proj=value,
                         fiscal_year=projected_fiscal_year))
    fcast_df1$cbo_proj_month = as.numeric(predict(monthly_shares_reg,data.frame(month=fcast_df1$month)))*fcast_df1$cbo_proj
    fcast_df1 = fcast_df1 %>% 
      mutate(cbo_proj_diff=(value/cbo_proj_month-1)*100) %>% 
      mutate(lag1_cbo_proj_diff=dplyr::lag(cbo_proj_diff,1),
             lag2_cbo_proj_diff=dplyr::lag(cbo_proj_diff,2))
  }
  
  if(cbo_category=="Other Spending"){
    
    mandatory = bind_rows(
      # Health mandatory spending
      spending_by_function %>% 
        filter((classification_desc%in%c("Income Security","Health","Transportation","Community and Regional Development",
                                         "Education, Training, Employment, and Social Services",
                                         "Veterans Benefits and Services","Administration of Justice",
                                         "General Government","Undistributed Offsetting Receipts",
                                         "International Affairs","General Science, Space, and Technology",
                                         "Energy","Natural Resources and Environment","Agriculture",
                                         "Commerce and Housing Credit","Medicare","Social Security"))) %>% 
        select(record_date,classification_desc,current_month_rcpt_outly_amt) %>% 
        mutate(current_month_net_outly_amt=(as.numeric(current_month_rcpt_outly_amt)),
               current_month_net_outly_amt=ifelse(classification_desc=="Social Security",current_month_net_outly_amt*(1-0.9941),current_month_net_outly_amt),
               current_month_net_outly_amt=ifelse(classification_desc=="Medicare",current_month_net_outly_amt*(1-.988828),current_month_net_outly_amt)) %>% 
        select(-c(current_month_rcpt_outly_amt))) %>% 
      mutate(fiscal_year=as.integer(quarter(record_date, with_year = TRUE, fiscal_start = 10))) %>% 
      group_by(record_date) %>% 
      summarize(current_month_net_outly_amt=sum(current_month_net_outly_amt,na.rm=TRUE),
                fiscal_year=fiscal_year[n()]) %>% 
      left_join(outlays %>% 
                  filter(classification_desc=="Grants to States for Medicaid") %>% 
                  select(record_date,current_month_net_outly_amt) %>% 
                  mutate(medicaid=as.numeric(current_month_net_outly_amt)) %>% 
                  select(-current_month_net_outly_amt)) %>% 
      mutate(current_month_rcpt_outly_amt=current_month_net_outly_amt-medicaid) %>% 
      select(-c(current_month_net_outly_amt,medicaid))
    
    monthly_shares = mandatory %>% 
      mutate(record_date=floor_date(record_date,"month"),
             current_month_net_rcpt_amt=as.numeric(current_month_rcpt_outly_amt)/1000000000) %>% 
      select(record_date,current_month_net_rcpt_amt) %>% 
      rename(date=record_date,
             value=current_month_net_rcpt_amt) %>% 
      mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10))) %>% 
      group_by(fiscal_year) %>% 
      mutate(total=sum(value,na.rm=TRUE)) %>% 
      ungroup() %>%  
      mutate(share=value/total,
             month=month(date))
    
    monthly_shares_reg = lm_robust(share~factor(month),monthly_shares %>% group_by(fiscal_year) %>% filter(n()==12))
    
    fcast_df1 = imputed_df %>% 
      arrange(date) %>%
      mutate(year=year(date),
             month=month(date)) %>%
      select(-c(PCE,PRS85006112)) %>%
      select(-one_of("ADPMNUSNERSA")) %>% 
      left_join(mandatory %>% 
                  mutate(record_date=floor_date(record_date,"month"),
                         current_month_net_rcpt_amt=as.numeric(current_month_rcpt_outly_amt)/1000000000) %>% 
                  select(record_date,current_month_net_rcpt_amt) %>% 
                  rename(date=record_date,
                         value=current_month_net_rcpt_amt)) %>% # join the yvariable
      arrange(date) %>%
      mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10),~((./dplyr::lag(.,1)-1)*100)) %>%
      mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,gt_1003:gt_999),~(.-dplyr::lag(.,1))) %>%
      mutate(lag1=dplyr::lag(value,1),
             lag2=dplyr::lag(value,2),
             lag3=dplyr::lag(value,3),
             lag4=dplyr::lag(value,4)) %>%
      ungroup() %>% 
      mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)))  %>% 
      left_join(cbo_proj %>% 
                  filter(subcategory%in%c("Nondefense Discretionary","Other Mandatory")) %>% 
                  group_by(projected_fiscal_year,subcategory) %>% 
                  slice(n()) %>% 
                  group_by(projected_fiscal_year) %>% 
                  summarize(value=sum(value,na.rm=TRUE)) %>% 
                  rename(cbo_proj=value,
                         fiscal_year=projected_fiscal_year))
    fcast_df1$cbo_proj_month = as.numeric(predict(monthly_shares_reg,data.frame(month=fcast_df1$month)))*fcast_df1$cbo_proj
    fcast_df1 = fcast_df1 %>% 
      mutate(cbo_proj_diff=(value/cbo_proj_month-1)*100) %>% 
      mutate(lag1_cbo_proj_diff=dplyr::lag(cbo_proj_diff,1),
             lag2_cbo_proj_diff=dplyr::lag(cbo_proj_diff,2))
    
  }

  X = model.matrix(as.formula(paste0("value","~",paste(colnames(fcast_df1)[c(2:252)],collapse="+"))),
                   fcast_df1 %>% filter(date<"2024-01-01"&year(date)>=2006&!is.na(value)))[, -1]
  y = (fcast_df1 %>% filter(date<"2024-01-01"&year(date)>=2006&!is.na(value)))[['value']]
  
  weight = (1:nrow(X))/nrow(X)
  weight = ifelse(weight<.5,.5,weight)
  fit_lasso_state = glmnet(X, y, alpha = 1,pmax=20,weights = weight)
  # weight by how recent the data is
  
  selected_coefs_state = data.frame(varImp(fit_lasso_state,lambda=min(fit_lasso_state$lambda), scale = FALSE)) %>% filter(Overall!=0)
  selected_coefs_state$var = as.numeric(gsub("gt_","",rownames(selected_coefs_state)))
  coef_value_state = coef(fit_lasso_state,s=min(fit_lasso_state$lambda))[,1][-1]
  coef_value_state = coef_value_state[coef_value_state!=0]
  selected_coefs_state = cbind(selected_coefs_state,coef_value_state)
  selected_coefs_state$category = sapply(selected_coefs_state$var,which_category)
  selected_coefs_state = selected_coefs_state %>% arrange(-Overall)
  
  test = lm_robust(as.formula(paste0("value","~lag1+lag2+lag3+lag4+cbo_proj_month+",paste(c(rownames(selected_coefs_state)),collapse="+"))),
                   data = fcast_df1 %>% filter(date<='2024-01-01') %>% mutate(weight=(1:n())/n()))
  
  pred_df = data.frame(
    date=fcast_df1[['date']],
    var=cbo_category,
    pred=predict(test,fcast_df1),
    actual=fcast_df1[['value']],
    cbo_proj=fcast_df1[['cbo_proj_month']]
  )
  
  return(list(
    'data'=fcast_df1,
    'reg'=test,
    'pred_df'=pred_df,
    'monthly_shares_reg'=monthly_shares_reg
  ))
  
}

nowcast_medicare_outlay = nowcast_budget_outlay("Medicare")
nowcast_medicaid_outlay = nowcast_budget_outlay("Medicaid")
nowcast_ss_outlay = nowcast_budget_outlay("Social Security")
nowcast_other_outlay = nowcast_budget_outlay("Other Spending")
nowcast_defense_outlay = nowcast_budget_outlay("National Defense")
nowcast_interest_outlay = nowcast_budget_outlay("Net Interest")

nowcast_outlay = bind_cols(
  nowcast_medicare_outlay[[3]] %>% select(date,pred) %>% rename(medicare=pred),
  nowcast_medicaid_outlay[[3]] %>% select(pred) %>% rename(medicaid=pred),
  nowcast_ss_outlay[[3]] %>% select(pred) %>% rename(ss=pred),
  nowcast_defense_outlay[[3]] %>% select(pred) %>% rename(defense=pred),
  nowcast_interest_outlay[[3]] %>% select(pred) %>% rename(interest=pred),
  nowcast_other_outlay[[3]] %>% select(pred) %>% rename(other=pred)
)

nowcast_receipt = bind_cols(
  nowcast_misc_receipts[[3]] %>% select(date,pred) %>% rename(misc=pred),
  nowcast_corporate_receipts[[3]] %>% select(pred) %>% rename(corp=pred),
  nowcast_payroll_receipts[[3]] %>% select(pred) %>% rename(payroll=pred),
  nowcast_individual_receipts[[3]] %>% select(pred) %>% rename(individ=pred),
  nowcast_excise_receipts[[3]] %>% select(pred) %>% rename(excise=pred),
  nowcast_estate_receipts[[3]] %>% select(pred) %>% rename(estate=pred),
  nowcast_customs_receipts[[3]] %>% select(pred) %>% rename(customs=pred),
)

nowcast_deficit = bind_cols(
  nowcast_receipt %>% group_by(date) %>% summarize(receipts=misc+corp+payroll+individ+excise+estate+customs),
  nowcast_outlay %>% group_by(date) %>% summarize(outlays=medicare+medicaid+ss+defense+interest+other) %>% select(-date)
) %>% 
  mutate(deficit=receipts-outlays) %>% 
  left_join(receipts_fred %>% select(date,value) %>% rename(actual_receipts=value)) %>% 
  left_join(outlays_fred %>% select(date,value) %>% rename(actual_outlays=value)) %>% 
  mutate(actual_deficit=actual_receipts-actual_outlays)




