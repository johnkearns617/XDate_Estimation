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

EM_DECLARATION = as.Date("2025-01-17")

# Step 1: Nowcast current levels

#imputed_df = read_csv(paste0("Data/Processing/imputed_data/imputed_data_asof",Sys.Date(),".csv"))

# outlays
nowcast_total_outlays = nowcast_headline(outlays_fred,"outlay")
nowcast_total_outlays[[3]] %>% drop_na() %>% summarize(my_proj=sqrt(mean(((pred/actual)-1)^2)),cbo_proj=sqrt(mean(((cbo_proj/actual)-1)^2)))

if(length(setdiff(as.character(tail(national_econ %>% arrange(date) %>% distinct(date) %>% pull(date))),as.character(nowcast_total_outlays$pred_df$date[!is.na(nowcast_total_outlays$pred_df$pred)])))>0){
  for(dat in setdiff(as.character(tail(national_econ %>% arrange(date) %>% distinct(date) %>% pull(date))),as.character(nowcast_total_outlays$pred_df$date[!is.na(nowcast_total_outlays$pred_df$pred)]))){
    
    nowcast_total_outlays = nowcast_headline(outlays_fred %>% 
                                               add_row(data.frame(date=as.Date(dat) %m-% months(1),
                                                                  series_id=outlays_fred$series_id[1],
                                                                  value=tail(na.omit(nowcast_total_outlays$pred_df$pred),1),
                                                                  fiscal_year=ifelse(month(as.Date(dat) %m-% months(1))>=10,year(as.Date(dat) %m-% months(1))+1,year(as.Date(dat) %m-% months(1))))),
                                             "outlay")
    
  }
}

nowcast_total_receipts = nowcast_headline(receipts_fred,"revenue")

if(length(setdiff(as.character(tail(national_econ %>% arrange(date) %>% distinct(date) %>% pull(date))),as.character(nowcast_total_receipts$pred_df$date[!is.na(nowcast_total_receipts$pred_df$pred)])))>0){
  for(dat in setdiff(as.character(tail(national_econ %>% arrange(date) %>% distinct(date) %>% pull(date))),as.character(nowcast_total_receipts$pred_df$date[!is.na(nowcast_total_receipts$pred_df$pred)]))){
    
    nowcast_total_receipts = nowcast_headline(receipts_fred %>% 
                                               add_row(data.frame(date=as.Date(dat) %m-% months(1),
                                                                  series_id=receipts_fred$series_id[1],
                                                                  value=tail(na.omit(nowcast_total_receipts$pred_df$pred),1),
                                                                  fiscal_year=ifelse(month(as.Date(dat) %m-% months(1))>=10,year(as.Date(dat) %m-% months(1))+1,year(as.Date(dat) %m-% months(1))))),
                                             "revenue")
    
  }
}

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

nowcast_misc_receipts = nowcast_budget_receipt(receipts,"Total -- Miscellaneous Receipts","revenue", "Miscellaneous Receipts")
nowcast_corporate_receipts = nowcast_budget_receipt(receipts,"Corporation Income Taxes","revenue", "Corporate Income Taxes")
nowcast_payroll_receipts = nowcast_budget_receipt(receipts,"Total -- Social Insurance and Retirement Receipts","revenue", "Payroll Taxes")
nowcast_individual_receipts = nowcast_budget_receipt(receipts,"Total -- Individual Income Taxes","revenue", "Individual Income Taxes")
nowcast_excise_receipts = nowcast_budget_receipt(receipts,"Total -- Excise Taxes","revenue", "Excise Taxes")
nowcast_estate_receipts = nowcast_budget_receipt(receipts,"Estate and Gift Taxes","revenue", "Estate and Gift Taxes")
nowcast_customs_receipts = nowcast_budget_receipt(receipts,"Customs Duties","revenue", "Customs Duties")


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

actual_receipt = bind_cols(
  nowcast_misc_receipts[[3]] %>% select(date,actual) %>% rename(misc=actual),
  nowcast_corporate_receipts[[3]] %>% select(actual) %>% rename(corp=actual),
  nowcast_payroll_receipts[[3]] %>% select(actual) %>% rename(payroll=actual),
  nowcast_individual_receipts[[3]] %>% select(actual) %>% rename(individ=actual),
  nowcast_excise_receipts[[3]] %>% select(actual) %>% rename(excise=actual),
  nowcast_estate_receipts[[3]] %>% select(actual) %>% rename(estate=actual),
  nowcast_customs_receipts[[3]] %>% select(actual) %>% rename(customs=actual),
)

nowcast_deficit = bind_cols(
  nowcast_receipt %>% group_by(date) %>% summarize(receipts=misc+corp+payroll+individ+excise+estate+customs),
  nowcast_outlay %>% group_by(date) %>% summarize(outlays=medicare+medicaid+ss+defense+interest+other) %>% select(-date)
) %>% 
  mutate(deficit=receipts-outlays) %>% 
  left_join(receipts_fred %>% select(date,value) %>% rename(actual_receipts=value)) %>% 
  left_join(outlays_fred %>% select(date,value) %>% rename(actual_outlays=value)) %>% 
  mutate(actual_deficit=actual_receipts-actual_outlays)




