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

set.seed(178)

#### headroom calculation ####

# debt limit
limit = debt_subject_to_limit %>% 
  filter(record_date<=end_date) %>% 
  select(record_date,debt_catg,debt_catg_desc,close_today_bal) %>% 
  distinct(record_date,debt_catg,debt_catg_desc,.keep_all = TRUE) %>% 
  pivot_wider(names_from=c(debt_catg,debt_catg_desc),values_from=close_today_bal) %>% 
  rowwise() %>% 
  summarize(record_date=record_date,
            total_debt_level=sum(c(`Debt Held by the Public_null`,
                                   `Intragovernmental Holdings_null`,
                                   -1*`Less Debt Not Subject to Limit_Other Debt`,
                                   -1*`Less Debt Not Subject to Limit_Unamortized Discount`,
                                   -1*`Less Debt Not Subject to Limit_Federal Financing Bank`,
                                   `Plus Other Debt Subject to Limit_Guaranteed Debt of Government Agencies`,
                                   -1*`Less Debt Not Subject to Limit_Repurchase Agreements`,
                                   -1*`Less Debt Not Subject to Limit_Hope Bonds`,
                                   -1*`Debt Not Subject to Limit_Other Debt (-)`,
                                   -1*`Debt Not Subject to Limit_Unamortized Discount (-)`,
                                   -1*`Debt Not Subject to Limit_Federal Financing Bank (-)`,
                                   `Other Debt Subject to Limit_Guaranteed Debt of Government Agencies`
                                   ),na.rm=TRUE)/1000,
            debt_limit=`Statutory Debt Limit_null`/1000) %>% 
  ungroup() %>% 
  mutate(debt_limit=case_when(record_date>="2023-06-03"&record_date<="2024-12-31"~Inf,
                              record_date>="2013-02-04"&record_date<="2013-05-17"~Inf,
                              record_date>="2013-10-17"&record_date<="2014-02-07"~Inf,
                              record_date>="2014-02-18"&record_date<="2015-03-13"~Inf,
                              record_date>="2015-11-02"&record_date<="2017-03-15"~Inf,
                              record_date>="2017-09-08"&record_date<="2017-12-11"~Inf,
                              record_date>="2018-02-09"&record_date<="2019-03-01"~Inf,
                              record_date>="2019-08-02"&record_date<="2021-07-30"~Inf,
                              TRUE~debt_limit),
         imputed=case_when(record_date>="2023-06-03"&record_date<="2024-12-31"~1,
                              record_date>="2013-02-04"&record_date<="2013-05-17"~1,
                              record_date>="2013-10-17"&record_date<="2014-02-07"~1,
                              record_date>="2014-02-18"&record_date<="2015-03-13"~1,
                              record_date>="2015-11-02"&record_date<="2017-03-15"~1,
                              record_date>="2017-09-08"&record_date<="2017-12-11"~1,
                              record_date>="2018-02-09"&record_date<="2019-03-01"~1,
                              record_date>="2019-08-02"&record_date<="2021-07-30"~1,
                           TRUE~0)
         ) %>%
  arrange(record_date)

# FIRST PREDICT WHEN DAY OF DEBT ISSUANCE WILL BEGIN, WHICH IS WHEN DEBT LIMIT IS NOMINALLY HIT
limit = limit %>% 
  left_join(op_cash_balance %>% 
              filter(account_type=="Treasury General Account (TGA) Closing Balance") %>% 
              select(record_date,open_today_bal) %>% 
              mutate(open_today_bal=open_today_bal/1000)) %>% 
  full_join(daily_forecast %>% 
              mutate_at(vars(final_pred_day:final_pred_day_upper),~.*-1) %>% 
              filter(record_date>max(limit$record_date)),
            by="record_date") %>% 
  arrange(record_date) %>% 
  fill(debt_limit,.direction="down") %>% 
  mutate(open_today_bal_lwr=open_today_bal,
         open_today_bal_upper=open_today_bal,
         total_debt_level_lwr=total_debt_level,
         total_debt_level_upper=total_debt_level)

if(is.na(limit$open_today_bal[limit$record_date=="2026-04-30"])&end_date>"2025-07-01"){
  
  limit = limit %>% 
    mutate(tga_adjustment=ifelse(record_date=="2026-04-30",1025,open_today_bal))
  
  limit$tga_adjustment[limit$record_date>=max(limit$record_date[!is.na(limit$total_debt_level)])&limit$record_date<="2026-04-30"] = na.approx(limit$tga_adjustment[limit$record_date>=max(limit$record_date[!is.na(limit$total_debt_level)])&limit$record_date<="2026-04-30"]) 
  
  # tga adjustment is to factor in that treasury will be issuing extra debt to get TGA buffer back up
  
  limit = limit %>% 
    mutate(tga_adjustment=c(NA,diff(tga_adjustment)),
           tga_adjustment=ifelse(record_date<=max(limit$record_date[!is.na(limit$total_debt_level)])|record_date>"2026-04-30",
                                 0,
                                 tga_adjustment))
  
  for(i in which(is.na(limit$total_debt_level))){
    
    limit$total_debt_level[i] = limit$total_debt_level[i-1]+limit$final_pred_day[i]+limit$tga_adjustment[i]
    limit$open_today_bal[i] = limit$open_today_bal[i-1]+limit$tga_adjustment[i]
    
    limit$total_debt_level_lwr[i] = limit$total_debt_level_lwr[i-1]+limit$final_pred_day_lwr[i]+limit$tga_adjustment[i]
    limit$open_today_bal_lwr[i] = limit$open_today_bal_lwr[i-1]+limit$tga_adjustment[i]
    
    limit$total_debt_level_upper[i] = limit$total_debt_level_upper[i-1]+limit$final_pred_day_upper[i]+limit$tga_adjustment[i]
    limit$open_today_bal_upper[i] = limit$open_today_bal_upper[i-1]+limit$tga_adjustment[i]
    
  }
  
} else{
  
  for(i in which(is.na(limit$total_debt_level))){
    
    limit$total_debt_level[i] = limit$total_debt_level[i-1]+limit$final_pred_day[i]
    limit$open_today_bal[i] = limit$open_today_bal[i-1]
    
    limit$total_debt_level_lwr[i] = limit$total_debt_level_lwr[i-1]+limit$final_pred_day_lwr[i]
    limit$open_today_bal_lwr[i] = limit$open_today_bal_lwr[i-1]
    
    limit$total_debt_level_upper[i] = limit$total_debt_level_upper[i-1]+limit$final_pred_day_upper[i]
    limit$open_today_bal_upper[i] = limit$open_today_bal_upper[i-1]
    
  }
  
}

FISCAL_SPACE = limit %>% 
  filter(record_date>=headroom_date) %>% 
  mutate(headroom=debt_limit-total_debt_level+open_today_bal,
         headroom_lwr=debt_limit-total_debt_level_lwr+open_today_bal_lwr,
         headroom_upper=debt_limit-total_debt_level_upper+open_today_bal_upper)

# Get dates where extraordinary measures would be triggered
exmeasures_date = ifelse(is.na(announcement_date),as.character(head(limit$record_date[limit$total_debt_level>=limit$debt_limit&limit$record_date>=headroom_date],1)-1),announcement_date)
exmeasures_lwr_date = ifelse(is.na(announcement_date),as.character(head(limit$record_date[limit$total_debt_level_lwr>=limit$debt_limit&limit$record_date>=headroom_date],1)-1),announcement_date)
exmeasures_upper_date = ifelse(is.na(announcement_date),as.character(head(limit$record_date[limit$total_debt_level_upper>=limit$debt_limit&limit$record_date>=headroom_date],1)-1),announcement_date)
 
# 1. Debt Issuance Suspension Period

# 1.a. Civil Service Retirement Fund and PSRF Early Redemptions
# You can redeem debt equal to the amount you will need to pay out during the period
# Jan 17, suspend from Jan 21 to Mar 14
# So it should be modelled as an increase in headroom at the beginning of the period that slowly declines as it converges back to its normal level
# Assume every two months, it is renewed

# suspending new investments, so predict monthly investments needed for CSRF
# get estimated monthly investment level from debt limit letters over time
interest_payments = investment_funds %>% 
  filter(account_nm=="CIVIL SERVICE RETIRE"&record_date<=end_date) %>% 
  mutate(security_desc=gsub("SPECIAL ISSUE BOND |CERTIFICATE OF INDEBTEDNESS ","",security_desc)) %>% 
  separate(security_desc,into=c("rate","maturity_date"),sep=" ") %>% 
  mutate(rate=as.numeric(gsub("%","",rate))/100/2,
         maturity_date=as.Date(maturity_date,format="%m/%d/%Y"),
         date=floor_date(record_date,"month"))

june_payments = interest_payments %>% 
  filter(month(date)==5) %>% 
  group_by(date) %>% 
  mutate(interest=rate*shares_per_par,
         maturity_year=year(maturity_date),
         record_calendar_year=case_when(
           month(date)<5~record_calendar_year,
           month(date)>5~record_calendar_year+1,
           TRUE~record_calendar_year
         )) %>% 
  summarize(interest_amt=sum(c(interest,shares_per_par[maturity_year==record_calendar_year],na.rm=TRUE))/1000000000) %>% 
  ungroup() %>% 
  mutate(date=date %m+% months(1))

dec_payments = interest_payments %>% 
  filter(month(date)==11) %>% 
  group_by(date) %>% 
  mutate(interest=rate*shares_per_par,
         maturity_year=year(maturity_date)) %>% 
  summarize(interest_amt=sum(c(interest,na.rm=TRUE))/1000000000) %>% 
  ungroup() %>% 
  mutate(date=date %m+% months(1))

next_date = case_when(
  month(max(interest_payments$date))<6~as.Date(paste0(year(max(interest_payments$date)),"-06-01")),
  month(max(interest_payments$date))<12~as.Date(paste0(year(max(interest_payments$date)),"-12-01")),
  month(max(interest_payments$date))==12~as.Date(paste0(year(max(interest_payments$date))+1,"-06-01"))
)

future_payments = data.frame()
for(dat in as.character(seq.Date(next_date,max(limit$record_date),by="6 months"))){
  
  if(month(dat)==6){
    tmp = interest_payments %>% 
      filter(month(date)==5) %>% 
      filter(date==max(date)) %>% 
      mutate(maturity=ceiling(as.numeric(maturity_date-record_date)/365))
  } else{
    tmp = interest_payments %>% 
      filter(month(date)==11) %>% 
      filter(date==max(date)) %>% 
      mutate(maturity=ceiling(as.numeric(maturity_date-record_date)/365))
  }
  
  int_rates = cbo_econ %>% filter(year(date)==year(dat)|year(date)==year(tmp$record_date[1])) %>% 
    select(date,treasury_note_rate_10yr:fed_funds_rate) %>% 
    mutate(year=year(date)) %>% 
    select(-date) %>% 
    group_by(year) %>% 
    summarize_all(mean,na.rm=TRUE) %>% 
    ungroup() %>% 
    select(-year) %>% 
    summarize_all(~.[2]-.[1])
  
  inflation = cbo_econ %>% 
    filter(year(date)==year(dat)|year(date)==year(tmp$record_date[1])) %>% 
    select(date,cpiu) %>% 
    mutate(year=year(date)) %>% 
    select(-date) %>% 
    group_by(year) %>% 
    summarize_all(mean,na.rm=TRUE) %>% 
    ungroup() %>% 
    select(-year) %>% 
    summarize_all(~.[2]/.[1])
  
  tmp = bind_cols(tmp,int_rates,inflation) %>% 
    mutate(ch_rate=case_when(
      maturity<=5~treasury_bill_rate_3mo,
      TRUE~treasury_note_rate_10yr
    ),
    shares_per_par=shares_per_par*cpiu,
    rate=case_when(
      maturity<=ceiling(as.numeric(as.Date(dat)-record_date)/365)~rate+(ch_rate/2/100),
      TRUE~rate
    ))
  
  if(month(dat)==6){
    tmp = tmp %>% 
      group_by(date) %>% 
      mutate(interest=rate*shares_per_par) %>% 
      summarize(interest_amt=sum(c(interest,shares_per_par[maturity==1],na.rm=TRUE))/1000000000) %>% 
      ungroup() %>% 
      mutate(date=dat)
    
  }else{
    
    tmp = tmp %>% 
      group_by(date) %>% 
      mutate(interest=rate*shares_per_par) %>% 
      summarize(interest_amt=sum(c(interest,na.rm=TRUE))/1000000000) %>% 
      ungroup() %>% 
      mutate(date=dat)
  }
  
  future_payments = bind_rows(future_payments,tmp)
  
}
future_payments$date = as.Date(future_payments$date)

csrdf_invest = data.frame(
  year=c(2003,2011,2012,2013,2014,2015,2017,2018,2019,2021,2023,2025),
  amt=c(1.25,2,2,2,2,2.5,2.9,3,3,4,4,5),
  sept_amt=c(NA,NA,NA,NA,NA,32.57,NA,38.66,39.237437,NA,46.540828,48.289972)
) %>% 
  complete(year=seq(min(year),max(year))) %>% 
  filter(year<=year(end_date))

csrdf_invest = data.frame(date=seq.Date(as.Date("2003-10-01"),floor_date(max(limit$record_date),"month"),by="1 month")) %>% 
  mutate(date=as.Date(date),
         year=year(date)) %>% 
  left_join(csrdf_invest) %>% 
  left_join(bind_rows(june_payments,dec_payments,future_payments)) %>% 
  mutate_at(vars(amt),~ifelse(date>=floor_date(as.Date(end_date),"month"),NA_real_,.)) %>% # in case we are calculating this for historical time period
  left_join(cbo_econ %>% 
              filter(year(date)>=max(csrdf_invest$year)) %>% 
              select(date,cpiu) %>% 
              mutate(year=year(date)) %>% 
              select(-date) %>% 
              group_by(year) %>% 
              summarize_all(mean,na.rm=TRUE) %>% 
              ungroup() %>% 
              mutate_at(vars(cpiu),~./.[1])) %>% 
  fill(amt,sept_amt,.direction="downup") %>% 
  group_by(year) %>% 
  mutate(sept_amt=case_when(month(date)==9~sept_amt,
                            TRUE~NA),
         amt=case_when(year(date)>max(max(csrdf_invest$year))~amt*cpiu,
                       TRUE~amt),
         sept_amt=case_when(year(date)>max(max(csrdf_invest$year))~sept_amt*cpiu,
                       TRUE~sept_amt)) %>% 
  group_by(date) %>% 
  summarize(year=year[1],
            amt=mean(amt,na.rm=TRUE),
            sept_amt=sum(sept_amt,na.rm=TRUE),
            interest_amt=sum(interest_amt,na.rm=TRUE),
            cpiu=mean(cpiu,na.rm=TRUE)) %>% 
  ungroup()
# extrapolate backwards and adjust for inflation forwards for monthly intake
# extrap backwards and hold constant forwards for sept amt
# extrap backwards for investment securities and hold distribution and rate fixed (with adjustment for change in interest rates)


interest_payments = investment_funds %>% 
  filter(account_nm=="POSTAL SERVICE RETIREE"&record_date<=end_date) %>% 
  mutate(security_desc=gsub("SPECIAL ISSUE BOND |CERTIFICATE OF INDEBTEDNESS ","",security_desc)) %>% 
  separate(security_desc,into=c("rate","maturity_date"),sep=" ") %>% 
  mutate(rate=as.numeric(gsub("%","",rate))/100/2,
         maturity_date=as.Date(maturity_date,format="%m/%d/%Y"),
         date=floor_date(record_date,"month"))

june_payments = interest_payments %>% 
  filter(month(date)==5) %>% 
  group_by(date) %>% 
  mutate(interest=rate*shares_per_par,
         maturity_year=year(maturity_date),
         record_calendar_year=case_when(
           month(date)<5~record_calendar_year,
           month(date)>5~record_calendar_year+1,
           TRUE~record_calendar_year
         )) %>% 
  summarize(interest_amt=sum(c(interest,shares_per_par[maturity_year==record_calendar_year],na.rm=TRUE))/1000000000) %>% 
  ungroup() %>% 
  mutate(date=date %m+% months(1))

dec_payments = interest_payments %>% 
  filter(month(date)==11) %>% 
  group_by(date) %>% 
  mutate(interest=rate*shares_per_par,
         maturity_year=year(maturity_date)) %>% 
  summarize(interest_amt=sum(c(interest,na.rm=TRUE))/1000000000) %>% 
  ungroup() %>% 
  mutate(date=date %m+% months(1))

next_date = case_when(
  month(max(interest_payments$date))<6~as.Date(paste0(year(max(interest_payments$date)),"-06-01")),
  month(max(interest_payments$date))<12~as.Date(paste0(year(max(interest_payments$date)),"-12-01")),
  month(max(interest_payments$date))==12~as.Date(paste0(year(max(interest_payments$date))+1,"-06-01"))
)

future_payments = data.frame()
for(dat in as.character(seq.Date(next_date,max(limit$record_date),by="6 months"))){
  
  if(month(dat)==6){
    tmp = interest_payments %>% 
      filter(month(date)==5) %>% 
      filter(date==max(date)) %>% 
      mutate(maturity=ceiling(as.numeric(maturity_date-record_date)/365))
  } else{
    tmp = interest_payments %>% 
      filter(month(date)==11) %>% 
      filter(date==max(date)) %>% 
      mutate(maturity=ceiling(as.numeric(maturity_date-record_date)/365))
  }
  
  int_rates = cbo_econ %>% filter(year(date)==year(dat)|year(date)==year(tmp$record_date[1])) %>% 
    select(date,treasury_note_rate_10yr:fed_funds_rate) %>% 
    mutate(year=year(date)) %>% 
    select(-date) %>% 
    group_by(year) %>% 
    summarize_all(mean,na.rm=TRUE) %>% 
    ungroup() %>% 
    select(-year) %>% 
    summarize_all(~.[2]-.[1])
  
  inflation = cbo_econ %>% 
    filter(year(date)==year(dat)|year(date)==year(tmp$record_date[1])) %>% 
    select(date,cpiu) %>% 
    mutate(year=year(date)) %>% 
    select(-date) %>% 
    group_by(year) %>% 
    summarize_all(mean,na.rm=TRUE) %>% 
    ungroup() %>% 
    select(-year) %>% 
    summarize_all(~.[2]/.[1])
  
  tmp = bind_cols(tmp,int_rates,inflation) %>% 
    mutate(ch_rate=case_when(
      maturity<=5~treasury_bill_rate_3mo,
      TRUE~treasury_note_rate_10yr
    ),
    shares_per_par=shares_per_par*cpiu,
    rate=case_when(
      maturity<=ceiling(as.numeric(as.Date(dat)-record_date)/365)~rate+(ch_rate/2/100),
      TRUE~rate
    ))
  
  if(month(dat)==6){
    tmp = tmp %>% 
      group_by(date) %>% 
      mutate(interest=rate*shares_per_par) %>% 
      summarize(interest_amt=sum(c(interest,shares_per_par[maturity==1],na.rm=TRUE))/1000000000) %>% 
      ungroup() %>% 
      mutate(date=dat)
    
  }else{
    
    tmp = tmp %>% 
      group_by(date) %>% 
      mutate(interest=rate*shares_per_par) %>% 
      summarize(interest_amt=sum(c(interest,na.rm=TRUE))/1000000000) %>% 
      ungroup() %>% 
      mutate(date=dat)
  }
  
  future_payments = bind_rows(future_payments,tmp)
  
}
future_payments$date = as.Date(future_payments$date)

psrhbf_invest = data.frame(
  year=c(2003,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024,2025),
  amt=0, # only amounts are given for PRF for one-time payments, otherwise seems to be wrapped up in csrdf
  sept_amt=0 
) %>% 
  complete(year=seq(min(year),max(year))) %>% 
  filter(year<=year(end_date))

psrhbf_invest = data.frame(date=seq.Date(as.Date("2003-10-01"),floor_date(max(limit$record_date),"month"),by="1 month")) %>% 
  mutate(date=as.Date(date),
         year=year(date)) %>% 
  left_join(psrhbf_invest) %>% 
  left_join(bind_rows(june_payments,dec_payments,future_payments)) %>% 
  mutate_at(vars(amt),~ifelse(date>=floor_date(as.Date(end_date),"month"),NA_real_,.)) %>% # in case we are calculating this for historical time period
  left_join(cbo_econ %>% 
              filter(year(date)>=max(psrhbf_invest$year)) %>% 
              select(date,cpiu) %>% 
              mutate(year=year(date)) %>% 
              select(-date) %>% 
              group_by(year) %>% 
              summarize_all(mean,na.rm=TRUE) %>% 
              ungroup() %>% 
              mutate_at(vars(cpiu),~./.[1])) %>% 
  fill(amt,sept_amt,.direction="downup") %>% 
  group_by(year) %>% 
  mutate(sept_amt=case_when(month(date)==9~sept_amt,
                            TRUE~NA),
         amt=case_when(year(date)>max(max(psrhbf_invest$year))~amt*cpiu,
                       TRUE~amt),
         sept_amt=case_when(year(date)>max(max(psrhbf_invest$year))~sept_amt*cpiu,
                            TRUE~sept_amt)) %>% 
  group_by(date) %>% 
  summarize(year=year[1],
            amt=mean(amt,na.rm=TRUE),
            sept_amt=sum(sept_amt,na.rm=TRUE),
            interest_amt=sum(interest_amt,na.rm=TRUE),
            cpiu=mean(cpiu,na.rm=TRUE)) %>% 
  ungroup()
# extrapolate backwards and adjust for inflation forwards for monthly intake
# extrap backwards and hold constant forwards for sept amt
# extrap backwards for investment securities and hold distribution and rate fixed (with adjustment for change in interest rates)

# finally get the estimated payments made each month for early redemptions
csrdf_pay = data.frame(
  year=c(2003,2011,2012,2013,2014,2015,2017,2019,2021,2023,2025),
  csrdf=c(4,6,6,6.4,6.4,6.75,7,7,7,8,8.5),
  psrhbf=c(0,0,0,0,0,0,0.3,0.3,0.3,0.3,0.3)
) %>% 
  complete(year=seq(min(year),max(year))) %>% 
  filter(year<=year(end_date))

csrdf_pay = data.frame(date=seq.Date(as.Date("2003-10-01"),floor_date(max(limit$record_date),"month"),by="1 month")) %>% 
  mutate(date=as.Date(date),
         year=year(date)) %>% 
  left_join(csrdf_pay) %>% 
  mutate_at(vars(csrdf,psrhbf),~ifelse(date>=floor_date(as.Date(end_date),"month"),NA_real_,.)) %>% # in case we are calculating this for historical time period
  left_join(cbo_econ %>% 
              filter(year(date)>=max(csrdf_pay$year)) %>% 
              select(date,cpiu) %>% 
              mutate(year=year(date)) %>% 
              select(-date) %>% 
              group_by(year) %>% 
              summarize_all(mean,na.rm=TRUE) %>% 
              ungroup() %>% 
              mutate_at(vars(cpiu),~./.[1])) %>% 
  fill(csrdf,psrhbf,.direction="downup") %>% 
  group_by(year) %>% 
  mutate_at(vars(csrdf,psrhbf),~case_when(year(date)>max(max(csrdf_pay$year))~.*cpiu,
                       TRUE~.))

# TODO: add comments

# Every three months you can redeem some early
dates = seq.Date(ceiling_date(as.Date(exmeasures_date),"month"),ceiling_date(max(limit$record_date),"month") %m+% months(3),by="3 months")
FISCAL_SPACE$csrdf_psrhdf_early_redeem=NA
for(i in 1:(length(dates)-1)){
  
  FISCAL_SPACE$headroom[FISCAL_SPACE$record_date>=dates[i]&FISCAL_SPACE$record_date<dates[i+1]] = FISCAL_SPACE$headroom[FISCAL_SPACE$record_date>=dates[i]&FISCAL_SPACE$record_date<dates[i+1]]+
    seq(sum(unlist(csrdf_pay[csrdf_pay$date>=dates[i]&csrdf_pay$date<dates[i+1],c("csrdf","psrhbf")]),na.rm=TRUE),0,length=length(FISCAL_SPACE$headroom[FISCAL_SPACE$record_date>=dates[i]&FISCAL_SPACE$record_date<dates[i+1]]))
  
  FISCAL_SPACE$csrdf_psrhdf_early_redeem[FISCAL_SPACE$record_date>=dates[i]&FISCAL_SPACE$record_date<dates[i+1]] = seq(sum(unlist(csrdf_pay[csrdf_pay$date>=dates[i]&csrdf_pay$date<dates[i+1],c("csrdf","psrhbf")]),na.rm=TRUE),0,length=length(FISCAL_SPACE$headroom[FISCAL_SPACE$record_date>=dates[i]&FISCAL_SPACE$record_date<dates[i+1]]))
  
}

dates = seq.Date(ceiling_date(as.Date(exmeasures_lwr_date),"month"),ceiling_date(max(limit$record_date),"month") %m+% months(3),by="3 months")
FISCAL_SPACE$csrdf_psrhdf_early_redeem_lwr=NA
for(i in 1:(length(dates)-1)){
  
  FISCAL_SPACE$headroom_lwr[FISCAL_SPACE$record_date>=dates[i]&FISCAL_SPACE$record_date<dates[i+1]] = FISCAL_SPACE$headroom_lwr[FISCAL_SPACE$record_date>=dates[i]&FISCAL_SPACE$record_date<dates[i+1]]+
    seq(sum(unlist(csrdf_pay[csrdf_pay$date>=dates[i]&csrdf_pay$date<dates[i+1],c("csrdf","psrhbf")]),na.rm=TRUE),0,length=length(FISCAL_SPACE$headroom_lwr[FISCAL_SPACE$record_date>=dates[i]&FISCAL_SPACE$record_date<dates[i+1]]))
  
  FISCAL_SPACE$csrdf_psrhdf_early_redeem_lwr[FISCAL_SPACE$record_date>=dates[i]&FISCAL_SPACE$record_date<dates[i+1]] = seq(sum(unlist(csrdf_pay[csrdf_pay$date>=dates[i]&csrdf_pay$date<dates[i+1],c("csrdf","psrhbf")]),na.rm=TRUE),0,length=length(FISCAL_SPACE$headroom_lwr[FISCAL_SPACE$record_date>=dates[i]&FISCAL_SPACE$record_date<dates[i+1]]))
  
}

dates = seq.Date(ceiling_date(as.Date(exmeasures_upper_date),"month"),ceiling_date(max(limit$record_date),"month") %m+% months(3),by="3 months")
FISCAL_SPACE$csrdf_psrhdf_early_redeem_upper=NA
for(i in 1:(length(dates)-1)){
  
  FISCAL_SPACE$headroom_upper[FISCAL_SPACE$record_date>=dates[i]&FISCAL_SPACE$record_date<dates[i+1]] = FISCAL_SPACE$headroom_upper[FISCAL_SPACE$record_date>=dates[i]&FISCAL_SPACE$record_date<dates[i+1]]+
    seq(sum(unlist(csrdf_pay[csrdf_pay$date>=dates[i]&csrdf_pay$date<dates[i+1],c("csrdf","psrhbf")]),na.rm=TRUE),0,length=length(FISCAL_SPACE$headroom_upper[FISCAL_SPACE$record_date>=dates[i]&FISCAL_SPACE$record_date<dates[i+1]]))
  
  FISCAL_SPACE$csrdf_psrhdf_early_redeem_upper[FISCAL_SPACE$record_date>=dates[i]&FISCAL_SPACE$record_date<dates[i+1]] = seq(sum(unlist(csrdf_pay[csrdf_pay$date>=dates[i]&csrdf_pay$date<dates[i+1],c("csrdf","psrhbf")]),na.rm=TRUE),0,length=length(FISCAL_SPACE$headroom_upper[FISCAL_SPACE$record_date>=dates[i]&FISCAL_SPACE$record_date<dates[i+1]]))
  
}

FISCAL_SPACE$headroom[FISCAL_SPACE$record_date>="2025-01-17"&FISCAL_SPACE$record_date<"2025-03-14"] = FISCAL_SPACE$headroom[FISCAL_SPACE$record_date>="2025-01-17"&FISCAL_SPACE$record_date<"2025-03-14"] + 
  seq((2*8.5+2*.3),0,length=length(FISCAL_SPACE$headroom[FISCAL_SPACE$record_date>="2025-01-17"&FISCAL_SPACE$record_date<"2025-03-14"]))


# 1.b. CRSF and PSRF suspended investments and interest

FISCAL_SPACE = FISCAL_SPACE %>% 
  mutate(date=floor_date(record_date,"month")) %>% 
  left_join(csrdf_invest %>% 
              filter(date>=floor_date(as.Date(exmeasures_date),"month")) %>% 
              rowwise() %>% 
              mutate(interest_amt=sum(c(sept_amt,interest_amt),na.rm=TRUE)) %>% 
              ungroup() %>% 
              select(date,csrdf_income_amt=amt,csrdf_interest_amt=interest_amt)) %>% 
  left_join(psrhbf_invest %>% 
              filter(date>=floor_date(as.Date(exmeasures_date),"month")) %>% 
              rowwise() %>% 
              mutate(interest_amt=sum(c(sept_amt,interest_amt),na.rm=TRUE)) %>% 
              ungroup() %>% 
              select(date,psrhbf_income_amt=amt,psrhbf_interest_amt=interest_amt)) %>% 
  group_by(date) %>% 
  mutate_at(vars(csrdf_income_amt,psrhbf_income_amt),~./n()) %>% 
  mutate_at(vars(csrdf_interest_amt,psrhbf_interest_amt),~case_when(row_number()==n()&!is.na(.)~.,
                                                                    record_date<exmeasures_date~NA,
                                                                    TRUE~0)) %>% 
  left_join(csrdf_invest %>% 
              filter(date>=floor_date(as.Date(exmeasures_lwr_date),"month")) %>% 
              rowwise() %>% 
              mutate(interest_amt=sum(c(sept_amt,interest_amt),na.rm=TRUE)) %>% 
              ungroup() %>% 
              select(date,csrdf_income_amt_lwr=amt,csrdf_interest_amt_lwr=interest_amt)) %>% 
  left_join(psrhbf_invest %>% 
              filter(date>=floor_date(as.Date(exmeasures_lwr_date),"month")) %>% 
              rowwise() %>% 
              mutate(interest_amt=sum(c(sept_amt,interest_amt),na.rm=TRUE)) %>% 
              ungroup() %>% 
              select(date,psrhbf_income_amt_lwr=amt,psrhbf_interest_amt_lwr=interest_amt)) %>% 
  group_by(date) %>% 
  mutate_at(vars(csrdf_income_amt_lwr,psrhbf_income_amt_lwr),~./n()) %>% 
  mutate_at(vars(csrdf_interest_amt_lwr,psrhbf_interest_amt_lwr),~case_when(row_number()==n()&!is.na(.)~.,
                                                                    record_date<exmeasures_lwr_date~NA,
                                                                    TRUE~0)) %>% 
  left_join(csrdf_invest %>% 
              filter(date>=floor_date(as.Date(exmeasures_upper_date),"month")) %>% 
              rowwise() %>% 
              mutate(interest_amt=sum(c(sept_amt,interest_amt),na.rm=TRUE)) %>% 
              ungroup() %>% 
              select(date,csrdf_income_amt_upper=amt,csrdf_interest_amt_upper=interest_amt)) %>% 
  left_join(psrhbf_invest %>% 
              filter(date>=floor_date(as.Date(exmeasures_upper_date),"month")) %>% 
              rowwise() %>% 
              mutate(interest_amt=sum(c(sept_amt,interest_amt),na.rm=TRUE)) %>% 
              ungroup() %>% 
              select(date,psrhbf_income_amt_upper=amt,psrhbf_interest_amt_upper=interest_amt)) %>% 
  group_by(date) %>% 
  mutate_at(vars(csrdf_income_amt_upper,psrhbf_income_amt_upper),~./n()) %>% 
  mutate_at(vars(csrdf_interest_amt_upper,psrhbf_interest_amt_upper),~case_when(row_number()==n()&!is.na(.)~.,
                                                                    record_date<exmeasures_upper_date~NA,
                                                                    TRUE~0)) %>% 
  ungroup() %>% 
  mutate_at(vars(csrdf_income_amt:psrhbf_interest_amt_upper),~cumsum(coalesce(., 0))) %>% 
  rowwise() %>% 
  mutate(headroom=sum(c(headroom,csrdf_income_amt,psrhbf_income_amt,csrdf_interest_amt,psrhbf_interest_amt),na.rm=TRUE),
         headroom_lwr=sum(c(headroom_lwr,csrdf_income_amt_lwr,psrhbf_income_amt_lwr,csrdf_interest_amt_lwr,psrhbf_interest_amt_lwr),na.rm=TRUE),
         headroom_upper=sum(c(headroom_upper,csrdf_income_amt_upper,psrhbf_income_amt_upper,csrdf_interest_amt_upper,psrhbf_interest_amt_upper),na.rm=TRUE)) %>% 
  ungroup()
  

# 2. Suspending Govt Securities Investment Fund (G Fund)
# They dont suspend all of it at once, so keep remaining on the books
# But record all of it as headroom
gfund_amt = funds %>% 
  filter(account_name=="Thrift Savings Fund"&line_item_nm=="Totals") %>% 
  separate(date_range,into=c("date1","date2")," to ") %>% 
  select(date=date1,gfund_amt=ending_balance) %>% 
  mutate(gfund_amt=gfund_amt/1000000000,
         date=as.Date(date,format="%b %d, %Y"),
         date=case_when(day(date)!=1~ceiling_date(date,"month"),
                        TRUE~date)) %>% 
  arrange(date) %>% 
  filter(date<floor_date(as.Date(end_date),"month")) %>% 
  complete(date=seq.Date(floor_date(min(limit$record_date),"month"),floor_date(max(limit$record_date),"month"),by="1 month")) %>% 
  mutate(year=year(date)) %>% 
  left_join(cbo_econ %>% 
    select(date,cpiu) %>% 
    mutate(year=year(date)) %>% 
    select(-date) %>% 
    group_by(year) %>% 
    summarize_all(mean,na.rm=TRUE) %>% 
    ungroup()) %>% 
  mutate(scale=case_when(
    is.na(gfund_amt)&year<=2011~cpiu/mean(cpiu[year==2011]),
    is.na(gfund_amt)&year>2011~cpiu/tail(cpiu[!is.na(gfund_amt)],1),
    TRUE~1
  )) %>% 
  fill(gfund_amt,.direction="updown") %>% 
  mutate(gfund_amt=gfund_amt*scale,
         gfund_amt_lwr=gfund_amt,
         gfund_amt_upper=gfund_amt,
         gfund_amt=ifelse(date>=floor_date(as.Date(exmeasures_date),"month"),NA,gfund_amt),
         gfund_amt_lwr=ifelse(date>=floor_date(as.Date(exmeasures_lwr_date),"month"),NA,gfund_amt_lwr),
         gfund_amt_upper=ifelse(date>=floor_date(as.Date(exmeasures_upper_date),"month"),NA,gfund_amt_upper)) %>% 
  fill(gfund_amt,gfund_amt_lwr,gfund_amt_upper,.direction="down") %>% 
  select(-c(year,cpiu,scale))
  

FISCAL_SPACE = FISCAL_SPACE %>% 
  left_join(gfund_amt) %>% 
  mutate(gfund_amt_upper=case_when(record_date<exmeasures_upper_date~NA,TRUE~gfund_amt_upper),
            gfund_amt_lwr=case_when(record_date<exmeasures_lwr_date~NA,TRUE~gfund_amt_lwr),
            gfund_amt=case_when(record_date<exmeasures_date~NA,TRUE~gfund_amt)) %>% 
  mutate_at(vars(gfund_amt:gfund_amt_upper),~coalesce(.,0)) %>% 
  ungroup() %>% 
  mutate(headroom=headroom+gfund_amt,
         headroom_lwr=headroom_lwr+gfund_amt_lwr,
         headroom_upper=headroom_upper+gfund_amt_upper)

# 3. Suspending investment of Exhcnage Stabilization Fund (ESF)
esf_amt = funds %>% 
  filter(account_name=="Exchange Stabilization"&line_item_nm=="Totals") %>% 
  separate(date_range,into=c("date1","date2")," to ") %>% 
  select(date=date1,esf_amt=ending_balance) %>% 
  mutate(esf_amt=esf_amt/1000000000,
         date=as.Date(date,format="%b %d, %Y"),
         date=case_when(day(date)!=1~ceiling_date(date,"month"),
                        TRUE~date)) %>% 
  arrange(date) %>% 
  filter(date<floor_date(as.Date(end_date),"month")) %>% 
  complete(date=seq.Date(floor_date(min(limit$record_date),"month"),floor_date(max(limit$record_date),"month"),by="1 month")) %>% 
  mutate(year=year(date)) %>% 
  left_join(cbo_econ %>% 
              select(date,cpiu) %>% 
              mutate(year=year(date)) %>% 
              select(-date) %>% 
              group_by(year) %>% 
              summarize_all(mean,na.rm=TRUE) %>% 
              ungroup()) %>% 
  mutate(scale=case_when(
    is.na(esf_amt)&year<=2011~cpiu/mean(cpiu[year==2011]),
    is.na(esf_amt)&year>2011~cpiu/tail(cpiu[!is.na(esf_amt)],1),
    TRUE~1
  )) %>% 
  fill(esf_amt,.direction="updown") %>% 
  mutate(esf_amt=esf_amt*scale,
         esf_amt_lwr=esf_amt,
         esf_amt_upper=esf_amt,
         esf_amt=ifelse(date>=floor_date(as.Date(exmeasures_date),"month"),NA,esf_amt),
         esf_amt_lwr=ifelse(date>=floor_date(as.Date(exmeasures_lwr_date),"month"),NA,esf_amt_lwr),
         esf_amt_upper=ifelse(date>=floor_date(as.Date(exmeasures_upper_date),"month"),NA,esf_amt_upper)) %>% 
  fill(esf_amt,esf_amt_lwr,esf_amt_upper,.direction="down") %>% 
  select(-c(year,cpiu,scale))


FISCAL_SPACE = FISCAL_SPACE %>% 
  left_join(esf_amt) %>% 
  mutate(esf_amt_upper=case_when(record_date<exmeasures_upper_date~NA,TRUE~esf_amt_upper),
            esf_amt_lwr=case_when(record_date<exmeasures_lwr_date~NA,TRUE~esf_amt_lwr),
            esf_amt=case_when(record_date<exmeasures_date~NA,TRUE~esf_amt)) %>% 
  mutate_at(vars(esf_amt:esf_amt_upper),~coalesce(.,0)) %>% 
  ungroup() %>% 
  mutate(headroom=headroom+esf_amt,
         headroom_lwr=headroom_lwr+esf_amt_lwr,
         headroom_upper=headroom_upper+esf_amt_upper)

# 4. federal financing bank transactions
ffb_amt = data.frame(
  year=c(2003,2011,2012,2013,2014,2015,2017,2018,2019,2021,2023,2025),
  ffb_amt=c(0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,1.9,0.3)
) %>% 
  filter(year<=year(end_date)) %>% 
  complete(year=seq(min(year),max(year(FISCAL_SPACE$record_date)))) %>% 
  fill(ffb_amt,.direction="downup")

FISCAL_SPACE = FISCAL_SPACE %>% 
  ungroup() %>% 
  mutate(year=year(record_date)) %>% 
  left_join(ffb_amt) %>% 
  mutate(ffb_amt_upper=case_when(record_date<exmeasures_upper_date~NA,TRUE~ffb_amt),
            ffb_amt_lwr=case_when(record_date<exmeasures_lwr_date~NA,TRUE~ffb_amt),
            ffb_amt=case_when(record_date<exmeasures_date~NA,TRUE~ffb_amt)) %>% 
  mutate_at(vars(ffb_amt:ffb_amt_upper),~coalesce(.,0)) %>% 
  ungroup() %>% 
  mutate(headroom=headroom+ffb_amt,
         headroom_lwr=headroom_lwr+ffb_amt,
         headroom_upper=headroom_upper+ffb_amt)
  

# 5. SLGS suspended issuance
# does not create headroom, just conserves it

my_chart = FISCAL_SPACE %>% 
  rowwise() %>% 
  mutate(running_bal=max(c(0,headroom)),
         running_bal_upper=max(c(0,headroom_upper)),
         running_bal_lower=max(c(0,headroom_lwr))) %>% 
    ungroup()

ggplot(my_chart %>% filter(year(record_date)%in%c(2026:2028)),aes(x=record_date)) + 
  geom_ribbon(aes(ymin=running_bal_lower,ymax=running_bal_upper),alpha=.3) +
  geom_line(aes(y=running_bal)) +
  geom_vline(xintercept=as.Date(exmeasures_date),color="red") +
  theme_bw() +
  labs(x="",y="Fiscal Space Remaining ($B)")

