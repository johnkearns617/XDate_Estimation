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


#### headroom calculation ####

# debt limit
limit = debt_subject_to_limit %>% 
  select(record_date,debt_catg,debt_catg_desc,close_today_bal) %>% 
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
  mutate(debt_limit=ifelse(debt_limit==0,NA,debt_limit)) %>%
  fill(debt_limit,.direction="up") %>% 
  mutate(headroom=debt_limit-total_debt_level) %>% 
  arrange(record_date)
 
# 1. Debt Issuance Suspension Period

FISCAL_SPACE = data.frame(
  date=seq(as.Date("2025-01-01"),as.Date("2025-10-01"),by=1)
) %>% 
  left_join(limit %>% select(record_date,headroom),by=c("date"="record_date")) %>% 
  fill(headroom,.direction="updown")


# 1.a. Civil Service Retirement Fund and PSRF Early Redemptions
# You can redeem debt equal to the amount you will need to pay out during the period
# Jan 17, suspend from Jan 21 to Mar 14
# So it should be modelled as an increase in headroom at the beginning of the period that slowly declines as it converges back to its normal level
# Assume every two months, it is renewed

FISCAL_SPACE$headroom[FISCAL_SPACE$date>="2025-01-17"&FISCAL_SPACE$date<"2025-03-14"] = FISCAL_SPACE$headroom[FISCAL_SPACE$date>="2025-01-17"&FISCAL_SPACE$date<"2025-03-14"] + 
  seq((2*8.5+2*.3),0,length=length(FISCAL_SPACE$headroom[FISCAL_SPACE$date>="2025-01-17"&FISCAL_SPACE$date<"2025-03-14"]))

FISCAL_SPACE$headroom[FISCAL_SPACE$date>="2025-03-14"&FISCAL_SPACE$date<"2025-05-16"] = FISCAL_SPACE$headroom[FISCAL_SPACE$date>="2025-03-14"&FISCAL_SPACE$date<"2025-05-16"] + 
  seq((2*8.5+2*.3),0,length=length(FISCAL_SPACE$headroom[FISCAL_SPACE$date>="2025-03-14"&FISCAL_SPACE$date<"2025-05-16"]))

FISCAL_SPACE$headroom[FISCAL_SPACE$date>="2025-05-16"&FISCAL_SPACE$date<"2025-07-18"] = FISCAL_SPACE$headroom[FISCAL_SPACE$date>="2025-05-16"&FISCAL_SPACE$date<"2025-07-18"] + 
  seq((2*8.5+2*.3),0,length=length(FISCAL_SPACE$headroom[FISCAL_SPACE$date>="2025-05-16"&FISCAL_SPACE$date<"2025-07-18"]))

FISCAL_SPACE$headroom[FISCAL_SPACE$date>="2025-07-18"&FISCAL_SPACE$date<"2025-09-19"] = FISCAL_SPACE$headroom[FISCAL_SPACE$date>="2025-07-18"&FISCAL_SPACE$date<"2025-09-19"] + 
  seq((2*8.5+2*.3),0,length=length(FISCAL_SPACE$headroom[FISCAL_SPACE$date>="2025-07-18"&FISCAL_SPACE$date<"2025-09-19"]))

FISCAL_SPACE$headroom[FISCAL_SPACE$date>="2025-09-19"] = FISCAL_SPACE$headroom[FISCAL_SPACE$date>="2025-09-19"] + (2*8.5+2*.3)

FISCAL_SPACE$headroom[FISCAL_SPACE$date>="2025-09-19"&FISCAL_SPACE$date<"2025-11-21"] = FISCAL_SPACE$headroom[FISCAL_SPACE$date>="2025-09-19"&FISCAL_SPACE$date<"2025-11-21"] + 
  seq((2*8.5+2*.3),0,length=length(FISCAL_SPACE$headroom[FISCAL_SPACE$date>="2025-09-19"&FISCAL_SPACE$date<"2025-11-21"]))


# 1.b. CRSF and PSRF suspended investments and interest

suspended_investments_interest_june = investment_funds %>% 
  filter(account_nm%in%c("CIVIL SERVICE RETIRE","POSTAL SERVICE RETIREE")&record_date==max(record_date)) %>% 
  mutate(int_rate=as.numeric(substr(gsub("SPECIAL ISSUE BOND |CERTIFICATE OF INDEBTEDNESS ",
                                         "",
                                         security_desc),1,4))/100) %>% 
  group_by(account_nm) %>% 
  summarize(
    june_due=sum(shares_per_par[grepl("06\\/30\\/2025",security_desc)])/1000000000,
    int_rate = weighted.mean(int_rate,w=shares_per_par), # assumption moving forward: redemptions does not affect average interest rate
    shares_per_par=sum(shares_per_par)/1000000000
  ) %>% 
  mutate(shares_per_par=case_when(
    account_nm=="CIVIL SERVICE RETIRE"~shares_per_par-(4*8.5), # future redemptions not already done
    TRUE~shares_per_par-(4*.3)
  ),
  int_due=shares_per_par*int_rate/2+june_due) %>% 
  ungroup() %>% 
  summarize(total=sum(int_due)) %>% 
  pull(total)

FISCAL_SPACE$headroom[FISCAL_SPACE$date>="2025-06-30"] = FISCAL_SPACE$headroom[FISCAL_SPACE$date>="2025-06-30"] + suspended_investments_interest_june


# suspend annual payment from general fund at end of fiscal year
if(!any((debt_subject_to_limit$record_calendar_year==2025&debt_subject_to_limit$record_calendar_month==9))){ # once you get the debt limit data, this should already be in effect
  
  FISCAL_SPACE$headroom[FISCAL_SPACE$date>="2025-09-30"] = FISCAL_SPACE$headroom[FISCAL_SPACE$date>="2025-09-30"] + 
    predict(lm_robust(ch~record_calendar_year,
                      investment_funds %>% 
                        filter(account_nm=="CIVIL SERVICE RETIRE") %>% 
                        group_by(record_calendar_year,record_calendar_month) %>% 
                        summarize(y=sum(shares_per_par)/1000000000) %>% 
                        ungroup() %>% 
                        mutate(ch=c(NA,diff(y))) %>% 
                        filter(record_calendar_month==9&record_calendar_year!=2021)),
            data.frame(record_calendar_year=2025))
  
}

suspended_investments_interest_dec = investment_funds %>% 
  filter(account_nm%in%c("CIVIL SERVICE RETIRE","POSTAL SERVICE RETIREE")&record_date==max(record_date)&!grepl("06\\/30\\/2025",security_desc)) %>% 
  mutate(int_rate=as.numeric(substr(gsub("SPECIAL ISSUE BOND |CERTIFICATE OF INDEBTEDNESS ",
                                         "",
                                         security_desc),1,4))/100) %>% 
  group_by(account_nm) %>% 
  summarize(
    int_rate = weighted.mean(int_rate,w=shares_per_par), # assumption moving forward: redemptions does not affect average interest rate
    shares_per_par=sum(shares_per_par)/1000000000
  ) %>% 
  mutate(shares_per_par=case_when(
    account_nm=="CIVIL SERVICE RETIRE"~shares_per_par-(8*8.5), # future redemptions not already done
    TRUE~shares_per_par-(8*.3)
  ),
  int_due=shares_per_par*int_rate/2) %>% 
  ungroup() %>% 
  summarize(total=sum(int_due)) %>% 
  pull(total)

FISCAL_SPACE$headroom[FISCAL_SPACE$date>="2025-12-31"] = FISCAL_SPACE$headroom[FISCAL_SPACE$date>="2025-12-31"] + suspended_investments_interest_dec


# 2. Suspending Govt Securities Investment Fund (G Fund)
# They dont suspend all of it at once, so keep remaining on the books
# But record all of it as headroom
gfund_amt = daily_gas_activity$daily_opening_balance_amt[daily_gas_activity$account_desc=="THRIFT SAVINGS FUND, FEDERAL RETIREMENT THRIFT INVESTMENT BOARD"&daily_gas_activity$record_date=='2025-01-21']/1000000000

FISCAL_SPACE$headroom = FISCAL_SPACE$headroom + gfund_amt

# 3. Suspending investment of Exhcnage Stabilization Fund (ESF)
esf_amt = daily_gas_activity$daily_opening_balance_amt[daily_gas_activity$account_desc=="EXCHANGE STABILIZATION FUND, OFFICE OF THE SECRETARY, TREASURY"&daily_gas_activity$record_date=='2025-01-21']/1000000000

FISCAL_SPACE$headroom = FISCAL_SPACE$headroom + esf_amt

# 4. federal financing bank transactions
ffb_amt = 0.3
FISCAL_SPACE$headroom = FISCAL_SPACE$headroom + ffb_amt

# 5. cash balance
cash_amt = op_cash_balance$open_today_bal[op_cash_balance$account_type=="Treasury General Account (TGA) Closing Balance"&op_cash_balance$record_date=="2025-01-21"]/1000
FISCAL_SPACE$headroom = FISCAL_SPACE$headroom + cash_amt

# See if Treasury daily statements matches aggregate statistics

test = outlay_daily_df %>% 
  select(record_fiscal_year,record_calendar_month,record_calendar_day,outlay=total_day) %>% 
  mutate(record_fiscal_year=ifelse(record_calendar_month%in%c(10:12),record_fiscal_year-1,record_fiscal_year),
         date=as.Date(paste0(record_fiscal_year,"-",record_calendar_month,"-",record_calendar_day))) %>% 
  left_join(receipt_daily_df %>% 
              select(record_fiscal_year,record_calendar_month,record_calendar_day,receipt=total_day) %>% 
              mutate(record_fiscal_year=ifelse(record_calendar_month%in%c(10:12),record_fiscal_year-1,record_fiscal_year),
                     date=as.Date(paste0(record_fiscal_year,"-",record_calendar_month,"-",record_calendar_day))),by="date") %>% 
  group_by(date) %>% 
  summarize(deficit=receipt+outlay)

if(any(test$date<as.Date(paste0(daily_forecast[1,1],"-",daily_forecast[1,2],"-01"),format="%Y-%m-%d"))){
  
  tmp = test %>% 
    filter(date<as.Date(paste0(daily_forecast[1,1],"-",daily_forecast[1,2],"-01"),format="%Y-%m-%d")) %>% 
    filter(month(date)==month(date[n()])&year(date)==year(date[n()])) 
  
  tmp = (tmp %>% 
           summarize(deficit=sum(deficit)) %>% 
           pull())/deficit_fred$value[deficit_fred$date==floor_date(tmp$date[1],"month")]
  
  if(length(tmp)==0){
    tmp = 1
  }
  
  tmp = FISCAL_SPACE %>% 
    left_join(test) %>% 
    mutate(deficit=replace_na(deficit,0),
           deficit=deficit/tmp/1000) %>% 
    filter(date<as.Date(paste0(daily_forecast[1,1],"-",daily_forecast[1,2],"-01"))) %>% 
    mutate(record_fiscal_year=ifelse(month(date)%in%10:12,year(date)+1,year(date)),
           record_calendar_month=month(date),
           record_calendar_day=day(date)) %>% 
    rename(daily_deficit=deficit) %>% 
    select(-c(date,headroom))
  
}

daily_forecast1 = bind_rows(tmp,
                            daily_forecast) %>%  
  mutate(record_fiscal_year=ifelse(record_calendar_month%in%c(10:12),record_fiscal_year-1,record_fiscal_year),
         date=as.Date(paste0(record_fiscal_year,"-",record_calendar_month,"-",record_calendar_day))) %>% 
  mutate(total_deficit=cumsum(daily_deficit)) %>% 
  inner_join(FISCAL_SPACE) %>% 
  mutate(running_bal=headroom+total_deficit)

daily_forecast_upper1 = bind_rows(tmp,
                                  daily_forecast_upper) %>%  
  mutate(record_fiscal_year=ifelse(record_calendar_month%in%c(10:12),record_fiscal_year-1,record_fiscal_year),
         date=as.Date(paste0(record_fiscal_year,"-",record_calendar_month,"-",record_calendar_day))) %>% 
  mutate(total_deficit=cumsum(daily_deficit)) %>% 
  inner_join(FISCAL_SPACE) %>% 
  mutate(running_bal=headroom+total_deficit)

daily_forecast_lower1 = bind_rows(tmp,
                                  daily_forecast_lower) %>%  
  mutate(record_fiscal_year=ifelse(record_calendar_month%in%c(10:12),record_fiscal_year-1,record_fiscal_year),
         date=as.Date(paste0(record_fiscal_year,"-",record_calendar_month,"-",record_calendar_day))) %>% 
  mutate(total_deficit=cumsum(daily_deficit)) %>% 
  inner_join(FISCAL_SPACE) %>% 
  mutate(running_bal=headroom+total_deficit)


my_chart = bind_cols(
  daily_forecast1,
  daily_forecast_upper1 %>% select(running_bal_upper=running_bal),
  daily_forecast_lower1 %>% select(running_bal_lower=running_bal)
) %>% 
  rowwise() %>% 
  mutate(running_bal=max(c(0,running_bal)),
         running_bal_upper=max(c(0,running_bal_upper)),
         running_bal_lower=max(c(0,running_bal_lower)))

ggplot(my_chart %>% filter(date<="2025-09-30"),aes(x=date)) + 
  geom_ribbon(aes(ymin=running_bal_lower,ymax=running_bal_upper),alpha=.3) +
  geom_line(aes(y=running_bal)) +
  theme_bw() +
  labs(x="",y="Fiscal Space Remaining ($B)")
