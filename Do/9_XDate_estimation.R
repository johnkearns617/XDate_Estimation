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


# recreate EPIC methodology


# See if Treasury daily statements matches aggregate statistics

CASH_BAL = 700
EXM = 298+44+20+.3

FY2025_DEFICIT = 1865.251

FY2025_DEFICIT_adj = FY2025_DEFICIT - (sum(deficit_fred$value[deficit_fred$date>="2024-10-01"])*-1)/1000

FISCAL_SPACE = CASH_BAL + EXM

ENDJAN = FISCAL_SPACE-128.64020667

daily_forecast1 = daily_forecast %>%  
  mutate(record_fiscal_year=ifelse(record_calendar_month%in%c(10:12),record_fiscal_year-1,record_fiscal_year),
         date=as.Date(paste0(record_fiscal_year,"-",record_calendar_month,"-",record_calendar_day))) %>% 
  mutate(ENDJAN=ENDJAN,
         ENDJAN=case_when(
           date>="2025-06-30"&date<="2025-09-30"~ENDJAN+147,
           date>="2025-09-30"~ENDJAN+147+51,
           TRUE~ENDJAN
         ),
         total_deficit=cumsum(daily_deficit),
         running_bal=ENDJAN+total_deficit)

daily_forecast_upper1 = daily_forecast_upper %>%  
  mutate(record_fiscal_year=ifelse(record_calendar_month%in%c(10:12),record_fiscal_year-1,record_fiscal_year),
         date=as.Date(paste0(record_fiscal_year,"-",record_calendar_month,"-",record_calendar_day))) %>% 
  mutate(ENDJAN=ENDJAN,
         ENDJAN=case_when(
           date>="2025-06-30"&date<="2025-09-30"~ENDJAN+147,
           date>="2025-09-30"~ENDJAN+147+51,
           TRUE~ENDJAN
         ),
         total_deficit=cumsum(daily_deficit),
         running_bal=ENDJAN+total_deficit)

daily_forecast_lower1 = daily_forecast_lower %>%  
  mutate(record_fiscal_year=ifelse(record_calendar_month%in%c(10:12),record_fiscal_year-1,record_fiscal_year),
         date=as.Date(paste0(record_fiscal_year,"-",record_calendar_month,"-",record_calendar_day))) %>% 
  mutate(ENDJAN=ENDJAN,
         ENDJAN=case_when(
           date>="2025-06-30"&date<="2025-09-30"~ENDJAN+147,
           date>="2025-09-30"~ENDJAN+147+51,
           TRUE~ENDJAN
         ),
         total_deficit=cumsum(daily_deficit),
         running_bal=ENDJAN+total_deficit)


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
