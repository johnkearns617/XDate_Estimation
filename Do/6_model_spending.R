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

# recreate EPIC methodology


# See if Treasury daily statements matches aggregate statistics

# Need to use MTS to get official deficit
data_check = deficit_summary %>% 
  mutate(date=ceiling_date(as.Date(paste0(year(record_date)," ",classification_desc," 01"),format="%Y %B %d"),"month")-1,
         fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10))) %>% 
  group_by(fiscal_year,classification_desc) %>% 
  filter(record_type_cd=="MTH"&print_order_nbr==max(print_order_nbr)) %>% 
  slice(1) %>% 
  group_by(record_fiscal_year,classification_desc) %>% 
  summarize(surplus=sum(as.numeric(current_month_dfct_sur_amt),na.rm=TRUE)/1000000000,
            date=date[1],
            fiscal_year=fiscal_year[1])

data_check %>% 
  filter(fiscal_year<2024&fiscal_year>2015) %>% 
  group_by(fiscal_year) %>% 
  mutate(total_surplus=sum(surplus,na.rm=TRUE)) %>% 
  ungroup() %>%  
  mutate(share=surplus/total_surplus) %>% 
  filter(share>min(share,na.rm=TRUE)&share<max(share,na.rm=TRUE)) %>% 
  group_by(month(date)) %>% 
  summarize(share=mean(share,na.rm=TRUE))
# My vs EPIC
# Jan: +3% vs +3%
# Feb: -19% vs -28%
# Mar: -19% vs. -19%
# Apr: +9% vs. +17%
# May: -12% vs. -17%
# Jun: -10% vs. +1%
# Jul: -11% vs. -14%
# Aug: -13% vs. -14%
# Sep: -2% vs. +7%
# Oct: -10% vs. 14%
# Nov: -14% vs. -17%
# Dec: -3% vs. 0%
  

deficit_fred = fredr(paste0("MTSDS133FMS")) %>% 
  mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)))
  
monthly_shares = deficit_fred %>% 
  filter(fiscal_year>=2002&fiscal_year<=2023) %>% 
  group_by(fiscal_year) %>% 
  mutate(total_surplus=sum(value,na.rm=TRUE)) %>% 
  ungroup() %>%  
  mutate(share=value/total_surplus,
         month=month(date))
  

monthly_shares1 = monthly_shares %>% 
  filter(share>quantile(share,.02)&share<quantile(share,.98)) %>% 
  group_by(month) %>% 
  summarize(share=mean(share,na.rm=TRUE))
# My vs EPIC
# Jan: +3% vs +3%
# Feb: -24% vs -28%
# Mar: -18% vs. -19%
# Apr: +9% vs. +17%
# May: -17% vs. -17%
# Jun: +1% vs. +1%
# Jul: -15% vs. -14%
# Aug: -16% vs. -14%
# Sep: +6% vs. +7%
# Oct: -14% vs. 14%
# Nov: -18% vs. -17%
# Dec: +1% vs. 0%

monthly_shares_reg = lm_robust(share~factor(month),monthly_shares %>% filter(share<quantile(share,.95)&share>quantile(share,.05)))
  
CASH_BAL = 700
EXM = 337

FY2025_DEFICIT = 1865.251

FY2025_DEFICIT_adj = FY2025_DEFICIT - (sum(deficit_fred$value[deficit_fred$date>="2024-10-01"])*-1)/1000

FISCAL_SPACE = CASH_BAL + EXM

ENDJAN = FISCAL_SPACE-(FY2025_DEFICIT*tidy(monthly_shares_reg)[1,2])
ENDJAN_HIGH = FISCAL_SPACE-(FY2025_DEFICIT*(tidy(monthly_shares_reg)[1,2]-tidy(monthly_shares_reg)[1,3]))
ENDJAN_LOW = FISCAL_SPACE-(FY2025_DEFICIT*(tidy(monthly_shares_reg)[1,2]+tidy(monthly_shares_reg)[1,3]))

ENDFEB = ENDJAN-(FY2025_DEFICIT*tidy(monthly_shares_reg)[2,2])
ENDFEB_HIGH = ENDJAN_HIGH-(FY2025_DEFICIT*(tidy(monthly_shares_reg)[2,2]-tidy(monthly_shares_reg)[2,3]))
ENDFEB_LOW = ENDJAN_LOW-(FY2025_DEFICIT*(tidy(monthly_shares_reg)[2,2]+tidy(monthly_shares_reg)[2,3]))

ENDMAR = ENDFEB-(FY2025_DEFICIT*tidy(monthly_shares_reg)[3,2])
ENDMAR_HIGH = ENDFEB_HIGH-(FY2025_DEFICIT*(tidy(monthly_shares_reg)[3,2]-tidy(monthly_shares_reg)[3,3]))
ENDMAR_LOW = ENDFEB_LOW-(FY2025_DEFICIT*(tidy(monthly_shares_reg)[3,2]+tidy(monthly_shares_reg)[3,3]))

ENDAPR = ENDMAR-(FY2025_DEFICIT*tidy(monthly_shares_reg)[4,2])
ENDAPR_HIGH = ENDMAR_HIGH-(FY2025_DEFICIT*(tidy(monthly_shares_reg)[4,2]-tidy(monthly_shares_reg)[4,3]))
ENDAPR_LOW = ENDMAR_LOW-(FY2025_DEFICIT*(tidy(monthly_shares_reg)[4,2]+tidy(monthly_shares_reg)[4,3]))

ENDMAY = ENDAPR-(FY2025_DEFICIT*tidy(monthly_shares_reg)[5,2])
ENDMAY_HIGH = ENDAPR_HIGH-(FY2025_DEFICIT*(tidy(monthly_shares_reg)[5,2]-tidy(monthly_shares_reg)[5,3]))
ENDMAY_LOW = ENDAPR_LOW-(FY2025_DEFICIT*(tidy(monthly_shares_reg)[5,2]+tidy(monthly_shares_reg)[5,3]))

ENDJUN = ENDMAY-(FY2025_DEFICIT*tidy(monthly_shares_reg)[6,2])
ENDJUN_HIGH = ENDMAY_HIGH-(FY2025_DEFICIT*(tidy(monthly_shares_reg)[6,2]-tidy(monthly_shares_reg)[6,3]))
ENDJUN_LOW = ENDMAY_LOW-(FY2025_DEFICIT*(tidy(monthly_shares_reg)[6,2]+tidy(monthly_shares_reg)[6,3]))

ENDJUL = ENDJUN-(FY2025_DEFICIT*tidy(monthly_shares_reg)[7,2])
ENDJUL_HIGH = ENDJUN_HIGH-(FY2025_DEFICIT*(tidy(monthly_shares_reg)[7,2]-tidy(monthly_shares_reg)[7,3]))
ENDJUL_LOW = ENDJUN_LOW-(FY2025_DEFICIT*(tidy(monthly_shares_reg)[7,2]+tidy(monthly_shares_reg)[7,3]))

EPIC_chart = data.frame(
  date=seq(as.Date("2025-01-01"), length=8, by="1 month") - 1,
  fiscal_space=c(FISCAL_SPACE,ENDJAN,ENDFEB,ENDMAR,ENDAPR,ENDMAY,ENDJUN,ENDJUL),
  fiscal_space_high=c(FISCAL_SPACE,ENDJAN_HIGH,ENDFEB_HIGH,ENDMAR_HIGH,ENDAPR_HIGH,ENDMAY_HIGH,ENDJUN_HIGH,ENDJUL_HIGH),
  fiscal_space_low=c(FISCAL_SPACE,ENDJAN_LOW,ENDFEB_LOW,ENDMAR_LOW,ENDAPR_LOW,ENDMAY_LOW,ENDJUN_LOW,ENDJUL_LOW)
) %>% 
  rowwise() %>% 
  mutate(fiscal_space=max(c(0,fiscal_space)),
         fiscal_space_high=max(c(0,fiscal_space_high)),
         fiscal_space_low=max(c(0,fiscal_space_low)))

ggplot(EPIC_chart,aes(x=date)) + 
  geom_ribbon(aes(ymin=fiscal_space_low,ymax=fiscal_space_high),alpha=.3) +
  geom_line(aes(y=fiscal_space)) +
  theme_bw() +
  labs(x="",y="Fiscal Space Remaining ($B)",caption="Ribbon shows +- 1 Standard Deviation")
