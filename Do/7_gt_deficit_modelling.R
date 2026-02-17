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
library(tis)

conflicted::conflict_prefer("filter","dplyr")
conflicted::conflicts_prefer(dplyr::last)

set.seed(178)

EM_DECLARATION = as.Date("2025-01-17")

# Step 1: Nowcast current levels

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

conflicted::conflicts_prefer(dplyr::lag)
conflicted::conflict_prefer("filter","dplyr")
conflicted::conflicts_prefer(dplyr::lead)
conflicted::conflicts_prefer(dplyr::select)
conflicted::conflicts_prefer(tidyr::replace_na)
conflicted::conflicts_prefer(lubridate::year)

set.seed(178)

# write_csv(op_cash_dep_withdraw %>%
#             distinct(transaction_type,transaction_catg,transaction_catg_desc) %>%
#             left_join(daily_categories %>% mutate(transaction_catg_desc="null")),
#           "Data/Processing/daily_categories1.csv")

daily_categories = read_csv("Data/Processing/daily_categories1.csv")

# get simple CBO forecast by month

dts = op_cash_dep_withdraw %>% 
  left_join(daily_categories) %>% # we want to keep only the things we are able to map
  filter(!is.na(cbo_category)) %>% # get rid of the ones we cant map, mostly are internal transfers
  distinct(record_date,account_type,transaction_type,transaction_catg,transaction_today_amt,.keep_all = TRUE) %>% 
  mutate(transaction_today_amt=ifelse(transaction_type=="Withdrawals",as.numeric(transaction_today_amt)*-1,as.numeric(transaction_today_amt)), # make withdrawawls negative
         transaction_mtd_amt=ifelse(transaction_type=="Withdrawals",as.numeric(transaction_mtd_amt)*-1,as.numeric(transaction_mtd_amt)))

# write_csv(dts,"Data/Raw/receipt_daily_df.csv") # used for running models

tax_days = read_csv("Data/Raw/tax_days_2000_2040.csv") %>% 
  mutate(`Tax Day`=gsub("\\(COVID-19 extension\\)","",`Tax Day`),
         date=paste0(`Tax Day`," ",Year),date=as.Date(date,format="%B %d %Y")) %>% 
  mutate(tax_day=1) %>% 
  select(date,tax_day)

#imputed_df = read_csv(paste0("Data/Processing/imputed_data/imputed_data_asof",Sys.Date(),".csv"))

nowcast_misc_receipts = nowcast_daily_budget_receipt(dts,receipts,end_date,"Miscellaneous Receipts","Total -- Miscellaneous Receipts",NA)
nowcast_corporate_receipts = nowcast_daily_budget_receipt(dts,receipts,end_date,"Corporate Income Taxes","Corporation Income Taxes",NA)
nowcast_payroll_receipts = nowcast_daily_budget_receipt(dts,receipts,end_date,"Payroll Taxes","Total -- Social Insurance and Retirement Receipts",NA)
nowcast_individual_receipts = nowcast_daily_budget_receipt(dts,receipts,end_date,"Individual Income Taxes","Total -- Individual Income Taxes",NA)
nowcast_excise_receipts = nowcast_daily_budget_receipt(dts,receipts,end_date,"Excise Taxes","Total -- Excise Taxes",NA)
nowcast_estate_receipts = nowcast_daily_budget_receipt(dts,receipts,end_date,"Estate and Gift Taxes","Estate and Gift Taxes",NA)
nowcast_customs_receipts = nowcast_daily_budget_receipt(dts,receipts,end_date,"Customs Duties","Customs Duties",NA)

nowcast_medicare_outlay = nowcast_daily_budget_outlay(dts,outlays,end_date,"Medicare","Medicare",NA)
nowcast_medicaid_outlay = nowcast_daily_budget_outlay(dts,outlays,end_date,"Medicaid","Grants to States for Medicaid",NA)
nowcast_ss_outlay = nowcast_daily_budget_outlay(dts,outlays,end_date,"Social Security","Social Security",NA)
nowcast_other_outlay = nowcast_daily_budget_outlay(dts,outlays,end_date,"Other Spending","Other Spending",NA)
nowcast_defense_outlay = nowcast_daily_budget_outlay(dts,outlays,end_date,"National Defense","National Defense",NA)
nowcast_interest_outlay = nowcast_daily_budget_outlay(dts,outlays,end_date,"Net Interest","Net Interest",NA)

nowcast_outlay = bind_rows(lapply(list(nowcast_medicare_outlay,nowcast_medicaid_outlay,
         nowcast_ss_outlay,nowcast_other_outlay,nowcast_defense_outlay,
         nowcast_interest_outlay),`[[`, "daily_df")) %>% 
         select(date,cbo_category,final_pred_day:final_pred_day_upper,cbo_proj) %>% 
         group_by(date,cbo_category) %>% 
         summarize(cbo_category=cbo_category[1],
                   final_pred_day=sum(final_pred_day),
                   final_pred_day_lwr=sum(final_pred_day_lwr),
                   final_pred_day_upper=sum(final_pred_day_upper),
                   cbo_proj=cbo_proj[1]) %>% 
  rename(final_pred_day_upper=4,
         final_pred_day_lwr=5)
         
nowcast_receipt = bind_rows(lapply(list(nowcast_misc_receipts,nowcast_corporate_receipts,
                                        nowcast_payroll_receipts,nowcast_individual_receipts,
                                        nowcast_excise_receipts,nowcast_estate_receipts,
                                        nowcast_customs_receipts),`[[`, "daily_df")) %>% 
  select(date,cbo_category,final_pred_day:final_pred_day_upper,cbo_proj) %>% 
  group_by(date,cbo_category) %>% 
  summarize(cbo_category=cbo_category[1],
            final_pred_day=sum(final_pred_day),
            final_pred_day_lwr=sum(final_pred_day_lwr),
            final_pred_day_upper=sum(final_pred_day_upper),
            cbo_proj=cbo_proj[1])

actuals = bind_rows(
  nowcast_misc_receipts[[2]] %>% select(date,actual) %>% mutate(cbo_category="Miscellaneous Receipts"),
  nowcast_corporate_receipts[[2]] %>% select(date,actual) %>% mutate(cbo_category="Corporate Income Taxes"),
  nowcast_payroll_receipts[[2]] %>% select(date,actual) %>% mutate(cbo_category="Payroll Taxes"),
  nowcast_individual_receipts[[2]] %>% select(date,actual) %>% mutate(cbo_category="Individual Income Taxes"),
  nowcast_excise_receipts[[2]] %>% select(date,actual) %>% mutate(cbo_category="Excise Taxes"),
  nowcast_estate_receipts[[2]] %>% select(date,actual) %>% mutate(cbo_category="Estate and Gift Taxes"),
  nowcast_customs_receipts[[2]] %>% select(date,actual) %>% mutate(cbo_category="Customs Duties"),
  nowcast_medicare_outlay[[2]] %>% select(date,actual) %>% mutate(cbo_category="Medicare"),
  nowcast_medicaid_outlay[[2]] %>% select(date,actual) %>% mutate(cbo_category="Medicaid"),
  nowcast_ss_outlay[[2]] %>% select(date,actual) %>% mutate(cbo_category="Social Security"),
  nowcast_other_outlay[[2]] %>% select(date,actual) %>% mutate(cbo_category="Other Spending"),
  nowcast_defense_outlay[[2]] %>% select(date,actual) %>% mutate(cbo_category="National Defense"),
  nowcast_interest_outlay[[2]] %>% select(date,actual) %>% mutate(cbo_category="Net Interest")
)

nowcast_deficit = bind_rows(
  nowcast_receipt,
  nowcast_outlay
) %>% 
  left_join(actuals) %>% 
  mutate_at(vars(final_pred_day:final_pred_day_upper,cbo_proj,actual),~ifelse(cbo_category%in%c("Medicare","Medicaid","Social Security",
                                                                                       "Other Spending","National Defense","Net Interest"),.*-1,.))


daily_forecast = bind_rows(lapply(list(nowcast_medicare_outlay,nowcast_medicaid_outlay,
                                       nowcast_ss_outlay,nowcast_other_outlay,nowcast_defense_outlay,
                                       nowcast_interest_outlay),`[[`, "daily_df")) %>% 
  select(record_date,final_pred_day:final_pred_day_upper) %>% 
  group_by(record_date) %>% 
  summarize(final_pred_day=sum(final_pred_day)*-1,
            final_pred_day_lwr=sum(final_pred_day_lwr)*-1,
            final_pred_day_upper=sum(final_pred_day_upper)*-1) %>% 
  rename(final_pred_day_upper=3,
         final_pred_day_lwr=4) %>% 
  bind_rows(bind_rows(lapply(list(nowcast_misc_receipts,nowcast_corporate_receipts,
                                  nowcast_payroll_receipts,nowcast_individual_receipts,
                                  nowcast_excise_receipts,nowcast_estate_receipts,
                                  nowcast_customs_receipts),`[[`, "daily_df")) %>% 
              select(record_date,final_pred_day:final_pred_day_upper) %>% 
              group_by(record_date) %>% 
              summarize(final_pred_day=sum(final_pred_day),
                        final_pred_day_lwr=sum(final_pred_day_lwr),
                        final_pred_day_upper=sum(final_pred_day_upper))) %>% 
  group_by(record_date) %>% 
  summarize(final_pred_day=sum(final_pred_day),
            final_pred_day_lwr=sum(final_pred_day_lwr),
            final_pred_day_upper=sum(final_pred_day_upper)) %>% 
  ungroup()

