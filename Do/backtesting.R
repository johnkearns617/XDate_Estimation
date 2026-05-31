# master.R
# John Kearns
# Goal: Write script to run all scripts needed to run google trends and debt models

library(fredr)
library(lubridate)

fred_key = Sys.getenv('FRED_KEY')
gt_key = Sys.getenv("GT_KEY")
bls_key = Sys.getenv("BLS_KEY")

Sys.setenv(TZ='America/New_York')

fredr_set_key(fred_key)

source('Do/0_model_functions.R')

backtest_models = list()

for(dat1 in as.character(seq(from=as.Date("2024-10-01"),to=as.Date("2025-07-01"),by="1 week"))){

end_date = as.Date(dat1)
headroom_date = end_date %m-% years(1)
announcement_date = ifelse(end_date<"2025-01-21",NA,"2025-01-21")

source('Do/2_assemble_econ_data.R')

# make sure scripts 1 and 2 are already run

source('Do/3_construct_dataset_with_data_lags.R')

source('Do/4_feature_imputation.R')

source('Do/5_construct_GT_index.R')

source('Do/7_gt_deficit_modelling.R')

source('Do/9_XDate_estimation.R')

daily_outlays_all = bind_rows(lapply(list(nowcast_medicare_outlay,nowcast_medicaid_outlay,
                                          nowcast_ss_outlay,nowcast_other_outlay,nowcast_defense_outlay,
                                          nowcast_interest_outlay),`[[`, "daily_df")) %>% 
  select(record_date,cbo_category,final_pred_day:final_pred_day_upper,cbo_proj) %>% 
  mutate_at(vars(final_pred_day:cbo_proj),~.*-1)

daily_receipts_all = bind_rows(lapply(list(nowcast_misc_receipts,nowcast_corporate_receipts,
                                           nowcast_payroll_receipts,nowcast_individual_receipts,
                                           nowcast_excise_receipts,nowcast_estate_receipts,
                                           nowcast_customs_receipts),`[[`, "daily_df")) %>% 
  select(record_date,cbo_category,final_pred_day:final_pred_day_upper,cbo_proj)

daily_chart_df = bind_rows(
  daily_outlays_all,
  daily_receipts_all
) %>% 
  group_by(record_date) %>% 
  mutate(daily_deficit=sum(final_pred_day)) %>% 
  ungroup() %>% 
  mutate(month=floor_date(record_date,"month")) %>% 
  group_by(month) %>% 
  mutate(deficit=sum(final_pred_day),
         share=final_pred_day/deficit) %>% 
  left_join(deficit_fred %>% mutate(value=value/1000) %>% select(date,value),by=c("month"="date")) %>% 
  mutate(final_pred_day=case_when(record_date<="2025-09-30"~share*value, # ensures it adds up to monthly 
                                  TRUE~final_pred_day)) %>% 
  ungroup() %>% 
  select(-c(month,deficit,value,share))

backtest_models[[end_date]] = list(my_chart,daily_chart_df,exmeasures_date)

}
