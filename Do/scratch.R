# model_daily_spending.R
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

conflicted::conflicts_prefer(dplyr::lag)
conflicted::conflict_prefer("filter","dplyr")
conflicted::conflicts_prefer(dplyr::lead)


set.seed(178)

fredr_set_key(fred_key)

# write_csv(op_cash_dep_withdraw %>% 
#             distinct(transaction_type,transaction_catg,transaction_catg_desc) %>% 
#             left_join(daily_categories %>% mutate(transaction_catg_desc="null")),
#           "Data/Processing/daily_categories1.csv")

daily_categories = read_csv("Data/Processing/daily_categories1.csv")

dts = op_cash_dep_withdraw %>% 
  left_join(daily_categories) %>% 
  filter(!is.na(cbo_category)) %>% 
  mutate(transaction_today_amt=ifelse(transaction_type=="Withdrawals",as.numeric(transaction_today_amt)*-1,as.numeric(transaction_today_amt)),
         transaction_mtd_amt=ifelse(transaction_type=="Withdrawals",as.numeric(transaction_mtd_amt)*-1,as.numeric(transaction_mtd_amt)))



receipt_daily_df = dts %>% 
  filter((grepl("Tax|Receipt|Duties",cbo_category))&!grepl("from Depositaries",transaction_catg)) %>% 
  group_by(record_fiscal_year,record_calendar_month,record_calendar_day) %>% 
  summarize(date=record_date[1],
            total_day=sum(transaction_today_amt,na.rm=TRUE)) %>% 
  group_by(record_fiscal_year,record_calendar_month) %>% 
  mutate(total_mtd=cumsum(total_day)) %>% 
  mutate(total1=total_mtd[n()]/1000,
         share=total_mtd/total1/1000) %>% 
  arrange(date) %>% 
  mutate(record_calendar_day_perc=(as.numeric(record_calendar_day))/as.numeric(days_in_month(date)),
         inv_record_calendar_day=1-record_calendar_day_perc,
         date=floor_date(date,"month"))  %>% 
  left_join(nowcast_deficit %>% select(date,pred=receipts,actual=actual_receipts)) %>% 
  group_by(record_calendar_day,record_calendar_month) %>% 
  mutate(avg_share=median(share,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(extrap_total=(total_mtd/avg_share)/1000)

receipt_daily_df = receipt_daily_df %>% 
  mutate(extrap_total=extrap_total*tidy(lm_robust(actual~total1-1,receipt_daily_df %>% filter(date<max(receipt_daily_df$date))  %>% group_by(date) %>% slice(n())))[1,2])

receipt_daily_df = receipt_daily_df %>% 
  rowwise() %>% 
  mutate(extrap_total=mean(c(pred,extrap_total))) %>% 
  ungroup()

outlay_daily_df = dts %>% 
  filter(!(grepl("Tax|Receipt|Duties|TTL Transfer",cbo_category))&!grepl("to Depositaries",transaction_catg)) %>% 
  group_by(record_fiscal_year,record_calendar_month,record_calendar_day) %>% 
  summarize(date=record_date[1],
            total_day=sum(transaction_today_amt,na.rm=TRUE)) %>% 
  group_by(record_fiscal_year,record_calendar_month) %>% 
  mutate(total_mtd=cumsum(total_day)) %>% 
  mutate(total1=total_mtd[n()]*-1/1000,
         share=total_mtd/total1*-1/1000) %>% 
  arrange(date) %>% 
  mutate(record_calendar_day_perc=(as.numeric(record_calendar_day))/as.numeric(days_in_month(date)),
         inv_record_calendar_day=1-record_calendar_day_perc,
         actual_date=date,
         date=floor_date(date,"month"))  %>% 
  left_join(nowcast_deficit %>% select(date,pred=outlays,actual=actual_outlays)) %>% 
  mutate(record_calendar_day=as.numeric(record_calendar_day))

outlay_daily_df = outlay_daily_df %>% 
  rowwise() %>% 
  mutate(avg_share=mean(outlay_daily_df[outlay_daily_df$record_calendar_month==record_calendar_month&outlay_daily_df$record_calendar_day<=record_calendar_day,] %>% group_by(record_fiscal_year) %>% slice(n()) %>% ungroup() %>% select(share) %>% pull())) %>% 
  ungroup() %>% 
  mutate(extrap_total=(total_mtd/avg_share)*(-1/1000))

outlay_daily_df = outlay_daily_df %>% 
  left_join(outlay_daily_df %>% 
              filter(share==1&record_fiscal_year>=2015) %>% 
              group_by(record_fiscal_year) %>% 
              summarize(scale_factor=mean(actual/extrap_total,na.rm=TRUE))) %>% 
  ungroup() %>% 
  fill(scale_factor,.direction="downup") %>% 
  mutate(extrap_total=extrap_total*scale_factor) %>% 
  rowwise() %>% 
  mutate(extrap_total=mean(c(pred,extrap_total))) %>% 
  ungroup()

ggplot(outlay_daily_df %>% filter(date=="2025-01-01"),aes(x=actual_date)) +
  geom_line(aes(y=actual,color="Actual")) +
  geom_line(aes(y=extrap_total,color="Daily estimate")) 

feb_forecast = bind_cols(
  outlay_daily_df %>% 
    select(outlay_day_amt=total_day,outlay_mtd_amt=total_mtd,record_fiscal_year:record_calendar_day,pred_outlay=pred,actual_outlay=actual,outlay_extrap_total=extrap_total) %>% 
    mutate(outlay_day_amt=-1*outlay_day_amt,
           outlay_mtd_amt=-1*outlay_mtd_amt),
  receipt_daily_df %>% 
    select(receipt_day_amt=total_day,receipt_mtd_amt=total_mtd,pred_receipt=pred,actual_receipt=actual,receipts_extrap_total=extrap_total) %>% 
    mutate(receipt_day_amt=receipt_day_amt,
           receipt_mtd_amt=receipt_mtd_amt)
) %>% 
  filter(record_fiscal_year==2025&record_calendar_month==2) %>% 
  mutate(date = as.Date(paste0(ifelse(record_calendar_month%in%10:12,record_fiscal_year-1,record_fiscal_year),"-",record_calendar_month,"-",record_calendar_day))) 

feb_forecast = feb_forecast %>% 
  bind_rows(data.frame(record_calendar_day=as.numeric(day(seq(max(feb_forecast$date)+1,ceiling_date(feb_forecast$date[1],"month")-1,by=1))))) %>% 
  left_join(outlay_daily_df %>% 
              filter(record_calendar_month==2) %>% 
              distinct(record_calendar_day,avg_share) %>% 
              rename(avg_share_outlay=avg_share) %>% 
              mutate(record_calendar_day=as.numeric(record_calendar_day))) %>% 
  left_join(receipt_daily_df %>% 
              filter(record_calendar_month==2) %>% 
              distinct(record_calendar_day,avg_share) %>% 
              rename(avg_share_receipt=avg_share)%>% 
              mutate(record_calendar_day=as.numeric(record_calendar_day))) %>% 
  fill(outlay_extrap_total,receipts_extrap_total,.direction="down") %>% 
  ungroup() %>% 
  mutate(avg_share_outlay=avg_share_outlay/avg_share_outlay[n()],
         avg_share_receipt=avg_share_receipt/avg_share_receipt[n()],
         outlay_mtd_amt=outlay_mtd_amt*tail(na.omit(outlay_extrap_total),1)*avg_share_outlay[tail(which(!is.na(record_calendar_month)),1)]/tail(na.omit(outlay_mtd_amt),1)*1000,
         outlay_day_amt=lead(outlay_mtd_amt,1)-outlay_mtd_amt,
         outlay_mtd_amt=ifelse(is.na(outlay_mtd_amt),outlay_extrap_total*avg_share_outlay*1000,outlay_mtd_amt),
         outlay_day_amt=outlay_mtd_amt-lag(outlay_mtd_amt,1),
         outlay_day_amt=ifelse(record_calendar_day==min(record_calendar_day),outlay_mtd_amt,outlay_day_amt),
         receipt_mtd_amt=receipt_mtd_amt*tail(na.omit(receipts_extrap_total),1)*avg_share_receipt[tail(which(!is.na(record_calendar_month)),1)]/tail(na.omit(receipt_mtd_amt),1)*1000,
         receipt_day_amt=lead(receipt_mtd_amt,1)-receipt_mtd_amt,
         receipt_mtd_amt=ifelse(is.na(receipt_mtd_amt),receipts_extrap_total*avg_share_receipt*1000,receipt_mtd_amt),
         receipt_day_amt=receipt_mtd_amt-lag(receipt_mtd_amt,1),
         receipt_day_amt=ifelse(record_calendar_day==min(record_calendar_day),receipt_mtd_amt,receipt_day_amt))



#### extend forecast using ARIMA

# get simple CBO forecast by month
cbo_by_year = cbo_proj %>% 
  filter(component%in%c("revenue","outlay")) %>% 
  group_by(projected_fiscal_year,subcategory) %>% 
  slice(n()) %>% 
  select(component,subcategory,projected_fiscal_year,value)

cbo_monthly_proj = data.frame(
  year=rep(c(min(cbo_by_year$projected_fiscal_year):max(cbo_by_year$projected_fiscal_year)),each=12),
  month=rep(c(1:12),times=length(min(cbo_by_year$projected_fiscal_year):max(cbo_by_year$projected_fiscal_year)))
) %>% 
  left_join(cbo_by_year %>% 
              pivot_wider(names_from=c(component,subcategory),values_from=value) %>% 
              rowwise() %>% 
              mutate(`outlay_Other`=sum(c(`outlay_Other Mandatory`,`outlay_Nondefense Discretionary`,`outlay_Fannie Freddie`),na.rm=TRUE),
                     `outlay_Total`=sum(c(`outlay_Total Mandatory`,`outlay_Total Discretionary`))) %>% 
              select(-c(`outlay_Other Mandatory`,`outlay_Nondefense Discretionary`,`outlay_Fannie Freddie`,`outlay_Total Mandatory`,`outlay_Total Discretionary`)),
            c("year"="projected_fiscal_year"))

cbo_monthly_proj$`revenue_Corporate Income Taxes` = predict(nowcast_corporate_receipts[[4]],cbo_monthly_proj %>% select(month))*cbo_monthly_proj$`revenue_Corporate Income Taxes`
cbo_monthly_proj$`revenue_Customs Duties` = predict(nowcast_customs_receipts[[4]],cbo_monthly_proj %>% select(month))*cbo_monthly_proj$`revenue_Customs Duties`
cbo_monthly_proj$`revenue_Estate and Gift Taxes` = predict(nowcast_estate_receipts[[4]],cbo_monthly_proj %>% select(month))*cbo_monthly_proj$`revenue_Estate and Gift Taxes`
cbo_monthly_proj$`revenue_Excise Taxes` = predict(nowcast_excise_receipts[[4]],cbo_monthly_proj %>% select(month))*cbo_monthly_proj$`revenue_Excise Taxes`
cbo_monthly_proj$`revenue_Individual Income Taxes` = predict(nowcast_individual_receipts[[4]],cbo_monthly_proj %>% select(month))*cbo_monthly_proj$`revenue_Individual Income Taxes`
cbo_monthly_proj$`revenue_Miscellaneous Receipts` = predict(nowcast_misc_receipts[[4]],cbo_monthly_proj %>% select(month))*cbo_monthly_proj$`revenue_Miscellaneous Receipts`
cbo_monthly_proj$`revenue_Payroll Taxes` = predict(nowcast_payroll_receipts[[4]],cbo_monthly_proj %>% select(month))*cbo_monthly_proj$`revenue_Payroll Taxes`
cbo_monthly_proj$revenue_Total = predict(nowcast_total_receipts[[4]],cbo_monthly_proj %>% select(month))*cbo_monthly_proj$revenue_Total
cbo_monthly_proj$outlay_Medicaid = predict(nowcast_medicaid_outlay[[4]],cbo_monthly_proj %>% select(month))*cbo_monthly_proj$outlay_Medicaid
cbo_monthly_proj$outlay_Medicare = predict(nowcast_medicare_outlay[[4]],cbo_monthly_proj %>% select(month))*cbo_monthly_proj$outlay_Medicare
cbo_monthly_proj$`outlay_Net Interest` = predict(nowcast_interest_outlay[[4]],cbo_monthly_proj %>% select(month))*cbo_monthly_proj$`outlay_Net Interest`
cbo_monthly_proj$`outlay_Social Security` = predict(nowcast_ss_outlay[[4]],cbo_monthly_proj %>% select(month))*cbo_monthly_proj$`outlay_Social Security`
cbo_monthly_proj$`outlay_Defense Discretionary` = predict(nowcast_defense_outlay[[4]],cbo_monthly_proj %>% select(month))*cbo_monthly_proj$`outlay_Defense Discretionary`
cbo_monthly_proj$outlay_Other = predict(nowcast_other_outlay[[4]],cbo_monthly_proj %>% select(month))*cbo_monthly_proj$outlay_Other
cbo_monthly_proj$outlay_Total = predict(nowcast_total_outlays[[4]],cbo_monthly_proj %>% select(month))*cbo_monthly_proj$outlay_Total


forecast_component = function(nowcast_object,nowcast_total_object,daily_df,cbo_monthly_proj_col,component_abbrev){
  
  tst = nowcast_object[[3]] %>% 
    mutate(year=year(date),
           month=month(date)) %>% 
    select(date,year,month,pred,actual) %>% 
    left_join(nowcast_total_object[[3]] %>% select(date,total_actual=actual)) %>% 
    mutate(share=actual/total_actual,
           record_fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10))) %>% 
    left_join(daily_df %>% 
                select(record_fiscal_year,record_calendar_month,total_pred=pred,extrap_total) %>%
                group_by(record_fiscal_year,record_calendar_month) %>% 
                mutate(extrap_total=extrap_total[n()]) %>% 
                slice(1),
              by=c("record_fiscal_year"="record_fiscal_year","month"="record_calendar_month")) %>% 
    select(date,record_calendar_year=year,record_calendar_month=month,share,actual,total_actual,pred,total_pred,extrap_total) %>% 
    group_by(record_calendar_month) %>% 
    mutate(pred_share=mean(share,na.rm=TRUE)) %>% 
    ungroup() %>% 
    left_join(data.frame(date=cbo_monthly_proj %>% mutate(date=as.Date(paste0(year,"-",month,"-01"))) %>% select(date) %>% pull(),cbo_outlay=rowSums(cbo_monthly_proj %>% select(outlay_Medicaid:outlay_Other)),cbo_revenue=rowSums(cbo_monthly_proj %>% select(`revenue_Corporate Income Taxes`:`revenue_Payroll Taxes`)))) %>% 
    left_join(cbo_monthly_proj %>% mutate(date=as.Date(paste0(year,"-",month,"-01"))) %>% select(date,!!sym(cbo_monthly_proj_col))) %>% 
    mutate(ch_component=actual-!!sym(cbo_monthly_proj_col),
           ch_cbo=total_actual-!!sym(paste0("cbo_",strsplit(cbo_monthly_proj_col,"_")[[1]][1])))
  
  
  tst$actual[tst$date==max(nowcast_object[[3]]$date)] = tst$pred[tst$date==max(nowcast_object[[3]]$date)]+predict(lm_robust(ch_component~ch_cbo*factor(record_calendar_month)-1,
                                                                                                                            tst),tst %>% slice(n()) %>% mutate(ch_cbo=extrap_total-total_pred))
  
  # make this into a regression that factors in month and prediction
  reg_df_all = tst %>% 
    arrange(date) %>% 
    mutate(record_fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10))) %>% 
    ungroup() %>% 
    select(date,record_fiscal_year,actual,pred) %>% 
    mutate(error=(pred/actual-1)*100,
           error=ifelse(date==max(date),NA,error),
           avg_error=rollmedian(error,6,align="right",partial=TRUE),
           month=month(date)) %>% 
    full_join(cbo_monthly_proj %>% 
                select(year,month,cbo_monthly_proj_col),
              by=c("record_fiscal_year"="year","month"="month")) %>% 
    filter(!is.na(record_fiscal_year)) %>% 
    mutate(date=ifelse(month>=10,paste0(as.numeric(record_fiscal_year)-1,"-",month,"-01"),paste0(record_fiscal_year,"-",month,"-01")),
           date=as.Date(date)) %>%
    left_join(nowcast_object[[1]] %>% select(date,names(nowcast_object[[2]]$coefficients)[-c(1:6)])) %>% 
    arrange(date)
  
  for(dat in as.character(reg_df_all$date[reg_df_all$date>max(cbo_econ$date[cbo_econ$date<Sys.Date()])&reg_df_all$date<=max(nowcast_object[[3]]$date)%m+%years(2)&is.na(reg_df_all$pred)])){
    
    component_reg_df = reg_df_all %>% 
      mutate(qtr=quarter(date),
             year=year(date)) %>%
      group_by(year,qtr) %>%
      mutate(date=date[1]) %>% 
      mutate_at(vars(9:26),~mean(.,na.rm=TRUE)) %>%
      slice(1) %>% 
      ungroup() %>% 
      inner_join(cbo_econ) %>% 
      select(-c(record_fiscal_year:month)) %>% 
      drop_na() 
    
    for(col in colnames(component_reg_df)[3:(which(colnames(component_reg_df)=="qtr"))-1]){
      
      comp_lm = lm_robust(as.formula(paste0('`',col,'`',"~lag+real_gdp+output_gap+pce_price_index+core_pce_price_index+
                                          eci_wages_salaries+lfpr_16yo+unemployment_rate+fed_funds_rate+
                                          treasury_note_rate_10yr+personal_income+nonwage_income+government_c_gi+
                                          real_exports+real_imports+gross_pri_dom_invest")),
                          data=component_reg_df %>% 
                            filter(date<min(reg_df_all$date[reg_df_all$date>max(cbo_econ$date[cbo_econ$date<Sys.Date()])&is.na(reg_df_all$pred)])) %>% 
                            mutate(lag=dplyr::lag(!!sym(col))) %>% 
                            mutate_at(vars(real_gdp,output_gap,pce_price_index,core_pce_price_index,
                                           eci_wages_salaries,lfpr_16yo,unemployment_rate,fed_funds_rate,
                                           treasury_note_rate_10yr,personal_income,nonwage_income,government_c_gi,
                                           real_exports,real_imports,gross_pri_dom_invest),~.-dplyr::lag(.,1)))
      
      
      reg_df_all[reg_df_all$date==dat,col] = as.numeric(predict(comp_lm,
                                                                data.frame(date=as.Date(dat)) %>% 
                                                                  left_join(reg_df_all %>%
                                                                              mutate(lag=dplyr::lag(!!sym(col))) %>% 
                                                                              select(date,lag)) %>% 
                                                                  mutate(qtr=floor_date(date,"quarter")) %>% 
                                                                  left_join(cbo_econ %>% 
                                                                              mutate_at(vars(real_gdp,output_gap,pce_price_index,core_pce_price_index,
                                                                                             eci_wages_salaries,lfpr_16yo,unemployment_rate,fed_funds_rate,
                                                                                             treasury_note_rate_10yr,personal_income,nonwage_income,government_c_gi,
                                                                                             real_exports,real_imports,gross_pri_dom_invest),~.-dplyr::lag(.,1)),
                                                                            by=c("qtr"="date"))
      ))
      
    }
    
  }
  
  test_lm = lm_robust(as.formula(paste0("actual~lag+",paste0("`",colnames(reg_df_all)[8],"`"),"+",paste(colnames(reg_df_all)[9:ncol(reg_df_all)],collapse="+"))),
                      reg_df_all %>% 
                        mutate(lag=dplyr::lag(actual,1),
                               lag12=dplyr::lag(actual,12)))
  
  for(dat in as.character(reg_df_all$date[reg_df_all$date>max(cbo_econ$date[cbo_econ$date<Sys.Date()])&reg_df_all$date<=max(nowcast_object[[3]]$date)%m+%years(2)&is.na(reg_df_all$pred)])){
    
    tmp = predict(test_lm,reg_df_all %>% 
                    mutate(lag=dplyr::lag(actual,1)) %>% 
                    filter(date==dat),
                  se.fit = TRUE)
    
    reg_df_all[reg_df_all$date==dat,"actual"] = as.numeric(tmp$fit)
    
    reg_df_all[reg_df_all$date==dat,"actual_lower"] = as.numeric(tmp$fit)-2*as.numeric(tmp$se.fit)
    reg_df_all[reg_df_all$date==dat,"actual_higher"] = as.numeric(tmp$fit)+2*as.numeric(tmp$se.fit)
    
  }
  
  
  
  forecast_data = bind_rows(reg_df_all %>%
                              filter(date<=max(nowcast_object[[3]]$date)) %>% 
                              select(date,actual),
                            reg_df_all %>% 
                              filter(date>max(nowcast_object[[3]]$date)&date<=max(nowcast_object[[3]]$date)%m+%years(2)) %>% 
                              select(date,mean=actual,lower=actual_lower,upper=actual_higher)
  ) %>% 
    mutate(var=cbo_monthly_proj_col) %>% 
    filter(date>="2015-03-01")
  
  return(list(
    "forecast_data"=forecast_data,
    "tst"=tst)
  )
  
}

forecast_list = list()

forecast_list[["revenue_Corporate Income Taxes"]] = forecast_component(nowcast_corporate_receipts,nowcast_total_receipts,receipt_daily_df,"revenue_Corporate Income Taxes","corp")
forecast_list[["revenue_Estate and Gift Taxes"]] =forecast_component(nowcast_estate_receipts,nowcast_total_receipts,receipt_daily_df,"revenue_Estate and Gift Taxes",'estate')
forecast_list[["revenue_Excise Taxes"]]=forecast_component(nowcast_excise_receipts,nowcast_total_receipts,receipt_daily_df,"revenue_Excise Taxes","excise")
forecast_list[["revenue_Customs Duties"]]=forecast_component(nowcast_customs_receipts,nowcast_total_receipts,receipt_daily_df,"revenue_Customs Duties","customs")
forecast_list[["revenue_Individual Income Taxes"]]=forecast_component(nowcast_individual_receipts,nowcast_total_receipts,receipt_daily_df,"revenue_Individual Income Taxes","individ")
forecast_list[["revenue_Miscellaneous Receipts"]]=forecast_component(nowcast_misc_receipts,nowcast_total_receipts,receipt_daily_df,"revenue_Miscellaneous Receipts","misc")
forecast_list[["revenue_Payroll Taxes"]]=forecast_component(nowcast_payroll_receipts,nowcast_total_receipts,receipt_daily_df,"revenue_Payroll Taxes","payroll")

forecast_list[["outlay_Medicaid"]] = forecast_component(nowcast_medicaid_outlay,nowcast_total_outlays,outlay_daily_df,"outlay_Medicaid","medicaid")
forecast_list[["outlay_Medicare"]] =forecast_component(nowcast_medicare_outlay,nowcast_total_outlays,outlay_daily_df,"outlay_Medicare",'medicare')
forecast_list[["outlay_Net Interest"]]=forecast_component(nowcast_interest_outlay,nowcast_total_outlays,outlay_daily_df,"outlay_Net Interest","interest")
forecast_list[["outlay_Social Security"]]=forecast_component(nowcast_ss_outlay,nowcast_total_outlays,outlay_daily_df,"outlay_Social Security","ss")
forecast_list[["outlay_Defense Discretionary"]]=forecast_component(nowcast_defense_outlay,nowcast_total_outlays,outlay_daily_df,"outlay_Defense Discretionary","defense")
forecast_list[["outlay_Other"]]=forecast_component(nowcast_other_outlay,nowcast_total_outlays,outlay_daily_df,"outlay_Other","other")

# tst_list = list()
# 
# tst_list[["revenue_Corporate Income Taxes"]] = forecast_component(nowcast_corporate_receipts,nowcast_total_receipts,receipt_daily_df,"revenue_Corporate Income Taxes","corp")[[2]]
# tst_list[["revenue_Estate and Gift Taxes"]] =forecast_component(nowcast_estate_receipts,nowcast_total_receipts,receipt_daily_df,"revenue_Estate and Gift Taxes",'estate')[[2]]
# tst_list[["revenue_Excise Taxes"]]=forecast_component(nowcast_excise_receipts,nowcast_total_receipts,receipt_daily_df,"revenue_Excise Taxes","excise")[[2]]
# tst_list[["revenue_Customs Duties"]]=forecast_component(nowcast_customs_receipts,nowcast_total_receipts,receipt_daily_df,"revenue_Customs Duties","customs")[[2]]
# tst_list[["revenue_Individual Income Taxes"]]=forecast_component(nowcast_individual_receipts,nowcast_total_receipts,receipt_daily_df,"revenue_Individual Income Taxes","individ")[[2]]
# tst_list[["revenue_Miscellaneous Receipts"]]=forecast_component(nowcast_misc_receipts,nowcast_total_receipts,receipt_daily_df,"revenue_Miscellaneous Receipts","misc")[[2]]
# tst_list[["revenue_Payroll Taxes"]]=forecast_component(nowcast_payroll_receipts,nowcast_total_receipts,receipt_daily_df,"revenue_Payroll Taxes","payroll")[[2]]
# 
# tst_list[["outlay_Medicaid"]] = forecast_component(nowcast_medicaid_outlay,nowcast_total_outlays,outlay_daily_df,"outlay_Medicaid","medicaid")[[2]]
# tst_list[["outlay_Medicare"]] =forecast_component(nowcast_medicare_outlay,nowcast_total_outlays,outlay_daily_df,"outlay_Medicare",'medicare')[[2]]
# tst_list[["outlay_Net Interest"]]=forecast_component(nowcast_interest_outlay,nowcast_total_outlays,outlay_daily_df,"outlay_Net Interest","interest")[[2]]
# tst_list[["outlay_Social Security"]]=forecast_component(nowcast_ss_outlay,nowcast_total_outlays,outlay_daily_df,"outlay_Social Security","ss")[[2]]
# tst_list[["outlay_Defense Discretionary"]]=forecast_component(nowcast_defense_outlay,nowcast_total_outlays,outlay_daily_df,"outlay_Defense Discretionary","defense")[[2]]
# tst_list[["outlay_Other"]]=forecast_component(nowcast_other_outlay,nowcast_total_outlays,outlay_daily_df,"outlay_Other","other")[[2]]


forecast_list1 = bind_rows(lapply(forecast_list, "[[", 1))

tail(na.omit(forecast_list[['outlay_Medicaid']]$actual),1)+
  tail(na.omit(forecast_list[['outlay_Medicare']]$actual),1)+
  tail(na.omit(forecast_list[['outlay_Net Interest']]$actual),1)+
  tail(na.omit(forecast_list[['outlay_Social Security']]$actual),1)+
  tail(na.omit(forecast_list[['outlay_Defense Discretionary']]$actual),1)+
  tail(na.omit(forecast_list[['outlay_Other']]$actual),1)

outlay_daily_df_groups = dts %>% 
  filter(!(grepl("Tax|Receipt|Duties|TTL Transfer",cbo_category))&!grepl("to Depositaries",transaction_catg)) %>% 
  mutate(group=case_when(
    transaction_catg%in%c("Dept of Defense (DoD) - misc","DoD - Military Active Duty Pay","DoD - Military Retirement","Defense Vendor Payments (EFT)")~"defense",
    transaction_catg%in%c("SSA - Benefits Payments","Social Security Benefits (EFT)")~"ss",
    transaction_catg%in%c("HHS - Grants to States for Medicaid","Medicaid")~"medicaid",
    transaction_catg%in%c("HHS - Medicare Prescription Drugs","HHS - Federal Supple Med Insr Trust Fund","HHS - Federal Hospital Insr Trust Fund","Medicare Advantage - Part C&D Payments","Medicare and Other CMS Payments")~"medicare",
    transaction_catg%in%c("Interest on Treasury Securities","Interest recd from cash investments")~"interest",
    TRUE~"other"
  )) %>% 
  group_by(group,record_fiscal_year,record_calendar_month,record_calendar_day) %>% 
  summarize(date=record_date[1],
            total_mtd=sum(transaction_mtd_amt,na.rm=TRUE),
            total_day=sum(transaction_today_amt,na.rm=TRUE)) %>% 
  group_by(group,record_fiscal_year,record_calendar_month) %>% 
  mutate(total1=total_mtd[n()]*-1/1000,
         share=total_mtd/total1*-1/1000) %>% 
  arrange(date) %>% 
  mutate(record_calendar_day_perc=(as.numeric(record_calendar_day))/as.numeric(days_in_month(date)),
         inv_record_calendar_day=1-record_calendar_day_perc,
         actual_date=date,
         date=floor_date(date,"month"))  %>% 
  left_join(nowcast_deficit %>% select(date,pred_total=outlays,actual_total=actual_outlays)) %>% 
  left_join(bind_rows(
    nowcast_medicaid_outlay[[3]] %>% select(date,actual,pred) %>% mutate(group="medicaid"),
    nowcast_medicare_outlay[[3]] %>% select(date,actual,pred) %>% mutate(group="medicare"),
    nowcast_ss_outlay[[3]] %>% select(date,actual,pred) %>% mutate(group="ss"),
    nowcast_defense_outlay[[3]] %>% select(date,actual,pred) %>% mutate(group="defense"),
    nowcast_interest_outlay[[3]] %>% select(date,actual,pred) %>% mutate(group="interest"),
    bind_rows(nowcast_other_outlay[[3]]) %>% group_by(date) %>% summarize(pred=sum(pred),actual=sum(actual),group="other")
  ),by=c("date","group")) %>% 
  mutate(record_calendar_day=as.numeric(record_calendar_day))

outlay_daily_df_groups = outlay_daily_df_groups %>% 
  rowwise() %>% 
  mutate(avg_share=weighted.mean(outlay_daily_df_groups[outlay_daily_df_groups$group==group&outlay_daily_df_groups$record_calendar_month==record_calendar_month&outlay_daily_df_groups$record_calendar_day<=record_calendar_day,] %>% group_by(record_fiscal_year) %>% slice(n()) %>% ungroup() %>% select(share) %>% pull(),weights=outlay_daily_df_groups[outlay_daily_df_groups$group==group&outlay_daily_df_groups$record_calendar_month==record_calendar_month&outlay_daily_df_groups$record_calendar_day<=record_calendar_day,] %>% group_by(record_fiscal_year) %>% slice(n()) %>% ungroup() %>% select(share) %>% pull())) %>% 
  ungroup() %>% 
  mutate(extrap_total=(total_mtd/avg_share)*(-1/1000))

outlay_daily_df_groups = outlay_daily_df_groups %>% 
  left_join(outlay_daily_df_groups %>% 
              filter(share==1&record_fiscal_year>=2015) %>% 
              group_by(record_fiscal_year,group) %>% 
              summarize(scale_factor=mean(actual/extrap_total,na.rm=TRUE))) %>% 
  ungroup() %>% 
  fill(scale_factor,.direction="downup") %>% 
  mutate(extrap_total=extrap_total*scale_factor) %>% 
  rowwise() %>% 
  mutate(extrp_total_avg=mean(c(extrap_total,pred))) %>% 
  ungroup()

daily_receipts = data.frame()

for(month in unique(forecast_list1 %>% filter(!is.na(mean)) %>% select(date) %>% pull())){
  
  var_type="mean"
  
  dat1 = forecast_list1 %>% 
    filter(date==month) %>% 
    pivot_wider(values_from=c(mean,lower,upper),names_from=var)
  
  for(var in colnames(cbo_monthly_proj)[3:9]){
    
    var_forecast = data.frame(date=as.Date(month),record_calendar_day=as.numeric(day(seq(as.Date(month),ceiling_date(as.Date(month,by=1),"month")-1,by=1)))) %>% 
      left_join(dat1 %>% 
                  select(date,paste0(var_type,"_",var))) %>% 
      left_join(receipt_daily_df %>% 
                  filter(record_calendar_month==month(as.Date(month))) %>% 
                  distinct(record_calendar_day,avg_share) %>% 
                  rename(avg_share_receipt=avg_share)%>% 
                  mutate(record_calendar_day=as.numeric(record_calendar_day))) %>% 
      mutate(avg_share_receipt=avg_share_receipt/avg_share_receipt[n()],
             receipt_mtd_amt=!!sym(paste0(var_type,"_",var))*avg_share_receipt,
             receipt_day_amt=receipt_mtd_amt-lag(receipt_mtd_amt,1),
             receipt_day_amt=ifelse(record_calendar_day==min(record_calendar_day),receipt_mtd_amt,receipt_day_amt)) %>% 
      select(date,record_calendar_day,receipt_day_amt) %>% 
      mutate(var=var)
    
    daily_receipts = bind_rows(daily_receipts,var_forecast)
    
  }
}

daily_outlays = data.frame() # you could make SS more exact by splitting monthly outlay into four equal parts

for(month in unique(forecast_list1 %>% filter(!is.na(mean)) %>% select(date) %>% pull())){
  
  var_type="mean"
  
  dat1 = forecast_list1 %>% 
    filter(date==month) %>% 
    pivot_wider(values_from=c(mean,lower,upper),names_from=var)
  
  for(var in colnames(cbo_monthly_proj)[11:16]){
    
    var_forecast = data.frame(date=as.Date(month),record_calendar_day=as.numeric(day(seq(as.Date(month),ceiling_date(as.Date(month,by=1),"month")-1,by=1)))) %>% 
      left_join(dat1 %>% 
                  select(date,paste0(var_type,"_",var))) %>% 
      left_join(outlay_daily_df_groups %>% 
                  mutate(flag=case_when(
                    var=="outlay_Medicaid"&group=="medicaid"~1,
                    var=="outlay_Medicare"&group=="medicare"~1,
                    var=="outlay_Social Security"&group=="ss"~1,
                    var=="outlay_Net Interest"&group=="interest"~1,
                    var=="outlay_Defense Discretionary"&group=="defense"~1,
                    var=="outlay_Other"&group=="other"~1,
                    TRUE~0
                  )) %>% 
                  filter(record_calendar_month==month(as.Date(month))&flag==1) %>% 
                  distinct(record_calendar_day,avg_share) %>% 
                  rename(avg_share_outlay=avg_share) %>% 
                  mutate(record_calendar_day=as.numeric(record_calendar_day))) %>% 
      mutate(avg_share_outlay=avg_share_outlay/avg_share_outlay[n()],
             outlay_mtd_amt=!!sym(paste0(var_type,"_",var))*avg_share_outlay,
             outlay_day_amt=outlay_mtd_amt-lag(outlay_mtd_amt,1),
             outlay_day_amt=ifelse(record_calendar_day==min(record_calendar_day),outlay_mtd_amt,outlay_day_amt)) %>% 
      select(date,record_calendar_day,outlay_day_amt) %>% 
      mutate(var=var)
    
    daily_outlays = bind_rows(daily_outlays,var_forecast)
    
  }
}

daily_receipts_upper = data.frame()

for(month in unique(forecast_list1 %>% filter(!is.na(mean)) %>% select(date) %>% pull())){
  
  var_type="upper"
  
  dat1 = forecast_list1 %>% 
    filter(date==month) %>% 
    pivot_wider(values_from=c(mean,lower,upper),names_from=var)
  
  for(var in colnames(cbo_monthly_proj)[3:9]){
    
    var_forecast = data.frame(date=as.Date(month),record_calendar_day=as.numeric(day(seq(as.Date(month),ceiling_date(as.Date(month,by=1),"month")-1,by=1)))) %>% 
      left_join(dat1 %>% 
                  select(date,paste0(var_type,"_",var))) %>% 
      left_join(receipt_daily_df %>% 
                  filter(record_calendar_month==month(as.Date(month))) %>% 
                  distinct(record_calendar_day,avg_share) %>% 
                  rename(avg_share_receipt=avg_share)%>% 
                  mutate(record_calendar_day=as.numeric(record_calendar_day))) %>% 
      mutate(avg_share_receipt=avg_share_receipt/avg_share_receipt[n()],
             receipt_mtd_amt=!!sym(paste0(var_type,"_",var))*avg_share_receipt,
             receipt_day_amt=receipt_mtd_amt-lag(receipt_mtd_amt,1),
             receipt_day_amt=ifelse(record_calendar_day==min(record_calendar_day),receipt_mtd_amt,receipt_day_amt)) %>% 
      select(date,record_calendar_day,receipt_day_amt) %>% 
      mutate(var=var)
    
    daily_receipts_upper = bind_rows(daily_receipts_upper,var_forecast)
    
  }
}

daily_outlays_upper = data.frame()

for(month in unique(forecast_list1 %>% filter(!is.na(mean)) %>% select(date) %>% pull())){
  
  var="lower"
  
  dat1 = forecast_list1 %>% 
    filter(date==month) %>% 
    pivot_wider(values_from=c(mean,lower,upper),names_from=var)
  
  for(var in colnames(cbo_monthly_proj)[11:16]){
    
    var_forecast = data.frame(date=as.Date(month),record_calendar_day=as.numeric(day(seq(as.Date(month),ceiling_date(as.Date(month,by=1),"month")-1,by=1)))) %>% 
      left_join(dat1 %>% 
                  select(date,paste0(var_type,"_",var))) %>% 
      left_join(outlay_daily_df_groups %>% 
                  mutate(flag=case_when(
                    var=="outlay_Medicaid"&group=="medicaid"~1,
                    var=="outlay_Medicare"&group=="medicare"~1,
                    var=="outlay_Social Security"&group=="ss"~1,
                    var=="outlay_Net Interest"&group=="interest"~1,
                    var=="outlay_Defense Discretionary"&group=="defense"~1,
                    var=="outlay_Other"&group=="other"~1,
                    TRUE~0
                  )) %>% 
                  filter(record_calendar_month==month(as.Date(month))&flag==1) %>% 
                  distinct(record_calendar_day,avg_share) %>% 
                  rename(avg_share_outlay=avg_share) %>% 
                  mutate(record_calendar_day=as.numeric(record_calendar_day))) %>% 
      mutate(avg_share_outlay=avg_share_outlay/avg_share_outlay[n()],
             outlay_mtd_amt=!!sym(paste0(var_type,"_",var))*avg_share_outlay,
             outlay_day_amt=outlay_mtd_amt-lag(outlay_mtd_amt,1),
             outlay_day_amt=ifelse(record_calendar_day==min(record_calendar_day),outlay_mtd_amt,outlay_day_amt)) %>% 
      select(date,record_calendar_day,outlay_day_amt) %>% 
      mutate(var=var)
    
    daily_outlays_upper = bind_rows(daily_outlays_upper,var_forecast)
    
  }
}

daily_receipts_lower = data.frame()

for(month in unique(forecast_list1 %>% filter(!is.na(mean)) %>% select(date) %>% pull())){
  
  var_type="lower"
  
  dat1 = forecast_list1 %>% 
    filter(date==month) %>% 
    pivot_wider(values_from=c(mean,lower,upper),names_from=var)
  
  for(var in colnames(cbo_monthly_proj)[3:9]){
    
    var_forecast = data.frame(date=as.Date(month),record_calendar_day=as.numeric(day(seq(as.Date(month),ceiling_date(as.Date(month,by=1),"month")-1,by=1)))) %>% 
      left_join(dat1 %>% 
                  select(date,paste0(var_type,"_",var))) %>% 
      left_join(receipt_daily_df %>% 
                  filter(record_calendar_month==month(as.Date(month))) %>% 
                  distinct(record_calendar_day,avg_share) %>% 
                  rename(avg_share_receipt=avg_share)%>% 
                  mutate(record_calendar_day=as.numeric(record_calendar_day))) %>% 
      mutate(avg_share_receipt=avg_share_receipt/avg_share_receipt[n()],
             receipt_mtd_amt=!!sym(paste0(var_type,"_",var))*avg_share_receipt,
             receipt_day_amt=receipt_mtd_amt-lag(receipt_mtd_amt,1),
             receipt_day_amt=ifelse(record_calendar_day==min(record_calendar_day),receipt_mtd_amt,receipt_day_amt)) %>% 
      select(date,record_calendar_day,receipt_day_amt) %>% 
      mutate(var=var)
    
    daily_receipts_lower = bind_rows(daily_receipts_lower,var_forecast)
    
  }
}

daily_outlays_lower = data.frame()

for(month in unique(forecast_list1 %>% filter(!is.na(mean)) %>% select(date) %>% pull())){
  
  var="upper"
  
  dat1 = forecast_list1 %>% 
    filter(date==month) %>% 
    pivot_wider(values_from=c(mean,lower,upper),names_from=var)
  
  for(var in colnames(cbo_monthly_proj)[11:16]){
    
    var_forecast = data.frame(date=as.Date(month),record_calendar_day=as.numeric(day(seq(as.Date(month),ceiling_date(as.Date(month,by=1),"month")-1,by=1)))) %>% 
      left_join(dat1 %>% 
                  select(date,paste0(var_type,"_",var))) %>% 
      left_join(outlay_daily_df_groups %>% 
                  mutate(flag=case_when(
                    var=="outlay_Medicaid"&group=="medicaid"~1,
                    var=="outlay_Medicare"&group=="medicare"~1,
                    var=="outlay_Social Security"&group=="ss"~1,
                    var=="outlay_Net Interest"&group=="interest"~1,
                    var=="outlay_Defense Discretionary"&group=="defense"~1,
                    var=="outlay_Other"&group=="other"~1,
                    TRUE~0
                  )) %>% 
                  filter(record_calendar_month==month(as.Date(month))&flag==1) %>% 
                  distinct(record_calendar_day,avg_share) %>% 
                  rename(avg_share_outlay=avg_share) %>% 
                  mutate(record_calendar_day=as.numeric(record_calendar_day))) %>% 
      mutate(avg_share_outlay=avg_share_outlay/avg_share_outlay[n()],
             outlay_mtd_amt=!!sym(paste0(var_type,"_",var))*avg_share_outlay,
             outlay_day_amt=outlay_mtd_amt-lag(outlay_mtd_amt,1),
             outlay_day_amt=ifelse(record_calendar_day==min(record_calendar_day),outlay_mtd_amt,outlay_day_amt)) %>% 
      select(date,record_calendar_day,outlay_day_amt) %>% 
      mutate(var=var)
    
    daily_outlays_lower = bind_rows(daily_outlays_lower,var_forecast)
    
  }
}

daily_forecast = bind_rows(
  feb_forecast %>% 
    mutate(daily_deficit=(receipt_day_amt-outlay_day_amt)/1000) %>% 
    select(record_fiscal_year,record_calendar_month,record_calendar_day,daily_deficit) %>% 
    fill(record_fiscal_year,record_calendar_month),
  daily_outlays %>% 
    group_by(date,record_calendar_day) %>% 
    summarize(outlay_day_amt=sum(outlay_day_amt,na.rm=TRUE)) %>% 
    ungroup() %>% 
    left_join(daily_receipts %>% group_by(date,record_calendar_day) %>% 
                summarize(receipt_day_amt=sum(receipt_day_amt,na.rm=TRUE))) %>% 
    mutate(daily_deficit=receipt_day_amt-outlay_day_amt) %>% 
    select(date,record_calendar_day,daily_deficit) %>% 
    mutate(record_fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)),
           record_calendar_month=month(date)) %>% 
    select(-date)
)

daily_forecast_upper = bind_rows(
  feb_forecast %>% 
    mutate(daily_deficit=(receipt_day_amt-outlay_day_amt)/1000) %>% 
    select(record_fiscal_year,record_calendar_month,record_calendar_day,daily_deficit) %>% 
    fill(record_fiscal_year,record_calendar_month),
  daily_outlays_lower %>% 
    group_by(date,record_calendar_day) %>% 
    summarize(outlay_day_amt=sum(outlay_day_amt,na.rm=TRUE)) %>% 
    ungroup() %>% 
    left_join(daily_receipts_upper %>% group_by(date,record_calendar_day) %>% 
                summarize(receipt_day_amt=sum(receipt_day_amt,na.rm=TRUE))) %>% 
    mutate(daily_deficit=receipt_day_amt-outlay_day_amt) %>% 
    select(date,record_calendar_day,daily_deficit) %>% 
    mutate(record_fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)),
           record_calendar_month=month(date)) %>% 
    select(-date)
)

daily_forecast_lower = bind_rows(
  feb_forecast %>% 
    mutate(daily_deficit=(receipt_day_amt-outlay_day_amt)/1000) %>% 
    select(record_fiscal_year,record_calendar_month,record_calendar_day,daily_deficit) %>% 
    fill(record_fiscal_year,record_calendar_month),
  daily_outlays_upper %>% 
    group_by(date,record_calendar_day) %>% 
    summarize(outlay_day_amt=sum(outlay_day_amt,na.rm=TRUE)) %>% 
    ungroup() %>% 
    left_join(daily_receipts_lower %>% group_by(date,record_calendar_day) %>% 
                summarize(receipt_day_amt=sum(receipt_day_amt,na.rm=TRUE))) %>% 
    mutate(daily_deficit=receipt_day_amt-outlay_day_amt) %>% 
    select(date,record_calendar_day,daily_deficit) %>% 
    mutate(record_fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)),
           record_calendar_month=month(date)) %>% 
    select(-date)
)

