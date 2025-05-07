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
conflicted::conflicts_prefer(dplyr::select)

set.seed(178)

# write_csv(op_cash_dep_withdraw %>% 
#             distinct(transaction_type,transaction_catg,transaction_catg_desc) %>% 
#             left_join(daily_categories %>% mutate(transaction_catg_desc="null")),
#           "Data/Processing/daily_categories1.csv")

daily_categories = read_csv("Data/Processing/daily_categories1.csv")

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



dts = op_cash_dep_withdraw %>% 
  left_join(daily_categories) %>% # we want to keep only the things we are able to map
  filter(!is.na(cbo_category)) %>% # get rid of the ones we cant map, mostly are internal transfers
  distinct(record_date,account_type,transaction_type,transaction_catg,transaction_today_amt,.keep_all = TRUE) %>% 
  mutate(transaction_today_amt=ifelse(transaction_type=="Withdrawals",as.numeric(transaction_today_amt)*-1,as.numeric(transaction_today_amt)), # make withdrawawls negative
         transaction_mtd_amt=ifelse(transaction_type=="Withdrawals",as.numeric(transaction_mtd_amt)*-1,as.numeric(transaction_mtd_amt)))

imputed_daily_receipts = dts %>% # TODO: replace with a better way to disaggregate, and disaggregate current month
  filter(cbo_category%in%c("Customs Duties","Estate and Gift Taxes","Miscellaneous Receipts",
                                                                               "Individual Income Taxes","Excuse Taxes","Corporate Income Taxes",
                                                                               "Payroll Taxes")) %>% # keep only the categories receipt categories
  group_by(record_calendar_year,record_calendar_month,record_calendar_day,cbo_category) %>% 
  summarize(today_amt=sum(transaction_today_amt)) %>% # get receipts by category
  group_by(record_calendar_year,record_calendar_month,record_calendar_day) %>% #
  mutate(today_share=today_amt/sum(today_amt)) %>% # get share of total spending in the month
  ungroup() %>% 
  mutate(record_calendar_day=as.numeric(record_calendar_day)) %>% 
  pivot_wider(id_cols=c(record_calendar_year,record_calendar_month:record_calendar_day),names_from=cbo_category,values_from=today_share) %>% 
  mutate_at(4:10,~replace_na(.,0))
  
# test = tax_deposits %>% 
#   filter(record_calendar_year==2023&record_calendar_month==1) %>% 
#   group_by(record_calendar_year,record_calendar_month,record_calendar_day,group) %>% 
#   summarize(group_amt=sum(today_amt),
#             group_mtd_amt=sum(mtd_amt)) %>% 
#   mutate(group_amt=case_when(
#     grepl("Corpor",group)~group_amt*1.427,
#     grepl("Indi",group)~group_amt*1.165,
#     grepl("Excise",group)~group_amt*0.746,
#     grepl("Misc",group)~group_amt*46.52
#   ),
#   group_mtd_amt=case_when(
#     grepl("Corpor",group)~group_mtd_amt*1.427,
#     grepl("Indi",group)~group_mtd_amt*1.165,
#     grepl("Excise",group)~group_mtd_amt*0.746,
#     grepl("Misc",group)~group_mtd_amt*46.52
#   )) %>% 
#   group_by(record_calendar_year,record_calendar_month,record_calendar_day) %>% 
#   mutate(total_amt=sum(group_mtd_amt)) %>% 
#   ungroup() %>% 
#   left_join(receipt_daily_df %>% filter(date=="2023-01-01") %>% mutate(record_calendar_day=as.numeric(record_calendar_day)))

tax_days = read_csv("Data/Raw/tax_days_2000_2040.csv") %>% 
  mutate(`Tax Day`=gsub("\\(COVID-19 extension\\)","",`Tax Day`),
         date=paste0(`Tax Day`," ",Year),date=as.Date(date,format="%B %d %Y")) %>% 
  mutate(tax_day=1) %>% 
  select(date,tax_day)

receipt_daily_df = dts %>% 
  filter((grepl("Tax|Receipt|Duties",cbo_category))&!grepl("from Depositaries",transaction_catg)) %>% # not able to differentiate when "from depositaries"
  group_by(record_fiscal_year,record_calendar_month,record_calendar_day) %>% 
  summarize(date=record_date[1],
            total_day=sum(transaction_today_amt,na.rm=TRUE)) %>% 
  group_by(record_fiscal_year,record_calendar_month) %>% 
  mutate(total_mtd=cumsum(total_day)) %>% # cumulative sum of receipts throughout the month
  mutate(total1=total_mtd[n()]/1000,
         share=total_mtd/total1/1000) %>% 
  arrange(date) %>% 
  mutate(record_calendar_day_perc=(as.numeric(record_calendar_day))/as.numeric(days_in_month(date)),
         inv_record_calendar_day=1-record_calendar_day_perc,
         actual_date=date,
         date=floor_date(date,"month"))  %>% 
  left_join(nowcast_deficit %>% select(date,pred=receipts,actual=actual_receipts)) %>% 
  mutate(record_calendar_day=as.numeric(record_calendar_day),
         record_calendar_month=as.numeric(record_calendar_month)) %>% 
  left_join(tax_days,by=c("actual_date"="date")) %>% 
  group_by(date) %>% 
  fill(tax_day,.direction="down") %>% 
  mutate(tax_day=ifelse(is.na(tax_day),0,tax_day)) %>% 
  ungroup()

# TODO: see what method does better at estimating the % of money up to given point in month
# receipt_daily_df = receipt_daily_df %>%
#   mutate(extrap_total=extrap_total*tidy(lm_robust(actual~total1-1,receipt_daily_df %>%
#                                                     filter(date<max(receipt_daily_df$date))  %>%
#                                                     group_by(date) %>%
#                                                     slice(n())))[1,2]) # this connects the total amount estimated based on only the data in the DTS to the actual amount that we see in FRED at monthly level


receipt_daily_df = receipt_daily_df %>%
  ungroup() %>% 
  mutate(avg_share=predict(lm(share~record_calendar_day*factor(record_calendar_month)+factor(record_calendar_day):factor(tax_day),
                              receipt_daily_df %>% filter(date<max(receipt_daily_df$date)&!(date%in%c("2020-03-01","2020-06-01","2021-03-01")))),receipt_daily_df)) %>% 
  ungroup() %>% 
  mutate(extrap_total=(total_mtd/avg_share)*(1/1000)) %>% 
  rowwise() %>% 
  mutate(extrap_total=mean(c(pred,extrap_total),na.rm=TRUE), # TODO: test if it makes sense to set min at the amount of revenue already seen in the data
         extrap_total=min(c(extrap_total,
                            quantile(c(receipts_fred %>% 
                                         filter(date>="2022-01-01"&date<=(Sys.Date() %m+% years(3))) %>% 
                                         pull(value),
                                       cbo_monthly_proj$revenue_Total[cbo_monthly_proj$year<=(year(Sys.Date())+3)&cbo_monthly_proj$year>=2022]),
                                     1,na.rm=TRUE)))) %>% 
  ungroup()

receipt_daily_df = receipt_daily_df %>% 
  left_join(receipt_daily_df %>% 
              filter(share==1&record_fiscal_year>=2015) %>% 
              group_by(record_fiscal_year) %>% 
              summarize(scale_factor_year=mean(actual/extrap_total,na.rm=TRUE))) %>% 
  left_join(receipt_daily_df %>% 
              filter(record_fiscal_year>=2015) %>% 
              group_by(record_fiscal_year,record_calendar_month) %>% 
              summarize(scale_factor_month=mean(actual[n()]/extrap_total[n()],na.rm=TRUE))) %>% 
  ungroup() %>% 
  mutate_at(vars(scale_factor_year,scale_factor_month),~ifelse(is.nan(.),NA,.)) %>% 
  fill(scale_factor_year,.direction="downup") %>% 
  group_by(record_fiscal_year,record_calendar_month) %>% 
  mutate(scaled_total=ifelse(!is.na(actual),extrap_total*scale_factor_month,extrap_total*scale_factor_year),
         scaled_total_day=ifelse(!is.na(actual),total_day*(actual[n()]/total_mtd[n()]),total_day/1000*scale_factor_year), # TODO: might need to make more exact
         scaled_total_mtd=ifelse(!is.na(actual),total_mtd*(actual[n()]/total_mtd[n()]),total_mtd/1000*scale_factor_year),
         extrap_total=ifelse(!is.na(actual),extrap_total*scale_factor_year,extrap_total*scale_factor_year)) %>%  # keep column that is the pure prediction
  rowwise() %>% 
  mutate(scaled_total=ifelse(!is.na(actual),scaled_total,extrap_total)) %>% 
  ungroup()

# repeat for outlays
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
  mutate(record_calendar_day=as.numeric(record_calendar_day),
         record_calendar_month=as.numeric(record_calendar_month)) %>% 
  left_join(tax_days,by=c("actual_date"="date")) %>% 
  group_by(date) %>% 
  fill(tax_day,.direction="down") %>% 
  mutate(tax_day=ifelse(is.na(tax_day),0,tax_day)) %>% 
  ungroup()

outlay_daily_df = outlay_daily_df %>%
  ungroup() %>% 
  mutate(avg_share=predict(lm(share~record_calendar_day*factor(record_calendar_month)+factor(record_calendar_day):factor(tax_day),
                              outlay_daily_df %>% filter(date<max(outlay_daily_df$date)&!(date%in%c("2020-03-01","2020-06-01","2021-03-01")))),outlay_daily_df)) %>% 
  ungroup() %>% 
  mutate(extrap_total=(total_mtd/avg_share)*(-1/1000)) %>% 
  rowwise() %>% 
  mutate(extrap_total=mean(c(pred,extrap_total),na.rm=TRUE), # TODO: test if it makes sense to set min at the amount of revenue already seen in the data
         extrap_total=min(c(extrap_total,
                            quantile(c(outlays_fred %>% 
                                         filter(date>="2022-01-01"&date<=(Sys.Date() %m+% years(3))) %>% 
                                         pull(value),
                                       cbo_monthly_proj$outlay_Total[cbo_monthly_proj$year<=(year(Sys.Date())+3)&cbo_monthly_proj$year>=2022]),
                                     1,na.rm=TRUE)))) %>% 
  ungroup()


outlay_daily_df = outlay_daily_df %>% 
  left_join(outlay_daily_df %>% 
              filter(date<max(date)) %>% 
              filter(share==1&record_fiscal_year>=2015) %>% 
              group_by(record_fiscal_year) %>% 
              summarize(scale_factor_year=mean(actual/extrap_total,na.rm=TRUE))) %>% 
  left_join(outlay_daily_df %>% 
              filter(date<max(date)) %>% 
              filter(record_fiscal_year>=2015) %>% 
              group_by(record_fiscal_year,record_calendar_month) %>% 
              summarize(scale_factor_month=mean(actual[n()]/extrap_total[n()],na.rm=TRUE))) %>% 
  ungroup() %>% 
  mutate_at(vars(scale_factor_year,scale_factor_month),~ifelse(is.nan(.),NA,.)) %>% 
  fill(scale_factor_year,.direction="downup") %>% 
  group_by(record_fiscal_year,record_calendar_month) %>% 
  mutate(scaled_total=ifelse(!is.na(actual),extrap_total*scale_factor_month,extrap_total*scale_factor_year),
         scaled_total_day=ifelse(!is.na(actual),total_day*-1*(actual[n()]/(total_mtd[n()])),total_day/1000*scale_factor_year),
         scaled_total_mtd=ifelse(!is.na(actual),total_mtd*-1*(actual[n()]/(total_mtd[n()])),total_mtd/1000*scale_factor_year),
         extrap_total=ifelse(!is.na(actual),extrap_total*scale_factor_year,extrap_total*scale_factor_year)) %>%  # keep column that is the pure prediction
  rowwise() %>% 
  mutate(scaled_total=ifelse(!is.na(actual),scaled_total,extrap_total)) %>% 
  ungroup()

ggplot(outlay_daily_df %>% filter(date=="2025-01-01"),aes(x=actual_date)) +
  geom_line(aes(y=actual,color="Actual")) +
  geom_line(aes(y=extrap_total,color="Daily estimate"))  +
  geom_line(aes(y=scaled_total,color="Scaled estimate"))

feb_forecast = data.frame()
for(dat in unique(as.character(unique(outlay_daily_df$date[is.na(outlay_daily_df$actual)])),as.character(floor_date(Sys.Date(),"month")))){
  
  dat1 = as.Date(dat)
  
  tmp_df = bind_cols(
    outlay_daily_df %>% 
      select(outlay_day_amt=scaled_total_day,outlay_mtd_amt=scaled_total_mtd,record_fiscal_year:record_calendar_day,pred_outlay=pred,actual_outlay=actual,outlay_extrap_total=extrap_total) %>% 
      mutate(outlay_day_amt=-1*outlay_day_amt,
             outlay_mtd_amt=-1*outlay_mtd_amt),
    receipt_daily_df %>% 
      select(receipt_day_amt=scaled_total_day,receipt_mtd_amt=scaled_total_mtd,pred_receipt=pred,actual_receipt=actual,receipts_extrap_total=extrap_total) %>% 
      mutate(receipt_day_amt=receipt_day_amt,
             receipt_mtd_amt=receipt_mtd_amt)
  ) %>% 
    mutate(date = as.Date(paste0(year(dat1),"-",record_calendar_month,"-",record_calendar_day))) %>% 
    filter(record_fiscal_year==ifelse(month(dat1)>=10,year(dat1)+1,year(dat1))&as.numeric(record_calendar_month)==month(dat1)) 
    
  if(nrow(tmp_df)==0){
    
    tmp_df = tmp_df %>% 
      select(-c(pred_receipt,pred_outlay)) %>% 
      full_join(nowcast_deficit %>% filter(date==dat1) %>% select(date,pred_outlay=outlays,pred_receipt=receipts)) %>% 
      mutate(record_calendar_day=day(date),
             record_calendar_month=month(date),
             record_fiscal_year=ifelse(month(dat1)>=10,year(dat1)+1,year(dat1)),
             outlay_extrap_total=pred_outlay,
             receipts_extrap_total=pred_receipt)
    
    tmp_df = tmp_df %>% 
      bind_rows(data.frame(record_calendar_day=as.numeric(day(seq(max(tmp_df$date,na.rm=TRUE)+1,ceiling_date(tmp_df$date[1],"month")-1,by=1))))) %>% 
      fill(record_calendar_month,record_fiscal_year,.direction="down") %>% 
      mutate(date=as.Date(paste0(year(date[1]),"-",month(date[1]),"-",record_calendar_day))) %>% 
      left_join(tax_days,by=c("date"="date")) %>% 
      fill(tax_day,.direction="down") %>% 
      mutate(tax_day=ifelse(is.na(tax_day),0,tax_day)) %>% 
      ungroup() 
    
    tmp_df = tmp_df %>% 
      mutate(avg_share_outlay=predict(lm(share~record_calendar_day*factor(record_calendar_month)+factor(record_calendar_day):factor(tax_day),
                                         outlay_daily_df %>% filter(date<max(outlay_daily_df$date))),tmp_df),
             avg_share_receipt=predict(lm(share~record_calendar_day*factor(record_calendar_month)+factor(record_calendar_day):factor(tax_day),
                                          receipt_daily_df %>% filter(date<max(receipt_daily_df$date))),tmp_df),
             avg_share_outlay=ifelse(date==(ceiling_date(tmp_df$date[1],"month")-1),1,avg_share_outlay),
             avg_share_receipt=ifelse(date==(ceiling_date(tmp_df$date[1],"month")-1),1,avg_share_receipt)) %>% 
      fill(outlay_extrap_total,receipts_extrap_total,pred_receipt,pred_outlay,.direction="down") %>% 
      ungroup() %>% 
      mutate(outlay_mtd_amt=avg_share_outlay*outlay_extrap_total,
             receipt_mtd_amt=avg_share_receipt*receipts_extrap_total,
             outlay_mtd_amt=ifelse(is.na(outlay_mtd_amt),outlay_extrap_total*avg_share_outlay,outlay_mtd_amt),
             outlay_day_amt=outlay_mtd_amt-lag(outlay_mtd_amt,1),
             outlay_day_amt=ifelse(record_calendar_day==min(record_calendar_day),outlay_mtd_amt,outlay_day_amt),
             receipt_mtd_amt=ifelse(is.na(receipt_mtd_amt),receipts_extrap_total*avg_share_receipt,receipt_mtd_amt),
             receipt_day_amt=receipt_mtd_amt-lag(receipt_mtd_amt,1),
             receipt_day_amt=ifelse(record_calendar_day==min(record_calendar_day),receipt_mtd_amt,receipt_day_amt))
    
    

  } else{
  if((max(tmp_df$date,na.rm=TRUE))<(ceiling_date(tmp_df$date[1],"month")-1)){ # testing if we have the last day of the month. If we have the last day of the month then we dont need to add the missing days
    
    tmp_df = tmp_df %>% 
      bind_rows(data.frame(record_calendar_day=as.numeric(day(seq(min(tmp_df$date,na.rm=TRUE),max(tmp_df$date)-1,by=1)))) %>% filter(!(record_calendar_day%in%tmp_df$record_calendar_day))) %>% 
      arrange(record_calendar_day) %>% 
      fill(outlay_mtd_amt,receipt_mtd_amt,record_fiscal_year,record_calendar_month,pred_outlay,outlay_extrap_total,pred_receipt,receipts_extrap_total,.direction="downup") %>% 
      mutate(outlay_day_amt=ifelse(is.na(outlay_day_amt)&!is.na(outlay_mtd_amt),outlay_mtd_amt-dplyr::lag(outlay_mtd_amt,1),outlay_day_amt),
             receipt_day_amt=ifelse(is.na(receipt_day_amt)&!is.na(receipt_mtd_amt),receipt_mtd_amt-dplyr::lag(receipt_mtd_amt,1),receipt_day_amt)) %>% 
      bind_rows(data.frame(record_calendar_day=as.numeric(day(seq(max(tmp_df$date,na.rm=TRUE)+1,ceiling_date(tmp_df$date[1],"month")-1,by=1)))) %>% filter(!(record_calendar_day%in%tmp_df$record_calendar_day))) %>% 
      arrange(record_calendar_day) 
  
    
  }
    
    tmp_df = tmp_df %>% 
      fill(record_calendar_month,.direction="down") %>% 
      mutate(date=as.Date(paste0(year(date[1]),"-",month(date[1]),"-",record_calendar_day))) %>% 
      left_join(tax_days,by=c("date"="date")) %>% 
      fill(tax_day,.direction="down") %>% 
      mutate(tax_day=ifelse(is.na(tax_day),0,tax_day)) %>% 
      ungroup() 
    
    tmp_df = tmp_df %>% 
      mutate(avg_share_outlay=predict(lm(share~record_calendar_day*factor(record_calendar_month)+factor(record_calendar_day):factor(tax_day),
                                         outlay_daily_df %>% filter(date<max(outlay_daily_df$date))),tmp_df),
             avg_share_receipt=predict(lm(share~record_calendar_day*factor(record_calendar_month)+factor(record_calendar_day):factor(tax_day),
                                          receipt_daily_df %>% filter(date<max(receipt_daily_df$date))),tmp_df),
             avg_share_outlay=ifelse(date==(ceiling_date(tmp_df$date[1],"month")-1),1,avg_share_outlay),
             avg_share_receipt=ifelse(date==(ceiling_date(tmp_df$date[1],"month")-1),1,avg_share_receipt)) %>% 
      fill(outlay_extrap_total,receipts_extrap_total,record_fiscal_year,pred_receipt,pred_outlay,.direction="down") %>% 
      ungroup() %>% 
      mutate(outlay_mtd_amt=outlay_mtd_amt*tail(na.omit(outlay_extrap_total),1)/(tail(na.omit(outlay_mtd_amt),1)/avg_share_outlay[max(which(!is.na(outlay_mtd_amt)))]),
             receipt_mtd_amt=receipt_mtd_amt*tail(na.omit(receipts_extrap_total),1)/(tail(na.omit(receipt_mtd_amt),1)/avg_share_receipt[max(which(!is.na(receipt_mtd_amt)))]),
             outlay_mtd_amt=ifelse(is.na(outlay_mtd_amt),outlay_extrap_total*avg_share_outlay,outlay_mtd_amt),
             outlay_day_amt=outlay_mtd_amt-lag(outlay_mtd_amt,1),
             outlay_day_amt=ifelse(record_calendar_day==min(record_calendar_day),outlay_mtd_amt,outlay_day_amt),
             receipt_mtd_amt=ifelse(is.na(receipt_mtd_amt),receipts_extrap_total*avg_share_receipt,receipt_mtd_amt),
             receipt_day_amt=receipt_mtd_amt-lag(receipt_mtd_amt,1),
             receipt_day_amt=ifelse(record_calendar_day==min(record_calendar_day),receipt_mtd_amt,receipt_day_amt))
    
  }
  
  
  feb_forecast = bind_rows(feb_forecast,tmp_df)
   
}

#### extend forecast using ARIMA

forecast_list = list()

forecast_list[["revenue_Corporate Income Taxes"]] = forecast_component(nowcast_corporate_receipts,nowcast_total_receipts,receipt_daily_df,"revenue_Corporate Income Taxes","corp",c(1,1,1))[[1]]
forecast_list[["revenue_Estate and Gift Taxes"]] =forecast_component(nowcast_estate_receipts,nowcast_total_receipts,receipt_daily_df,"revenue_Estate and Gift Taxes",'estate',c(1,1,1))[[1]]
forecast_list[["revenue_Excise Taxes"]]=forecast_component(nowcast_excise_receipts,nowcast_total_receipts,receipt_daily_df,"revenue_Excise Taxes","excise",c(1,1,1))[[1]]
forecast_list[["revenue_Customs Duties"]]=forecast_component(nowcast_customs_receipts,nowcast_total_receipts,receipt_daily_df,"revenue_Customs Duties","customs",c(1,1,1))[[1]]
forecast_list[["revenue_Individual Income Taxes"]]=forecast_component(nowcast_individual_receipts,nowcast_total_receipts,receipt_daily_df,"revenue_Individual Income Taxes","individ",c(1,1,1))[[1]]
forecast_list[["revenue_Miscellaneous Receipts"]]=forecast_component(nowcast_misc_receipts,nowcast_total_receipts,receipt_daily_df,"revenue_Miscellaneous Receipts","misc",c(1,1,1))[[1]]
forecast_list[["revenue_Payroll Taxes"]]=forecast_component(nowcast_payroll_receipts,nowcast_total_receipts,receipt_daily_df,"revenue_Payroll Taxes","payroll",c(1,1,1))[[1]]

forecast_list[["outlay_Medicaid"]] = forecast_component(nowcast_medicaid_outlay,nowcast_total_outlays,outlay_daily_df,"outlay_Medicaid","medicaid",c(1,1,1))[[1]]
forecast_list[["outlay_Medicare"]] =forecast_component(nowcast_medicare_outlay,nowcast_total_outlays,outlay_daily_df,"outlay_Medicare",'medicare',c(1,1,1))[[1]]
forecast_list[["outlay_Net Interest"]]=forecast_component(nowcast_interest_outlay,nowcast_total_outlays,outlay_daily_df,"outlay_Net Interest","interest",c(1,1,1))[[1]]
forecast_list[["outlay_Social Security"]]=forecast_component(nowcast_ss_outlay,nowcast_total_outlays,outlay_daily_df,"outlay_Social Security","ss",c(1,1,1))[[1]]
forecast_list[["outlay_Defense Discretionary"]]=forecast_component(nowcast_defense_outlay,nowcast_total_outlays,outlay_daily_df,"outlay_Defense Discretionary","defense",c(1,1,1))[[1]]
forecast_list[["outlay_Other"]]=forecast_component(nowcast_other_outlay,nowcast_total_outlays,outlay_daily_df,"outlay_Other","other",c(6,1,0))[[1]]

tst_list = list()

tst_list[["revenue_Corporate Income Taxes"]] = forecast_component(nowcast_corporate_receipts,nowcast_total_receipts,receipt_daily_df,"revenue_Corporate Income Taxes","corp",c(1,1,1))[[2]]
tst_list[["revenue_Estate and Gift Taxes"]] =forecast_component(nowcast_estate_receipts,nowcast_total_receipts,receipt_daily_df,"revenue_Estate and Gift Taxes",'estate',c(1,1,1))[[2]]
tst_list[["revenue_Excise Taxes"]]=forecast_component(nowcast_excise_receipts,nowcast_total_receipts,receipt_daily_df,"revenue_Excise Taxes","excise",c(1,1,1))[[2]]
tst_list[["revenue_Customs Duties"]]=forecast_component(nowcast_customs_receipts,nowcast_total_receipts,receipt_daily_df,"revenue_Customs Duties","customs",c(1,1,1))[[2]]
tst_list[["revenue_Individual Income Taxes"]]=forecast_component(nowcast_individual_receipts,nowcast_total_receipts,receipt_daily_df,"revenue_Individual Income Taxes","individ",c(1,1,1))[[2]]
tst_list[["revenue_Miscellaneous Receipts"]]=forecast_component(nowcast_misc_receipts,nowcast_total_receipts,receipt_daily_df,"revenue_Miscellaneous Receipts","misc",c(1,1,1))[[2]]
tst_list[["revenue_Payroll Taxes"]]=forecast_component(nowcast_payroll_receipts,nowcast_total_receipts,receipt_daily_df,"revenue_Payroll Taxes","payroll",c(1,1,1))[[2]]

tst_list[["outlay_Medicaid"]] = forecast_component(nowcast_medicaid_outlay,nowcast_total_outlays,outlay_daily_df,"outlay_Medicaid","medicaid",c(1,1,1))[[2]]
tst_list[["outlay_Medicare"]] =forecast_component(nowcast_medicare_outlay,nowcast_total_outlays,outlay_daily_df,"outlay_Medicare",'medicare',c(1,1,1))[[2]]
tst_list[["outlay_Net Interest"]]=forecast_component(nowcast_interest_outlay,nowcast_total_outlays,outlay_daily_df,"outlay_Net Interest","interest",c(1,1,1))[[2]]
tst_list[["outlay_Social Security"]]=forecast_component(nowcast_ss_outlay,nowcast_total_outlays,outlay_daily_df,"outlay_Social Security","ss",c(1,1,1))[[2]]
tst_list[["outlay_Defense Discretionary"]]=forecast_component(nowcast_defense_outlay,nowcast_total_outlays,outlay_daily_df,"outlay_Defense Discretionary","defense",c(1,1,1))[[2]]
tst_list[["outlay_Other"]]=forecast_component(nowcast_other_outlay,nowcast_total_outlays,outlay_daily_df,"outlay_Other","other",c(6,1,0))[[2]]


forecast_list1 = bind_rows(forecast_list)

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
  mutate(record_calendar_day=as.numeric(record_calendar_day)) %>% 
  left_join(tax_days,by=c("actual_date"="date")) %>% 
  group_by(date) %>% 
  fill(tax_day,.direction="down") %>% 
  mutate(tax_day=ifelse(is.na(tax_day),0,tax_day)) %>% 
  ungroup() %>% 
  mutate(record_calendar_month=as.numeric(record_calendar_month))

outlay_daily_df_groups = outlay_daily_df_groups %>% 
  ungroup() %>% 
  mutate(avg_share=predict(lm(share~record_calendar_day*factor(record_calendar_month)*factor(group)+factor(record_calendar_day):factor(tax_day):factor(group),
                              outlay_daily_df_groups %>% filter(date<max(outlay_daily_df_groups$date))),outlay_daily_df_groups)) %>% 
  ungroup() %>% 
  mutate(extrap_total=(total_mtd/avg_share)*(-1/1000)) %>% 
  rowwise() %>% 
  mutate(extrap_total=mean(c(pred,extrap_total),na.rm=TRUE), # TODO: test if it makes sense to set min at the amount of revenue already seen in the data
         extrap_total=min(c(extrap_total,
                            quantile(c(outlays_fred %>% 
                                         filter(date>="2022-01-01"&date<=(Sys.Date() %m+% years(3))) %>% 
                                         pull(value),
                                       cbo_monthly_proj$outlay_Total[cbo_monthly_proj$year<=(year(Sys.Date())+3)&cbo_monthly_proj$year>=2022]),
                                     1,na.rm=TRUE)))) %>% 
  ungroup()


outlay_daily_df_groups = outlay_daily_df_groups %>% 
  left_join(outlay_daily_df_groups %>% 
              filter(share==1&record_fiscal_year>=2015) %>% 
              group_by(record_fiscal_year,group) %>% 
              summarize(scale_factor_year=mean(actual/extrap_total,na.rm=TRUE))) %>% 
  left_join(outlay_daily_df_groups %>% 
              filter(record_fiscal_year>=2015) %>% 
              group_by(record_fiscal_year,record_calendar_month,group) %>% 
              summarize(scale_factor_month=mean(actual[n()]/extrap_total[n()],na.rm=TRUE))) %>% 
  ungroup() %>% 
  mutate_at(vars(scale_factor_year,scale_factor_month),~ifelse(is.nan(.),NA,.)) %>% 
  group_by(group) %>% 
  fill(scale_factor_year,.direction="downup") %>% 
  group_by(record_fiscal_year,record_calendar_month,group) %>% 
  mutate(scaled_total=ifelse(!is.na(actual),extrap_total*scale_factor_month,extrap_total*scale_factor_year),
         scaled_total_day=ifelse(!is.na(actual),total_day*-1*(actual[n()]/total_mtd[n()]),total_day*scale_factor_year),
         scaled_total_mtd=ifelse(!is.na(actual),total_mtd*-1*(actual[n()]/total_mtd[n()]),total_mtd*scale_factor_year),
         extrap_total=ifelse(!is.na(actual),extrap_total*scale_factor_year,extrap_total*scale_factor_year)) %>%  # keep column that is the pure prediction
  rowwise() %>% 
  mutate(extrap_total=mean(c(pred,extrap_total)),
         scaled_total=ifelse(!is.na(actual),scaled_total,extrap_total)) %>% 
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
    mutate(daily_deficit=(receipt_day_amt-outlay_day_amt)) %>% 
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
    mutate(daily_deficit=(receipt_day_amt-outlay_day_amt)) %>% 
    select(date,record_fiscal_year,record_calendar_month,record_calendar_day,daily_deficit) %>% 
    fill(record_fiscal_year,record_calendar_month,date) %>% 
    mutate(year=year(date)) %>% 
    left_join(nowcast_deficit %>% 
                select(date,deficit_lower,deficit) %>% 
                mutate(adj=deficit_lower-deficit,
                       year=year(date),
                       month=month(date)) %>% 
                select(adj,year,month),
              by=c('year'='year','record_calendar_month'='month')) %>% 
    group_by(record_calendar_month) %>% 
    mutate(daily_deficit=daily_deficit+(adj/n())) %>% 
    select(record_fiscal_year,record_calendar_month,record_calendar_day,daily_deficit),
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
    mutate(daily_deficit=(receipt_day_amt-outlay_day_amt)) %>% 
    select(date,record_fiscal_year,record_calendar_month,record_calendar_day,daily_deficit) %>% 
    fill(record_fiscal_year,record_calendar_month,date) %>% 
    mutate(year=year(date)) %>% 
    left_join(nowcast_deficit %>% 
                select(date,deficit_upper,deficit) %>% 
                mutate(adj=deficit_upper-deficit,
                       year=year(date),
                       month=month(date)) %>% 
                select(adj,year,month),
              by=c('year'='year','record_calendar_month'='month')) %>% 
    group_by(record_calendar_month) %>% 
    mutate(daily_deficit=daily_deficit+(adj/n())) %>% 
    select(record_fiscal_year,record_calendar_month,record_calendar_day,daily_deficit),
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

