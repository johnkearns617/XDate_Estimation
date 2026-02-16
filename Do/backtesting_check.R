# Backtesting

#### Google Trends calculations ####




#### Feature Imputation #####



#### Nowcast Headline ####
# df = make_df(end_date,most_recent = FALSE) %>% 
#   group_by(year,qtr) %>%
#   fill(PRS85006112,.direction="down") %>% 
#   ungroup() %>% 
#   select(-c(MTSR133FMS:W017RC1Q027SBEA,A261RX1Q020SBEA:SLCEC1,B096RC1Q027SBEA:A091RC1Q027SBEA,B243RC1Q027SBEA:AD02RC1Q027SBEA,year,qtr)) %>%  # remove indeed and retail variables to speed up code, even though they do improve the model fit
#   mutate_at(vars(-c(date)),~ifelse(is.infinite(.)|is.nan(.),NA,.)) %>% 
#   select_if(~sum(!is.na(.))>0|is.character(.)|is.Date(.)) %>% 
#   select_if(~sd(.,na.rm=TRUE)!=0|is.character(.)|is.Date(.)) %>% 
#   filter(date>="2004-01-01")
# 
# write_csv(df,paste0("Data/Processing/raw_data/data_asof",end_date,".csv"))
# 
# set.seed(178)
# 
# imputed_df = impute_function(df,end_date)
# 
# write_csv(imputed_df,paste0("Data/Processing/imputed_data/imputed_data_asof",end_date,".csv"))


test_check = lapply(sapply(seq(from=as.Date("2024-01-01"),
                               to=as.Date("2025-05-01"),
                               by="1 month"),
                           function(x) as.character(ceiling_date(x,"month")-1)),nowcast_headline,dataset=outlays_fred,cbo_component="outlay")

for(i in 1:17){
  
  if(i==1){
    
    check_preds = test_check[[1]]$pred_df
    
  }else{
    
    check_preds = bind_rows(check_preds,tail(test_check[[i]]$pred_df,1))
    
  }
  
}

# In-Sample Fit
RMSE(check_preds$pred[check_preds$date<"2024-01-01"],check_preds$actual[check_preds$date<"2024-01-01"],na.rm = TRUE)
RMSE(check_preds$cbo_proj[check_preds$date<"2024-01-01"],check_preds$actual[check_preds$date<"2024-01-01"],na.rm = TRUE)
# 62.13 vs 76.29

# Out of Sample Fit
RMSE(check_preds$pred[check_preds$date>="2024-01-01"&check_preds$date!="2025-01-01"],check_preds$actual[check_preds$date>="2024-01-01"&check_preds$date!="2025-01-01"],na.rm = TRUE)
RMSE(check_preds$cbo_proj[check_preds$date>="2024-01-01"&check_preds$date!="2025-01-01"],check_preds$actual[check_preds$date>="2024-01-01"&check_preds$date!="2025-01-01"],na.rm = TRUE)
# excluding the one outlier, 51.22 vs 65.79
# but why is there the outlier in 2025-01? Weird movement in Campers & RVS and Footwear

ggplot(check_preds,aes(x=date)) +
  geom_line(aes(y=pred,color="Predicted")) +
  geom_line(aes(y=actual,color="Actual")) +
  geom_line(aes(y=cbo_proj,color="CBO Proj")) +
  theme_bw()

fcast_df1 = get_deficit_imputed_data('2025-05-01',dataset,'outlays',monthly_shares_reg)
(fcast_df1 %>% filter(date=="2025-01-01") %>% select(cbo_proj_month,rownames(selected_coefs_state)) %>% pivot_longer(cols=c(cbo_proj_month,rownames(selected_coefs_state)))) %>% left_join(tidy(test)[,1:2],by=c("name"="term")) %>% mutate(pred=value*estimate)





test_check = lapply(sapply(seq(from=as.Date("2024-01-01"),
                               to=as.Date("2025-05-01"),
                               by="1 month"),
                           function(x) as.character(ceiling_date(x,"month")-1)),nowcast_headline,dataset=receipts_fred,cbo_component="revenue")

for(i in 1:17){
  
  if(i==1){
    
    check_preds = test_check[[1]]$pred_df
    
  }else{
    
    check_preds = bind_rows(check_preds,tail(test_check[[i]]$pred_df,1))
    
  }
  
}

# In-Sample Fit
RMSE(check_preds$pred[check_preds$date<"2024-01-01"],check_preds$actual[check_preds$date<"2024-01-01"],na.rm = TRUE)
RMSE(check_preds$cbo_proj[check_preds$date<"2024-01-01"],check_preds$actual[check_preds$date<"2024-01-01"],na.rm = TRUE)
# 31.71 vs 38.93

# Out of Sample Fit
RMSE(check_preds$pred[check_preds$date>="2024-01-01"&check_preds$date!="2025-03-01"],check_preds$actual[check_preds$date>="2024-01-01"&check_preds$date!="2025-03-01"],na.rm = TRUE)
RMSE(check_preds$cbo_proj[check_preds$date>="2024-01-01"&check_preds$date!="2025-03-01"],check_preds$actual[check_preds$date>="2024-01-01"&check_preds$date!="2025-03-01"],na.rm = TRUE)
# 60.38 vs 55.97

ggplot(check_preds,aes(x=date)) +
  geom_line(aes(y=pred,color="Predicted")) +
  geom_line(aes(y=actual,color="Actual")) +
  geom_line(aes(y=cbo_proj,color="CBO Proj")) +
  theme_bw()


#### component nowcast ####
iterate_df = t(data.frame(
c("Total -- Miscellaneous Receipts", "Miscellaneous Receipts"),
c("Corporation Income Taxes","Corporate Income Taxes"),
c("Total -- Social Insurance and Retirement Receipts", "Payroll Taxes"),
c("Total -- Individual Income Taxes", "Individual Income Taxes"),
c("Total -- Excise Taxes", "Excise Taxes"),
c("Estate and Gift Taxes", "Estate and Gift Taxes"),
c("Customs Duties", "Customs Duties")))
rownames(iterate_df)=NULL
colnames(iterate_df)=c("col","cat")
iterate_df=data.frame(iterate_df)

coef_flag=FALSE

col_mts=iterate_df$col[i]
cbo_category=iterate_df$cat[i]
cbo_component="revenue"
test_check = lapply(sapply(seq(from=as.Date("2024-01-01"),
                               to=as.Date("2025-05-01"),
                               by="1 month"),
                           function(x) as.character(ceiling_date(x,"month")-1)),nowcast_budget_receipt,mts_dataset=receipts,col_mts=col_mts,cbo_category=cbo_category,cbo_component=cbo_component)

for(i in 1:17){
  
  if(i==1){
    
    check_preds = test_check[[1]]$pred_df
    
  }else{
    
    check_preds = bind_rows(check_preds,tail(test_check[[i]]$pred_df,1))
    
  }
  
}

# In-Sample Fit
RMSE(check_preds$pred[check_preds$date<"2024-01-01"],check_preds$actual[check_preds$date<"2024-01-01"],na.rm = TRUE)
RMSE(check_preds$cbo_proj[check_preds$date<"2024-01-01"],check_preds$actual[check_preds$date<"2024-01-01"],na.rm = TRUE)
# 2.95 vs 3.45 Misc Receipts ---- 2.71, 3.45
# 9.59 vs 10.86 Corp Income Tax --- 8.71
# 6.89 vs 6.74 Payroll Tax --- 6.78
# 34.36 vs 47.56 Income Tax --- 31.86
# 2.01 vs 2.29 Excise Tax --- 1.86
# 0.78 vs 0.92 Estate Tax --- 0.75
# 0.62 vs 0.76 Customs Duties --- 0.40

# Out of Sample Fit
RMSE(check_preds$pred[check_preds$date>="2024-01-01"],check_preds$actual[check_preds$date>="2024-01-01"],na.rm = TRUE)
RMSE(check_preds$cbo_proj[check_preds$date>="2024-01-01"],check_preds$actual[check_preds$date>="2024-01-01"],na.rm = TRUE)
# 2.74 vs 1.61 Misc Receipts. ---- 2.36
# 12.17 vs 13.52 Corp Income Tax. --- 12.04
# 6.78 vs 6.12 Payroll Tax --- 7.44
# 58.46 vs 41.87 Income Tax --- 62.48
# 3.99 vs 3.22 Excise Tax --- 3.72
# 0.79 vs 0.78 Estate Tax --- 0.89
# 4.72 vs 4.60 Customs Duties --- 3.17

ggplot(check_preds,aes(x=date)) +
  geom_line(aes(y=pred,color="Predicted")) +
  geom_line(aes(y=actual,color="Actual")) +
  geom_line(aes(y=cbo_proj,color="CBO Proj")) +
  theme_bw()




test_check = lapply(sapply(seq(from=as.Date("2024-01-01"),
                               to=as.Date("2025-05-01"),
                               by="1 month"),
                           function(x) as.character(ceiling_date(x,"month")-1)),nowcast_budget_outlay,cbo_category="Net Interest")


for(i in 1:17){
  
  if(i==1){
    
    check_preds = test_check[[1]]$pred_df
    
  }else{
    
    check_preds = bind_rows(check_preds,tail(test_check[[i]]$pred_df,1))
    
  }
  
}

# In-Sample Fit
RMSE(check_preds$pred[check_preds$date>="2024-01-01"],check_preds$actual[check_preds$date>="2024-01-01"],na.rm = TRUE)
RMSE(check_preds$cbo_proj[check_preds$date>="2024-01-01"],check_preds$actual[check_preds$date>="2024-01-01"],na.rm = TRUE)
RMSE(check_preds$pred,check_preds$actual,na.rm = TRUE)
RMSE(check_preds$cbo_proj,check_preds$actual,na.rm = TRUE)
RMSE(check_preds$pred[year(check_preds$date)%in%c(2007:2010,2020)],check_preds$actual[year(check_preds$date)%in%c(2007:2010,2020)],na.rm = TRUE)
RMSE(check_preds$cbo_proj[year(check_preds$date)%in%c(2007:2010,2020)],check_preds$actual[year(check_preds$date)%in%c(2007:2010,2020)],na.rm = TRUE)
# 33.76 vs 41.57 Medicare
# 2.54 vs 2.48 Medicaid
# 7.36 vs 5.01 Social Security
# 47.70 vs 42.17 Other Spending
# 8.94 vs 8.40 National Defense
# 10.38 vs 12.56 Net Interest

