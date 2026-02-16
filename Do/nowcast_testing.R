# test imputation

nowcast_function = function(dataset,cbo_category){
  
  monthly_shares = dataset %>% 
    filter(fiscal_year>=2002&fiscal_year<=2023) %>% 
    group_by(fiscal_year) %>% 
    mutate(total=sum(value,na.rm=TRUE)) %>% 
    ungroup() %>%  
    mutate(share=value/total,
           month=month(date))
  
  monthly_shares_reg = lm_robust(share~factor(month),monthly_shares %>% group_by(fiscal_year) %>% filter(n()==12))
  
  fcast_df1 = get_deficit_imputed_data(floor_date(Sys.Date(),"year")-1,dataset,cbo_category,monthly_shares_reg)
  
  X = model.matrix(as.formula(paste0("value","~",paste(colnames(fcast_df1)[c(2:which(colnames(fcast_df1)=="gt_999"))],collapse="+"))),
                   fcast_df1 %>% filter(date<"2024-01-01"&year(date)>=2006&!is.na(value)))[, -1]
  y = (fcast_df1 %>% filter(date<"2024-01-01"&year(date)>=2006&!is.na(value)))[["value"]]
  
  weight = (1:nrow(X))/nrow(X)
  weight = ifelse(weight<.5,.5,weight)
  fit_lasso_state = glmnet(X, y, alpha = 1,pmax=20,weights = weight)
  # weight by how recent the data is
  
  selected_coefs_state = data.frame(varImp(fit_lasso_state,lambda=min(fit_lasso_state$lambda), scale = FALSE)) %>% filter(Overall!=0)
  selected_coefs_state$var = as.numeric(gsub("gt_","",rownames(selected_coefs_state)))
  coef_value_state = coef(fit_lasso_state,s=min(fit_lasso_state$lambda))[,1][-1]
  coef_value_state = coef_value_state[coef_value_state!=0]
  selected_coefs_state = cbind(selected_coefs_state,coef_value_state)
  selected_coefs_state$category = sapply(selected_coefs_state$var,which_category)
  selected_coefs_state = selected_coefs_state %>% arrange(-Overall)
  
  test = lm_robust(as.formula(paste0("value","~lag1+lag2+lag3+lag4+cbo_proj_month+",paste(c(rownames(selected_coefs_state)),collapse="+"))),
                   data = fcast_df1 %>% filter(date<='2024-01-01') %>% mutate(weight=(1:n())/n()))
  
  fcast_df1 = get_deficit_imputed_data(Sys.Date(),dataset,cbo_category,monthly_shares_reg)
  
  for(dat in tail(fcast_df1,10) %>% filter(is.na(value)) %>% pull(date)){
    
    fcast_df1$value[fcast_df1$date==dat] = predict(test,fcast_df1 %>% filter(date==dat)) 
    
    fcast_df1 = fcast_df1 %>% 
      mutate(cbo_proj_diff=(value/cbo_proj_month-1)*100) %>% 
      mutate(lag1_cbo_proj_diff=dplyr::lag(cbo_proj_diff,1),
             lag2_cbo_proj_diff=dplyr::lag(cbo_proj_diff,2)) %>% 
      mutate(lag1=dplyr::lag(value,1),
             lag2=dplyr::lag(value,2),
             lag3=dplyr::lag(value,3),
             lag4=dplyr::lag(value,4))
    
  }
  
  pred_df = data.frame(
    date=fcast_df1[['date']],
    var=cbo_category,
    pred=predict(test,fcast_df1),
    actual=fcast_df1[['value']],
    cbo_proj=fcast_df1[['cbo_proj_month']]
  )
  
  return(list(
    'data'=fcast_df1,
    'reg'=test,
    'pred_df'=pred_df,
    'monthly_shares_reg'=monthly_shares_reg
  ))
  
}


nowcast_total_outlays = nowcast_function(outlays_fred,cbo_category ="outlay")
nowcast_total_outlays[[3]] %>% drop_na() %>% summarize(my_proj=sqrt(mean(((pred/actual)-1)^2)),cbo_proj=sqrt(mean(((cbo_proj/actual)-1)^2)))

nowcast_total_receipts = nowcast_function(receipts_fred,cbo_category ="revenue")
nowcast_total_receipts[[3]] %>% drop_na() %>% summarize(my_proj=sqrt(mean(((pred/actual)-1)^2)),cbo_proj=sqrt(mean(((cbo_proj/actual)-1)^2)))

mape = function(pred,obs){
  
  return(mean(abs((obs-pred)/obs),na.rm=TRUE))
  
}

# mice
mice_level_test = function(test_dates,cbo_category){
  
  library(mice)
  
  if(cbo_category=="outlay"){
    
    dataset = outlays_fred
    
  } else{
    
    dataset = receipts_fred
    
  }
  
  monthly_shares = dataset %>% 
    filter(fiscal_year>=2002&fiscal_year<=2023) %>% 
    group_by(fiscal_year) %>% 
    mutate(total=sum(value,na.rm=TRUE)) %>% 
    ungroup() %>%  
    mutate(share=value/total,
           month=month(date))
  
  monthly_shares_reg = lm_robust(share~factor(month),monthly_shares %>% group_by(fiscal_year) %>% filter(n()==12))

  pred_df = data.frame()
  for(dat in as.character(test_dates)){
    
    system(sprintf('echo "\n%s\n"', paste0(as.character(dat), collapse="")))
    
    df = get_deficit_imputed_data(dat,dataset,cbo_category,monthly_shares_reg) %>% 
      mutate(value=ifelse(date>=as.Date(dat) %m-% months(1),NA,value))
    
    tempData <- ((mice(df %>% select(PAYEMS:gt_999,value,lag1:lag4,cbo_proj_month),meth='rf',seed=178,formulas=list(as.formula("value~.")),m=1)))
    tmp=complete(tempData) %>% 
      bind_cols(df %>% select(date)) %>% 
      left_join(dataset %>% select(date,value),by="date") %>% 
      relocate(date,1) %>% 
      filter(date>=as.Date(dat) %m-% months(1))
    
    pred_df = bind_rows(pred_df,tmp)
    
  }
  
  return(pred_df)
  
}

mice_level_preds_outlay = mclapply(as.Date(gsub("imputed_data_asof|.csv","",grep("-30|-31|-28",list.files("Data/Processing/imputed_data/"),value=TRUE))),
                            mice_level_test,
                            cbo_category="outlay",
                            mc.cores=8)

mice_level_preds_revenue = mclapply(as.Date(gsub("imputed_data_asof|.csv","",grep("-30|-31|-28",list.files("Data/Processing/imputed_data/"),value=TRUE))),
                                   mice_level_test,
                                   cbo_category="revenue",
                                   mc.cores=8)

mice_level_preds_full = bind_rows(mice_level_preds_outlay[which(sapply(mice_level_preds_outlay,function(x) !is.null(nrow(x))))],
                                  mice_level_preds_revenue[which(sapply(mice_level_preds_revenue,function(x) !is.null(nrow(x))))])

mape(mice_level_preds_full$value.x,mice_level_preds_full$value.y)
mape(mice_level_preds_full$cbo_proj_month,mice_level_preds_full$value.y)
# 0.27 vs 0.10 for CBO

tmp = bind_rows(#nowcast_total_receipts[[3]] %>% drop_na()
                #,
                nowcast_total_outlays[[3]] %>% drop_na() 
                )
mape(tmp$pred,tmp$actual)
mape(tmp$cbo_proj,tmp$actual)
# actual forecast 0.10 vs 0.094 for CBO alone
# worse accuracy for outlays than revenue


library(fpp)
library(mltsp)
library(e1071)
spec = build_narx(svm,p=2,d=0,P=1,D=1,freq=12)
tmp = ts(fcast_df1 %>% filter(date<"2024-01-01"&date>="2015-01-01") %>% select(value),start=2015,frequency=12)
model = narx(spec,tmp)
fcst = forecast(model, h = 10)
plot(fcst)
tmp2 = ts(fcast_df1 %>% filter(date>="2024-01-01") %>% select(value),start=2024,frequency=12)
lines(tmp2, col="red")

tmpx = xts(fcast_df1 %>% filter(date<"2024-01-01"&date>="2015-01-01") %>% select(cbo_proj_month),
          as.Date(fcast_df1 %>% filter(date<"2024-01-01"&date>="2015-01-01") %>% pull(date)))

tmpy = xts(fcast_df1 %>% filter(date<"2024-01-01"&date>="2015-01-01") %>% select(value),
           as.Date(fcast_df1 %>% filter(date<"2024-01-01"&date>="2015-01-01") %>% pull(date)))

model = narx(tmpy, SimpleLM, p=2,d=0,P=1,D=1,freq=12,xreg=tmpx)

tmpx = xts(fcast_df1 %>% filter(date>="2024-01-01") %>% select(cbo_proj_month,rownames(selected_coefs_state)),
           as.Date(fcast_df1 %>% filter(date>="2024-01-01") %>% pull(date)))

tmpy = xts(fcast_df1 %>% filter(date>="2024-01-01") %>% select(value),
           as.Date(fcast_df1 %>% filter(date>="2024-01-01") %>% pull(date)))

pred1 = forecast(model, xreg=tmpx)
plot(pred1$mean)
lines(tmpy, col="red")
mape(head(as.numeric(pred1$mean),-1),head(tmpy$value,-1))
rmse_check %>% filter(date>="2024-01-01"&date<="2025-12-01"&var=="Individual Income Taxes") %>% summarize(mape=mape(pred,actual))

library(tuneRanger)
library(mlr)

iris.task = makeRegrTask(id="test",data = fcast_df1 %>% filter(date<"2024-01-01"&date>="2015-08-01") %>% select(value,lag1:lag4,cbo_proj_month,rownames(selected_coefs_state)), target = "value")

# Tune the model with a single line of code
# Tunes mtry, min.node.size, and sample.fraction by default
# Out-of-bag predictions are used for evaluation
results = tuneRanger(
  iris.task
)

# View the recommended hyperparameters and the final model
print(results$recommended.pars)
tuned_model = results$model





tmpx = xts(fcast_df1 %>% filter(date>="2015-03-01") %>% select(cbo_proj_month,value,rownames(selected_coefs_state)),
           as.Date(fcast_df1 %>% filter(date>="2015-03-01") %>% pull(date)))

tmpx$value = log(tmpx$value)
tmpx$cbo_proj_month = log(tmpx$cbo_proj_month)

adf.test(na.omit(diff(tmpx$value)))
kpss.test(na.omit(diff(tmpx$value)),null="Trend")
po.test(tmpx[,c("value","cbo_proj_month")])

tmpx <- cbind(tmpx, covid = 0)
tmpx["2020-03-01/2021-12-01", "covid"] <- 1

# This estimates the cointegration equation
cieq <- lm(value ~ cbo_proj_month + covid, tmpx)
res = resid(cieq) %>% as.xts(dateFormat = "Date")
plot(res[-1L, ], main = "Residuals from Cointegration Equation", 
     major.ticks = "years", grid.ticks.on = "years")

ecm <- lm(D(value) ~ L(D(value), 1:2) + L(D(cbo_proj_month), 0:1) + L(res) + covid, merge(tmpx, res))

ecm2 <- lm(D(value) ~ L(D(value), 1:2) + L(D(cbo_proj_month)) + L(res) + covid, merge(tmpx, res))

forecast_oos <- function(x, start = 2023) {
  n <- nrow(x[paste0("/", start - 1), ])
  fc <- numeric(0L)
  xdf <- qDF(x)
  # Forecasting with expanding window
  for(i in n:(nrow(x)-1L)) {
    samp <- ss(xdf, 1:i)
    ci <- lm(value ~ cbo_proj_month + covid, samp)
    samp <- tfm(samp, res = resid(ci))
    mod <- lm(D(value) ~ L(D(value), 1:2) + L(D(cbo_proj_month), 0:1) + L(res) + covid, samp)
    fc <- c(fc, flast(predict(mod, newdata = rbind(samp, 0)))) # predict does not re-estimate
  }
  xfc <- cbind(D(x[, "value"]), ECM_fc = NA)
  xfc[(n+1L):nrow(x), "ECM_fc"] <- unattrib(fc)
  return(xfc)
}

# Forecasting
ECM_oos_fc <- forecast_oos(na_omit(tmpx))

# Plotting
plot(ECM_oos_fc["2023/", ], 
     main = "Out of Sample Expanding Window Forecast from ECM", 
     legend.loc = "topleft", major.ticks = "years", grid.ticks.on = "years")


X = model.matrix(as.formula(paste0("diff","~",paste(colnames(fcast_df1)[c(10:which(colnames(fcast_df1)=="gt_999"))],collapse="+"))),
                 monthly_shares)[, -1]
y = (monthly_shares)[['value']]

weight = (1:nrow(X))/nrow(X)
weight = ifelse(weight<.5,.5,weight)
fit_lasso_state = glmnet(X, y, alpha = 1,pmax=10,weights = weight)
# weight by how recent the data is

selected_coefs_state = data.frame(varImp(fit_lasso_state,lambda=min(fit_lasso_state$lambda), scale = FALSE)) %>% filter(Overall!=0)
selected_coefs_state$var = as.numeric(gsub("gt_","",rownames(selected_coefs_state)))
coef_value_state = coef(fit_lasso_state,s=min(fit_lasso_state$lambda))[,1][-1]
coef_value_state = coef_value_state[coef_value_state!=0]
selected_coefs_state = cbind(selected_coefs_state,coef_value_state)
selected_coefs_state$category = sapply(selected_coefs_state$var,which_category)
selected_coefs_state = selected_coefs_state %>% arrange(-Overall)

test = lm_robust(as.formula(paste0("diff","~factor(month)+factor(covid)+",paste(c(rownames(selected_coefs_state)),collapse="+"))),
                 data = monthly_shares)

monthly_shares$pred2 = predict(test,newdata=monthly_shares)


#### Individual Income Tax Test ####
nowcast_budget_receipt = function(mts_dataset,dat,col_mts,cbo_component,cbo_category){
  
  load("Data/Final/models.RDS")  
  
  fcast_df1 = read_csv(paste0("Data/Processing/imputed_data/imputed_data_asof",dat,".csv"))  %>% 
    select(-any_of(paste0("gt_",bad_vars$category))) %>% 
    arrange(date) %>%
    fill(PRS85006112,.direction='down') %>% 
    ungroup() %>% 
    mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10),.funs=list(ch12m=~((./dplyr::lag(.,12)-1)*100),ch1m=~((./dplyr::lag(.,1)-1)*100))) %>%
    mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,grep("gt_",colnames(.),value=TRUE)),.funs=list(ch12m=~.-dplyr::lag(.,12),ch1m=~.-dplyr::lag(.,1))) %>%
    mutate_at(vars(PAYEMS:gt_999_ch1m),.funs=list(lag1=~dplyr::lag(.,1),lag2=~dplyr::lag(.,2),lag3=~dplyr::lag(.,3),lag4=~dplyr::lag(.,4)))
  
  monthly_shares = mts_dataset %>% 
    filter(classification_desc==col_mts) %>% 
    mutate(record_date=floor_date(record_date,"month"),
           current_month_net_rcpt_amt=as.numeric(current_month_net_rcpt_amt)/1000000000) %>% 
    select(record_date,current_month_net_rcpt_amt) %>% 
    rename(date=record_date,
           value=current_month_net_rcpt_amt) %>% 
    mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)),
           value=ifelse(date>=floor_date(as.Date(dat),"month"),NA,value)) %>% 
    group_by(fiscal_year) %>% 
    mutate(total=sum(value,na.rm=TRUE)) %>% 
    ungroup() %>%  
    mutate(share=value/total,
           month=month(date),
           fy_month=case_when(
             month%in%c(10:12)~month-9,
             month%in%c(1:9)~month+3
           )) %>% 
    group_by(fiscal_year) %>% 
    arrange(fy_month) %>% 
    mutate(cum_total=cumsum(value),
           cum_share=cumsum(share),
           num=n())
  
  fcast_df1 = right_join(monthly_shares,fcast_df1,by="date") 
  
  fcast_df1 = fcast_df1 %>% 
    mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)),
           month=month(date),
           fy_month=case_when(
             month%in%c(10:12)~month-9,
             month%in%c(1:9)~month+3
           )) %>% 
    left_join(cbo_proj %>% 
                filter(component==cbo_component&category==cbo_category) %>% 
                group_by(projected_fiscal_year) %>% 
                filter(baseline_date<=as.Date(paste0(projected_fiscal_year,"-09-30"))) %>% 
                slice(n()) %>% 
                select(projected_fiscal_year,value) %>% 
                rename(cbo_proj=value,
                       fiscal_year=projected_fiscal_year)) %>% 
    mutate(error=total/cbo_proj) %>% 
    ungroup() %>% 
    arrange(fiscal_year,fy_month) %>% 
    mutate(error_ly=dplyr::lag(error,12),
           error_ly=ifelse(fiscal_year==2016,error[fiscal_year==2015][1],error_ly)) %>% 
    ungroup() %>%
    select(-num) %>% 
    arrange(date)
  
  if(cbo_category=="Individual Income Taxes"){
    fcast_df1 = fcast_df1 %>% 
      mutate(tax_due=case_when(
        !(fiscal_year%in%c(2020,2021))&month==4~1,
        fiscal_year==2020&month==7~1,
        fiscal_year==2021&month==5~1,
        TRUE~0
      ),
      quarter_end=ifelse(month%in%c(1,3,6,9),1,0))
  }
  if(cbo_category=="Corporate Income Taxes"){
    fcast_df1 = fcast_df1 %>% 
      mutate(tax_due=case_when(
        !(fiscal_year%in%c(2020))&month==4~1,
        fiscal_year==2020&month==7~1,
        TRUE~0
      ),
      quarter_end=ifelse(month%in%c(4,6,9,12),1,0))
  }
  if(cbo_category=="Excise Taxes"){
    fcast_df1=fcast_df1 %>% 
      mutate(tax_due=case_when(
        fiscal_year==2020&month==9~1,
        TRUE~0
      ))
  }

  fcast_df1$pred_cumshare=as.numeric(predict(models[[cbo_category]]$share,fcast_df1))
  fcast_df1 = fcast_df1 %>% 
    mutate(pred_total=ifelse(fiscal_year<2016,cbo_proj,cum_total/pred_cumshare),
           total=ifelse(fiscal_year<2016,NA,total)) %>% 
    relocate(pred_cumshare,pred_total,.after = cum_share) %>% 
    group_by(fiscal_year) %>% 
    fill(pred_total,.direction = "down")
  
  fcast_df1$final_pred = as.numeric(predict(models[[cbo_category]]$total,fcast_df1))
  fcast_df1$final_pred_month = fcast_df1$final_pred*fcast_df1$pred_cumshare
  fcast_df1$cbo_proj_month = fcast_df1$cbo_proj*fcast_df1$pred_cumshare
  fcast_df1 = fcast_df1 %>% 
    relocate(final_pred,final_pred_month,.after = pred_total) %>% 
    group_by(fiscal_year) %>% 
    arrange(fiscal_year,fy_month) %>% 
    mutate(tst1=final_pred_month-dplyr::lag(final_pred_month,1),
           final_pred_month=case_when((fy_month==1)|(date=="2004-01-01")~final_pred_month,TRUE~tst1),
           tst1=cbo_proj_month-dplyr::lag(cbo_proj_month,1),
           cbo_proj_month=case_when((fy_month==1)|(date=="2004-01-01")~cbo_proj_month,TRUE~tst1)) %>% 
    ungroup()
  
  pred_df = data.frame(
    date=fcast_df1[['date']],
    var=cbo_category,
    predict(models[[cbo_category]]$rf,fcast_df1,se.fit=TRUE, interval="confidence", alpha=0.70),
    actual=fcast_df1[['value']],
    extrap_total=fcast_df1[['final_pred_month']],
    cbo_proj=fcast_df1[['cbo_proj_month']]
  ) %>% 
    rename(pred=fit.fit)
  
  return(list(
    'data'=fcast_df1,
    'reg'=test,
    'pred_df'=pred_df,
    'models'=models[[cbo_category]]
  ))
  
}

sim_df = data.frame()
for(dat in grep("2025",gsub("imputed_data_asof|.csv","",list.files("Data/Processing/imputed_data/")),value=TRUE)){
  
  print(dat)
  
  income = nowcast_budget_receipt(receipts,dat,"Total -- Individual Income Taxes","revenue","Individual Income Taxes")$pred_df %>% 
    filter(date>="2015-03-01"&is.na(actual)) %>% 
    select(-actual) %>% 
    left_join(mts_dataset %>% 
                filter(classification_desc=="Total -- Individual Income Taxes") %>% 
                mutate(record_date=floor_date(record_date,"month"),
                       current_month_net_rcpt_amt=as.numeric(current_month_net_rcpt_amt)/1000000000) %>% 
                select(record_date,current_month_net_rcpt_amt) %>% 
                rename(date=record_date,
                       actual=current_month_net_rcpt_amt)) %>% 
    mutate(run_date=dat)

  sim_df = bind_rows(sim_df,income)
  
}

sim_df = data.frame()
for(dat in grep("2025",gsub("imputed_data_asof|.csv","",list.files("Data/Processing/imputed_data/")),value=TRUE)){
  
  print(dat)
  
  income = nowcast_budget_receipt(receipts,dat,"Total -- Miscellaneous Receipts","revenue","Miscellaneous Receipts")$pred_df %>% 
    filter(date>="2015-03-01"&is.na(actual)) %>% 
    select(-actual) %>% 
    left_join(mts_dataset %>% 
                filter(classification_desc=="Total -- Miscellaneous Receipts") %>% 
                mutate(record_date=floor_date(record_date,"month"),
                       current_month_net_rcpt_amt=as.numeric(current_month_net_rcpt_amt)/1000000000) %>% 
                select(record_date,current_month_net_rcpt_amt) %>% 
                rename(date=record_date,
                       actual=current_month_net_rcpt_amt)) %>% 
    mutate(run_date=dat)
  
  sim_df = bind_rows(sim_df,income)
  
}

nowcast_budget_spending = function(dat,col_mts,cbo_category){
  
  load("Data/Final/models.RDS")  
  
  fcast_df1 = read_csv(paste0("Data/Processing/imputed_data/imputed_data_asof",dat,".csv"))  %>% 
    select(-any_of(paste0("gt_",bad_vars$category))) %>% 
    arrange(date) %>%
    fill(PRS85006112,.direction='down') %>% 
    ungroup() %>% 
    mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10),.funs=list(ch12m=~((./dplyr::lag(.,12)-1)*100),ch1m=~((./dplyr::lag(.,1)-1)*100))) %>%
    mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,grep("gt_",colnames(.),value=TRUE)),.funs=list(ch12m=~.-dplyr::lag(.,12),ch1m=~.-dplyr::lag(.,1))) %>%
    mutate_at(vars(PAYEMS:gt_999_ch1m),.funs=list(lag1=~dplyr::lag(.,1),lag2=~dplyr::lag(.,2),lag3=~dplyr::lag(.,3),lag4=~dplyr::lag(.,4)))
  
  monthly_shares = get_monthly_shares_df_spending(col_mts,cbo_category) %>% 
    select(-num) %>% 
    mutate(value=ifelse(date>=floor_date(as.Date(dat),"month"),NA,value)) %>% 
    group_by(fiscal_year) %>% 
    arrange(fy_month) %>% 
    mutate(cum_total=cumsum(value),
           cum_share=cumsum(share))
  
  fcast_df1 = right_join(monthly_shares,fcast_df1,by="date") 
  
  fcast_df1 = fcast_df1 %>% 
    mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)),
           month=month(date),
           fy_month=case_when(
             month%in%c(10:12)~month-9,
             month%in%c(1:9)~month+3
           )) %>% 
    ungroup() %>%
    arrange(date) %>% 
    rowwise() %>% 
    mutate(first_day_weekend=(weekdays(date,abbreviate=TRUE)%in%c("Sat","Sun")|date%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=1,
           last_day_weekend=(weekdays(date %m+% months(1),abbreviate=TRUE)%in%c("Sat","Sun")|(date %m+% months(1))%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12) %>% 
    ungroup()
  
  fcast_df1$pred_cumshare=as.numeric(predict(models[[cbo_category]]$share,fcast_df1))
  fcast_df1 = fcast_df1 %>% 
    mutate(pred_total=ifelse(fiscal_year<2016,cbo_proj,cum_total/pred_cumshare),
           total=ifelse(fiscal_year<2016,NA,total)) %>% 
    relocate(pred_cumshare,pred_total,.after = cum_share) %>% 
    group_by(fiscal_year) %>% 
    fill(pred_total,cbo_proj,.direction = "down")
  
  fcast_df1$final_pred = as.numeric(predict(models[[cbo_category]]$total,fcast_df1))
  fcast_df1$final_pred_month = fcast_df1$final_pred*fcast_df1$pred_cumshare
  fcast_df1$cbo_proj_month = fcast_df1$cbo_proj*fcast_df1$pred_cumshare
  fcast_df1 = fcast_df1 %>% 
    relocate(final_pred,final_pred_month,.after = pred_total) %>% 
    group_by(fiscal_year) %>% 
    arrange(fiscal_year,fy_month) %>% 
    mutate(tst1=final_pred_month-dplyr::lag(final_pred_month,1),
           final_pred_month=case_when((fy_month==1)|(date=="2004-01-01")~final_pred_month,TRUE~tst1),
           tst1=cbo_proj_month-dplyr::lag(cbo_proj_month,1),
           cbo_proj_month=case_when((fy_month==1)|(date=="2004-01-01")~cbo_proj_month,TRUE~tst1)) %>% 
    ungroup()
  
  pred_df = data.frame(
    date=fcast_df1[['date']],
    var=cbo_category,
    predict(models[[cbo_category]]$rf,fcast_df1,se.fit=TRUE, interval="confidence", alpha=0.70),
    actual=fcast_df1[['value']],
    extrap_total=fcast_df1[['final_pred_month']],
    cbo_proj=fcast_df1[['cbo_proj_month']]
  ) %>% 
    rename(pred=fit.fit)
  
  return(list(
    'data'=fcast_df1,
    'reg'=test,
    'pred_df'=pred_df,
    'models'=models[[cbo_category]]
  ))
  
}


sim_df = data.frame()
for(dat in grep("2025",gsub("imputed_data_asof|.csv","",list.files("Data/Processing/imputed_data/")),value=TRUE)){
  
  print(dat)
  
  income = nowcast_budget_spending(dat,"Other Spending","Other Spending")$pred_df %>% 
    filter(date>="2015-03-01"&is.na(actual)) %>% 
    select(-actual) %>% 
    left_join(get_budget_outlay_df("Other Spending") %>% 
                select(date,
                       actual=value)) %>% 
    mutate(run_date=dat)
  
  sim_df = bind_rows(sim_df,income)
  
}
