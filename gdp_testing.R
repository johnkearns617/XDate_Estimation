col="GDPC1"  

dat = "2025-01-15"

print(paste0(col," ",dat))

imputed_df = read_csv(paste0("Data/Processing/imputed_data/imputed_data_asof",dat,".csv"))

fcast_gdp_ols = function(dat,col){
  
  fcast_df1 = read_csv(paste0("Data/Processing/imputed_data/imputed_data_asof",dat,".csv")) %>% 
    arrange(date) %>%
    mutate(year=year(date),
           qtr=quarter(date)) %>%
    select(-c(PCE,PRS85006112)) %>%
    select(-one_of("ADPMNUSNERSA")) %>% 
    group_by(year,qtr) %>%
    mutate_at(vars(PAYEMS:gt_999),~mean(.,na.rm=TRUE)) %>%
    summarize_all(~.[1]) %>%
    ungroup() %>% 
    left_join(national_econ %>% 
                filter(release_date<=dat) %>% 
                select(date,series_id,value) %>%
                pivot_wider(names_from=series_id,values_from=value) %>% 
                select(date,A261RX1Q020SBEA:SLCEC1)) %>%
    arrange(date) %>%
    mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10,col),~((./dplyr::lag(.,1)-1)*100)) %>%
    mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,gt_1003:gt_999),~(.-dplyr::lag(.,1))) %>%
    mutate(lag1=dplyr::lag(!!sym(col),1),
           lag2=dplyr::lag(!!sym(col),2),
           lag3=dplyr::lag(!!sym(col),3),
           lag4=dplyr::lag(!!sym(col),4)) %>%
    ungroup() %>% 
    mutate(IHLIDXUS=ifelse(is.nan(IHLIDXUS),0,IHLIDXUS))
  
X = model.matrix(as.formula(paste0(col,"~",paste(colnames(fcast_df1)[c(4:246)],collapse="+"))),
                 fcast_df1 %>% filter(date<dat&year(date)>=2006&!is.na(!!sym(col))))[, -1]
y = (fcast_df1 %>% filter(date<dat&year(date)>=2006&!is.na(!!sym(col))))[[col]]

if(length(y)<4){next}

weight = (1:nrow(X))/nrow(X)
weight = ifelse(weight<.5,.5,weight)
fit_lasso_state = glmnet(X, y, alpha = 1,pmax=20)
# weight by how recent the data is

selected_coefs_state = data.frame(varImp(fit_lasso_state,lambda=min(fit_lasso_state$lambda), scale = FALSE)) %>% filter(Overall!=0)
selected_coefs_state$var = as.numeric(gsub("gt_","",rownames(selected_coefs_state)))
coef_value_state = coef(fit_lasso_state,s=min(fit_lasso_state$lambda))[,1][-1]
coef_value_state = coef_value_state[coef_value_state!=0]
selected_coefs_state = cbind(selected_coefs_state,coef_value_state)
selected_coefs_state$category = sapply(selected_coefs_state$var,which_category)
selected_coefs_state = selected_coefs_state %>% arrange(-Overall)

test = lm_robust(as.formula(paste0(col,"~lag1+lag2+",paste(rownames(selected_coefs_state),collapse="+"))),
                 data = fcast_df1 %>% filter(date<=dat))

if(length(tail(fcast_df1,10) %>% filter(is.na(!!sym(col))) %>% pull(date))>1){
  
  fcast_df1$lag1[fcast_df1$date==(tail(fcast_df1,10) %>% filter(is.na(!!sym(col))) %>% pull(date))[2]] = predict(test,fcast_df1 %>% filter(date%in%(tail(fcast_df1,10) %>% filter(is.na(!!sym(col))) %>% pull(date))[1]))
  
}

gdp_pred_df = data.frame(
    prediction_date=dat,
    date=tail(fcast_df1,10) %>% filter(is.na(!!sym(col))) %>% pull(date),
    var=col,
    pred=predict(test,fcast_df1 %>% filter(date%in%(tail(fcast_df1,10) %>% filter(is.na(!!sym(col))) %>% pull(date))))
  ) %>% 
  left_join(national_econ %>% filter(series_id==col) %>% select(date,value) %>% mutate(value=(value/dplyr::lag(value,1)-1)*100))

return(gdp_pred_df)

}

fcast_gdp_ols("2025-01-15",col)

col = "GDPC1"
fcast_df1 = imputed_df %>% 
  arrange(date) %>%
  mutate(year=year(date),
         qtr=quarter(date)) %>%
  select(-c(PCE,PRS85006112)) %>%
  #select(-one_of("ADPMNUSNERSA")) %>% 
  group_by(year,qtr) %>%
  mutate_at(vars(PAYEMS:gt_999),~mean(.,na.rm=TRUE)) %>%
  summarize_all(~.[1]) %>%
  ungroup() %>% 
  left_join(national_econ %>% 
              filter(release_date<=dat) %>% 
              select(date,series_id,value) %>%
              pivot_wider(names_from=series_id,values_from=value) %>% 
              select(date,A261RX1Q020SBEA:SLCEC1))

gdp_pred = data.frame()
dat=Sys.Date()
for(i in as.character(tail(fcast_df1,10) %>% filter(is.na(GDPC1)) %>% pull(date))){
  
  consump = (fcast_gdp_ols(dat,"PCECC96") %>% filter(date==i) %>% pull(pred)/100+1)*(fcast_df1 %>% filter(date<i) %>% slice(n()) %>% pull(PCECC96))
  invest = (fcast_gdp_ols(dat,"GPDIC1") %>% filter(date==i) %>% pull(pred)/100+1)*(fcast_df1 %>% filter(date<i) %>% slice(n()) %>% pull(GPDIC1))
  exports = (fcast_gdp_ols(dat,"EXPGSC1") %>% filter(date==i) %>% pull(pred)/100+1)*(fcast_df1 %>% filter(date<i) %>% slice(n()) %>% pull(EXPGSC1))
  imports = (fcast_gdp_ols(dat,"IMPGSC1") %>% filter(date==i) %>% pull(pred)/100+1)*(fcast_df1 %>% filter(date<i) %>% slice(n()) %>% pull(IMPGSC1))
  govt = (fcast_gdp_ols(dat,"GCEC1") %>% filter(date==i) %>% pull(pred)/100+1)*(fcast_df1 %>% filter(date<i) %>% slice(n()) %>% pull(GCEC1))

  fcast_df1$PCECC96[fcast_df1$date==i] = consump
  fcast_df1$GPDIC1[fcast_df1$date==i] = invest
  fcast_df1$EXPGSC1[fcast_df1$date==i] = exports
  fcast_df1$IMPGSC1[fcast_df1$date==i] = imports
  fcast_df1$GCEC1[fcast_df1$date==i] = govt
  
  fcast_df1$GDPC1[fcast_df1$date==i] = sum(consump,invest,exports,-1*imports,govt)
  
  gdp_pred = bind_rows(
    gdp_pred,
    data.frame(date=i,
               consump,
               invest,
               exports,
               imports,
               govt,
  gdp = fcast_df1 %>% mutate(pred=(GDPC1/dplyr::lag(GDPC1,1)-1)*100) %>%  filter(date==i) %>% pull(pred)
  )
  ) %>% 
    left_join(national_econ %>% filter(series_id=="GDPC1") %>% select(date,value) %>% mutate(date=as.character(date),value=(value/dplyr::lag(value,1)-1)*100) %>% filter(date==i))
    
}



fcast_gdp_arima = function(fcast_df1,dat,col){
  
  require(forecast)
  
  test = auto.arima(fcast_df1 %>% filter(date<=dat) %>% pull(col))
  
  gdp_pred_df = data.frame(
    prediction_date=dat,
    date=tail(fcast_df1,10) %>% filter(is.na(!!sym(col))) %>% pull(date),
    var=col,
    pred=as.numeric(forecast(test,h=length(tail(fcast_df1,10) %>% filter(is.na(!!sym(col))) %>% pull(date)))$mean)
  ) %>% 
    left_join(national_econ %>% filter(series_id==col) %>% select(date,value) %>% mutate(value=(value/dplyr::lag(value,1)-1)*100))
  
  
  return(gdp_pred_df)
  
}

fcast_gdp_arima(fcast_df1,"2025-01-15","GDPC1")

fcast_gdp_rf = function(fcast_df1,dat,col){
  
  require(tuneRanger)
  
  set.seed(178)
  
  task = makeRegrTask(data = fcast_df1 %>% 
                                select(any_of(c(colnames(imputed_df),col,"lag1","lag2","lag3","lag4"))) %>% 
                        select(-date) %>%
                        drop_na(), 
                      target = col,
                      id="id")
  
  estimateTimeTuneRanger(task)
  
  res = tuneRanger(task, measure = list(mse), num.trees = 1000, 
                   num.threads = 2, iters = 70)
  
  test = ranger(
    as.formula(paste0(col,"~.-id")),
    data = fcast_df1 %>% 
      select(any_of(c(colnames(imputed_df),col,"lag1","lag2","lag3","lag4"))) %>% 
      select(-date) %>%
      drop_na(),
    mtry=res$recommended.pars$mtry,
    min.node.size = res$recommended.pars$min.node.size,
    sample.fraction = res$recommended.pars$sample.fraction,
    importance="impurity",
    regularization.factor = .9
  )
  
  if(length(tail(fcast_df1,10) %>% filter(is.na(!!sym(col))) %>% pull(date))>1){
    
    fcast_df1$lag1[fcast_df1$date==(tail(fcast_df1,10) %>% filter(is.na(!!sym(col))) %>% pull(date))[2]] = predict(test,fcast_df1 %>% filter(date%in%(tail(fcast_df1,10) %>% filter(is.na(!!sym(col))) %>% pull(date))[1]))$predictions
    
  }
  
  gdp_pred_df = data.frame(
    prediction_date=dat,
    date=tail(fcast_df1,10) %>% filter(is.na(!!sym(col))) %>% pull(date),
    var=col,
    pred=predict(test,fcast_df1 %>% filter(date%in%(tail(fcast_df1,10) %>% filter(is.na(!!sym(col))) %>% pull(date))))
  ) %>% 
    left_join(national_econ %>% filter(series_id==col) %>% select(date,value) %>% mutate(value=(value/dplyr::lag(value,1)-1)*100))
  
  return(gdp_pred_df)
  
}

fcast_gdp_rf(fcast_df1,"2025-01-15","GDPC1")

