# transform_data_quarterly.R
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
library(DALEX)

conflicted::conflict_prefer("filter","dplyr")
conflicted::conflicts_prefer(DALEX::explain)
conflicted::conflict_prefer("select","dplyr")


set.seed(178)

# make the transformations you need

fcast_df = imputed_df %>% 
  arrange(date) %>%
  mutate(year=year(date),
         qtr=quarter(date)) %>%
  select(-c(PCE,PRS85006112)) %>% 
  group_by(year,qtr) %>%
  mutate_at(vars(PAYEMS:gt_999),~mean(.,na.rm=TRUE)) %>%
  summarize_all(~.[1]) %>%
  ungroup() %>% 
  left_join(national_econ %>% 
              select(date,series_id,value) %>%
              pivot_wider(names_from=series_id,values_from=value) %>% 
              select(date,A261RX1Q020SBEA:SLCEC1))


avail_files = gsub("imputed_data_asof|.csv","",list.files("Data/Processing/imputed_data"))
avail_files = avail_files[which(avail_files>as.character((tail(fcast_df,10) %>% filter(is.na(GDPC1)) %>% pull(date))[1]-1))]
  
gdp_pred_df = list()
for(dat in avail_files){
  
  imputed_df1 = read_csv(paste0("Data/Processing/imputed_data/imputed_data_asof",dat,".csv"))

  fcast_df1 = imputed_df1 %>% 
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
    
  for(i in as.character(tail(fcast_df1,10) %>% filter(is.na(GDPC1)) %>% pull(date))){
    
  consump_list = fcast_gdp_ols(dat,"PCECC96") 
  consump = (consump_list[[1]] %>% filter(date==i) %>% pull(pred)/100+1)*(fcast_df1 %>% filter(date<i) %>% slice(n()) %>% pull(PCECC96))
  invest_list = fcast_gdp_ols(dat,"GPDIC1") 
  invest = (invest_list[[1]] %>% filter(date==i) %>% pull(pred)/100+1)*(fcast_df1 %>% filter(date<i) %>% slice(n()) %>% pull(GPDIC1))
  exports_list = fcast_gdp_ols(dat,"EXPGSC1") 
  exports = (exports_list[[1]] %>% filter(date==i) %>% pull(pred)/100+1)*(fcast_df1 %>% filter(date<i) %>% slice(n()) %>% pull(EXPGSC1))
  imports_list = fcast_gdp_ols(dat,"IMPGSC1")
  imports = (imports_list[[1]] %>% filter(date==i) %>% pull(pred)/100+1)*(fcast_df1 %>% filter(date<i) %>% slice(n()) %>% pull(IMPGSC1))
  govt_list = fcast_gdp_ols(dat,"GCEC1") 
  govt = (govt_list[[1]] %>% filter(date==i) %>% pull(pred)/100+1)*(fcast_df1 %>% filter(date<i) %>% slice(n()) %>% pull(GCEC1))
  
  fcast_df1 = fcast_df1 %>% 
    mutate(residual=GDPC1-PCECC96-GPDIC1-EXPGSC1+IMPGSC1-GCEC1)
  residual = as.numeric(forecast(auto.arima(fcast_df1 %>% filter(!is.na(residual)) %>% pull(residual)),1)$mean)
  
  fcast_df1$PCECC96[fcast_df1$date==i] = consump
  fcast_df1$GPDIC1[fcast_df1$date==i] = invest
  fcast_df1$EXPGSC1[fcast_df1$date==i] = exports
  fcast_df1$IMPGSC1[fcast_df1$date==i] = imports
  fcast_df1$GCEC1[fcast_df1$date==i] = govt
  fcast_df1$residual[fcast_df1$date==i] = residual
  
  fcast_df1$GDPC1[fcast_df1$date==i] = sum(consump,invest,exports,-1*imports,govt,residual)
  
  gdp_pred_df[[paste0(dat,"-",i)]] = list(
    data=data.frame(date=i,
                    prediction_date=dat,
                    consump,
                    invest,
                    exports,
                    imports,
                    govt,
                    residual,
                    gdp = fcast_df1 %>% mutate(pred=(GDPC1/dplyr::lag(GDPC1,1)-1)*100) %>%  filter(date==i) %>% pull(pred)
    ) %>% 
      left_join(national_econ %>% filter(series_id=="GDPC1") %>% select(date,value) %>% mutate(date=as.character(date),value=(value/dplyr::lag(value,1)-1)*100) %>% filter(date==i) %>% rename(actual=value)),
    explainers=lapply(c("consump","invest","exports","imports","govt"),function(x) get(paste0(x,"_list"))[[2]]),
    breakdowns=lapply(c("consump","invest","exports","imports","govt"),function(x) get(paste0(x,"_list"))[[3]])
  )
  
  }
}

gdp_data = bind_rows(lapply(gdp_pred_df, `[[`, 'data')) %>% 
  mutate(gdp=((1+gdp/100)^4-1)*100,
         actual=((1+actual/100)^4-1)*100)



# Compute variable contributions for multiple observations (e.g., over time)
breakdown_df = bind_rows(lapply(gdp_pred_df, `[[`, 'breakdowns')) %>% 
  filter(variable!='prediction') %>% 
  rowwise() %>% 
  mutate(diff=fcast_df1[[var]][fcast_df1$date==date],
         diff=diff-fcast_df[[var]][fcast_df$date==(date %m-% months(3))],
         gdp=fcast_df[['GDPC1']][fcast_df$date==(date %m-% months(3))],
         tmp=(contribution/100),
         diff=ifelse(var=="IMPGSC1",-1*diff,diff)) %>% 
  ungroup() %>% 
  group_by(prediction_date,date,var) %>% 
  mutate(tmp=tmp/sum(tmp)*(diff)/gdp) %>% 
  ungroup() %>% 
  bind_rows(gdp_data %>% 
              filter(!is.na(residual)) %>% 
              select(date,prediction_date,residual) %>% 
              rowwise() %>% 
              mutate(diff=fcast_df1[['residual']][fcast_df$date==(as.Date(date) %m-% months(3))],
                     gdp=fcast_df[['GDPC1']][fcast_df$date==(as.Date(date) %m-% months(3))],
                     tmp=(residual-diff)/gdp,
                     date=as.Date(date),
                     var='residual',
                     variable_name='residual')) %>% 
  group_by(prediction_date,date,variable_name) %>% 
  summarize(contribution=sum(tmp)) %>% 
  ungroup() %>% 
  mutate(contribution=((1+contribution)^4-1)*100)

# Plot contribution of variables over observations
plotly::ggplotly(ggplot(breakdown_df %>% mutate(prediction_date=as.Date(prediction_date)), aes(x = prediction_date, y = contribution, fill = variable_name)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_line(data=gdp_data %>% ungroup() %>% mutate(prediction_date=as.Date(prediction_date)),
            aes(x=prediction_date,y=(gdp)),inherit.aes = FALSE) +
  geom_point(data=gdp_data %>% ungroup()  %>% mutate(prediction_date=as.Date(prediction_date)),
             aes(x=prediction_date,y=(gdp)),inherit.aes = FALSE) +
  geom_hline(data=gdp_data %>% ungroup()  %>% mutate(prediction_date=as.Date(prediction_date)),
               aes(yintercept=(actual)),color="darkblue",inherit.aes = FALSE) +
  labs(title = "Variable Contribution Over Time",
       x = "Observation (Time)", 
       y = "Contribution to Fitted Value") +
  theme_minimal() +
    facet_wrap(~date,scales="free",ncol=1)
) 



