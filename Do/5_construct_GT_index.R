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

gdp_pred_df = data.frame()
#for(col in colnames(fcast_df %>% select(A261RX1Q020SBEA:SLCEC1))){
for(col in c("GDPC1")){

  avail_files = gsub("imputed_data_asof|.csv","",list.files("Data/Processing/imputed_data"))
  avail_files = avail_files[which(avail_files>="2024-12-31"&avail_files<="2025-03-31")]
  
for(dat in avail_files){
  
  print(paste0(col," ",dat))
  
  imputed_df = read_csv(paste0("Data/Processing/imputed_data/imputed_data_asof",dat,".csv"))
  
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
                select(date,series_id,value) %>%
                pivot_wider(names_from=series_id,values_from=value) %>% 
                select(date,A261RX1Q020SBEA:SLCEC1)) %>%
    arrange(date) %>%
    mutate_at(vars(PAYEMS:JTSJOL,ADPMNUSNERSA,INDPRO:DGS10,col),~((./dplyr::lag(.,1)-1)*100)) %>%
    mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,gt_1003:gt_999),~(.-dplyr::lag(.,1))) %>%
    mutate(lag1=dplyr::lag(!!sym(col),1),
           lag2=dplyr::lag(!!sym(col),2),
           lag3=dplyr::lag(!!sym(col),3),
           lag4=dplyr::lag(!!sym(col),4)) %>%
    ungroup() %>% 
    mutate(IHLIDXUS=ifelse(is.nan(IHLIDXUS),0,IHLIDXUS))
  
  
  X = model.matrix(as.formula(paste0(col,"~",paste(colnames(fcast_df1)[c(4:246)],collapse="+"))),
                   fcast_df1 %>% filter(date<"2020-01-01"&date<dat&year(date)>=2006&!is.na(!!sym(col))))[, -1]
  y = (fcast_df1 %>% filter(date<"2020-01-01"&date<dat&year(date)>=2006&!is.na(!!sym(col))))[[col]]
  
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
  
  test = lm_robust(as.formula(paste0(col,"~lag1+lag2+ADPMNUSNERSA+",paste(rownames(selected_coefs_state),collapse="+"))),
                   data = fcast_df1 %>% filter(date<=dat))
  
  gdp_pred_df = bind_rows(
    gdp_pred_df,
    data.frame(
      date=dat,
      var=col,
      pred=predict(test,fcast_df1 %>% filter(date==floor_date(as.Date(dat),"quarter"))),
      actual=fcast_df1[[col]][fcast_df1$date==floor_date(as.Date(dat),"quarter")]
    )
  )
}
sqrt(mean((gdp_pred_df$pred-gdp_pred_df$actual)^2,na.rm=TRUE))
gdp_pred_df$error[gdp_pred_df$var==col]=sqrt(mean((gdp_pred_df$pred[gdp_pred_df$var==col]-gdp_pred_df$actual[gdp_pred_df$var==col])^2,na.rm=TRUE))/mean(gdp_pred_df$actual[gdp_pred_df$var==col],na.rm=TRUE)
}


q1_preds = data.frame()
for(dat in avail_files){
imputed_df = read_csv(paste0("Data/Processing/imputed_data/imputed_data_asof",dat,".csv"))

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
              select(date,series_id,value) %>%
              pivot_wider(names_from=series_id,values_from=value) %>% 
              select(date,A261RX1Q020SBEA:SLCEC1)) %>%
  arrange(date) %>%
  mutate_at(vars(PAYEMS:JTSJOL,ADPMNUSNERSA,INDPRO:DGS10,col),~((./dplyr::lag(.,1)-1)*100)) %>%
  mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,gt_1003:gt_999),~(.-dplyr::lag(.,1))) %>%
  mutate(lag1=dplyr::lag(!!sym(col),1),
         lag2=dplyr::lag(!!sym(col),2),
         lag3=dplyr::lag(!!sym(col),3),
         lag4=dplyr::lag(!!sym(col),4)) %>%
  ungroup() %>% 
  mutate(IHLIDXUS=ifelse(is.nan(IHLIDXUS),0,IHLIDXUS))

q1_preds = bind_rows(
  q1_preds,
  data.frame(obs_date=dat,fcast_df1 %>% filter(date=="2025-01-01"))
)

}

# Create an explainer
explainer <- explain(test, 
                     data = fcast_df1 %>% filter(date<"2025-01-01"&!is.na(GDPC1)&!is.na(lag2)) %>% select(names(test$coefficients)[-1]), 
                     y = (fcast_df1 %>% filter(date<"2025-01-01"&!is.na(GDPC1)&!is.na(lag2)))$GDPC1)

# Compute variable contributions for multiple observations (e.g., over time)
breakdown_list <- lapply(1:nrow(q1_preds), function(i) {
  breakdown <- predict_parts(explainer, new_observation = (q1_preds %>% slice(i) %>% select(names(test$coefficients)[-1])), type = "break_down")
  breakdown$observation <- i  # Add observation index (time)
  breakdown
})

# Combine results
breakdown_df <- do.call(rbind, breakdown_list) %>% 
  left_join(data.frame(eval_date=unique(q1_preds$obs_date),
                       observation=1:length(unique(q1_preds$obs_date))))

# Plot contribution of variables over observations
plotly::ggplotly(ggplot(breakdown_df %>% filter(variable!="prediction"), aes(x = eval_date, y = contribution, fill = variable_name)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_line(data=gdp_pred_df %>% ungroup() %>% filter(date>="2025-01-01") %>% mutate(date=1:n()),
            aes(x=date,y=pred),inherit.aes = FALSE) +
  geom_point(data=gdp_pred_df %>% ungroup() %>% filter(date>="2025-01-01") %>% mutate(date=1:n()),
              aes(x=date,y=pred),inherit.aes = FALSE) +
  labs(title = "Variable Contribution Over Time",
       x = "Observation (Time)", 
       y = "Contribution to Fitted Value") +
  theme_minimal()
)


