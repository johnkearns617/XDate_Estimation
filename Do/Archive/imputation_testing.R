library(mcprogress)

set.seed(178)

numberOfCores = detectCores()
arima_rmse = mclapply(as.character(national_econ %>% filter(date>="2006-01-01") %>% distinct(date) %>% pull()),
                    function(dat1){
                      
                      message_parallel(dat1)
                      
                      df = make_df(dat1) %>% 
                        filter(date>="2004-01-01")
                      
                      return(impute_function_arima(df,dat1))
                      
                    },
                    mc.cores = numberOfCores)

arima_results = bind_rows(arima_rmse) %>% 
  left_join(national_econ,by=c("date"="date","variable"="series_id")) %>% 
  group_by(variable,year(prediction_date)) %>% 
  summarize(rmse=RMSE(replacement,value,na.rm=TRUE)/sd(national_econ$value[national_econ$series_id==variable[1]],na.rm=TRUE)) %>% 
  ungroup()


numberOfCores = detectCores()
ols_rmse = mclapply( unique(state_trends$date[state_trends$date>="2006-01-01"]),  #unique(state_trends$date[state_trends$date>="2006-01-01"]),
                     function(dat1){
                       
                       message_parallel(dat1)
                       
                       df = make_df(dat1) %>% 
                         filter(date>="2004-01-01")
                       
                       return(impute_function_ols(df,dat1))
                     },
                     mc.cores = numberOfCores)

ols_results = bind_rows(ols_rmse) %>% 
  left_join(national_econ,by=c("date"="date","variable"="series_id")) %>% 
  group_by(variable,year(prediction_date)) %>% 
  summarize(rmse=RMSE(replacement,value,na.rm=TRUE)/sd(national_econ$value[national_econ$series_id==variable[1]],na.rm=TRUE)) %>% 
  ungroup()

all_rf_rmse = data.frame()
for(col2 in c("PAYEMS","UNRATE","AMTMUO","RRSFS","TOTALSA","CPILFESL")){
  
rf_rmse = mclapply(unique(state_trends$date[state_trends$date>="2019-01-01"]),  #unique(state_trends$date[state_trends$date>="2006-01-01"]),
                     function(dat1){
                       
                       message_parallel(dat1)
                       
                       df = make_df(dat1) %>% 
                         filter(date>="2004-01-01")
                       
                       return(impute_function_rf(df,dat1,col2))
                     },
                     mc.cores = numberOfCores-1)
  
  all_rf_rmse = bind_rows(all_rf_rmse,bind_rows(rf_rmse))
  
}

rf_results = all_rf_rmse %>% 
  left_join(national_econ,by=c("date"="date","variable"="series_id")) %>% 
  group_by(variable,year(prediction_date)) %>% 
  summarize(rmse=RMSE(replacement,value,na.rm=TRUE)/sd(national_econ$value[national_econ$series_id==variable[1]],na.rm=TRUE)) %>% 
  ungroup()


numberOfCores = detectCores()
lasso_rmse = mclapply( unique(state_trends$date[state_trends$date>="2006-01-01"]),  #unique(state_trends$date[state_trends$date>="2006-01-01"]),
                     function(dat1){
                       
                       message_parallel(dat1)
                       
                       df = make_df(dat1) %>% 
                         filter(date>="2004-01-01")
                       
                       return(impute_function_lasso(df,dat1))
                     },
                     mc.cores = numberOfCores)

lasso_results = bind_rows(lasso_rmse) %>% 
  left_join(national_econ,by=c("date"="date","variable"="series_id")) %>% 
  group_by(variable,year(prediction_date)) %>% 
  summarize(rmse=RMSE(replacement,value,na.rm=TRUE)/sd(national_econ$value[national_econ$series_id==variable[1]],na.rm=TRUE)) %>% 
  ungroup()


numberOfCores = detectCores()
kalman_rmse = mclapply(unique(state_trends$date[state_trends$date>="2006-01-01"]),
                      function(dat1){
                        
                        message_parallel(dat1)
                        
                        df = make_df(dat1) %>% 
                          filter(date>="2004-01-01")
                        
                        return(impute_function_kalman(df,dat1))
                        
                      },
                      mc.cores = numberOfCores)

kalman_results = bind_rows(kalman_rmse) %>% 
  left_join(national_econ,by=c("date"="date","variable"="series_id")) %>% 
  group_by(variable,year(prediction_date)) %>% 
  summarize(rmse=RMSE(replacement,value,na.rm=TRUE)/sd(national_econ$value[national_econ$series_id==variable[1]],na.rm=TRUE)) %>% 
  ungroup()


numberOfCores = detectCores()
mice_rmse = mclapply(unique(state_trends$date[state_trends$date>="2006-01-01"]),
                       function(dat1){
                         
                         message_parallel(dat1)
                         
                         df = make_df(dat1) %>% 
                           filter(date>="2004-01-01")
                         
                         return(impute_function_kalman(df,dat1))
                         
                       },
                       mc.cores = numberOfCores)

mice_results = bind_rows(mice_rmse) %>% 
  left_join(national_econ,by=c("date"="date","variable"="series_id")) %>% 
  group_by(variable,year(prediction_date)) %>% 
  summarize(rmse=RMSE(replacement,value,na.rm=TRUE)/sd(national_econ$value[national_econ$series_id==variable[1]],na.rm=TRUE)) %>% 
  ungroup()

# numberOfCores = detectCores()
# nixtla_rmse = mclapply(unique(state_trends$date[state_trends$date>="2006-01-01"]),
#                      function(dat1){
#                        
#                        message_parallel(dat1)
#                        
#                        df = make_df(dat1) %>% 
#                          filter(date>="2004-01-01")
#                        
#                        return(impute_function_nixtla(df,dat1))
#                        
#                      },
#                      mc.cores = numberOfCores)
# 
# nixtla_results = bind_rows(nixtla_rmse) %>% 
#   left_join(national_econ,by=c("date"="date","variable"="series_id")) %>% 
#   group_by(variable,year(prediction_date)) %>% 
#   summarize(rmse=RMSE(replacement,value,na.rm=TRUE)/sd(national_econ$value[national_econ$series_id==variable[1]],na.rm=TRUE)) %>% 
#   ungroup()

# compare performance
results_df = arima_results %>% 
  rename(arima=rmse) %>% 
  left_join(ols_results %>% rename(ols=rmse)) %>% 
  left_join(rf_results %>% rename(rf=rmse)) %>% 
  left_join(lasso_results %>% rename(lasso=rmse)) %>% 
  left_join(kalman_results %>% rename(kalman=rmse)) %>% 
  left_join(mice_results %>% rename(mice=rmse))
colnames(results_df)[2] = "year"

ggplot(results_df %>% pivot_longer(cols=arima:mice),aes(x=year,y=value,color=name)) +
  geom_smooth()
