# get_google_trends.R
# John Kearns
# Goal: Write script to pull categories to replicate Nakazawa 2022

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

conflicted::conflict_prefer("filter","dplyr")

data(categories) # categories from Google Trends
fred_key = "156b9cd1b9a52db3b9fc0bab8aca2b39"

# initialize FRED link
fredr_set_key(fred_key)

# read in categories to pull
ci_str_detect <- function(x, y) {
  str_detect(y, pattern = sub('(?<=.{3})', '.', x, perl = TRUE))
}

which_category = function(num){
  
  return(trends_cats$category[trends_cats$id==num][1])
  
}

nk_categories = read_csv(paste0("Data/Raw/Nakazawa_Categories.csv")) %>% 
  left_join(categories %>% mutate(name=gsub(" ","",name)),by=c("variables"="name")) %>% 
  filter(!is.na(id)) %>% 
  distinct(id,.keep_all=TRUE)
# do we want to add anymore categories?

trends_cats = read_delim(paste0("Data/google_trend_categories.txt"), delim=': ')
colnames(trends_cats) = c('category','id')

trends_data = read_csv(paste0("Data/Raw/full_trends_df_weekly.csv")) %>% 
  select(-`...1`) %>% 
  mutate(flag=ifelse(date<dplyr::lag(date,1),1,0)) %>% 
  mutate(pull_grp=case_when(
    flag==1&date<"2005-01-01"~1,
    flag==1&date<"2009-01-01"~2,
    flag==1&date<"2013-01-01"~3,
    flag==1&date<'2017-01-01'~4,
    flag==1&date<'2021-01-01'~5,
    flag==1&date<'2025-01-01'~6
  ))

trends_data$pull_grp[1] = 1

trends_data = trends_data %>% 
  fill(pull_grp,.direction="down")


# # are there any missing categories?
# setdiff(nk_categories$id,trends_data$cat)
# # we are good
# 
# # for categories that were captured, are all filled?
# completeness_check = trends_data %>%
#   group_by(cat,date) %>%
#   distinct(state) %>%
#   summarize(num=n()) %>%
#   ungroup() %>%
#   filter(num<50)
# nrow(completeness_check)
# # we are good
# 
# check2 = trends_data %>%
#   group_by(state,substr(date,1,7),cat) %>%
#   summarize(num=n())
# we are good

# fix pull group jumps
trends_data = trends_data %>% 
  group_by(cat) %>% 
  mutate(weight=case_when(
    year(date)==2008&pull_grp==1~mean(tail(value[year(date)==2008&pull_grp==1],40)-tail(value[year(date)==2008&pull_grp==2],40),na.rm=TRUE),
    year(date)==2012&pull_grp==2~mean(tail(value[year(date)==2012&pull_grp==2],40)-tail(value[year(date)==2012&pull_grp==3],40),na.rm=TRUE),
    year(date)==2016&pull_grp==3~mean(tail(value[year(date)==2016&pull_grp==3],40)-tail(value[year(date)==2016&pull_grp==4],40),na.rm=TRUE),
    year(date)==2020&pull_grp==4~mean(tail(value[year(date)==2020&pull_grp==4],40)-tail(value[year(date)==2020&pull_grp==5],40),na.rm=TRUE),
    year(date)==2024&pull_grp==5~mean(tail(value[year(date)==2024&pull_grp==5],40)-tail(value[year(date)==2024&pull_grp==6],40),na.rm=TRUE)
    )) %>% 
  mutate(value_adj=value,
         value_adj=ifelse(pull_grp<=1,value_adj-tail(weight[pull_grp==1],1),value_adj),
         value_adj=ifelse(pull_grp<=2,value_adj-tail(weight[pull_grp==2],1),value_adj),
         value_adj=ifelse(pull_grp<=3,value_adj-tail(weight[pull_grp==3],1),value_adj),       
         value_adj=ifelse(pull_grp<=4,value_adj-tail(weight[pull_grp==4],1),value_adj),
         value_adj=ifelse(pull_grp<=5,value_adj-tail(weight[pull_grp==5],1),value_adj)) %>% 
  filter(pull_grp==1|(year(date)>2008&pull_grp==2)|(year(date)>2012&pull_grp==3)|(year(date)>2016&pull_grp==4)|(year(date)>2020&pull_grp==5)|(year(date)>2024&pull_grp==6))

#get rid of duplicates
lazy_trends_data = dtplyr::lazy_dt(trends_data)
trends_data1 = lazy_trends_data %>%
  group_by(cat) %>% 
  mutate(value_adj2=ifelse(date<"2011-01-01",value_adj+(mean(tail(value_adj[year(date)==2011],15))-mean(tail(value_adj[year(date)==2010],15))),value_adj), # level adjustment for all data before 2011
         value_adj2=ifelse(date<"2016-01-01",value_adj2+(mean(tail(value_adj2[year(date)==2016],15))-mean(tail(value_adj2[year(date)==2015],15))),value_adj2)) %>% 
  ungroup() %>% 
  as.data.frame()

coef_df = data.frame()

reg_df = trends_data1 %>% 
  filter(date<"2016-01-01") %>% 
  group_by(cat) %>%
  mutate(time_var=1:n()) %>%
  ungroup() %>%
  select(time_var,date,value_adj2,cat)
  
lm1 = lm(value_adj2~time_var*factor(cat),reg_df)
  
tmp = broom::tidy(lm1) %>% 
  slice(2,220:nrow(tidy(lm1))) %>% 
  mutate(term=gsub("time_var:factor\\(cat\\)","",term),
         estimate=estimate+estimate[1]) %>% 
  select(cat=term,coef=estimate) %>% 
  mutate(cat=as.numeric(cat))
tmp$cat[1] = 77
  
coef_df = bind_rows(coef_df,tmp)

time_match = reg_df %>% 
  filter(cat==249) %>% 
  mutate(date=rev(as.Date(date))) %>% 
  select(time_var,date)
trends_data1 = left_join(trends_data1,time_match,by=c("date"="date")) %>% 
  mutate(time_var=ifelse(is.na(time_var),0,time_var))
trends_data1 = left_join(trends_data1,coef_df,by=c("cat"))
trends_data1$value_detrend = ifelse(trends_data1$date<"2016-01-01",trends_data1$value_adj2+(trends_data1$coef*trends_data1$time_var),trends_data1$value_adj2)

 # smooth over jumps, seasonally adjust by category, by state


  
trends_sa = data.frame()

for(cat1 in unique(trends_data1$cat[trends_data1$cat%in%nk_categories$id])){

 print(paste0(cat1))
 test_cat = trends_data1[trends_data1$cat==cat1,] %>%
   select(date,value,value_detrend) %>%
   mutate(date=as.Date(date))

 hits <- test_cat$value_detrend
 #--------------------------------------------------------------

 #do some other convenience operations---------------------------
 dates <- test_cat$date
 hits <- ts(hits,start=c(year(dates[1]),month(dates[1]),day(dates[1])),frequency=52)

 decompose_air = decompose(hits, "multiplicative")
 adjust_air = hits / decompose_air$seasonal
 adjust_air = ifelse(is.nan(adjust_air)|is.infinite(adjust_air),0,adjust_air)

 hits_smooth = as.numeric(modelbased::smoothing(as.numeric(adjust_air), method = "smooth"))

 test_cat = cbind(test_cat,hits_smooth)
 colnames(test_cat)[ncol(test_cat)] = "value_sa"

 hits_loess = hpfilter(as.numeric(hits_smooth),freq=(1600*(12^4)))$trend
 test_cat = cbind(test_cat,hits_loess)
 colnames(test_cat)[ncol(test_cat)] = "value_loess"

 trends_sa = bind_rows(trends_sa,test_cat %>% mutate(category=cat1,value_sa=as.numeric(value_sa)))

}


# get rid of the categories with little to no data
drop_sd_cats = trends_sa %>% 
  group_by(category) %>% 
  summarize(sd=sd(value_sa,na.rm=TRUE)) %>% 
  group_by(category) %>% 
  summarize(sd=mean(sd,na.rm=TRUE)) %>% 
  filter(sd<1)

trends_sa2 = trends_sa %>% 
  filter(!(category%in%drop_sd_cats$category))

trends_sa2 = trends_sa2 %>% 
  group_by(category) %>% 
  mutate(deviation=(value_sa/value_loess-1)*100,
         deviation_sd=sd(deviation[year(date)%in%c(2010:2019)],na.rm=TRUE),
         deviation=deviation/deviation_sd)

plot_cat = function(cat1){
  
  plt1 = ggplot(trends_sa %>% 
                  dplyr::filter(category%in%cat1) %>% 
                  mutate(category=factor(category,levels=trends_cats$id,labels=trends_cats$category)),aes(x=date)) + 
    geom_line(aes(y=value,color="Raw")) + 
    geom_line(aes(y=value_detrend,color="Detrend")) + 
    geom_line(aes(y=value_sa,color="SA")) + 
    geom_line(aes(y=value_loess,color="LOESS")) +
    facet_wrap(~category,scales="free_y") +
    labs(subtitle = paste0(cat1)) +
    geom_vline(xintercept=as.Date("2007-09-01")) + 
    geom_vline(xintercept = as.Date("2010-06-01")) +
    theme_bw()
  
  print(plt1) 
  
}


write_csv(trends_sa2,paste0("Data/Processing/trends_full_sa_",gsub("-","",Sys.Date()),".csv"))
#trends_sa3 = read_csv(paste0(data_folder,"Processing/trends_full_sa_20230925.csv"))


# ideas:
# weight recent data more heavily
# predict state tax and spending components



# # write code to make pseudo-out-of-sample RMSE measurement and test set
# AR_model_function = function(data,extra_vars=c(),gdp_var="rgdp_yoy_pchange") {
#   
#   mod = lm_robust(as.formula(paste0(gdp_var,"~lag1_",gdp_var,paste(c("",extra_vars),collapse="+"),"+factor(state)")),data=data)
#   
#   return(mod)
#   
# }
# 
# ELNET_model_function = function(X,Y,extra_vars=c(),gdp_var="rgdp_yoy_pchange") {
#   
#   control <- trainControl(method = "repeatedcv",
#                           number = 5,
#                           repeats = 5,
#                           search = "random",
#                           verboseIter = TRUE)
#   
#   # Training ELastic Net Regression model
#   mod <- train(as.formula(paste0(gdp_var,"~lag1_",gdp_var,paste(c("",extra_vars),collapse="+"),paste(c("",state.abb),collapse="+"))),
#                data = cbind(X, Y),
#                method = "glmnet",
#                preProcess = c("center", "scale"),
#                tuneLength = 25,
#                penalty.factor=ifelse(nchar(colnames(X))==2,0,1),
#                trControl = control)
#   
#   return(mod)
#   
# }
# 
# pseudo_OOS_RMSE = function(data_type=c("test","train"),model_type=c("AR"),extra_vars=c(),gdp_var="rgdp_yoy_pchange"){
#   
#   if(data_type=="train"){
#     
#     data = reg_data %>% filter(date<"2020-01-01")
#     
#     if(model_type=="ELNET"){
#       
#       data = data %>% 
#         mutate(dummy=1) %>%
#         spread(key=state,value=dummy, fill=0)
#       
#     }
#     
#     rmse_df = data.frame()
#     for(dat in as.Date((data %>% filter(date>="2017-04-01") %>% distinct(date))$date,format="%Y-%m-%d")){
#       
#       if(model_type=="AR"){
#         mod = AR_model_function(data %>% filter(date<dat),extra_vars,gdp_var)
#       
#         rmse_df = bind_rows(rmse_df,cbind(data %>% filter(date==dat),
#                                         pred_gdp=predict(mod,data %>% filter(date==dat))))
#       
#         }
#     
#     if(model_type=="ELNET"){
#       
#       X <- data %>%
#         filter(date>"2016-03-31"&date<dat) %>% 
#         select(c(paste0("lag1_",gdp_var),extra_vars,state.abb)) %>% 
#         mutate_at(vars(-state.abb), funs(c(scale(.,center=TRUE,scale=FALSE))))
#       Y <- data %>% 
#         filter(date>"2016-03-31"&date<dat) %>% 
#         select(gdp_var)
#       
#       mod = ELNET_model_function(X,Y,extra_vars,gdp_var)
#       
#       rmse_df = bind_rows(rmse_df,cbind(data %>% filter(date==dat),
#                                         pred_gdp=predict(mod,data %>% filter(date==dat))))
#       
#       
#     }
#     }
#     
#     if(model_type=="ELNET"){
#       RMSE = rmse_df %>%
#         mutate_at(vars(AK:WY), ~ ifelse(. == 0, NA, .)) %>%
#         gather("state", "present", AK:WY, na.rm = TRUE) %>% 
#         select(-present) %>% 
#         group_by(state) %>% 
#         summarize(rmse = sqrt(sum((pred_gdp-rgdp_yoy_pchange)^2)/n())) %>% 
#         ungroup() %>% 
#         summarize(rmse=mean(rmse))
#       
#       RMSE = RMSE$rmse[1]
#     }
#     
#     if(model_type=="AR"){
#       
#       RMSE = rmse_df %>% 
#         group_by(state) %>% 
#         summarize(rmse = sqrt(sum((pred_gdp-rgdp_yoy_pchange)^2)/n())) %>% 
#         ungroup() %>% 
#         summarize(rmse=mean(rmse))
#       
#       RMSE = RMSE$rmse[1]
#       
#     }
#     
#     
#     
#   }
#   
#   if(data_type=="test"){
#     
#     
#     if(model_type=="AR"){
#       mod = AR_model_function(reg_data %>% filter(date<"2020-01-01"),extra_vars,gdp_var)
#     
#       data = reg_data %>% filter(date>="2020-01-01")
#     
#     rmse_df = cbind(data,
#                     pred_gdp=predict(mod,data))
#     
#     }
#     
#     if(model_type=="ELNET"){
#       
#         data = reg_data %>% 
#           mutate(dummy=1) %>%
#           spread(key=state,value=dummy, fill=0)
#       
#       X <- data %>%
#         filter(date<"2020-01-01"&date>"2016-03-31") %>% 
#         select(c(paste0("lag1_",gdp_var),extra_vars,state.abb)) %>% 
#         mutate_at(vars(-state.abb), funs(c(scale(.,center=TRUE,scale=FALSE))))
#       Y <- data %>% 
#         filter(date<"2020-01-01"&date>"2016-03-31") %>% 
#         select(gdp_var)
#       
#       mod = ELNET_model_function(X,Y,extra_vars,gdp_var)
#       
#       data = data %>% filter(date>="2020-01-01")
#       
#       rmse_df = cbind(data,
#                       pred_gdp=predict(mod,data))
#       
#     }
#     
#     
#     if(model_type=="AR"){
#     RMSE = rmse_df %>% 
#       group_by(state) %>% 
#       summarize(rmse = sqrt(sum((pred_gdp-rgdp_yoy_pchange)^2)/n())) %>% 
#       ungroup() %>% 
#       summarize(rmse=mean(rmse))
#     
#     RMSE = RMSE$rmse[1]
#     }
#     
#     if(model_type=="ELNET"){
#       RMSE = rmse_df %>%
#         mutate_at(vars(AK:WY), ~ ifelse(. == 0, NA, .)) %>%
#         gather("state", "present", AK:WY, na.rm = TRUE) %>% 
#         select(-present) %>% 
#         group_by(state) %>% 
#         summarize(rmse = sqrt(sum((pred_gdp-rgdp_yoy_pchange)^2)/n())) %>% 
#         ungroup() %>% 
#         summarize(rmse=mean(rmse))
#       
#       RMSE = RMSE$rmse[1]
#     }
#     
#   }
#   
#   return(list(RMSE=RMSE,model=mod,pred_df=rmse_df %>% select(-c(hits_sa_yoy_pchange_674:hits_sa_yoy_pchange_340))))
# }
# 
# plot_forecast = function(model,data){
#   
#   predictions = cbind(data,pred_gdp=predict(model,data))
#   
#   if(!("state"%in%colnames(predictions))){
#     
#     predictions = predictions %>% 
#       mutate_at(vars(AK:WY), ~ ifelse(. == 0, NA, .)) %>%
#       gather("state", "present", AK:WY, na.rm = TRUE) %>% 
#       select(-present)
#       
#   }
#   
#   predictions = predictions %>% 
#     select(date,state,rgdp_yoy_pchange,pred_gdp)
#   
#   plot1 = ggplot(predictions,aes(x=date)) +
#     geom_line(aes(y=rgdp_yoy_pchange,color="Actual RGSP"),size=1) +
#     geom_line(aes(y=pred_gdp,color="Predicted RGSP"),size=1) +
#     facet_wrap(~state) +
#     labs(x="Quarter",y="Annual Real GSP Growth (%)",caption="Source: BEA, Google")
#   
#   return(plot1)
# }
# 
# #### AR without Google Trends or External Info ####
# train_AR_eval = pseudo_OOS_RMSE(data_type="train",model_type="AR")
# print(train_AR_eval$RMSE)
# # 0.97
# 
# test_AR_eval = pseudo_OOS_RMSE(data_type="test",model_type="AR")
# print(test_AR_eval$RMSE)
# # 5.16
# 
# model_AR_forecast = plot_forecast(test_AR_eval$model,reg_data %>% filter(date>"2016-03-31"))
# ggsave(paste0(charts_folder,"AR_model_plots.png"),model_AR_forecast,width=20,height=10,units="in")
# 
# #### AR with Google Trends, No External Info, No Dimensionality Reduction ####
# train_AR_GT_eval = pseudo_OOS_RMSE(data_type="train",model_type="AR",extra_vars=colnames(reg_data)[grep("hits_sa",colnames(reg_data))])
# print(train_AR_GT_eval$RMSE)
# # 4.18 without clipping, 3.80 with clipping
# # note that we do need dimension reduction here
# # and there is a problem with huge outliers (I think clipping is okay)
# 
# test_AR_GT_eval = pseudo_OOS_RMSE(data_type="test",model_type="AR",extra_vars=colnames(reg_data)[grep("hits_sa",colnames(reg_data))])
# print(test_AR_GT_eval$RMSE)
# # 5.49 without clipping, 5.11 with clipping
# 
# model_AR_GT_forecast = plot_forecast(test_AR_GT_eval$model,reg_data %>% filter(date>"2016-03-31"))
# ggsave(paste0(charts_folder,"AR_GT_model_plots.png"),model_AR_GT_forecast,width=20,height=10,units="in")
# 
# #### ELNET without Google Trends ####
# train_ELNET_eval = pseudo_OOS_RMSE(data_type="train",model_type="ELNET")
# print(train_ELNET_eval$RMSE)
# # 1.34 without clipping, 1.28 with clipping
# 
# test_ELNET_eval = pseudo_OOS_RMSE(data_type="test",model_type="ELNET")
# print(test_ELNET_eval$RMSE)
# # XXX without clipping, 5.05 with clipping
# 
# model_ELNET_forecast = plot_forecast(test_ELNET_eval$model,reg_data %>% filter(date>"2016-03-31") %>%  mutate(dummy=1) %>%
#                                        spread(key=state,value=dummy, fill=0))
# ggsave(paste0(charts_folder,"ELNET_model_plots.png"),model_ELNET_forecast,width=20,height=10,units="in")
# 
# 
# #### ELNET with Google Trends ####
# train_ELNET_GT_eval = pseudo_OOS_RMSE(data_type="train",model_type="ELNET",extra_vars=colnames(reg_data)[grep("hits_sa",colnames(reg_data))])
# print(train_ELNET_GT_eval$RMSE)
# # 1.59 without clipping, 1.24 with clipping
# 
# test_ELNET_GT_eval = pseudo_OOS_RMSE(data_type="test",model_type="ELNET",extra_vars=colnames(reg_data)[grep("hits_sa",colnames(reg_data))])
# print(test_ELNET_GT_eval$RMSE)
# # XXX without clipping, 5.47 with clipping
# 
# model_ELNET_GT_forecast = plot_forecast(test_ELNET_GT_eval$model,reg_data %>% filter(date>"2016-03-31") %>%  mutate(dummy=1) %>%
#                                        spread(key=state,value=dummy, fill=0))
# ggsave(paste0(charts_folder,"ELNET_GT_model_plots.png"),model_ELNET_GT_forecast,width=20,height=10,units="in")
# 

# make summary statistics table

#save.image(paste0(data_folder,"Processing/get_google_trends_20230929.RData"))


