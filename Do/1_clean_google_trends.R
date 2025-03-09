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
library(boiwsa)

conflicted::conflict_prefer("filter","dplyr")
conflicted::conflicts_prefer(base::print)

data(categories) # categories from Google Trends

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
  slice(which(grepl("time_var",broom::tidy(lm1)$term))) %>% # look at only the time coefficients (ignore category FE)
  mutate(term=gsub("time_var:factor\\(cat\\)","",term),
         estimate=estimate+estimate[1]) %>% 
  select(cat=term,coef=estimate) %>% 
  mutate(cat=as.numeric(cat))
tmp$cat[1] = 77
  
coef_df = bind_rows(coef_df,tmp)

time_match = reg_df %>% 
  filter(cat==249) %>% # just need one set, the specific category doesnt matter
  mutate(date=rev(as.Date(date))) %>% 
  select(time_var,date)
trends_data1 = left_join(trends_data1,time_match,by=c("date"="date")) %>% 
  mutate(time_var=ifelse(is.na(time_var),0,time_var))
trends_data1 = left_join(trends_data1,coef_df,by=c("cat"))
trends_data1$value_detrend = ifelse(trends_data1$date<"2016-01-01",trends_data1$value_adj2+(trends_data1$coef*trends_data1$time_var),trends_data1$value_adj2)

trends_sa = data.frame()

# mclapply
numberOfCores = detectCores()
trends_sa = mclapply(setdiff(unique(trends_data1$cat[trends_data1$cat%in%nk_categories$id]),987),
                        seas_adjust_gt,
                      trends_df=trends_data1,
                        mc.cores = numberOfCores)

trends_sa = bind_rows(trends_sa)

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

write_csv(trends_sa2,paste0("Data/Processing/gt_trends/trends_full_sa_",gsub("-","",Sys.Date()),".csv"))
#trends_sa2 = read_csv(paste0("Data/Processing/trends_full_sa_20250128.csv"))
