# assemble_econ_data.R
# John Kearns
# Goal: Write script to get all of the other economic data



master_dir = "/Users/johnkearns/Documents/GitHub/State_GDP_Trends/"
do_folder = paste0(master_dir,"Do/")
data_folder = paste0(master_dir,"Data/")
results_folder = paste0(master_dir,"Results/")
charts_folder = paste0(master_dir,"Charts/")

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

conflicted::conflict_prefer("filter","dplyr")

data(categories) # categories from Google Trends
fred_key = "156b9cd1b9a52db3b9fc0bab8aca2b39"
bls_key = "913f38a6f2e245e593a66a3b2604f6d3"

# initialize FRED link
fredr_set_key(fred_key)

seasonal_adj = function(df,mode="additive"){
  
  hits <- df$value
  #--------------------------------------------------------------
  
  #do some other convenience operations---------------------------
  dates <- df$date
  hits <- ts(hits,start=c(year(dates[1]),month(dates[1])),frequency=12)
  
  decompose_air = decompose(hits, mode)
  if(mode=="additive"){
  adjust_air = hits - decompose_air$seasonal
  }else{
    adjust_air = hits / decompose_air$seasonal
  }
  adjust_air = ifelse(is.nan(adjust_air)|is.infinite(adjust_air),0,adjust_air)
  
  return(adjust_air)
}

bls_naics_codes = read_csv("https://data.bls.gov/cew/doc/titles/industry/industry_titles.csv")
bls_area_codes = read_csv("https://data.bls.gov/cew/doc/titles/area/area_titles.csv")

states_codes = bls_area_codes %>% 
  filter(grepl("-- Statewide",area_title)&grepl(paste(state.name,collapse="|"),area_title)) %>% 
  distinct(area_title,.keep_all = TRUE)


#### get state GDP ####
state_gdp = data.frame()
for(stat in state.abb){
  
  df = fredr(paste0(stat,"RQGSP"),realtime_start = as.Date("2004-01-01")) %>% 
    group_by(date) %>% 
    mutate(release_date=min(realtime_start)) %>% 
    filter(realtime_start==max(realtime_start)) %>%  
    ungroup() %>% 
    mutate(flag=as.numeric(release_date-dplyr::lead(release_date,1))) %>% 
    mutate(release_date1=rev(seq(from=max(release_date,na.rm=TRUE),by=mean(flag[flag<0],na.rm=TRUE),length.out=n())),
           release_date=ifelse(flag==0&!is.na(flag),release_date1,release_date),
           release_date=as.Date(release_date)) %>% 
    select(-c(release_date1,flag,realtime_start,realtime_end))
  
  state_gdp = bind_rows(state_gdp,df)
  
}


state_gdp = state_gdp %>%
  select(date,release_date,series_id,value) %>%
  mutate(state=substr(series_id,1,2),
         rgdp = value) %>%
  group_by(state) %>%
  mutate(rgdp_yoy_pchange = (rgdp/dplyr::lag(rgdp,4)-1)*100,
         rgdp_qoq_pchange = (rgdp/dplyr::lag(rgdp,1)-1)*100) %>%
  ungroup() %>%
  select(date,state,release_date,rgdp,rgdp_yoy_pchange,rgdp_qoq_pchange)

# units: Millions of Chained 2017 Dollars, Seasonally Adjusted Annual Rate
state_component_gdp = data.frame()
for(comp in c("MAN","EDCAT","AGR","RETAIL","CONST","WHOLE","PROBUS","TRANSWARE","ACCOMD","INFO","GOV","HLTHSOCASS","FININS","MNGCOENTPR","ADMINWAST","ARTENTREC","MIN","OTHSERVE","RERENTLEA","UTIL")){
  for(stat in state.abb){
  
  print(stat)
  
  df = fredr(paste0(stat,comp,"RQGSP"),realtime_start = as.Date("2004-01-01")) %>% 
    group_by(date) %>% 
    mutate(release_date=min(realtime_start)) %>% 
    filter(realtime_start==max(realtime_start)) %>%  
    ungroup() %>% 
    mutate(flag=as.numeric(release_date-dplyr::lead(release_date,1))) %>% 
    mutate(release_date1=rev(seq(from=max(release_date,na.rm=TRUE),by=mean(flag[flag<0],na.rm=TRUE),length.out=n())),
           release_date=ifelse(flag==0&!is.na(flag),release_date1,release_date),
           release_date=as.Date(release_date)) %>% 
    select(-c(release_date1,flag,realtime_start,realtime_end))
  
  state_component_gdp = bind_rows(state_component_gdp,df)
  
}
}

titles = data.frame()
for(comp in unique(state_component_gdp$series_id)){
  
  titles = bind_rows(
    titles,
    fredr_series(comp) %>% select(id,title) %>% mutate(title=gsub(paste(paste0(" in ",state.name),collapse="|"),"",title))
  )
  
}

state_component_gdp = state_component_gdp %>%
  select(date,release_date,series_id,value) %>%
  group_by(series_id) %>% 
  mutate(state=substr(series_id,1,2)) %>%
  group_by(state) %>%
  mutate(value_yoy_pchange = (value/dplyr::lag(value,4)-1)*100,
         value_qoq_pchange = (value/dplyr::lag(value,1)-1)*100) %>%
  ungroup() %>%
  select(date,state,series_id,release_date,value,value_yoy_pchange,value_qoq_pchange) %>% 
  left_join(titles,by=c("series_id"="id"))

#### get other national economic variables ####
national_econ = data.frame()
for(metric in c("PAYEMS","JTSJOL","UNRATE","ADPMNUSNERSA","PRS85006112",
                "GACDISA066MSFRBNY","DTCDISA066MSFRBNY","GACDFSA066MSFRBPHI","DTCDFSA066MSFRBPHI","INDPRO",
                "DGORDER","WHLSLRIMSA","TOTBUSIMNSA","AMDMVS","AMTMUO",
                "RRSFS","PCE","HSN1F","IHLIDXUS","HOUST","TTLCONS","PERMIT",
                "BOPTEXP","BOPTIMP","IR","IQ","CPIAUCSL","CPILFESL","PCEPI","PCEPILFE",
                "DSPIC96","A261RX1Q020SBEA","GDPC1","ICSA",
                "WTISPLC","UMCSENT","TOTALSA",
                "MTSR133FMS","MTSO133FMS")){
  
  df = fredr(paste0(metric),realtime_start = as.Date("2004-01-01")) %>% 
    group_by(date) %>% 
    mutate(release_date=min(realtime_start)) %>% 
    filter(realtime_start==max(realtime_start)) %>%  
    ungroup() %>% 
    mutate(flag=as.numeric(release_date-dplyr::lead(release_date,1))) %>% 
    mutate(release_date1=rev(seq(from=max(release_date,na.rm=TRUE),by=mean(flag[flag<0],na.rm=TRUE),length.out=n())),
           release_date=ifelse(flag==0&!is.na(flag)&release_date>"2004-01-01",release_date1,release_date),
           release_date=as.Date(release_date)) %>% 
    select(-c(release_date1,flag,realtime_start,realtime_end))
  
  national_econ = bind_rows(national_econ,df)
  
}

titles = data.frame()
for(comp in unique(national_econ$series_id)){
  
  titles = bind_rows(
    titles,
    fredr_series(comp) %>% select(id,title)
  )
  
}

national_econ[national_econ$series_id=="DGORDER",] = national_econ %>% 
  filter(series_id=="DGORDER") %>% 
  left_join(national_econ %>% filter(series_id=="PCEPI") %>% select(date,value) %>% rename(price=value)) %>% 
  mutate(value=value/price*100) %>% 
  select(-c(price))

national_econ[national_econ$series_id=="WHLSLRIMSA",] = national_econ %>% 
  filter(series_id=="WHLSLRIMSA") %>% 
  left_join(national_econ %>% filter(series_id=="PCEPI") %>% select(date,value) %>% rename(price=value)) %>% 
  mutate(value=value/price*100) %>% 
  select(-c(price))

national_econ[national_econ$series_id=="TOTBUSIMNSA",] = national_econ %>% 
  filter(series_id=="TOTBUSIMNSA") %>% 
  mutate(value=seasonal_adj(national_econ %>% 
                              filter(series_id=="TOTBUSIMNSA") )) %>% 
  left_join(national_econ %>% filter(series_id=="PCEPI") %>% select(date,value) %>% rename(price=value)) %>% 
  mutate(value=value/price*100) %>% 
  select(-c(price))

national_econ[national_econ$series_id=="AMDMVS",] = national_econ %>% 
  filter(series_id=="AMDMVS") %>% 
  left_join(national_econ %>% filter(series_id=="PCEPI") %>% select(date,value) %>% rename(price=value)) %>% 
  mutate(value=value/price*100) %>% 
  select(-c(price))

national_econ[national_econ$series_id=="AMTMUO",] = national_econ %>% 
  filter(series_id=="AMTMUO") %>% 
  left_join(national_econ %>% filter(series_id=="PCEPI") %>% select(date,value) %>% rename(price=value)) %>% 
  mutate(value=value/price*100) %>% 
  select(-c(price))

national_econ[national_econ$series_id=="PCE",] = national_econ %>% 
  filter(series_id=="PCE") %>% 
  left_join(national_econ %>% filter(series_id=="PCEPI") %>% select(date,value) %>% rename(price=value)) %>% 
  mutate(value=value/price*100) %>% 
  select(-c(price))

national_econ = bind_rows(
  national_econ %>% 
    filter(series_id!="IHLIDXUS"),
  national_econ %>% 
  filter(series_id=="IHLIDXUS") %>% 
  mutate(year=year(date),
         month=month(date)) %>% 
  group_by(year,month,series_id) %>% 
  summarize(value=mean(value,na.rm=TRUE),
            date=as.Date(paste0(year[1],"-",month[1],"-01")),
            release_date=release_date[1]) %>% 
  ungroup() %>% 
  select(-c(year,month))
)

national_econ[national_econ$series_id=="TTLCONS",] = national_econ %>% 
  filter(series_id=="TTLCONS") %>% 
  left_join(national_econ %>% filter(series_id=="PCEPI") %>% select(date,value) %>% rename(price=value)) %>% 
  mutate(value=value/price*100) %>% 
  select(-c(price))

national_econ[national_econ$series_id=="BOPTEXP",] = national_econ %>% 
  filter(series_id=="BOPTEXP") %>% 
  left_join(national_econ %>% filter(series_id=="PCEPI") %>% select(date,value) %>% rename(price=value)) %>% 
  mutate(value=value/price*100) %>% 
  select(-c(price))

national_econ[national_econ$series_id=="BOPTIMP",] = national_econ %>% 
  filter(series_id=="BOPTIMP") %>% 
  left_join(national_econ %>% filter(series_id=="PCEPI") %>% select(date,value) %>% rename(price=value)) %>% 
  mutate(value=value/price*100) %>% 
  select(-c(price))

national_econ = bind_rows(
  national_econ %>% 
    filter(series_id!="ICSA"),
  national_econ %>% 
    filter(series_id=="ICSA") %>% 
    mutate(year=year(date),
           month=month(date)) %>% 
    group_by(year,month,series_id) %>% 
    summarize(value=sum(value,na.rm=TRUE),
              date=as.Date(paste0(year[1],"-",month[1],"-01")),
              release_date=release_date[1]) %>% 
    ungroup() %>% 
    select(-c(year,month))
)

national_econ[national_econ$series_id=="WTISPLC",] = national_econ %>% 
  filter(series_id=="WTISPLC") %>% 
  left_join(national_econ %>% filter(series_id=="PCEPI") %>% select(date,value) %>% rename(price=value)) %>% 
  mutate(value=value/price*100) %>% 
  select(-c(price))

national_econ[national_econ$series_id=="UMCSENT"&national_econ$date>="1978-01-01",] = national_econ %>% 
  filter(series_id=="UMCSENT"&date>="1978-01-01") %>% 
  mutate(value=seasonal_adj(national_econ %>% 
                              filter(series_id=="UMCSENT"&date>="1978-01-01") ))

national_econ[national_econ$series_id=="MTSR133FMS",] = national_econ %>% 
  filter(series_id=="MTSR133FMS") %>% 
  mutate(value=seasonal_adj(national_econ %>% 
                              filter(series_id=="MTSR133FMS"),mode='multiplicative')) %>% 
  left_join(national_econ %>% filter(series_id=="PCEPI") %>% select(date,value) %>% rename(price=value)) %>% 
  mutate(value=value/price*100) %>% 
  select(-c(price))

national_econ[national_econ$series_id=="MTSO133FMS",] = national_econ %>% 
  filter(series_id=="MTSO133FMS") %>% 
  mutate(value=seasonal_adj(national_econ %>% 
                              filter(series_id=="MTSO133FMS"),mode='multiplicative')) %>% 
  left_join(national_econ %>% filter(series_id=="PCEPI") %>% select(date,value) %>% rename(price=value)) %>% 
  mutate(value=value/price*100) %>% 
  select(-c(price))

national_econ = national_econ %>% 
  left_join(titles,by=c('series_id'='id'))


#### state economic variables ####

# from FRED
state_retail = data.frame()
# units: Percent Change from Year Ago, Not Seasonally Adjusted
for(stat in state.abb){
  
  df = fredr(paste0("MSRS",stat,"TOTAL"),realtime_start = as.Date("2004-01-01")) %>% 
    group_by(date) %>% 
    mutate(release_date=min(realtime_start)) %>% 
    filter(realtime_start==max(realtime_start)) %>%  
    ungroup() %>% 
    mutate(flag=as.numeric(release_date-dplyr::lead(release_date,1))) %>% 
    mutate(release_date1=rev(seq(from=max(release_date,na.rm=TRUE),by=mean(flag[flag<0],na.rm=TRUE),length.out=n())),
           release_date=ifelse(flag==0&!is.na(flag),release_date1,release_date),
           release_date=as.Date(release_date)) %>% 
    select(-c(release_date1,flag,realtime_start,realtime_end))
  
  hits <- df$value
  #--------------------------------------------------------------
  
  #do some other convenience operations---------------------------
  dates <- df$date
  hits <- ts(hits,start=c(year(dates[1]),month(dates[1])),frequency=12)
  
  decompose_air = decompose(hits, "additive")
  adjust_air = hits - decompose_air$seasonal
  adjust_air = ifelse(is.nan(adjust_air)|is.infinite(adjust_air),0,adjust_air)
  
  df = cbind(df,adjust_air)
  colnames(df)[ncol(df)] = "value_sa"
  
  df$title = "Monthly State Retail Sales: Total Retail Sales Excluding Nonstore Retailers"
  
  state_retail = bind_rows(state_retail,df)
}


indeed_state=data.frame()
# units: Index Feb, 1 2020=100, Seasonally Adjusted
for(stat in state.abb){
  
  df = fredr(paste0("IHLIDXUS",stat),realtime_start = as.Date("2004-01-01")) %>% 
    group_by(date) %>% 
    mutate(release_date=min(realtime_start)) %>% 
    filter(realtime_start==max(realtime_start)) %>%  
    ungroup() %>% 
    mutate(flag=as.numeric(release_date-dplyr::lead(release_date,1))) %>% 
    mutate(release_date1=rev(seq(from=max(release_date,na.rm=TRUE),by=mean(flag[flag<0],na.rm=TRUE),length.out=n())),
           release_date=date+12,
           release_date=as.Date(release_date)) %>% 
    select(-c(release_date1,flag,realtime_start,realtime_end)) %>% 
    mutate(year=year(date),
           month=month(date)) %>% 
    group_by(year,month,series_id) %>% 
    summarize(value=mean(value,na.rm=TRUE),
              date=as.Date(paste0(year[1],"-",month[1],"-01")),
              release_date=release_date[1],
              title="Indeed job postings",
              state=stat) %>% 
    ungroup()
  
  indeed_state = bind_rows(indeed_state,df)
  
}

housing_starts_state = data.frame()
# units: Units, NSA
for(stat in state.abb){
  
  df = fredr(paste0(stat,"BPPRIV"),realtime_start = as.Date("2004-01-01")) %>% 
    group_by(date) %>% 
    mutate(release_date=min(realtime_start)) %>% 
    filter(realtime_start==max(realtime_start)) %>%  
    ungroup() %>% 
    mutate(flag=as.numeric(release_date-dplyr::lead(release_date,1))) %>% 
    mutate(release_date1=rev(seq(from=max(release_date,na.rm=TRUE),by=mean(flag[flag<0],na.rm=TRUE),length.out=n())),
           release_date=ifelse(flag==0&!is.na(flag),release_date1,release_date),
           release_date=as.Date(release_date)) %>% 
    select(-c(release_date1,flag,realtime_start,realtime_end))
  
  hits <- df$value
  #--------------------------------------------------------------
  
  #do some other convenience operations---------------------------
  dates <- df$date
  hits <- ts(hits,start=c(year(dates[1]),month(dates[1])),frequency=12)
  
  decompose_air = decompose(hits, "additive")
  adjust_air = hits - decompose_air$seasonal
  adjust_air = ifelse(is.nan(adjust_air)|is.infinite(adjust_air),0,adjust_air)
  
  df = cbind(df,adjust_air)
  colnames(df)[ncol(df)] = "value_sa"
  
  df = df %>% 
    mutate(state=stat,
           title=(fredr_series(paste0(stat,"BPPRIV"))%>% mutate(title=gsub(paste(paste0(" for ",state.name),collapse="|"),"",title)))$title) 
  
  housing_starts_state = bind_rows(housing_starts_state,df)
  
}

exports_state=data.frame()
# units: Millions of Dollars, Not Seasonally Adjusted
for(stat in state.abb){
  
  df = fredr(paste0("EXPTOT",stat),realtime_start = as.Date("2004-01-01")) %>% 
    group_by(date) %>% 
    mutate(release_date=min(realtime_start)) %>% 
    filter(realtime_start==max(realtime_start)) %>%  
    ungroup() %>% 
    mutate(flag=as.numeric(release_date-dplyr::lead(release_date,1))) %>% 
    mutate(release_date1=rev(seq(from=max(release_date,na.rm=TRUE),by=mean(flag[flag<0],na.rm=TRUE),length.out=n())),
           release_date=ifelse(flag==0&!is.na(flag),release_date1,release_date),
           release_date=as.Date(release_date)) %>% 
    select(-c(release_date1,flag,realtime_start,realtime_end))
  
  hits <- df$value
  #--------------------------------------------------------------
  
  #do some other convenience operations---------------------------
  dates <- df$date
  hits <- ts(hits,start=c(year(dates[1]),month(dates[1])),frequency=12)
  
  decompose_air = decompose(hits, "additive")
  adjust_air = hits - decompose_air$seasonal
  adjust_air = ifelse(is.nan(adjust_air)|is.infinite(adjust_air),0,adjust_air)
  
  df = cbind(df,adjust_air)
  colnames(df)[ncol(df)] = "value_sa"
  
  df = df %>% 
    mutate(state=stat,
           title=(fredr_series(paste0("EXPTOT",stat))%>% mutate(title=gsub(paste(paste0(" for ",state.name),collapse="|"),"",title)))$title) %>% 
    left_join(national_econ %>% filter(series_id=="PCEPI") %>% select(date,value) %>% rename(price=value)) %>% 
    mutate(value=value/price*100,
           value_sa=value_sa/price*100) %>% 
    select(-price)
  
  exports_state = bind_rows(exports_state,df)
  
}

imports_state=data.frame()
# units: Millions of Dollars, Not Seasonally Adjusted
for(stat in state.abb){
  
  df = fredr(paste0("IMPTOT",stat),realtime_start = as.Date("2004-01-01")) %>% 
    group_by(date) %>% 
    mutate(release_date=min(realtime_start)) %>% 
    filter(realtime_start==max(realtime_start)) %>%  
    ungroup() %>% 
    mutate(flag=as.numeric(release_date-dplyr::lead(release_date,1))) %>% 
    mutate(release_date1=rev(seq(from=max(release_date,na.rm=TRUE),by=mean(flag[flag<0],na.rm=TRUE),length.out=n())),
           release_date=ifelse(flag==0&!is.na(flag),release_date1,release_date),
           release_date=as.Date(release_date)) %>% 
    select(-c(release_date1,flag,realtime_start,realtime_end))
  
  hits <- df$value
  #--------------------------------------------------------------
  
  #do some other convenience operations---------------------------
  dates <- df$date
  hits <- ts(hits,start=c(year(dates[1]),month(dates[1])),frequency=12)
  
  decompose_air = decompose(hits, "additive")
  adjust_air = hits - decompose_air$seasonal
  adjust_air = ifelse(is.nan(adjust_air)|is.infinite(adjust_air),0,adjust_air)
  
  df = cbind(df,adjust_air)
  colnames(df)[ncol(df)] = "value_sa"
  
  df = df %>% 
    mutate(state=stat,
           title=(fredr_series(paste0("IMPTOT",stat))%>% mutate(title=gsub(paste(paste0(" for ",state.name),collapse="|"),"",title)))$title) %>% 
    left_join(national_econ %>% filter(series_id=="PCEPI") %>% select(date,value) %>% rename(price=value)) %>% 
    mutate(value=value/price*100,
           value_sa=value_sa/price*100) %>% 
    select(-price)
  
  
  imports_state = bind_rows(imports_state,df)
  
}

ui_claims_state = data.frame()
# units: Number, Not Seasonally Adjusted
for(stat in state.abb){
  
  df = fredr(paste0(stat,"ICLAIMS"),realtime_start = as.Date("2004-01-01")) %>% 
    group_by(date) %>% 
    mutate(release_date=min(realtime_start)) %>% 
    filter(realtime_start==max(realtime_start)) %>%  
    ungroup() %>% 
    mutate(flag=as.numeric(release_date-dplyr::lead(release_date,1))) %>% 
    mutate(release_date1=rev(seq(from=max(release_date,na.rm=TRUE),by=mean(flag[flag<0],na.rm=TRUE),length.out=n())),
           release_date=ifelse(flag==0&!is.na(flag),release_date1,release_date),
           release_date=as.Date(release_date)) %>% 
    select(-c(release_date1,flag,realtime_start,realtime_end)) %>% 
    mutate(year=year(date),
           month=month(date)) %>% 
    group_by(year,month) %>% 
    summarize(value=sum(value,na.rm=TRUE),
              date=as.Date(paste0(year[1],"-",month[1],"-01")),
              release_date=release_date[1]) %>% 
    ungroup()
  
  
  hits <- df$value
  #--------------------------------------------------------------
  
  #do some other convenience operations---------------------------
  dates <- df$date
  hits <- ts(hits,start=c(year(dates[1]),month(dates[1])),frequency=12)
  
  decompose_air = decompose(hits, "additive")
  adjust_air = hits - decompose_air$seasonal
  adjust_air = ifelse(is.nan(adjust_air)|is.infinite(adjust_air),0,adjust_air)
  
  df = cbind(df,adjust_air)
  colnames(df)[ncol(df)] = "value_sa"
  
  df = df %>% 
    mutate(state=stat,
           title=(fredr_series(paste0(stat,"ICLAIMS"))%>% mutate(title=gsub(paste(paste0(" for ",state.name),collapse="|"),"",title)))$title)
  
  ui_claims_state = bind_rows(ui_claims_state,df)
  
}

tax_state = data.frame()
# units: Millions of U.S. Dollars, Not Seasonally Adjusted
for(tax in c("01","09",10:16,19:25,27:29,40:41,50:51,53,99,"OTAL")){
  for(stat in state.abb){
    
    print(stat)
    
    df = fredr(paste0("QTAXT",tax,"QTAXCAT3",stat,"NO"),realtime_start = as.Date("2004-01-01")) %>% 
      group_by(date) %>% 
      mutate(release_date=min(realtime_start)) %>% 
      filter(realtime_start==max(realtime_start)) %>%  
      ungroup() %>% 
      mutate(flag=as.numeric(release_date-dplyr::lead(release_date,1)),
             value=ifelse(is.na(value),0,value)) 
    
    if(!all(df$flag%in%c(0,NA))){
      df= df %>% 
      mutate(release_date1=rev(seq(from=max(release_date,na.rm=TRUE),by=mean(flag[flag<0],na.rm=TRUE),length.out=n())),
             release_date=ifelse(flag==0&!is.na(flag),release_date1,release_date),
             release_date=as.Date(release_date)) %>% 
      select(-c(release_date1,flag,realtime_start,realtime_end))}else{
        df = df %>% select(-c(flag,realtime_start,realtime_end))
      }
    
    hits <- df$value
    #--------------------------------------------------------------
    
    #do some other convenience operations---------------------------
    dates <- df$date
    hits <- ts(hits,start=c(year(dates[1]),quarter(dates[1])),frequency=4)
    
    decompose_air = decompose(hits, "additive")
    adjust_air = hits - decompose_air$seasonal
    adjust_air = ifelse(is.nan(adjust_air)|is.infinite(adjust_air)|adjust_air<0,0,adjust_air)
    
    df = cbind(df,adjust_air)
    colnames(df)[ncol(df)] = "value_sa"
    
    df = df %>% 
      mutate(state=stat,
             title=(fredr_series(paste0("QTAXT",tax,"QTAXCAT3",stat,"NO"))%>% mutate(title=gsub(paste(paste0(" for ",state.name),collapse="|"),"",title)))$title) %>% 
      left_join(national_econ %>% filter(series_id=="PCEPI") %>% select(date,value) %>% rename(price=value)) %>% 
      mutate(value=value/price*100,
             value_sa=value_sa/price*100) %>% 
      select(-price)
    
    tax_state = bind_rows(tax_state,df)
    
  }
}

business_app_state = data.frame()
# units: Number,Not Seasonally Adjusted
for(stat in state.abb){
  
  df = fredr(paste0("BUSAPPWNSA",stat),realtime_start = as.Date("2004-01-01")) %>% 
    group_by(date) %>% 
    mutate(release_date=min(realtime_start)) %>% 
    filter(realtime_start==max(realtime_start)) %>%  
    ungroup() %>% 
    mutate(flag=as.numeric(release_date-dplyr::lead(release_date,1))) %>% 
    mutate(release_date1=rev(seq(from=max(release_date,na.rm=TRUE),by=mean(flag[flag<0],na.rm=TRUE),length.out=n())),
           release_date=ifelse(flag==0&!is.na(flag),release_date1,release_date),
           release_date=as.Date(release_date)) %>% 
    select(-c(release_date1,flag,realtime_start,realtime_end)) %>% 
    mutate(year=year(date),
           month=month(date)) %>% 
    group_by(year,month,series_id) %>% 
    summarize(value=sum(value,na.rm=TRUE),
              date=as.Date(paste0(year[1],"-",month[1],"-01")),
              release_date=release_date[1]) %>% 
    ungroup()
  
  hits <- df$value
  #--------------------------------------------------------------
  
  #do some other convenience operations---------------------------
  dates <- df$date
  hits <- ts(hits,start=c(year(dates[1]),month(dates[1])),frequency=12)
  
  decompose_air = decompose(hits, "multiplicative")
  adjust_air = hits / decompose_air$seasonal
  adjust_air = ifelse(is.nan(adjust_air)|is.infinite(adjust_air)|adjust_air<0,0,adjust_air)
  
  df = cbind(df,adjust_air)
  colnames(df)[ncol(df)] = "value_sa"
  
  df = df %>% 
    mutate(state=stat,
           title=(fredr_series(paste0("BUSAPPWNSA",stat))%>% mutate(title=gsub(paste(paste0(" for ",state.name),collapse="|"),"",title)))$title) 
  
  business_app_state = bind_rows(business_app_state,df)
  
}

business_form_state = data.frame()
# units: Number, Seasonally Adjusted
for(stat in state.abb){
  
  df = fredr(paste0("BFPBF8QTOTALSA",stat),realtime_start = as.Date("2004-01-01")) %>% 
    group_by(date) %>% 
    mutate(release_date=min(realtime_start)) %>% 
    filter(realtime_start==max(realtime_start)) %>%  
    ungroup() %>% 
    mutate(flag=as.numeric(release_date-dplyr::lead(release_date,1))) %>% 
    mutate(release_date1=rev(seq(from=max(release_date,na.rm=TRUE),by=mean(flag[flag<0],na.rm=TRUE),length.out=n())),
           release_date=ifelse(flag==0&!is.na(flag),release_date1,release_date),
           release_date=as.Date(release_date)) %>% 
    select(-c(release_date1,flag,realtime_start,realtime_end))
  
  df$title="Projected Business Formations Within Eight Quarters"
  
  business_form_state = bind_rows(business_form_state,df)
  
}

# from BLS
bls_codes = read_delim(paste0(data_folder,"Raw/sae_codes.txt"),delim="\n",col_names = FALSE) %>% 
  mutate(X1=gsub(" ","",X1))

industry_codes = read_csv(paste0(data_folder,"Raw/industry_codes.csv")) %>% 
  mutate(code=gsub("-","",code))

sae_data = data.frame()
# units: Thousands of Persons, Seasonally Adjusted
j = 1
for(i in c(seq(from=50,to=length(bls_codes$X1),by=50),1746)){
  print(i)
  
  payload <- list(
    'seriesid'  = bls_codes$X1[j:i],
    'startyear' = 2004,
    'endyear'   = year(Sys.Date()),
    'registrationKey'=bls_key,
    'catalog'=TRUE)
  response <- blsAPI(payload,api_version=2,return_data_frame = TRUE)
  
  response = response %>% 
    mutate(code=substr(seriesID,11,18),
           area=substr(seriesID,4,8)) %>% 
    left_join(industry_codes,by=c('code'='code')) %>% 
    left_join(states_codes,by=c("area"="area_fips")) %>% 
    mutate(area_title=ifelse(area=="00000","US",area_title),
           area_title=gsub(" -- Statewide","",area_title)) %>% 
    rename(title=industry,
           state=area_title) %>% 
    select(-c(code,area))
  
  sae_data = bind_rows(sae_data,response)
  
  j = j+50
  
}
sae_data = sae_data %>% 
  mutate(date=as.Date(paste0(year,substr(period,2,3),01),format="%Y%m%d"))

qcewGetAreaData <- function(year, qtr, area) {
  url <- "http://data.bls.gov/cew/data/api/YEAR/QTR/area/AREA.csv"
  url <- sub("YEAR", year, url, ignore.case=FALSE)
  url <- sub("QTR", tolower(qtr), url, ignore.case=FALSE)
  url <- sub("AREA", toupper(area), url, ignore.case=FALSE)
  read.csv(url, header = TRUE, sep = ",", quote="\"", dec=".", na.strings=" ", skip=0)
}

options(timeout=10000)
qcew_data = data.frame()
for(yr in 2004:year(Sys.Date())){

temp <- tempfile()
download.file(paste0("https://data.bls.gov/cew/data/files/",yr,"/csv/",yr,"_qtrly_by_area.zip"),temp)

list_of_txts<-unzip(temp,list=TRUE)[,1]
list_of_txts<-list_of_txts[str_detect(list_of_txts,"-- Statewide|U.S. TOTAL")] # use ".csv" since you are looking for csv files instead 
#Then loop over it without unzipping:

final_data<-list("vector")
for (i in 1:length(list_of_txts)){
  conn<-unz(temp, list_of_txts[i])
  final_data[[i]]<-read_csv(conn) #replace fread with the command you want to use to read in the data. Worked with readr::read_csv()
}
final_data = data.frame(do.call(rbind,final_data))
final_data = final_data %>% 
  filter(industry_code%in%c(10,1011,1012,1013,1021,1022,1023,1024,1025,1026,1027,1028,1029)) %>% 
  group_by(area_fips,industry_code,year,qtr) %>% 
  filter(own_code==0|(!(any(own_code%in%c(0,9)))&(own_code%in%c(5,3,2,1)))) %>% 
  summarize(area_title=area_title[1],
            industry_title=industry_title[1],
            month1_emplvl=sum(month1_emplvl,na.rm=TRUE),
            month2_emplvl=sum(month2_emplvl,na.rm=TRUE),
            month3_emplvl=sum(month3_emplvl,na.rm=TRUE),
            avg_wkly_wage=mean(avg_wkly_wage,na.rm=TRUE)) %>% 
  ungroup()

qcew_data = bind_rows(qcew_data,final_data)

}

qcew_data = qcew_data %>% 
  mutate(date=as.Date(as.yearqtr(paste0(year,qtr),format="%Y%q")))

laus_data = bind_rows(
  laus_get_data(location.vector=state.name, measure.vector=laus_measure$measure_text[1], 
                start.year=2004, end.year=2022,
                api.version=2,bls.key = bls_key),
  laus_get_data(location.vector=state.name, measure.vector=laus_measure$measure_text[1], 
                start.year=2023, end.year=year(Sys.Date()),
                api.version=2,bls.key = bls_key)
) %>% 
  mutate(date=as.Date(paste0(year,substr(period,2,3),01),format="%Y%m%d"))

releases <- read_delim("Documents/State GDP/Data/releases.txt", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
releases_clean = data.frame()
for(i in 1:nrow(releases)){
  
  releases_clean = rbind(releases_clean,releases[i,as.vector(!is.na(releases[i,]))] %>% rename('1'=1,'2'=2,'3'=3))
  
}
releases_clean = releases_clean %>% 
  separate('2',into=c('month','year'),sep=', ') %>% 
  fill(year,.direction = "down") %>% 
  rowwise() %>% 
  mutate(date=parsedate::parse_date(paste0(month," ",year))) %>% 
  select(1,5)
colnames(releases_clean) = c('release','release_date')

releases_2 <-  readxl::read_excel("Documents/State GDP/Data/BLS_releases.xlsx", 
                                         sheet = "Sheet2",col_names=FALSE) %>% 
  mutate(date=as.Date(as.numeric(`...1`),origin="1899-12-30")) %>% 
  filter(!is.na(date)) %>% 
  select(3,4)
colnames(releases_2) = c('release','release_date')
releases_2 = bind_rows(
  releases_2,
  data.frame(release=paste0("County Employment and Wages for ",c(rep(c("First","Second","Third","Fourth"),times=4),"First")," Quarter ",c(rep(c(2003:2006),each=4),2007)),
             release_date=as.Date(c("2003-10-13",paste(rep(c(2004:2007),each=4),c("-"),c("01-11","04-11","07-11","10-11"),sep=""))))
)

releases_clean = bind_rows(releases_clean,releases_2)

laus_release_dates = releases_clean %>% 
  filter(grepl("Regional and State|State Employment",release)&!grepl("Annual",release)) %>% 
  filter(release_date>"2004-02-01"&release_date<=Sys.Date()) %>% 
  distinct(release_date,.keep_all = TRUE) %>% 
  bind_rows(data.frame(release="Regional and State Employment and Unemployment (Monthly) for November 2013",
                       release_date=as.Date("2013-10-20"))) %>% 
  arrange(release_date)

sae_release_dates = releases_clean %>% 
  filter(grepl("Regional and State|State Employment",release)&!grepl("Annual",release)) %>% 
  filter(release_date>"2004-02-01"&release_date<=Sys.Date()) %>% 
  distinct(release_date,.keep_all = TRUE) %>% 
  bind_rows(data.frame(release="Regional and State Employment and Unemployment (Monthly) for November 2013",
                       release_date=as.Date("2013-10-20"))) %>% 
  arrange(release_date)

laus_release_dates = cbind(
  laus_data %>% arrange(date) %>% distinct(date),
  laus_release_dates
)

qcew_release_dates = cbind(
  qcew_data %>% arrange(date) %>% distinct(year,qtr),
  releases_2 %>% filter(grepl("County Employment and Wages for",release)|release=="County Employment and Wages") %>% arrange(release_date) %>% slice(5:n()) %>% filter(release_date<=Sys.Date())
)
laus_data = left_join(laus_data,laus_release_dates)
qcew_data = left_join(qcew_data,qcew_release_dates)

qcew_data_long = data.frame()
for(stat in unique(qcew_data$area_fips)){
  for(cod in unique(qcew_data$industry_code)){
    for(dat in unique(qcew_data$date)){
        
      print(paste0(stat," ",dat))
      
        old_df = qcew_data %>% 
          filter(area_fips==stat&industry_code==cod&date==dat)
        
        if(nrow(old_df)==0){
          next
        }
        
        new_df = bind_rows(
          data.frame(old_df[,c('area_fips','industry_code','year','qtr','area_title','industry_title','release','release_date')],
                     date=old_df$date[1],
                     emp_lvl=old_df$month1_emplvl[1],
                     avg_wkly_wage=old_df$avg_wkly_wage[1]),
          data.frame(old_df[,c('area_fips','industry_code','year','qtr','area_title','industry_title','release','release_date')],
                     date=old_df$date[1] %m+% months(1),
                     emp_lvl=old_df$month2_emplvl[1],
                     avg_wkly_wage=old_df$avg_wkly_wage[1]),
          data.frame(old_df[,c('area_fips','industry_code','year','qtr','area_title','industry_title','release','release_date')],
                     date=old_df$date[1] %m+% months(2),
                     emp_lvl=old_df$month3_emplvl[1],
                     avg_wkly_wage=old_df$avg_wkly_wage[1])
        )
        
        qcew_data_long = bind_rows(qcew_data_long,new_df)
        
        
      }
      
    }
    
  }

sae_data = left_join(sae_data,laus_release_dates)

