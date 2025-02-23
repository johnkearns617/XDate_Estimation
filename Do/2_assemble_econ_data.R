# assemble_econ_data.R
# John Kearns
# Goal: Write script to get all of the other economic data

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
library(httr)
library(jsonlite)

conflicted::conflict_prefer("filter","dplyr")
conflicted::conflicts_prefer(jsonlite::fromJSON)

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


#### get other national economic variables ####
national_econ = data.frame()
for(metric in c("PAYEMS","JTSJOL","UNRATE","ADPMNUSNERSA","PRS85006112",
                "GACDISA066MSFRBNY","DTCDISA066MSFRBNY","GACDFSA066MSFRBPHI","DTCDFSA066MSFRBPHI","INDPRO",
                "DGORDER","WHLSLRIMSA","TOTBUSIMNSA","AMDMVS","AMTMUO",
                "RRSFS","PCE","HSN1F","IHLIDXUS","HOUST","TTLCONS","PERMIT",
                "BOPTEXP","BOPTIMP","IR","IQ","CPIAUCSL","CPILFESL","PCEPI","PCEPILFE",
                "DSPIC96","A261RX1Q020SBEA",
                "GDPC1","PCECC96","DGDSRX1Q020SBEA","PCDGCC96","PCNDGC96","PCESVC96","GPDIC1","FPIC1","PNFIC1","PRFIC1","EXPGSC1","IMPGSC1","GCEC1","FGCEC1","SLCEC1",
                "ICSA",
                "WTISPLC","UMCSENT","TOTALSA",
                "MTSR133FMS","MTSO133FMS",
                "W006RC1Q027SBEA","A074RC1Q027SBEA","W007RC1Q027SBEA","B234RC1Q027SBEA","B235RC1Q027SBEA","B075RC1Q027SBEA","W780RC1Q027SBEA","W009RC1Q027SBEA",
                "B094RC1Q027SBEA","W053RC1Q027SBEA","B1040C1Q027SBEA","W011RC1Q027SBEA","W012RC1Q027SBEA","B233RC1Q027SBEA","B097RC1Q027SBEA","FGEXPND","A957RC1Q027SBEA",
                "W014RC1Q027SBEA","W015RC1Q027SBEA","B087RC1Q027SBEA","FGSL","W017RC1Q027SBEA","A091RC1Q027SBEA","B096RC1Q027SBEA","B243RC1Q027SBEA","W018RC1Q027SBEA","W019RCQ027SBEA","AD02RC1Q027SBEA",
                "DGS10","DFF")){
  
  if(metric%in%c("DGS10","DFF")){
    df = fredr(paste0(metric),frequency="wef")
    
    df = df %>% 
      mutate(release_date=date) %>% 
      select(-c(realtime_start,realtime_end))
    
  } else{
    
    df = fredr(paste0(metric),realtime_start = as.Date("2004-01-01"))
    
    df = df %>% 
      group_by(date) %>% 
      mutate(release_date=min(realtime_start)) %>% 
      filter(realtime_start==max(realtime_start)) %>%  
      ungroup() %>% 
      mutate(flag=as.numeric(release_date-dplyr::lead(release_date,1))) %>% 
      mutate(release_date1=rev(seq(from=max(release_date,na.rm=TRUE),by=mean(flag[flag<0],na.rm=TRUE),length.out=n())),
             release_date=ifelse(flag==0&!is.na(flag)&release_date>"2004-01-01",release_date1,release_date),
             release_date=as.Date(release_date)) %>% 
      select(-c(release_date1,flag,realtime_start,realtime_end))
  }
  
  national_econ = bind_rows(national_econ,df)
  
}

titles = data.frame()
for(comp in unique(national_econ$series_id)){
  
  titles = bind_rows(
    titles,
    fredr_series(comp) %>% select(id,title)
  )
  
}

national_econ_weekly = national_econ %>% 
  filter(series_id%in%c("ICSA","IHLIDXUS","DGS10","DFF"))

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

national_econ = bind_rows(
  national_econ %>% 
    filter(series_id!="DFF"),
  national_econ %>% 
    filter(series_id=="DFF") %>% 
    mutate(year=year(date),
           month=month(date)) %>% 
    group_by(year,month,series_id) %>% 
    summarize(value=mean(value,na.rm=TRUE),
              date=as.Date(paste0(year[1],"-",month[1],"-01")),
              release_date=release_date[1]) %>% 
    ungroup() %>% 
    select(-c(year,month))
)

national_econ = bind_rows(
  national_econ %>% 
    filter(series_id!="DGS10"),
  national_econ %>% 
    filter(series_id=="DGS10") %>% 
    mutate(year=year(date),
           month=month(date)) %>% 
    group_by(year,month,series_id) %>% 
    summarize(value=mean(value,na.rm=TRUE),
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


# Treasury data

new_bind <- function(a, b) {
  common_cols <- intersect(names(a), names(b))
  b[common_cols] <- map2_df(b[common_cols], 
                            map(a[common_cols], class), ~{class(.x) <- .y;.x})
  bind_rows(a, b)  
}
op_cash_dep_withdraw = data.frame()
for(yr in c(2005:year(Sys.Date()))){
  
  print(as.character(yr)) 
  
  request = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                   "accounting/dts/deposits_withdrawals_operating_cash",
                   "?sort=-record_date",
                   "&format=json",
                   "&filter=record_calendar_year:eq:",as.character(yr),
                   "&page[size]=10000")
  response=GET(request) 
  out=fromJSON(rawToChar(response$content))
  
  for(page_num in c(1:out$meta$`total-pages`)){
    
    request_2 = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                       "accounting/dts/deposits_withdrawals_operating_cash",
                       "?sort=-record_date",
                       "&format=csv",
                       "&filter=record_calendar_year:eq:",as.character(yr),
                       "&page[number]=",page_num,
                       "&page[size]=10000")
    
    data = read_csv(request_2)
    
    op_cash_dep_withdraw = new_bind(op_cash_dep_withdraw,data)
    
  }
  
}

debt_subject_to_limit = data.frame()
for(yr in c(2005:year(Sys.Date()))){
  
  print(as.character(yr)) 
  
  request = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                   "accounting/dts/debt_subject_to_limit",
                   "?sort=-record_date",
                   "&format=json",
                   "&filter=record_calendar_year:eq:",as.character(yr),
                   "&page[size]=10000")
  response=GET(request) 
  out=fromJSON(rawToChar(response$content))
  
  for(page_num in c(1:out$meta$`total-pages`)){
    
    request_2 = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                       "accounting/dts/debt_subject_to_limit",
                       "?sort=-record_date",
                       "&format=csv",
                       "&filter=record_calendar_year:eq:",as.character(yr),
                       "&page[number]=",page_num,
                       "&page[size]=10000")
    
    data = read_csv(request_2)
    
    debt_subject_to_limit = new_bind(debt_subject_to_limit,data)
    
  }
  
}

deficit_summary = data.frame()
for(yr in c(2015:year(Sys.Date()))){
  
  print(as.character(yr)) 
  
  request = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                   "accounting/mts/mts_table_1",
                   "?sort=-record_date",
                   "&format=json",
                   "&filter=record_calendar_year:eq:",as.character(yr),
                   "&page[size]=10000")
  response=GET(request) 
  out=fromJSON(rawToChar(response$content))
  
  for(page_num in c(1:out$meta$`total-pages`)){
    
    request_2 = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                       "accounting/mts/mts_table_1",
                       "?sort=-record_date",
                       "&format=csv",
                       "&filter=record_calendar_year:eq:",as.character(yr),
                       "&page[number]=",page_num,
                       "&page[size]=10000")
    
    data = read_csv(request_2)
    
    deficit_summary = new_bind(deficit_summary,data)
    
  }
  
}

outlays = data.frame()
for(yr in c(2015:year(Sys.Date()))){
  
  print(as.character(yr)) 
  
  request = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                   "accounting/mts/mts_table_5",
                   "?sort=-record_date",
                   "&format=json",
                   "&filter=record_calendar_year:eq:",as.character(yr),
                   "&page[size]=10000")
  response=GET(request) 
  out=fromJSON(rawToChar(response$content))
  
  for(page_num in c(1:out$meta$`total-pages`)){
    
    request_2 = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                       "accounting/mts/mts_table_5",
                       "?sort=-record_date",
                       "&format=csv",
                       "&filter=record_calendar_year:eq:",as.character(yr),
                       "&page[number]=",page_num,
                       "&page[size]=10000")
    
    data = read_csv(request_2)
    
    outlays = new_bind(outlays,data)
    
  }
  
}

receipts = data.frame()
for(yr in c(2015:year(Sys.Date()))){
  
  print(as.character(yr)) 
  
  request = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                   "accounting/mts/mts_table_4",
                   "?sort=-record_date",
                   "&format=json",
                   "&filter=record_calendar_year:eq:",as.character(yr),
                   "&page[size]=10000")
  response=GET(request) 
  out=fromJSON(rawToChar(response$content))
  
  for(page_num in c(1:out$meta$`total-pages`)){
    
    request_2 = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                       "accounting/mts/mts_table_4",
                       "?sort=-record_date",
                       "&format=csv",
                       "&filter=record_calendar_year:eq:",as.character(yr),
                       "&page[number]=",page_num,
                       "&page[size]=10000")
    
    data = read_csv(request_2)
    
    receipts = new_bind(receipts,data)
    
  }
  
}

fed_invest_programs = data.frame()
for(yr in c(2017:year(Sys.Date()))){
  
  print(as.character(yr)) 
  
  request = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                   "accounting/od/fip_principal_outstanding_table1",
                   "?sort=-record_date",
                   "&format=json",
                   "&filter=record_calendar_year:eq:",as.character(yr),
                   "&page[size]=10000")
  response=GET(request) 
  out=fromJSON(rawToChar(response$content))
  
  for(page_num in c(1:out$meta$`total-pages`)){
    
    request_2 = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                       "accounting/od/fip_principal_outstanding_table1",
                       "?sort=-record_date",
                       "&format=csv",
                       "&filter=record_calendar_year:eq:",as.character(yr),
                       "&page[number]=",page_num,
                       "&page[size]=10000")
    
    data = read_csv(request_2)
    
    fed_invest_programs = new_bind(fed_invest_programs,data)
    
  }
  
}

spending_by_function = data.frame()
for(yr in c(2015:year(Sys.Date()))){
  
  print(as.character(yr)) 
  
  request = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                   "accounting/mts/mts_table_9",
                   "?sort=-record_date",
                   "&format=json",
                   "&filter=record_calendar_year:eq:",as.character(yr),
                   "&page[size]=10000")
  response=GET(request) 
  out=fromJSON(rawToChar(response$content))
  
  for(page_num in c(1:out$meta$`total-pages`)){
    
    request_2 = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                       "accounting/mts/mts_table_9",
                       "?sort=-record_date",
                       "&format=csv",
                       "&filter=record_calendar_year:eq:",as.character(yr),
                       "&page[number]=",page_num,
                       "&page[size]=10000")
    
    data = read_csv(request_2)
    
    spending_by_function = new_bind(spending_by_function,data)
    
  }
  
}



overall_debt = data.frame()
for(yr in c(2001:year(Sys.Date()))){
  
  print(as.character(yr)) 
  
  request = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                   "debt/mspd/mspd_table_2",
                   "?sort=-record_date",
                   "&format=json",
                   "&filter=record_calendar_year:eq:",as.character(yr),
                   "&page[size]=10000")
  response=GET(request) 
  out=fromJSON(rawToChar(response$content))
  
  for(page_num in c(1:out$meta$`total-pages`)){
    
    request_2 = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                       "debt/mspd/mspd_table_2",
                       "?sort=-record_date",
                       "&format=csv",
                       "&filter=record_calendar_year:eq:",as.character(yr),
                       "&page[number]=",page_num,
                       "&page[size]=10000")
    
    data = read_csv(request_2)
    
    overall_debt = new_bind(overall_debt,data)
    
  }
  
}


treasury_securities = data.frame()
for(yr in c(2001:year(Sys.Date()))){
  
  print(as.character(yr)) 
  
  request = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                   "debt/mspd/mspd_table_3",
                   "?sort=-record_date",
                   "&format=json",
                   "&filter=record_calendar_year:eq:",as.character(yr),
                   "&page[size]=10000")
  response=GET(request) 
  out=fromJSON(rawToChar(response$content))
  
  for(page_num in c(1:out$meta$`total-pages`)){
    
    request_2 = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                       "debt/mspd/mspd_table_3",
                       "?sort=-record_date",
                       "&format=csv",
                       "&filter=record_calendar_year:eq:",as.character(yr),
                       "&page[number]=",page_num,
                       "&page[size]=10000")
    
    data = read_csv(request_2)
    
    treasury_securities = new_bind(treasury_securities,data)
    
  }
  
}


debt_level = data.frame()
for(yr in c(2001:year(Sys.Date()))){
  
  print(as.character(yr)) 
  
  request = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v2/",
                   "accounting/od/debt_to_penny",
                   "?sort=-record_date",
                   "&format=json",
                   "&filter=record_calendar_year:eq:",as.character(yr),
                   "&page[size]=10000")
  response=GET(request) 
  out=fromJSON(rawToChar(response$content))
  
  for(page_num in c(1:out$meta$`total-pages`)){
    
    request_2 = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v2/",
                       "accounting/od/debt_to_penny",
                       "?sort=-record_date",
                       "&format=csv",
                       "&filter=record_calendar_year:eq:",as.character(yr),
                       "&page[number]=",page_num,
                       "&page[size]=10000")
    
    data = read_csv(request_2)
    
    debt_level = new_bind(debt_level,data)
    
  }
  
}

cbo_proj = read_csv("https://raw.githubusercontent.com/US-CBO/eval-projections/refs/heads/main/input_data/baselines.csv")
