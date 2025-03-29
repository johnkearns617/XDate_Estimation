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

bls_naics_codes = read_csv("https://data.bls.gov/cew/doc/titles/industry/industry_titles.csv")
bls_area_codes = read_csv("https://data.bls.gov/cew/doc/titles/area/area_titles.csv")

states_codes = bls_area_codes %>% 
  filter(grepl("-- Statewide",area_title)&grepl(paste(state.name,collapse="|"),area_title)) %>% 
  distinct(area_title,.keep_all = TRUE)

# load old data
load("Data/Processing/fiscal_service_data_old.RData")

#### get other national economic variables ####
national_econ = data.frame()
for(metric in c("PAYEMS","CE16OV","JTSJOL","UNRATE","ADPMNUSNERSA","PRS85006112",
                "GACDISA066MSFRBNY","DTCDISA066MSFRBNY","GACDFSA066MSFRBPHI","DTCDFSA066MSFRBPHI","INDPRO",
                "DGORDER","WHLSLRIMSA","TOTBUSIMNSA","AMDMVS","AMTMUO",
                "RRSFS","PCE","HSN1F","IHLIDXUS","HOUST","TTLCONS","PERMIT",
                "BOPTEXP","BOPTIMP","IR","IQ","CPIAUCSL","CPILFESL","PCEPI","PCEPILFE",
                "DSPIC96","A261RX1Q020SBEA",
                "GDPC1","PCECC96","DGDSRX1Q020SBEA","PCDGCC96","PCNDGC96","PCESVC96","GPDIC1","FPIC1","PNFIC1","PRFIC1","EXPGSC1","IMPGSC1","GCEC1","FGCEC1","SLCEC1","A960RX1Q020SBEA",
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

op_cash_dep_withdraw_new = data.frame()
for(yr in c(2025:year(Sys.Date()))){
  
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
    
    op_cash_dep_withdraw_new = new_bind(op_cash_dep_withdraw_new,data)
    
  }
  
}

op_cash_dep_withdraw = data.table::rbindlist(list(
  op_cash_dep_withdraw,
  op_cash_dep_withdraw_new
))

debt_subject_to_limit_new = data.frame()
for(yr in c(2025:year(Sys.Date()))){
  
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
    
    debt_subject_to_limit_new = new_bind(debt_subject_to_limit_new,data)
    
  }
  
}

debt_subject_to_limit = data.table::rbindlist(list(
  debt_subject_to_limit,
  debt_subject_to_limit_new
))

deficit_summary_new = data.frame()
for(yr in c(2025:year(Sys.Date()))){
  
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
    
    deficit_summary_new = new_bind(deficit_summary_new,data)
    
  }
  
}

deficit_summary = data.table::rbindlist(list(
  deficit_summary,
  deficit_summary_new
))

outlays_new = data.frame()
for(yr in c(2025:year(Sys.Date()))){
  
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
    
    outlays_new = new_bind(outlays_new,data)
    
  }
  
}

outlays = data.table::rbindlist(list(
  outlays,
  outlays_new
))

receipts_new = data.frame()
for(yr in c(2025:year(Sys.Date()))){
  
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
    
    receipts_new = new_bind(receipts_new,data)
    
  }
  
}

receipts = data.table::rbindlist(list(
  receipts,
  receipts_new
))

fed_invest_programs_new = data.frame()
for(yr in c(2025:year(Sys.Date()))){
  
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
    
    fed_invest_programs_new = new_bind(fed_invest_programs_new,data)
    
  }
  
}

fed_invest_programs = data.table::rbindlist(list(
  fed_invest_programs,
  fed_invest_programs_new
))

spending_by_function_new = data.frame()
for(yr in c(2025:year(Sys.Date()))){
  
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
    
    spending_by_function_new = new_bind(spending_by_function_new,data)
    
  }
  
}

spending_by_function = data.table::rbindlist(list(
  spending_by_function,
  spending_by_function_new
))

overall_debt_new = data.frame()
for(yr in c(2025:year(Sys.Date()))){
  
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
    
    overall_debt_new = new_bind(overall_debt_new,data)
    
  }
  
}

overall_debt = data.table::rbindlist(list(
  overall_debt,
  overall_debt_new
))


treasury_securities_new = data.frame()
for(yr in c(2025:year(Sys.Date()))){
  
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
    
    treasury_securities_new = new_bind(treasury_securities_new,data)
    
  }
  
}

treasury_securities = data.table::rbindlist(list(
  treasury_securities,
  treasury_securities_new
))


debt_level_new = data.frame()
for(yr in c(2025:year(Sys.Date()))){
  
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
    
    debt_level_new = new_bind(debt_level_new,data)
    
  }
  
}

debt_level = data.table::rbindlist(list(
  debt_level,
  debt_level_new
))

investment_funds_new = data.frame()
for(yr in c(2025:year(Sys.Date()))){
  
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
    
    investment_funds_new = new_bind(investment_funds_new,data)
    
  }
  
}

investment_funds = data.table::rbindlist(list(
  investment_funds,
  investment_funds_new
))

op_cash_balance_new = data.frame()
for(yr in c(2025:year(Sys.Date()))){
  
  print(as.character(yr)) 
  
  request = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                   "accounting/dts/operating_cash_balance",
                   "?sort=-record_date",
                   "&format=json",
                   "&filter=record_calendar_year:eq:",as.character(yr),
                   "&page[size]=10000")
  response=GET(request) 
  out=fromJSON(rawToChar(response$content))
  
  for(page_num in c(1:out$meta$`total-pages`)){
    
    request_2 = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                       "accounting/dts/operating_cash_balance",
                       "?sort=-record_date",
                       "&format=csv",
                       "&filter=record_calendar_year:eq:",as.character(yr),
                       "&page[number]=",page_num,
                       "&page[size]=10000")
    
    data = read_csv(request_2)
    
    op_cash_balance_new = new_bind(op_cash_balance_new,data)
    
  }
  
}

op_cash_balance = data.table::rbindlist(list(
  op_cash_balance,
  op_cash_balance_new
))

# tax_deposits1 = data.frame()
# for(yr in c(2005:2023)){
#   
#   print(as.character(yr)) 
#   
#   request = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
#                    "accounting/dts/federal_tax_deposits",
#                    "?sort=-record_date",
#                    "&format=json",
#                    "&filter=record_calendar_year:eq:",as.character(yr),
#                    "&page[size]=10000")
#   response=GET(request) 
#   out=fromJSON(rawToChar(response$content))
#   
#   for(page_num in c(1:out$meta$`total-pages`)){
#     
#     request_2 = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
#                        "accounting/dts/federal_tax_deposits",
#                        "?sort=-record_date",
#                        "&format=csv",
#                        "&filter=record_calendar_year:eq:",as.character(yr),
#                        "&page[number]=",page_num,
#                        "&page[size]=10000")
#     
#     data = read_csv(request_2)
#     
#     tax_deposits1 = new_bind(tax_deposits1,data)
#     
#   }
#   
# }

tax_deposits2_new = data.frame()
for(yr in c(2025:year(Sys.Date()))){
  
  print(as.character(yr)) 
  
  request = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                   "accounting/dts/inter_agency_tax_transfers",
                   "?sort=-record_date",
                   "&format=json",
                   "&filter=record_calendar_year:eq:",as.character(yr),
                   "&page[size]=10000")
  response=GET(request) 
  out=fromJSON(rawToChar(response$content))
  
  for(page_num in c(1:out$meta$`total-pages`)){
    
    request_2 = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                       "accounting/dts/inter_agency_tax_transfers",
                       "?sort=-record_date",
                       "&format=csv",
                       "&filter=record_calendar_year:eq:",as.character(yr),
                       "&page[number]=",page_num,
                       "&page[size]=10000")
    
    data = read_csv(request_2)
    
    tax_deposits2_new = new_bind(tax_deposits2_new,data)
    
  }
  
}

tax_deposits2 = data.table::rbindlist(list(
  tax_deposits2,
  tax_deposits2_new
))

tax_deposits1a = tax_deposits1 %>% 
  mutate(group=case_when(
    tax_deposit_type%in%c("Withheld Income and Employment Taxes","Individual Income Taxes","Railroad Retirement Taxes","Federal Unemployment Taxes")~"revenue_Individual Income_Payroll",
    tax_deposit_type%in%c("Corporation Income Taxes")~"revenue_Corporate Income Taxes",
    tax_deposit_type%in%c("Estate and Gift Taxes & Misc IRS Rcpts.","Change in Balance of Unclassified Taxes")~"revenue_Estate_Gift_Customs_Misc",
    tax_deposit_type%in%c("Excise Taxes")~"revenue_Excise Taxes"
  )) %>% 
  filter(!is.na(group)) %>% 
  select(record_date,group,today_amt=tax_deposit_today_amt,mtd_amt=tax_deposit_mtd_amt,record_calendar_year,record_fiscal_year,record_calendar_month,record_calendar_day) %>% 
  mutate_at(vars(record_calendar_month:record_calendar_day),as.numeric)


tax_deposits2a = tax_deposits2 %>% 
  mutate(group=case_when(
    classification%in%c("Taxes - Withheld Individual/FICA","Taxes - Railroad Retirement")~"revenue_Individual Income_Payroll",
    classification%in%c("Taxes - Corporate Income")~"revenue_Corporate Income Taxes",
    classification%in%c("Taxes - Miscellaneous Excise")~"revenue_Excise Taxes"
  )) %>% 
  filter(!is.na(group)) %>% 
  select(record_date,group,today_amt,mtd_amt,record_calendar_year,record_fiscal_year,record_calendar_month,record_calendar_day) %>% 
  mutate_at(vars(record_calendar_month:record_calendar_day),as.numeric)


tax_refunds_new = data.frame()
for(yr in c(2025:year(Sys.Date()))){
  
  print(as.character(yr)) 
  
  request = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                   "accounting/dts/income_tax_refunds_issued",
                   "?sort=-record_date",
                   "&format=json",
                   "&filter=record_calendar_year:eq:",as.character(yr),
                   "&page[size]=10000")
  response=GET(request) 
  out=fromJSON(rawToChar(response$content))
  
  for(page_num in c(1:out$meta$`total-pages`)){
    
    request_2 = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                       "accounting/dts/income_tax_refunds_issued",
                       "?sort=-record_date",
                       "&format=csv",
                       "&filter=record_calendar_year:eq:",as.character(yr),
                       "&page[number]=",page_num,
                       "&page[size]=10000")
    
    data = read_csv(request_2)
    
    tax_refunds_new = new_bind(tax_refunds_new,data)
    
  }
  
}

tax_refunds = data.table::rbindlist(list(
  tax_refunds,
  tax_refunds_new
))

tax_refunds = tax_refunds %>% 
  mutate(group=case_when(
    tax_refund_type%in%c("Individual","IRS Tax Refunds Individual","IRS - Advanced Child Tax Credit (EFT)","IRS - Advanced Child Tax Credit (Checks)","Taxes - Individual Tax Refunds (Checks)","Taxes - Individual Tax Refunds (EFT)")~"revenue_Individual Income_Payroll",
    tax_refund_type%in%c("Business","IRS Tax Refunds Business","Taxes - Business Tax Refunds (Checks)","Taxes - Business Tax Refunds (EFT)")~"revenue_Corporate Income Taxes",
    tax_refund_type%in%c("IRS - Economic Impact Payments (Checks)", "IRS - Economic Impact Payments (EFT)")~"revenue_Estate_Gift_Customs_Misc"
  )) %>% 
  filter(!is.na(group)) %>% 
  select(record_date,group,today_amt=tax_refund_today_amt,mtd_amt=tax_refund_mtd_amt,record_calendar_year,record_fiscal_year,record_calendar_month,record_calendar_day) %>% 
  mutate_at(vars(record_calendar_month:record_calendar_day),as.numeric) %>% 
  mutate(today_amt=-1*today_amt,
         mtd_amt=-1*mtd_amt)

tax_deposits = data.table::rbindlist(list(
  tax_deposits1a,
  tax_deposits2a,
  tax_refunds
))


daily_gas_activity_new = data.frame()
for(var in c("gas_held_by_public_daily_activity","gas_intragov_holdings_daily_activity")){
for(yr in c(2025:year(Sys.Date()))){
  
  print(as.character(yr)) 
  
  request = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                   "accounting/od/",var,
                   "?sort=-record_date",
                   "&format=json",
                   "&filter=record_calendar_year:eq:",as.character(yr),
                   "&page[size]=10000")
  response=GET(request) 
  out=fromJSON(rawToChar(response$content))
  
  for(page_num in c(1:out$meta$`total-pages`)){
    
    request_2 = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                       "accounting/od/",var,
                       "?sort=-record_date",
                       "&format=csv",
                       "&filter=record_calendar_year:eq:",as.character(yr),
                       "&page[number]=",page_num,
                       "&page[size]=10000")
    
    data = read_csv(request_2)
    
    daily_gas_activity_new = new_bind(daily_gas_activity_new,data)
    
  }
  
}
}

daily_gas_activity = data.table::rbindlist(list(
  daily_gas_activity,
  daily_gas_activity_new
))

cbo_proj = read_csv("https://raw.githubusercontent.com/US-CBO/eval-projections/refs/heads/main/input_data/baselines.csv")

cbo_econ = read_csv("Data/Raw/Quarterly_January2025.csv") %>% 
  mutate(date=as.Date(as.yearqtr(date,format="%Yq%q")))

outlays_fred = fredr(paste0("MTSO133FMS")) %>% 
  mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)),
         value=value/1000)

receipts_fred = fredr(paste0("MTSR133FMS")) %>% 
  mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)),
         value=value/1000)

deficit_fred = fredr(paste0("MTSDS133FMS")) %>% 
  mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)))

