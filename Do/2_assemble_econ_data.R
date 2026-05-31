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

conflicted::conflicts_prefer(dplyr::filter,
                            jsonlite::fromJSON,
                            httr::config,
                            dplyr::lag,
                            dplyr::lead,
                            lubridate::quarter,
                            zoo::yearmon,
                            zoo::yearqtr,
                            lubridate::month,
                            lubridate::day)




data(categories) # categories from Google Trends

set.seed(178)

bls_naics_codes = read_csv("https://data.bls.gov/cew/doc/titles/industry/industry_titles.csv")
bls_area_codes = read_csv("https://data.bls.gov/cew/doc/titles/area/area_titles.csv")

states_codes = bls_area_codes %>% 
  filter(grepl("-- Statewide",area_title)&grepl(paste(state.name,collapse="|"),area_title)) %>% 
  distinct(area_title,.keep_all = TRUE)

gdpnow_vintages = fredr_series_vintagedates("GDPNOW")

# load old data
load("Data/Processing/fiscal_service_data_old.RData")

fredr_set_key(fred_key)

#### get other national economic variables ####
get_national_econ_data = function(end_date){
  
  fredr_set_key(fred_key)
  
  national_econ = data.frame()
  for(metric in c("PAYEMS","CE16OV","JTSJOL","UNRATE","ADPMNUSNERSA","PRS85006112",
                  "GACDISA066MSFRBNY","DTCDISA066MSFRBNY","GACDFSA066MSFRBPHI","DTCDFSA066MSFRBPHI","INDPRO",
                  "DGORDER","WHLSLRIMSA","TOTBUSIMNSA","AMDMVS","AMTMUO",
                  "CES0500000003","W209RC1","CIS1020000000000I",
                  "RRSFS","PCE","HSN1F","IHLIDXUS","HOUST","TTLCONS","PERMIT",
                  "BOPTEXP","BOPTIMP","IR","IQ","CPIAUCSL","CPILFESL","PCEPI","PCEPILFE",
                  "DSPIC96","A261RX1Q020SBEA",
                  "GDPC1","PCECC96","DGDSRX1Q020SBEA","PCDGCC96","PCNDGC96","PCESVC96","GPDIC1","FPIC1","PNFIC1","PRFIC1","EXPGSC1","IMPGSC1","GCEC1","FGCEC1","SLCEC1",
                  "ICSA","CCSA",
                  "WTISPLC","UMCSENT","TOTALSA",
                  "MTSR133FMS","MTSO133FMS",
                  "W006RC1Q027SBEA","A074RC1Q027SBEA","W007RC1Q027SBEA","B234RC1Q027SBEA","B235RC1Q027SBEA","B075RC1Q027SBEA","W780RC1Q027SBEA","W009RC1Q027SBEA",
                  "B094RC1Q027SBEA","W053RC1Q027SBEA","B1040C1Q027SBEA","W011RC1Q027SBEA","W012RC1Q027SBEA","B233RC1Q027SBEA","B097RC1Q027SBEA","FGEXPND","A957RC1Q027SBEA",
                  "W014RC1Q027SBEA","W015RC1Q027SBEA","B087RC1Q027SBEA","FGSL","W017RC1Q027SBEA","A091RC1Q027SBEA","B096RC1Q027SBEA","B243RC1Q027SBEA","W018RC1Q027SBEA","W019RCQ027SBEA","AD02RC1Q027SBEA",
                  "DGS10","DFF")){
    
    if(metric%in%c("DGS10","DFF")){
      
      df = tryCatch({
        fredr(paste0(metric),frequency="wef")
      },error=function(e) fredr(paste0(metric),frequency="wef"))
      
      df = df %>% 
        mutate(release_date=date) %>% 
        select(-c(realtime_start,realtime_end)) %>% 
        filter(release_date<=end_date)
      
    } 
    
    if(!(metric%in%c("DGS10","DFF"))){ # these have extra problems because they are daily data
      
      df = tryCatch({
        fredr(paste0(metric),realtime_end = as.Date(end_date))
      },error=function(e) fredr(paste0(metric),realtime_start = as.Date(end_date)))
      
      if(nrow(df)==0) next
      
      df = df %>% 
        group_by(date) %>% 
        mutate(release_date=min(realtime_start)) %>% 
        filter(realtime_start==max(realtime_start)) %>%  
        ungroup() %>% 
        mutate(flag=as.numeric(release_date-dplyr::lead(release_date,1))) %>% # need to find where there is a jump in time or a duplicate
        mutate(release_date1=rev(seq(from=max(release_date,na.rm=TRUE),by=mean(flag[flag<0],na.rm=TRUE),length.out=n())),
               release_date=ifelse(flag==0&!is.na(flag)&!(date%in%c("2025-10-01","2025-11-01"))&release_date>"2004-01-01",release_date1,release_date), # have to deal with the times when fed data got delayed
               release_date=as.Date(release_date)) %>% 
        select(-c(release_date1,flag,realtime_start,realtime_end))
    }
    
    national_econ = bind_rows(national_econ,df)
    
  }
  
  titles = data.frame()
  for(comp in c(unique(national_econ$series_id))){
    
    titles = bind_rows(
      titles,
      fredr_series(comp) %>% select(id,title)
    )
    
  }
  
  national_econ_weekly = national_econ %>% 
    filter(series_id%in%c("ICSA","CCSA","IHLIDXUS","DGS10","DFF"))
  
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
      filter(!(series_id%in%c("IHLIDXUS","ICSA","CCSA"))),
    national_econ %>% 
      filter(series_id%in%c("IHLIDXUS","ICSA","CCSA")) %>% 
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
  
  # fix dates on national_econ
  national_econ = national_econ %>% 
    # fix when release date is before the month of actual observation
    mutate(release_date=case_when(
      series_id=="GACDFSA066MSFRBPHI"&release_date<date~date+17,
      series_id=="DTCDFSA066MSFRBPHI"&release_date<date~date+17,
      series_id=="AMTMUO"&release_date<date~date+62,
      series_id=="TTLCONS"&release_date<date~date+59,
      series_id=="UMCSENT"&release_date<date~date+26,
      series_id=="MTSR133FMS"&release_date<date~date+40,
      series_id=="MTSO133FMS"&release_date<date~date+40,
      series_id=="AD02RC1Q027SBEA"&release_date<date~date+146,
      series_id=="IHLIDXUS"&release_date<date~date+5,
      series_id=="ICSA"&release_date<date~date+7,
      series_id=="CCSA"&release_date<date~date+7,
      series_id=="BOPTIMP"&release_date<date~date+62,
      series_id%in%c("IR","IQ")&release_date<date~date+45,
      series_id%in%c("TOTALSA")&release_date<date~date+35,
      TRUE~release_date
    ))
  
  # now fix where old data is given release date of when first posted to fred
  national_econ = national_econ %>% 
    group_by(series_id) %>% 
    mutate(ch=release_date-dplyr::lag(release_date,1),
           diff=release_date-date) %>% 
    ungroup() %>% 
    mutate(release_date=case_when(
      !(date%in%c("2025-10-01","2025-11-01"))&diff>32&series_id=="UMCSENT"~date+26,
      !(date%in%c("2025-10-01","2025-11-01"))&diff>365&series_id=="W009RC1Q027SBEA"~date+284,
      !(date%in%c("2025-10-01","2025-11-01"))&diff>365&series_id=="B1040C1Q027SBEA"~date+284,
      !(date%in%c("2025-10-01","2025-11-01"))&diff>365&series_id=="AD02RC1Q027SBEA"~date+284,
      !(date%in%c("2025-10-01","2025-11-01"))&diff>365&series_id=="W053RC1Q027SBEA"~date+170,
      !(date%in%c("2025-10-01","2025-11-01"))&diff>365&series_id=="W053RC1Q027SBEA"~date+170,
      !(date%in%c("2025-10-01","2025-11-01"))&diff>365&series_id=="W053RC1Q027SBEA"~date+170,
      !(date%in%c("2025-10-01","2025-11-01"))&diff>365&series_id=="W053RC1Q027SBEA"~date+170,
      !(date%in%c("2025-10-01","2025-11-01"))&diff>150&series_id%in%c("EXPGSC1" ,"FGCEC1" , "FPIC1" ,  "GCEC1" ,  
                              "GDPC1" ,  "GPDIC1" , "IMPGSC1" ,"PCDGCC96", 
                              "PCECC96","PCESVC96","PCNDGC96","PNFIC1" , 
                              "PRFIC1" , "SLCEC1" , "FGEXPND","FGSL") ~ date+119,
      !(date%in%c("2025-10-01","2025-11-01"))&diff>80&series_id%in%c("RRSFS")~date+45,
      !(date%in%c("2025-10-01","2025-11-01"))&diff>100&series_id%in%c("HSN1F")~date+53,
      !(date%in%c("2025-10-01","2025-11-01"))&(diff>=90|ch==0)&series_id%in%c("DSPIC96","PCE","PCEPI",'PCEPILFE')~date+61,
      !(date%in%c("2025-10-01","2025-11-01"))&diff>=90&series_id%in%c("PERMIT","HOUST")~date+47,
      !(date%in%c("2025-10-01","2025-11-01"))&diff>=90&series_id%in%c("DGORDER")~date+56,
      !(date%in%c("2025-10-01","2025-11-01"))&(diff>=60|ch==0)&series_id%in%c("CPILFESL","CPIAUCSL","INDPRO")~date+45,
      !(date%in%c("2025-10-01","2025-11-01"))&(diff>=60|ch==0)&series_id%in%c("CE16OV","UNRATE","PAYEMS")~date+35,
      TRUE~release_date
    )) %>% 
    select(-c(diff,ch))
  
  return(national_econ)
}

national_econ = get_national_econ_data(end_date)

# Treasury data
set_config( config( ssl_verifypeer = 0L ) )

op_cash_dep_withdraw_new = data.frame()
for(yr in c(2025:year(end_date))){
  
  print(as.character(yr)) 
  
  request = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                   "accounting/dts/deposits_withdrawals_operating_cash",
                   "?sort=-record_date",
                   "&format=json",
                   "&filter=record_calendar_year:eq:",as.character(yr),
                   "&page[size]=10000")
  response=GET(request) 
  out=fromJSON(rawToChar(response$content))
  
  if(out$meta$`total-pages`==0){next}
  
  for(page_num in c(1:out$meta$`total-pages`)){
    
    request_2 = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                       "accounting/dts/deposits_withdrawals_operating_cash",
                       "?sort=-record_date",
                       "&format=csv",
                       "&filter=record_calendar_year:eq:",as.character(yr),
                       "&page[number]=",page_num,
                       "&page[size]=10000")
    
    data = read_csv(url(request_2))
    
    op_cash_dep_withdraw_new = new_bind(op_cash_dep_withdraw_new,data)
    
  }
  
}

op_cash_dep_withdraw = data.table::rbindlist(list(
  op_cash_dep_withdraw,
  op_cash_dep_withdraw_new
)) %>% 
  filter(record_date<=end_date)

debt_subject_to_limit_new = data.frame()
for(yr in c(2025:year(end_date))){
  
  print(as.character(yr)) 
  
  request = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                   "accounting/dts/debt_subject_to_limit",
                   "?sort=-record_date",
                   "&format=json",
                   "&filter=record_calendar_year:eq:",as.character(yr),
                   "&page[size]=10000")
  response=GET(request) 
  out=fromJSON(rawToChar(response$content))
  
  if(out$meta$`total-pages`==0){next}
  
  for(page_num in c(1:out$meta$`total-pages`)){
    
    request_2 = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                       "accounting/dts/debt_subject_to_limit",
                       "?sort=-record_date",
                       "&format=csv",
                       "&filter=record_calendar_year:eq:",as.character(yr),
                       "&page[number]=",page_num,
                       "&page[size]=10000")
    
    data = read_csv(url(request_2))
    
    debt_subject_to_limit_new = new_bind(debt_subject_to_limit_new,data)
    
  }
  
}

debt_subject_to_limit = data.table::rbindlist(list(
  debt_subject_to_limit,
  debt_subject_to_limit_new
)) %>% 
  filter(record_date<=end_date)

deficit_summary_new = data.frame()
for(yr in c(2025:year(end_date))){
  
  print(as.character(yr)) 
  
  request = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                   "accounting/mts/mts_table_1",
                   "?sort=-record_date",
                   "&format=json",
                   "&filter=record_calendar_year:eq:",as.character(yr),
                   "&page[size]=10000")
  response=GET(request) 
  out=fromJSON(rawToChar(response$content))
  
  if(out$meta$`total-pages`==0){next}
  
  for(page_num in c(1:out$meta$`total-pages`)){
    
    request_2 = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                       "accounting/mts/mts_table_1",
                       "?sort=-record_date",
                       "&format=csv",
                       "&filter=record_calendar_year:eq:",as.character(yr),
                       "&page[number]=",page_num,
                       "&page[size]=10000")
    
    data = read_csv(url(request_2))
    
    deficit_summary_new = new_bind(deficit_summary_new,data)
    
  }
  
}

deficit_summary = data.table::rbindlist(list(
  deficit_summary,
  deficit_summary_new
)) %>% 
  filter(record_date<=end_date)

outlays_new = data.frame()
for(yr in c(2025:year(end_date))){
  
  print(as.character(yr)) 
  
  request = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                   "accounting/mts/mts_table_5",
                   "?sort=-record_date",
                   "&format=json",
                   "&filter=record_calendar_year:eq:",as.character(yr),
                   "&page[size]=10000")
  response=GET(request) 
  out=fromJSON(rawToChar(response$content))
  
  if(out$meta$`total-pages`==0){next}
  
  for(page_num in c(1:out$meta$`total-pages`)){
    
    request_2 = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                       "accounting/mts/mts_table_5",
                       "?sort=-record_date",
                       "&format=csv",
                       "&filter=record_calendar_year:eq:",as.character(yr),
                       "&page[number]=",page_num,
                       "&page[size]=10000")
    
    data = read_csv(url(request_2))
    
    outlays_new = new_bind(outlays_new,data)
    
  }
  
}

outlays = data.table::rbindlist(list(
  outlays,
  outlays_new
)) %>% 
  filter(record_date<=end_date)

receipts_new = data.frame()
for(yr in c(2025:year(end_date))){
  
  print(as.character(yr)) 
  
  request = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                   "accounting/mts/mts_table_4",
                   "?sort=-record_date",
                   "&format=json",
                   "&filter=record_calendar_year:eq:",as.character(yr),
                   "&page[size]=10000")
  response=GET(request) 
  out=fromJSON(rawToChar(response$content))
  
  if(out$meta$`total-pages`==0){next}
  
  for(page_num in c(1:out$meta$`total-pages`)){
    
    request_2 = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                       "accounting/mts/mts_table_4",
                       "?sort=-record_date",
                       "&format=csv",
                       "&filter=record_calendar_year:eq:",as.character(yr),
                       "&page[number]=",page_num,
                       "&page[size]=10000")
    
    data = read_csv(url(request_2))
    
    receipts_new = new_bind(receipts_new,data)
    
  }
  
}

receipts = data.table::rbindlist(list(
  receipts,
  receipts_new
)) %>% 
  filter(record_date<=end_date)

fed_invest_programs_new = data.frame()
for(yr in c(2025:year(end_date))){
  
  print(as.character(yr)) 
  
  request = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                   "accounting/od/fip_principal_outstanding_table1",
                   "?sort=-record_date",
                   "&format=json",
                   "&filter=record_calendar_year:eq:",as.character(yr),
                   "&page[size]=10000")
  response=GET(request) 
  out=fromJSON(rawToChar(response$content))
  
  if(out$meta$`total-pages`==0){next}
  
  for(page_num in c(1:out$meta$`total-pages`)){
    
    request_2 = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                       "accounting/od/fip_principal_outstanding_table1",
                       "?sort=-record_date",
                       "&format=csv",
                       "&filter=record_calendar_year:eq:",as.character(yr),
                       "&page[number]=",page_num,
                       "&page[size]=10000")
    
    data = read_csv(url(request_2))
    
    fed_invest_programs_new = new_bind(fed_invest_programs_new,data)
    
  }
  
}

fed_invest_programs = data.table::rbindlist(list(
  fed_invest_programs,
  fed_invest_programs_new
)) %>% 
  filter(record_date<=end_date)

spending_by_function_new = data.frame()
for(yr in c(2025:year(end_date))){
  
  print(as.character(yr)) 
  
  request = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                   "accounting/mts/mts_table_9",
                   "?sort=-record_date",
                   "&format=json",
                   "&filter=record_calendar_year:eq:",as.character(yr),
                   "&page[size]=10000")
  response=GET(request) 
  out=fromJSON(rawToChar(response$content))
  
  if(out$meta$`total-pages`==0){next}
  
  for(page_num in c(1:out$meta$`total-pages`)){
    
    request_2 = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                       "accounting/mts/mts_table_9",
                       "?sort=-record_date",
                       "&format=csv",
                       "&filter=record_calendar_year:eq:",as.character(yr),
                       "&page[number]=",page_num,
                       "&page[size]=10000")
    
    data = read_csv(url(request_2))
    
    spending_by_function_new = new_bind(spending_by_function_new,data)
    
  }
  
}

spending_by_function = data.table::rbindlist(list(
  spending_by_function,
  spending_by_function_new
)) %>% 
  filter(record_date<=end_date)

overall_debt_new = data.frame()
for(yr in c(2025:year(end_date))){
  
  print(as.character(yr)) 
  
  request = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                   "debt/mspd/mspd_table_2",
                   "?sort=-record_date",
                   "&format=json",
                   "&filter=record_calendar_year:eq:",as.character(yr),
                   "&page[size]=10000")
  response=GET(request) 
  out=fromJSON(rawToChar(response$content))
  
  if(out$meta$`total-pages`==0){next}
  
  for(page_num in c(1:out$meta$`total-pages`)){
    
    request_2 = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                       "debt/mspd/mspd_table_2",
                       "?sort=-record_date",
                       "&format=csv",
                       "&filter=record_calendar_year:eq:",as.character(yr),
                       "&page[number]=",page_num,
                       "&page[size]=10000")
    
    data = read_csv(url(request_2))
    
    overall_debt_new = new_bind(overall_debt_new,data)
    
  }
  
}

overall_debt = data.table::rbindlist(list(
  overall_debt,
  overall_debt_new
)) %>% 
  filter(record_date<=end_date)


treasury_securities_new = data.frame()
for(yr in c(2025:year(end_date))){
  
  print(as.character(yr)) 
  
  request = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                   "debt/mspd/mspd_table_3",
                   "?sort=-record_date",
                   "&format=json",
                   "&filter=record_calendar_year:eq:",as.character(yr),
                   "&page[size]=10000")
  response=GET(request) 
  out=fromJSON(rawToChar(response$content))
  
  if(out$meta$`total-pages`==0){next}
  
  for(page_num in c(1:out$meta$`total-pages`)){
    
    request_2 = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                       "debt/mspd/mspd_table_3",
                       "?sort=-record_date",
                       "&format=csv",
                       "&filter=record_calendar_year:eq:",as.character(yr),
                       "&page[number]=",page_num,
                       "&page[size]=10000")
    
    data = read_csv(url(request_2))
    
    treasury_securities_new = new_bind(treasury_securities_new,data)
    
  }
  
}

treasury_securities = data.table::rbindlist(list(
  treasury_securities,
  treasury_securities_new
)) %>% 
  filter(record_date<=end_date)


debt_level_new = data.frame()
for(yr in c(2025:year(end_date))){
  
  print(as.character(yr)) 
  
  request = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v2/",
                   "accounting/od/debt_to_penny",
                   "?sort=-record_date",
                   "&format=json",
                   "&filter=record_calendar_year:eq:",as.character(yr),
                   "&page[size]=10000")
  response=GET(request) 
  out=fromJSON(rawToChar(response$content))
  
  if(out$meta$`total-pages`==0){next}
  
  for(page_num in c(1:out$meta$`total-pages`)){
    
    request_2 = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v2/",
                       "accounting/od/debt_to_penny",
                       "?sort=-record_date",
                       "&format=csv",
                       "&filter=record_calendar_year:eq:",as.character(yr),
                       "&page[number]=",page_num,
                       "&page[size]=10000")
    
    data = read_csv(url(request_2))
    
    debt_level_new = new_bind(debt_level_new,data)
    
  }
  
}

debt_level = data.table::rbindlist(list(
  debt_level,
  debt_level_new
)) %>% 
  filter(record_date<=end_date)

investment_funds_new = data.frame()
for(yr in c(2025:year(end_date))){
  
  print(as.character(yr)) 
  
  request = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                   "accounting/od/fip_principal_outstanding_table1",
                   "?sort=-record_date",
                   "&format=json",
                   "&filter=record_calendar_year:eq:",as.character(yr),
                   "&page[size]=10000")
  response=GET(request) 
  out=fromJSON(rawToChar(response$content))
  
  if(out$meta$`total-pages`==0){next}
  
  for(page_num in c(1:out$meta$`total-pages`)){
    
    request_2 = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                       "accounting/od/fip_principal_outstanding_table1",
                       "?sort=-record_date",
                       "&format=csv",
                       "&filter=record_calendar_year:eq:",as.character(yr),
                       "&page[number]=",page_num,
                       "&page[size]=10000")
    
    data = read_csv(url(request_2))
    
    investment_funds_new = new_bind(investment_funds_new,data)
    
  }
  
}

investment_funds = data.table::rbindlist(list(
  investment_funds,
  investment_funds_new
)) %>% 
  filter(record_date<=end_date)

op_cash_balance_new = data.frame()
for(yr in c(2025:year(end_date))){
  
  print(as.character(yr)) 
  
  request = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                   "accounting/dts/operating_cash_balance",
                   "?sort=-record_date",
                   "&format=json",
                   "&filter=record_calendar_year:eq:",as.character(yr),
                   "&page[size]=10000")
  response=GET(request) 
  out=fromJSON(rawToChar(response$content))
  
  if(out$meta$`total-pages`==0){next}
  
  for(page_num in c(1:out$meta$`total-pages`)){
    
    request_2 = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                       "accounting/dts/operating_cash_balance",
                       "?sort=-record_date",
                       "&format=csv",
                       "&filter=record_calendar_year:eq:",as.character(yr),
                       "&page[number]=",page_num,
                       "&page[size]=10000")
    
    data = read_csv(url(request_2))
    
    op_cash_balance_new = new_bind(op_cash_balance_new,data)
    
  }
  
}

op_cash_balance = data.table::rbindlist(list(
  op_cash_balance,
  op_cash_balance_new
)) %>% 
  filter(record_date<=end_date)

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
#     data = read_csv(url(request_2))
#     
#     tax_deposits1 = new_bind(tax_deposits1,data)
#     
#   }
#   
# }

tax_deposits2_new = data.frame()
for(yr in c(2025:year(end_date))){
  
  print(as.character(yr)) 
  
  request = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                   "accounting/dts/inter_agency_tax_transfers",
                   "?sort=-record_date",
                   "&format=json",
                   "&filter=record_calendar_year:eq:",as.character(yr),
                   "&page[size]=10000")
  response=GET(request) 
  out=fromJSON(rawToChar(response$content))
  
  if(out$meta$`total-pages`==0){next}
  
  for(page_num in c(1:out$meta$`total-pages`)){
    
    request_2 = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                       "accounting/dts/inter_agency_tax_transfers",
                       "?sort=-record_date",
                       "&format=csv",
                       "&filter=record_calendar_year:eq:",as.character(yr),
                       "&page[number]=",page_num,
                       "&page[size]=10000")
    
    data = read_csv(url(request_2))
    
    tax_deposits2_new = new_bind(tax_deposits2_new,data)
    
  }
  
}

tax_deposits2 = data.table::rbindlist(list(
  tax_deposits2,
  tax_deposits2_new
)) %>% 
  filter(record_date<=end_date)

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
  mutate_at(vars(record_calendar_month:record_calendar_day),as.numeric) %>% 
  filter(record_date<=end_date)


tax_refunds_new = data.frame()
for(yr in c(2025:year(end_date))){
  
  print(as.character(yr)) 
  
  request = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                   "accounting/dts/income_tax_refunds_issued",
                   "?sort=-record_date",
                   "&format=json",
                   "&filter=record_calendar_year:eq:",as.character(yr),
                   "&page[size]=10000")
  response=GET(request) 
  out=fromJSON(rawToChar(response$content))
  
  if(out$meta$`total-pages`==0){next}
  
  for(page_num in c(1:out$meta$`total-pages`)){
    
    request_2 = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                       "accounting/dts/income_tax_refunds_issued",
                       "?sort=-record_date",
                       "&format=csv",
                       "&filter=record_calendar_year:eq:",as.character(yr),
                       "&page[number]=",page_num,
                       "&page[size]=10000")
    
    data = read_csv(url(request_2))
    
    tax_refunds_new = new_bind(tax_refunds_new,data)
    
  }
  
}

tax_refunds = data.table::rbindlist(list(
  tax_refunds,
  tax_refunds_new
)) %>% 
  filter(record_date<=end_date)

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
)) %>% 
  filter(record_date<=end_date)


daily_gas_activity_new = data.frame()
for(var in c("gas_held_by_public_daily_activity","gas_intragov_holdings_daily_activity")){
for(yr in c(2025:year(end_date))){
  
  print(as.character(yr)) 
  
  request = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                   "accounting/od/",var,
                   "?sort=-record_date",
                   "&format=json",
                   "&filter=record_calendar_year:eq:",as.character(yr),
                   "&page[size]=10000")
  response=GET(request) 
  out=fromJSON(rawToChar(response$content))
  
  if(out$meta$`total-pages`==0){next}
  
  for(page_num in c(1:out$meta$`total-pages`)){
    
    request_2 = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                       "accounting/od/",var,
                       "?sort=-record_date",
                       "&format=csv",
                       "&filter=record_calendar_year:eq:",as.character(yr),
                       "&page[number]=",page_num,
                       "&page[size]=10000")
    
    data = read_csv(url(request_2))
    
    daily_gas_activity_new = new_bind(daily_gas_activity_new,data)
    
  }
  
}
}

daily_gas_activity = data.table::rbindlist(list(
  daily_gas_activity,
  daily_gas_activity_new
)) %>% 
  filter(record_date<=end_date)

debt_subject_to_limit = data.table::rbindlist(list(
  debt_subject_to_limit,
  debt_subject_to_limit_new
)) %>% 
  filter(record_date<=end_date)

issuance_new = data.frame()
for(yr in c(2025:year(end_date))){
  
  print(as.character(yr)) 
  
  request = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                   "accounting/mts/mts_table_6d",
                   "?sort=-record_date",
                   "&format=json",
                   "&filter=record_calendar_year:eq:",as.character(yr),
                   "&page[size]=10000")
  response=GET(request) 
  out=fromJSON(rawToChar(response$content))
  
  if(out$meta$`total-pages`==0){next}
  
  for(page_num in c(1:out$meta$`total-pages`)){
    
    request_2 = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                       "accounting/mts/mts_table_6d",
                       "?sort=-record_date",
                       "&format=csv",
                       "&filter=record_calendar_year:eq:",as.character(yr),
                       "&page[number]=",page_num,
                       "&page[size]=10000")
    
    data = read_csv(url(request_2))
    
    issuance_new = new_bind(issuance_new,data)
    
  }
  
}

issuance = data.table::rbindlist(list(
  issuance,
  issuance_new
)) %>% 
  filter(record_date<=end_date)

funds_new = data.frame()
for(yr in c(2025:year(end_date))){
  
  print(as.character(yr)) 
  
  request = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                   "accounting/od/fip_statement_of_account_table2",
                   "?sort=-record_date",
                   "&format=json",
                   "&filter=record_calendar_year:eq:",as.character(yr),
                   "&page[size]=10000")
  response=GET(request) 
  out=fromJSON(rawToChar(response$content))
  
  if(out$meta$`total-pages`==0){next}
  
  for(page_num in c(1:out$meta$`total-pages`)){
    
    request_2 = paste0("https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v1/",
                       "accounting/od/fip_statement_of_account_table2",
                       "?sort=-record_date",
                       "&format=csv",
                       "&filter=record_calendar_year:eq:",as.character(yr),
                       "&page[number]=",page_num,
                       "&page[size]=10000")
    
    data = read_csv(url(request_2))
    
    funds_new = new_bind(funds_new,data)
    
  }
  
}

funds = data.table::rbindlist(list(
  funds,
  funds_new
)) %>% 
  filter(record_date<=end_date)

cbo_proj = read_csv("https://raw.githubusercontent.com/US-CBO/eval-projections/refs/heads/main/input_data/baselines.csv")
cbo_actual = read_csv("https://raw.githubusercontent.com/US-CBO/eval-projections/refs/heads/main/input_data/actuals.csv")

if(end_date>="2025-02-22"){
  
  cbo_proj = cbo_proj %>% 
    mutate(value=case_when(
      baseline_date<"2026-02-01"~value,
      subcategory!="Customs Duties"~value,
      projected_fiscal_year==2026~value-148.25,
      projected_fiscal_year==2027~value-186.75,
      projected_fiscal_year==2028~value-80,
      projected_fiscal_year==2029~value-82.5,
      projected_fiscal_year==2030~value-85.5,
      projected_fiscal_year==2031~value-88.5,
      projected_fiscal_year==2032~value-92,
      projected_fiscal_year==2033~value-96,
      projected_fiscal_year==2034~value-99,
      projected_fiscal_year==2035~value-104,
      projected_fiscal_year==2036~value-110
    ))
  
}

cbo_proj = cbo_proj %>% 
  filter(baseline_date<=end_date)

cbo_actual = cbo_actual %>% 
  filter(fiscal_year<ifelse(month(end_date)<10,year(end_date),year(end_date)+1))

temp <- tempfile()
download.file("https://www.cbo.gov/system/files/2026-02/55022-2026-02-Historical-Economic-Data.zip",temp)
cbo_econ <- read_csv(unz(temp, "Quarterly_February2026.csv")) %>% 
  mutate(date=as.Date(as.yearqtr(date,format="%Yq%q")))
unlink(temp)

outlays_fred = tryCatch({
    fredr(paste0("MTSO133FMS"),realtime_end = as.Date(end_date))
  },error=function(e) fredr(paste0("MTSO133FMS"),realtime_start = as.Date(end_date))) %>% 
  mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)),
         value=value/1000)

receipts_fred = tryCatch({
    fredr(paste0("MTSR133FMS"),realtime_end = as.Date(end_date))
  },error=function(e) fredr(paste0("MTSR133FMS"),realtime_start = as.Date(end_date))) %>% 
  mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)),
         value=value/1000)

deficit_fred = tryCatch({
    fredr(paste0("MTSDS133FMS"),realtime_end = as.Date(end_date))
  },error=function(e) fredr(paste0("MTSDS133FMS"),realtime_start = as.Date(end_date))) %>% 
  mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)))

