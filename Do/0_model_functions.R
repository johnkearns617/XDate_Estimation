#' which_category
#' 
#' \code{which_category} tells you what category a given Google Trends category id refers to
#' 
#' User must pass the id for one category and trends_cats must be loaded in
#' 
#' @param num numeric or character of Google Trends ID
#' 
#' @return string name of category
#' 

which_category = function(num){
  
  return(trends_cats$category[trends_cats$id==num][1])
  
}

#' 
#' seas_adjust_gt
#' 
#' \code{seas_adjust_gt} will take a weekly entry for Google Trends and seasonally adjust it
#' 
#' @param trends_df data.frame of the set of weekly data series. Must have columns cat, value_detrend, value, and date
#' @param cat1 string or numeric of one GT category
#' 
#' @return data.frame that appends seasonally adjusted and LOESS data
#' 

seas_adjust_gt = function(trends_df,cat1){
  
  set.seed(178)
  
  if(cat1=="987"){
    stop("Category 987 has bad data")
  } # 987 has bad data
  
  test_cat = trends_df[trends_df$cat==cat1,] %>%
    select(date,value,value_detrend) %>%
    mutate(date=as.Date(date))
  
  hits_smooth = boiwsa(test_cat$value_detrend,test_cat$date,auto.ao.search = FALSE)
  hits_smooth = hits_smooth$sa
  
  hits_smooth = as.numeric(smooth(hits_smooth,kind="3RSS",endrule="Tukey"))
  
  test_cat = cbind(test_cat,value_sa=hits_smooth)
  
  hits_loess = hpfilter(as.numeric(hits_smooth),freq=(1600*(12^4)))$trend
  test_cat = cbind(test_cat,value_loess=hits_loess)
  
  return(test_cat %>% mutate(category=cat1,value_sa=as.numeric(value_sa)))
  
}

#' 
#' plot_cat
#' \code{plot_cat} will plot the data for google trends to see how SA process went
#' You must have trends_sa2 loaded
#' 
#' @param cat1 numeric or vector of numerics for google trends categories
#' 
#' @return prints a ggplot object of the raw, detrended, seasonally adjusted, and loess

plot_cat = function(cat1){
  
  plt1 = ggplot(trends_sa2 %>% 
                  dplyr::filter(category%in%cat1) %>% 
                  mutate(category=factor(category,levels=trends_cats$id,labels=trends_cats$category)),aes(x=date)) + 
    #geom_line(aes(y=value,color="Raw")) + 
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

#'
#' seasonal_adj
#' 
#' \code{seasonal_adj} is a mroe generalized seasonal adjustment function for data pulled from FRED
#' 
#' @param df is a data.frame with columns date and value. Should be monthly data
#' @param mode optional parameter if you want to seasonally adjust with additive or multiplicative
#' 
#' @return vector with seasonally adjusted data
#' 

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

#'
#' new_bind
#' 
#' \code{new_bind} helps map the treasury data
#' 
#' @param a a data object loaded from Treasury Fiscal Service API
#' @param b another data object from BFS
#' 
#' @return a joined data object

new_bind <- function(a, b) {
  common_cols <- intersect(names(a), names(b))
  b[common_cols] <- map2_df(b[common_cols], 
                            map(a[common_cols], class), ~{class(.x) <- .y;.x})
  bind_rows(a, b)  
}


#' 
#' make_df
#' 
#' \code{make_df} will make a data frame of data available from a given date
#' 
#' @param 

make_df = function(end_date){
  
  
  df = national_econ %>% 
    filter(date<=end_date) %>% 
    mutate(value=ifelse(release_date>end_date,NA,value)) %>% 
    pivot_wider(id_cols=c('date'),names_from='series_id',values_from='value') %>% 
    mutate(year=year(date),
           qtr=quarter(date)) %>% 
    relocate(date,.before=1) %>% 
    # other state variables
    left_join(state_trends %>% 
                mutate(date=date+6,
                       series_id=paste0("gt_",category),
                       month=month(date),
                       year=year(date)) %>%
                group_by(year,month,series_id) %>%
                summarize(deviation=mean(deviation,na.rm=TRUE)) %>% 
                mutate(date=as.Date(paste0(year,"-",month,"-","01"),format="%Y-%m-%d")) %>% 
                pivot_wider(id_cols=c('date'),names_from='series_id',values_from=c('deviation')),
              by=c('date'))
  
  
  return(df)
  
}


#'
#' impute_function
#' \code{impute_function} takes an incomplete data frame of economic data and makes it full based on the available observations
#' 
#'  @param df data.frame with monthly economic data
#'  @param dat is the date that is the 'end date' of the data
#'  
#'  @return imputed data frame


impute_function = function(df,dat){
  
  set.seed(178)
  
  test_dineof=df
  
  flag = 0
  while(flag<3){
    for(col1 in colnames(test_dineof)[c(2:ncol(test_dineof))]){
      
      print(paste0(col1))
      
      if(length(which(is.na(test_dineof[c((nrow(test_dineof)-10):nrow(test_dineof)),col1])))==0&col1!="IHLIDXUS"){ next }
      if(!(col1%in%colnames(test_dineof))){next}
      if(col1%in%c("ADPMNUSNERSA")&as.Date(dat)<"2010-01-01"){next}
      if(col1=="IHLIDXUS"&as.Date(dat)<"2021-01-01"){next}
      
      value = data.frame(date=test_dineof$date)
      for(i in 1:30){
        if("IHLIDXUS"%in%colnames(test_dineof)&"ADPMNUSNERSA"%in%colnames(test_dineof)){
          if(col1=="IHLIDXUS"){potential_cols = colnames(test_dineof %>% select(-c(col1,IHLIDXUS,ADPMNUSNERSA,gt_1003:gt_999)) %>% filter(date==max(date)) %>% select(-date) %>% select_if(!is.na(.)))}else{
            potential_cols = colnames(test_dineof %>% select(-c(col1,IHLIDXUS,ADPMNUSNERSA,gt_1003:gt_999)) %>% filter(date==test_dineof$date[(nrow(test_dineof)-3):nrow(test_dineof)][head(which(is.na(test_dineof[[col1]][(nrow(test_dineof)-3):nrow(test_dineof)])),1)]) %>% select(-date) %>% select_if(!is.na(.)))
          }
        } else{
          potential_cols = colnames(test_dineof %>% select(-c(col1,gt_1003:gt_999)) %>% select(-one_of("ADPMNUSNERSA","IHLIDXUS")) %>% filter(date==test_dineof$date[(nrow(test_dineof)-3):nrow(test_dineof)][max(head(which(is.na(test_dineof[[col1]][(nrow(test_dineof)-3):nrow(test_dineof)])),1),1)]) %>% select(-date) %>% select_if(!is.na(.)))
        }
        cols = c(sample(potential_cols,min(c(15,floor(length(potential_cols)/2)))),sample(colnames(test_dineof %>% select(gt_1003:gt_999)),15))
        test = lm_robust(as.formula(paste0(paste0(col1,"~lag+lag2+"),paste(cols,collapse="+"))),
                         data=test_dineof %>% select(col1,cols) %>% 
                           mutate(lag=dplyr::lag(!!sym(col1),1),
                                  lag2=dplyr::lag(!!sym(col1),2),
                                  lag3=dplyr::lag(!!sym(col1),3),
                                  lag4=dplyr::lag(!!sym(col1),4),
                                  lag5=dplyr::lag(!!sym(col1),5),
                                  lag6=dplyr::lag(!!sym(col1),6)))
        imp <- predict(test,test_dineof %>% select(col1,cols) %>% mutate(lag=dplyr::lag(!!sym(col1),1),
                                                                         lag2=dplyr::lag(!!sym(col1),2),
                                                                         lag3=dplyr::lag(!!sym(col1),3),
                                                                         lag4=dplyr::lag(!!sym(col1),4),
                                                                         lag5=dplyr::lag(!!sym(col1),5),
                                                                         lag6=dplyr::lag(!!sym(col1),6)) %>% 
                         fill(lag:lag6,.direction="up"))
        
        value=bind_cols(value,imp)
      }
      
      value1 = data.frame(
        date=value$date,
        replacement=rowMeans(value[,2:ncol(value)],na.rm=TRUE)
      )
      
      for(i in 1:nrow(value)){
        
        if(is.na(test_dineof[i,col1])){
          test_dineof[i,col1] = value1[i,"replacement"]
        }
      }
    }
    if(col1=="gt_999"){
      flag = flag+1
    }
  }
  
  return(test_dineof)
  
}


#' 
#' nowcast_headline
#' 
#' \code{nowcast_headline}
#' 
#' @param dataset
#' @param cbo_category
#' 
#' @return list with input data, regression, predictions, and the monthly shares regression
#' 

get_deficit_imputed_data = function(dat,dataset,cbo_category,monthly_shares_reg){
  
  fcast_df1 = read_csv(paste0("Data/Processing/imputed_data/imputed_data_asof",dat,".csv")) %>% 
    arrange(date) %>%
    mutate(year=year(date),
           month=month(date)) %>%
    select(-c(PCE,PRS85006112)) %>%
    select(-one_of("ADPMNUSNERSA")) %>% 
    left_join(dataset %>% 
                select(date,value)) %>% # join the yvariable
    arrange(date) %>%
    left_join(national_econ %>% 
                filter(series_id=="GDPC1") %>% 
                select(date,GDPC1=value)) %>% 
    group_by(year,quarter(date)) %>% 
    mutate(GDPC1=GDPC1[1])  %>% 
    ungroup() %>% 
    select(-`quarter(date)`) %>% 
    mutate(GDPC1 = (GDPC1/dplyr::lag(GDPC1,3)-1)*100) %>% 
    rowwise() %>% 
    mutate(GDPC1=ifelse(floor_date(date,"quarter")%in%gdp_data$date&is.na(GDPC1),
                        tail(gdp_data$gdp[gdp_data$date==floor_date(date,"quarter")],1),
                        GDPC1)) %>% 
    ungroup() %>% 
    mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10),~((./dplyr::lag(.,1)-1)*100)) %>%
    mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,gt_1003:gt_999),~(.-dplyr::lag(.,1))) %>%
    mutate(lag1=dplyr::lag(value,1),
           lag2=dplyr::lag(value,2),
           lag3=dplyr::lag(value,3),
           lag4=dplyr::lag(value,4)) %>%
    ungroup() %>% 
    mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)))  %>% 
    left_join(cbo_proj %>% 
                filter(component==cbo_category&category=="Total") %>% 
                group_by(projected_fiscal_year) %>% 
                slice(n()) %>% 
                select(projected_fiscal_year,value) %>% 
                rename(cbo_proj=value,
                       fiscal_year=projected_fiscal_year))
  fcast_df1$cbo_proj_month = as.numeric(predict(monthly_shares_reg,data.frame(month=fcast_df1$month)))*fcast_df1$cbo_proj
  fcast_df1 = fcast_df1 %>% 
    mutate(cbo_proj_diff=(value/cbo_proj_month-1)*100) %>% 
    mutate(lag1_cbo_proj_diff=dplyr::lag(cbo_proj_diff,1),
           lag2_cbo_proj_diff=dplyr::lag(cbo_proj_diff,2))
  
  return(fcast_df1)
}

nowcast_headline = function(dataset,cbo_category){
  
  monthly_shares = dataset %>% 
    filter(fiscal_year>=2002&fiscal_year<=2023) %>% 
    group_by(fiscal_year) %>% 
    mutate(total=sum(value,na.rm=TRUE)) %>% 
    ungroup() %>%  
    mutate(share=value/total,
           month=month(date))
  
  monthly_shares_reg = lm_robust(share~factor(month),monthly_shares %>% group_by(fiscal_year) %>% filter(n()==12))
  
  fcast_df1 = get_deficit_imputed_data(floor_date(Sys.Date(),"year")-1,dataset,cbo_category,monthly_shares_reg)
  
  X = model.matrix(as.formula(paste0("value","~",paste(colnames(fcast_df1)[c(2:252)],collapse="+"))),
                   fcast_df1 %>% filter(date<"2024-01-01"&year(date)>=2006&!is.na(value)))[, -1]
  y = (fcast_df1 %>% filter(date<"2024-01-01"&year(date)>=2006&!is.na(value)))[["value"]]
  
  weight = (1:nrow(X))/nrow(X)
  weight = ifelse(weight<.5,.5,weight)
  fit_lasso_state = glmnet(X, y, alpha = 1,pmax=20,weights = weight)
  # weight by how recent the data is
  
  selected_coefs_state = data.frame(varImp(fit_lasso_state,lambda=min(fit_lasso_state$lambda), scale = FALSE)) %>% filter(Overall!=0)
  selected_coefs_state$var = as.numeric(gsub("gt_","",rownames(selected_coefs_state)))
  coef_value_state = coef(fit_lasso_state,s=min(fit_lasso_state$lambda))[,1][-1]
  coef_value_state = coef_value_state[coef_value_state!=0]
  selected_coefs_state = cbind(selected_coefs_state,coef_value_state)
  selected_coefs_state$category = sapply(selected_coefs_state$var,which_category)
  selected_coefs_state = selected_coefs_state %>% arrange(-Overall)
  
  test = lm_robust(as.formula(paste0("value","~lag1+lag2+lag3+lag4+cbo_proj_month+GDPC1+",paste(c(rownames(selected_coefs_state)),collapse="+"))),
                   data = fcast_df1 %>% filter(date<='2024-01-01') %>% mutate(weight=(1:n())/n()))
  
  fcast_df1 = get_deficit_imputed_data(Sys.Date(),dataset,cbo_category,monthly_shares_reg)
  
  for(dat in tail(fcast_df1,10) %>% filter(is.na(value)) %>% pull(date)){
    
    fcast_df1$value[fcast_df1$date==dat] = predict(test,fcast_df1 %>% filter(date==dat)) 
    
    fcast_df1 = fcast_df1 %>% 
      mutate(cbo_proj_diff=(value/cbo_proj_month-1)*100) %>% 
      mutate(lag1_cbo_proj_diff=dplyr::lag(cbo_proj_diff,1),
             lag2_cbo_proj_diff=dplyr::lag(cbo_proj_diff,2)) %>% 
      mutate(lag1=dplyr::lag(value,1),
             lag2=dplyr::lag(value,2),
             lag3=dplyr::lag(value,3),
             lag4=dplyr::lag(value,4))
    
  }
  
  pred_df = data.frame(
    date=fcast_df1[['date']],
    var=cbo_category,
    pred=predict(test,fcast_df1),
    actual=fcast_df1[['value']],
    cbo_proj=fcast_df1[['cbo_proj_month']]
  )
  
  return(list(
    'data'=fcast_df1,
    'reg'=test,
    'pred_df'=pred_df,
    'monthly_shares_reg'=monthly_shares_reg
  ))
  
}

#' 
#' nowcast_budget_receipt
#' 
#' \code{nowcast_budget_receipt}
#' 
#' @param mts_dataset data fram from BFS for receipts
#' @param col_mts name in BFS dataset for specific receipt category
#' @param cbo_component
#' @param cbo_category
#' 
#' @return list with input data, regression, predictions, and the monthly shares regression
#' 

nowcast_budget_receipt = function(mts_dataset,col_mts,cbo_component,cbo_category){
  
  monthly_shares = mts_dataset %>% 
    filter(classification_desc==col_mts) %>% 
    mutate(record_date=floor_date(record_date,"month"),
           current_month_net_rcpt_amt=as.numeric(current_month_net_rcpt_amt)/1000000000) %>% 
    select(record_date,current_month_net_rcpt_amt) %>% 
    rename(date=record_date,
           value=current_month_net_rcpt_amt) %>% 
    mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10))) %>% 
    group_by(fiscal_year) %>% 
    mutate(total=sum(value,na.rm=TRUE)) %>% 
    ungroup() %>%  
    mutate(share=value/total,
           month=month(date))
  
  monthly_shares_reg = lm_robust(share~factor(month),monthly_shares %>% group_by(fiscal_year) %>% filter(n()==12))
  
  fcast_df1 = read_csv(paste0("Data/Processing/imputed_data/imputed_data_asof",floor_date(Sys.Date(),"year")-1,".csv"))  %>% 
    arrange(date) %>%
    mutate(year=year(date),
           month=month(date)) %>%
    select(-c(PCE,PRS85006112)) %>%
    select(-one_of("ADPMNUSNERSA")) %>% 
    left_join(mts_dataset %>% filter(classification_desc==col_mts) %>% 
                mutate(record_date=floor_date(record_date,"month"),
                       current_month_net_rcpt_amt=as.numeric(current_month_net_rcpt_amt)/1000000000) %>% 
                select(record_date,current_month_net_rcpt_amt) %>% 
                rename(date=record_date,
                       value=current_month_net_rcpt_amt)) %>% # join the yvariable
    arrange(date) %>%
    left_join(national_econ %>% 
                filter(series_id=="GDPC1") %>% 
                select(date,GDPC1=value)) %>% 
    group_by(year,quarter(date)) %>% 
    mutate(GDPC1=GDPC1[1])  %>% 
    ungroup() %>% 
    select(-`quarter(date)`) %>% 
    mutate(GDPC1 = (GDPC1/dplyr::lag(GDPC1,3)-1)*100) %>% 
    rowwise() %>% 
    mutate(GDPC1=ifelse(floor_date(date,"quarter")%in%gdp_data$date&is.na(GDPC1),
                        tail(gdp_data$gdp[gdp_data$date==floor_date(date,"quarter")],1),
                        GDPC1)) %>% 
    ungroup() %>% 
    mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10),~((./dplyr::lag(.,1)-1)*100)) %>%
    mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,gt_1003:gt_999),~(.-dplyr::lag(.,1))) %>%
    mutate(lag1=dplyr::lag(value,1),
           lag2=dplyr::lag(value,2),
           lag3=dplyr::lag(value,3),
           lag4=dplyr::lag(value,4)) %>%
    ungroup() %>% 
    mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)))  %>% 
    left_join(cbo_proj %>% 
                filter(component==cbo_component&category==cbo_category) %>% 
                group_by(projected_fiscal_year) %>% 
                slice(n()) %>% 
                select(projected_fiscal_year,value) %>% 
                rename(cbo_proj=value,
                       fiscal_year=projected_fiscal_year))
  fcast_df1$cbo_proj_month = as.numeric(predict(monthly_shares_reg,data.frame(month=fcast_df1$month)))*fcast_df1$cbo_proj
  fcast_df1 = fcast_df1 %>% 
    mutate(cbo_proj_diff=(value/cbo_proj_month-1)*100) %>% 
    mutate(lag1_cbo_proj_diff=dplyr::lag(cbo_proj_diff,1),
           lag2_cbo_proj_diff=dplyr::lag(cbo_proj_diff,2))
  
  X = model.matrix(as.formula(paste0("value","~",paste(colnames(fcast_df1)[c(2:252)],collapse="+"))),
                   fcast_df1 %>% filter(date<"2024-01-01"&year(date)>=2006&!is.na(value)))[, -1]
  y = (fcast_df1 %>% filter(date<"2024-01-01"&year(date)>=2006&!is.na(value)))[['value']]
  
  weight = (1:nrow(X))/nrow(X)
  weight = ifelse(weight<.5,.5,weight)
  fit_lasso_state = glmnet(X, y, alpha = 1,pmax=20,weights = weight)
  # weight by how recent the data is
  
  selected_coefs_state = data.frame(varImp(fit_lasso_state,lambda=min(fit_lasso_state$lambda), scale = FALSE)) %>% filter(Overall!=0)
  selected_coefs_state$var = as.numeric(gsub("gt_","",rownames(selected_coefs_state)))
  coef_value_state = coef(fit_lasso_state,s=min(fit_lasso_state$lambda))[,1][-1]
  coef_value_state = coef_value_state[coef_value_state!=0]
  selected_coefs_state = cbind(selected_coefs_state,coef_value_state)
  selected_coefs_state$category = sapply(selected_coefs_state$var,which_category)
  selected_coefs_state = selected_coefs_state %>% arrange(-Overall)
  
  test = lm_robust(as.formula(paste0("value","~lag1+lag2+lag3+lag4+cbo_proj_month+GDPC1+",paste(c(rownames(selected_coefs_state)),collapse="+"))),
                   data = fcast_df1 %>% filter(date<='2024-01-01') %>% mutate(weight=(1:n())/n()))
  
  fcast_df1 = read_csv(paste0("Data/Processing/imputed_data/imputed_data_asof",Sys.Date(),".csv"))  %>% 
    arrange(date) %>%
    mutate(year=year(date),
           month=month(date)) %>%
    select(-c(PCE,PRS85006112)) %>%
    select(-one_of("ADPMNUSNERSA")) %>% 
    left_join(mts_dataset %>% filter(classification_desc==col_mts) %>% 
                mutate(record_date=floor_date(record_date,"month"),
                       current_month_net_rcpt_amt=as.numeric(current_month_net_rcpt_amt)/1000000000) %>% 
                select(record_date,current_month_net_rcpt_amt) %>% 
                rename(date=record_date,
                       value=current_month_net_rcpt_amt)) %>% # join the yvariable
    arrange(date) %>%
    left_join(national_econ %>% 
                filter(series_id=="GDPC1") %>% 
                select(date,GDPC1=value)) %>% 
    group_by(year,quarter(date)) %>% 
    mutate(GDPC1=GDPC1[1])  %>% 
    ungroup() %>% 
    select(-`quarter(date)`) %>% 
    mutate(GDPC1 = (GDPC1/dplyr::lag(GDPC1,3)-1)*100) %>% 
    rowwise() %>% 
    mutate(GDPC1=ifelse(floor_date(date,"quarter")%in%gdp_data$date&is.na(GDPC1),
                        tail(gdp_data$gdp[gdp_data$date==floor_date(date,"quarter")],1),
                        GDPC1)) %>% 
    ungroup() %>% 
    mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10),~((./dplyr::lag(.,1)-1)*100)) %>%
    mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,gt_1003:gt_999),~(.-dplyr::lag(.,1))) %>%
    mutate(lag1=dplyr::lag(value,1),
           lag2=dplyr::lag(value,2),
           lag3=dplyr::lag(value,3),
           lag4=dplyr::lag(value,4)) %>%
    ungroup() %>% 
    mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)))  %>% 
    left_join(cbo_proj %>% 
                filter(component==cbo_component&category==cbo_category) %>% 
                group_by(projected_fiscal_year) %>% 
                slice(n()) %>% 
                select(projected_fiscal_year,value) %>% 
                rename(cbo_proj=value,
                       fiscal_year=projected_fiscal_year))
  fcast_df1$cbo_proj_month = as.numeric(predict(monthly_shares_reg,data.frame(month=fcast_df1$month)))*fcast_df1$cbo_proj
  fcast_df1 = fcast_df1 %>% 
    mutate(cbo_proj_diff=(value/cbo_proj_month-1)*100) %>% 
    mutate(lag1_cbo_proj_diff=dplyr::lag(cbo_proj_diff,1),
           lag2_cbo_proj_diff=dplyr::lag(cbo_proj_diff,2))
  
  if(is.infinite(max(abs(which(is.na(tail(fcast_df1$value,5)))-6)))){
    next
  }else{
    for(dat in tail(fcast_df1$date,max(abs(which(is.na(tail(fcast_df1$value,5)))-6)))){
      
      fcast_df1$value[fcast_df1$date==dat] = predict(test,fcast_df1 %>% filter(date==dat))
      
      fcast_df1 = fcast_df1 %>% 
        mutate(lag1=dplyr::lag(value,1),
               lag2=dplyr::lag(value,2),
               lag3=dplyr::lag(value,3),
               lag4=dplyr::lag(value,4))
      
    }
  }
  
  pred_df = data.frame(
    date=fcast_df1[['date']],
    var=cbo_category,
    pred=predict(test,fcast_df1),
    actual=fcast_df1[['value']],
    cbo_proj=fcast_df1[['cbo_proj_month']]
  )
  
  return(list(
    'data'=fcast_df1,
    'reg'=test,
    'pred_df'=pred_df,
    'monthly_shares_reg'=monthly_shares_reg
  ))
  
}

#' 
#' nowcast_budget_outlay
#' 
#' \code{nowcast_budget_outlay}
#' 
#' @param mts_dataset data fram from BFS for receipts
#' @param col_mts name in BFS dataset for specific receipt category
#' @param cbo_component
#' @param cbo_category
#' 
#' @return list with input data, regression, predictions, and the monthly shares regression
#' 

nowcast_budget_outlay = function(cbo_category){
  
  if(cbo_category=="Medicare"){
    mandatory = spending_by_function %>% 
      filter(classification_desc=="Medicare") %>% 
      mutate(current_month_rcpt_outly_amt=as.numeric(current_month_rcpt_outly_amt)*.988828) %>% 
      select(record_date,current_month_rcpt_outly_amt)
    
    monthly_shares = mandatory %>% 
      mutate(record_date=floor_date(record_date,"month"),
             current_month_net_rcpt_amt=as.numeric(current_month_rcpt_outly_amt)/1000000000) %>% 
      select(record_date,current_month_net_rcpt_amt) %>% 
      rename(date=record_date,
             value=current_month_net_rcpt_amt) %>% 
      mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10))) %>% 
      group_by(fiscal_year) %>% 
      mutate(total=sum(value,na.rm=TRUE)) %>% 
      ungroup() %>%  
      mutate(share=value/total,
             month=month(date))
    
    monthly_shares_reg = lm_robust(share~factor(month),monthly_shares %>% group_by(fiscal_year) %>% filter(n()==12))
    
    fcast_df1 = read_csv(paste0("Data/Processing/imputed_data/imputed_data_asof",floor_date(Sys.Date(),"year")-1,".csv")) %>% 
      arrange(date) %>%
      mutate(year=year(date),
             month=month(date)) %>%
      select(-c(PCE,PRS85006112)) %>%
      select(-one_of("ADPMNUSNERSA")) %>% 
      left_join(mandatory %>% 
                  mutate(record_date=floor_date(record_date,"month"),
                         current_month_net_rcpt_amt=as.numeric(current_month_rcpt_outly_amt)/1000000000) %>% 
                  select(record_date,current_month_net_rcpt_amt) %>% 
                  rename(date=record_date,
                         value=current_month_net_rcpt_amt)) %>% # join the yvariable
      arrange(date) %>%
      left_join(national_econ %>% 
                  filter(series_id=="GDPC1") %>% 
                  select(date,GDPC1=value)) %>% 
      group_by(year,quarter(date)) %>% 
      mutate(GDPC1=GDPC1[1])  %>% 
      ungroup() %>% 
      select(-`quarter(date)`) %>% 
      mutate(GDPC1 = (GDPC1/dplyr::lag(GDPC1,3)-1)*100) %>% 
      rowwise() %>% 
      mutate(GDPC1=ifelse(floor_date(date,"quarter")%in%gdp_data$date&is.na(GDPC1),
                          tail(gdp_data$gdp[gdp_data$date==floor_date(date,"quarter")],1),
                          GDPC1)) %>% 
      ungroup() %>% 
      mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10),~((./dplyr::lag(.,1)-1)*100)) %>%
      mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,gt_1003:gt_999),~(.-dplyr::lag(.,1))) %>%
      mutate(lag1=dplyr::lag(value,1),
             lag2=dplyr::lag(value,2),
             lag3=dplyr::lag(value,3),
             lag4=dplyr::lag(value,4)) %>%
      ungroup() %>% 
      mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)))  %>% 
      left_join(cbo_proj %>% 
                  filter(subcategory=="Medicare") %>% 
                  group_by(projected_fiscal_year) %>% 
                  slice(n()) %>% 
                  select(projected_fiscal_year,value) %>% 
                  rename(cbo_proj=value,
                         fiscal_year=projected_fiscal_year))
    fcast_df1$cbo_proj_month = as.numeric(predict(monthly_shares_reg,data.frame(month=fcast_df1$month)))*fcast_df1$cbo_proj
    fcast_df1 = fcast_df1 %>% 
      mutate(cbo_proj_diff=(value/cbo_proj_month-1)*100) %>% 
      mutate(lag1_cbo_proj_diff=dplyr::lag(cbo_proj_diff,1),
             lag2_cbo_proj_diff=dplyr::lag(cbo_proj_diff,2))

    fcast_df2 = read_csv(paste0("Data/Processing/imputed_data/imputed_data_asof",Sys.Date(),".csv")) %>% 
      arrange(date) %>%
      mutate(year=year(date),
             month=month(date)) %>%
      select(-c(PCE,PRS85006112)) %>%
      select(-one_of("ADPMNUSNERSA")) %>% 
      left_join(mandatory %>% 
                  mutate(record_date=floor_date(record_date,"month"),
                         current_month_net_rcpt_amt=as.numeric(current_month_rcpt_outly_amt)/1000000000) %>% 
                  select(record_date,current_month_net_rcpt_amt) %>% 
                  rename(date=record_date,
                         value=current_month_net_rcpt_amt)) %>% # join the yvariable
      arrange(date) %>%
      left_join(national_econ %>% 
                  filter(series_id=="GDPC1") %>% 
                  select(date,GDPC1=value)) %>% 
      group_by(year,quarter(date)) %>% 
      mutate(GDPC1=GDPC1[1])  %>% 
      ungroup() %>% 
      select(-`quarter(date)`) %>% 
      mutate(GDPC1 = (GDPC1/dplyr::lag(GDPC1,3)-1)*100) %>% 
      rowwise() %>% 
      mutate(GDPC1=ifelse(floor_date(date,"quarter")%in%gdp_data$date&is.na(GDPC1),
                          tail(gdp_data$gdp[gdp_data$date==floor_date(date,"quarter")],1),
                          GDPC1)) %>% 
      ungroup() %>% 
      mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10),~((./dplyr::lag(.,1)-1)*100)) %>%
      mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,gt_1003:gt_999),~(.-dplyr::lag(.,1))) %>%
      mutate(lag1=dplyr::lag(value,1),
             lag2=dplyr::lag(value,2),
             lag3=dplyr::lag(value,3),
             lag4=dplyr::lag(value,4)) %>%
      ungroup() %>% 
      mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)))  %>% 
      left_join(cbo_proj %>% 
                  filter(subcategory=="Medicare") %>% 
                  group_by(projected_fiscal_year) %>% 
                  slice(n()) %>% 
                  select(projected_fiscal_year,value) %>% 
                  rename(cbo_proj=value,
                         fiscal_year=projected_fiscal_year))
    fcast_df2$cbo_proj_month = as.numeric(predict(monthly_shares_reg,data.frame(month=fcast_df2$month)))*fcast_df2$cbo_proj
    fcast_df2 = fcast_df2 %>% 
      mutate(cbo_proj_diff=(value/cbo_proj_month-1)*100) %>% 
      mutate(lag1_cbo_proj_diff=dplyr::lag(cbo_proj_diff,1),
             lag2_cbo_proj_diff=dplyr::lag(cbo_proj_diff,2))
    
  }
  
  if(cbo_category=="Medicaid"){
    mandatory = outlays %>% 
      filter(classification_desc=="Grants to States for Medicaid") %>% 
      select(record_date,current_month_net_outly_amt) %>% 
      mutate(current_month_rcpt_outly_amt=as.numeric(current_month_net_outly_amt)) %>% 
      select(-current_month_net_outly_amt)
    
    monthly_shares = mandatory %>% 
      mutate(record_date=floor_date(record_date,"month"),
             current_month_net_rcpt_amt=as.numeric(current_month_rcpt_outly_amt)/1000000000) %>% 
      select(record_date,current_month_net_rcpt_amt) %>% 
      rename(date=record_date,
             value=current_month_net_rcpt_amt) %>% 
      mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10))) %>% 
      group_by(fiscal_year) %>% 
      mutate(total=sum(value,na.rm=TRUE)) %>% 
      ungroup() %>%  
      mutate(share=value/total,
             month=month(date))
    
    monthly_shares_reg = lm_robust(share~factor(month),monthly_shares %>% group_by(fiscal_year) %>% filter(n()==12))
    
    fcast_df1 = read_csv(paste0("Data/Processing/imputed_data/imputed_data_asof",floor_date(Sys.Date(),"year")-1,".csv")) %>% 
      arrange(date) %>%
      mutate(year=year(date),
             month=month(date)) %>%
      select(-c(PCE,PRS85006112)) %>%
      select(-one_of("ADPMNUSNERSA")) %>% 
      left_join(mandatory %>% 
                  mutate(record_date=floor_date(record_date,"month"),
                         current_month_net_rcpt_amt=as.numeric(current_month_rcpt_outly_amt)/1000000000) %>% 
                  select(record_date,current_month_net_rcpt_amt) %>% 
                  rename(date=record_date,
                         value=current_month_net_rcpt_amt)) %>% # join the yvariable
      arrange(date) %>%
      left_join(national_econ %>% 
                  filter(series_id=="GDPC1") %>% 
                  select(date,GDPC1=value)) %>% 
      group_by(year,quarter(date)) %>% 
      mutate(GDPC1=GDPC1[1])  %>% 
      ungroup() %>% 
      select(-`quarter(date)`) %>% 
      mutate(GDPC1 = (GDPC1/dplyr::lag(GDPC1,3)-1)*100) %>% 
      rowwise() %>% 
      mutate(GDPC1=ifelse(floor_date(date,"quarter")%in%gdp_data$date&is.na(GDPC1),
                          tail(gdp_data$gdp[gdp_data$date==floor_date(date,"quarter")],1),
                          GDPC1)) %>% 
      ungroup() %>% 
      mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10),~((./dplyr::lag(.,1)-1)*100)) %>%
      mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,gt_1003:gt_999),~(.-dplyr::lag(.,1))) %>%
      mutate(lag1=dplyr::lag(value,1),
             lag2=dplyr::lag(value,2),
             lag3=dplyr::lag(value,3),
             lag4=dplyr::lag(value,4)) %>%
      ungroup() %>% 
      mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)))  %>% 
      left_join(cbo_proj %>% 
                  filter(subcategory=="Medicaid") %>% 
                  group_by(projected_fiscal_year) %>% 
                  slice(n()) %>% 
                  select(projected_fiscal_year,value) %>% 
                  rename(cbo_proj=value,
                         fiscal_year=projected_fiscal_year))
    fcast_df1$cbo_proj_month = as.numeric(predict(monthly_shares_reg,data.frame(month=fcast_df1$month)))*fcast_df1$cbo_proj
    fcast_df1 = fcast_df1 %>% 
      mutate(cbo_proj_diff=(value/cbo_proj_month-1)*100) %>% 
      mutate(lag1_cbo_proj_diff=dplyr::lag(cbo_proj_diff,1),
             lag2_cbo_proj_diff=dplyr::lag(cbo_proj_diff,2))
    
    fcast_df2 = read_csv(paste0("Data/Processing/imputed_data/imputed_data_asof",Sys.Date(),".csv")) %>% 
      arrange(date) %>%
      mutate(year=year(date),
             month=month(date)) %>%
      select(-c(PCE,PRS85006112)) %>%
      select(-one_of("ADPMNUSNERSA")) %>% 
      left_join(mandatory %>% 
                  mutate(record_date=floor_date(record_date,"month"),
                         current_month_net_rcpt_amt=as.numeric(current_month_rcpt_outly_amt)/1000000000) %>% 
                  select(record_date,current_month_net_rcpt_amt) %>% 
                  rename(date=record_date,
                         value=current_month_net_rcpt_amt)) %>% # join the yvariable
      arrange(date) %>%
      left_join(national_econ %>% 
                  filter(series_id=="GDPC1") %>% 
                  select(date,GDPC1=value)) %>% 
      group_by(year,quarter(date)) %>% 
      mutate(GDPC1=GDPC1[1])  %>% 
      ungroup() %>% 
      select(-`quarter(date)`) %>% 
      mutate(GDPC1 = (GDPC1/dplyr::lag(GDPC1,3)-1)*100) %>% 
      rowwise() %>% 
      mutate(GDPC1=ifelse(floor_date(date,"quarter")%in%gdp_data$date&is.na(GDPC1),
                          tail(gdp_data$gdp[gdp_data$date==floor_date(date,"quarter")],1),
                          GDPC1)) %>% 
      ungroup() %>% 
      mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10),~((./dplyr::lag(.,1)-1)*100)) %>%
      mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,gt_1003:gt_999),~(.-dplyr::lag(.,1))) %>%
      mutate(lag1=dplyr::lag(value,1),
             lag2=dplyr::lag(value,2),
             lag3=dplyr::lag(value,3),
             lag4=dplyr::lag(value,4)) %>%
      ungroup() %>% 
      mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)))  %>% 
      left_join(cbo_proj %>% 
                  filter(subcategory=="Medicaid") %>% 
                  group_by(projected_fiscal_year) %>% 
                  slice(n()) %>% 
                  select(projected_fiscal_year,value) %>% 
                  rename(cbo_proj=value,
                         fiscal_year=projected_fiscal_year))
    fcast_df2$cbo_proj_month = as.numeric(predict(monthly_shares_reg,data.frame(month=fcast_df2$month)))*fcast_df2$cbo_proj
    fcast_df2 = fcast_df2 %>% 
      mutate(cbo_proj_diff=(value/cbo_proj_month-1)*100) %>% 
      mutate(lag1_cbo_proj_diff=dplyr::lag(cbo_proj_diff,1),
             lag2_cbo_proj_diff=dplyr::lag(cbo_proj_diff,2))
    
  }
  
  if(cbo_category=="Social Security"){
    mandatory = spending_by_function %>% 
      filter(classification_desc=="Social Security") %>% 
      select(record_date,current_month_rcpt_outly_amt) %>% 
      mutate(current_month_rcpt_outly_amt=as.numeric(current_month_rcpt_outly_amt)*0.9941)
    
    monthly_shares = mandatory %>% 
      mutate(record_date=floor_date(record_date,"month"),
             current_month_net_rcpt_amt=as.numeric(current_month_rcpt_outly_amt)/1000000000) %>% 
      select(record_date,current_month_net_rcpt_amt) %>% 
      rename(date=record_date,
             value=current_month_net_rcpt_amt) %>% 
      mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10))) %>% 
      group_by(fiscal_year) %>% 
      mutate(total=sum(value,na.rm=TRUE)) %>% 
      ungroup() %>%  
      mutate(share=value/total,
             month=month(date))
    
    monthly_shares_reg = lm_robust(share~factor(month),monthly_shares %>% group_by(fiscal_year) %>% filter(n()==12))
    
    fcast_df1 = read_csv(paste0("Data/Processing/imputed_data/imputed_data_asof",floor_date(Sys.Date(),"year")-1,".csv")) %>% 
      arrange(date) %>%
      mutate(year=year(date),
             month=month(date)) %>%
      select(-c(PCE,PRS85006112)) %>%
      select(-one_of("ADPMNUSNERSA")) %>% 
      left_join(mandatory %>% 
                  mutate(record_date=floor_date(record_date,"month"),
                         current_month_net_rcpt_amt=as.numeric(current_month_rcpt_outly_amt)/1000000000) %>% 
                  select(record_date,current_month_net_rcpt_amt) %>% 
                  rename(date=record_date,
                         value=current_month_net_rcpt_amt)) %>% # join the yvariable
      arrange(date) %>%
      left_join(national_econ %>% 
                  filter(series_id=="GDPC1") %>% 
                  select(date,GDPC1=value)) %>% 
      group_by(year,quarter(date)) %>% 
      mutate(GDPC1=GDPC1[1])  %>% 
      ungroup() %>% 
      select(-`quarter(date)`) %>% 
      mutate(GDPC1 = (GDPC1/dplyr::lag(GDPC1,3)-1)*100) %>% 
      rowwise() %>% 
      mutate(GDPC1=ifelse(floor_date(date,"quarter")%in%gdp_data$date&is.na(GDPC1),
                          tail(gdp_data$gdp[gdp_data$date==floor_date(date,"quarter")],1),
                          GDPC1)) %>% 
      ungroup() %>% 
      mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10),~((./dplyr::lag(.,1)-1)*100)) %>%
      mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,gt_1003:gt_999),~(.-dplyr::lag(.,1))) %>%
      mutate(lag1=dplyr::lag(value,1),
             lag2=dplyr::lag(value,2),
             lag3=dplyr::lag(value,3),
             lag4=dplyr::lag(value,4)) %>%
      ungroup() %>% 
      mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)))  %>% 
      left_join(cbo_proj %>% 
                  filter(subcategory=="Social Security") %>% 
                  group_by(projected_fiscal_year) %>% 
                  slice(n()) %>% 
                  select(projected_fiscal_year,value) %>% 
                  rename(cbo_proj=value,
                         fiscal_year=projected_fiscal_year))
    fcast_df1$cbo_proj_month = as.numeric(predict(monthly_shares_reg,data.frame(month=fcast_df1$month)))*fcast_df1$cbo_proj
    fcast_df1 = fcast_df1 %>% 
      mutate(cbo_proj_diff=(value/cbo_proj_month-1)*100) %>% 
      mutate(lag1_cbo_proj_diff=dplyr::lag(cbo_proj_diff,1),
             lag2_cbo_proj_diff=dplyr::lag(cbo_proj_diff,2))
    
    fcast_df2 = read_csv(paste0("Data/Processing/imputed_data/imputed_data_asof",Sys.Date(),".csv")) %>% 
      arrange(date) %>%
      mutate(year=year(date),
             month=month(date)) %>%
      select(-c(PCE,PRS85006112)) %>%
      select(-one_of("ADPMNUSNERSA")) %>% 
      left_join(mandatory %>% 
                  mutate(record_date=floor_date(record_date,"month"),
                         current_month_net_rcpt_amt=as.numeric(current_month_rcpt_outly_amt)/1000000000) %>% 
                  select(record_date,current_month_net_rcpt_amt) %>% 
                  rename(date=record_date,
                         value=current_month_net_rcpt_amt)) %>% # join the yvariable
      arrange(date) %>%
      left_join(national_econ %>% 
                  filter(series_id=="GDPC1") %>% 
                  select(date,GDPC1=value)) %>% 
      group_by(year,quarter(date)) %>% 
      mutate(GDPC1=GDPC1[1])  %>% 
      ungroup() %>% 
      select(-`quarter(date)`) %>% 
      mutate(GDPC1 = (GDPC1/dplyr::lag(GDPC1,3)-1)*100) %>% 
      rowwise() %>% 
      mutate(GDPC1=ifelse(floor_date(date,"quarter")%in%gdp_data$date&is.na(GDPC1),
                          tail(gdp_data$gdp[gdp_data$date==floor_date(date,"quarter")],1),
                          GDPC1)) %>% 
      ungroup() %>% 
      mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10),~((./dplyr::lag(.,1)-1)*100)) %>%
      mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,gt_1003:gt_999),~(.-dplyr::lag(.,1))) %>%
      mutate(lag1=dplyr::lag(value,1),
             lag2=dplyr::lag(value,2),
             lag3=dplyr::lag(value,3),
             lag4=dplyr::lag(value,4)) %>%
      ungroup() %>% 
      mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)))  %>% 
      left_join(cbo_proj %>% 
                  filter(subcategory=="Social Security") %>% 
                  group_by(projected_fiscal_year) %>% 
                  slice(n()) %>% 
                  select(projected_fiscal_year,value) %>% 
                  rename(cbo_proj=value,
                         fiscal_year=projected_fiscal_year))
    fcast_df2$cbo_proj_month = as.numeric(predict(monthly_shares_reg,data.frame(month=fcast_df2$month)))*fcast_df2$cbo_proj
    fcast_df2 = fcast_df2 %>% 
      mutate(cbo_proj_diff=(value/cbo_proj_month-1)*100) %>% 
      mutate(lag1_cbo_proj_diff=dplyr::lag(cbo_proj_diff,1),
             lag2_cbo_proj_diff=dplyr::lag(cbo_proj_diff,2))
    
  }
  
  if(cbo_category=="National Defense"){
    mandatory = spending_by_function %>% 
      filter(classification_desc=="National Defense") %>% 
      select(record_date,current_month_rcpt_outly_amt) %>% 
      mutate(current_month_rcpt_outly_amt=as.numeric(current_month_rcpt_outly_amt))
    
    monthly_shares = mandatory %>% 
      mutate(record_date=floor_date(record_date,"month"),
             current_month_net_rcpt_amt=as.numeric(current_month_rcpt_outly_amt)/1000000000) %>% 
      select(record_date,current_month_net_rcpt_amt) %>% 
      rename(date=record_date,
             value=current_month_net_rcpt_amt) %>% 
      mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10))) %>% 
      group_by(fiscal_year) %>% 
      mutate(total=sum(value,na.rm=TRUE)) %>% 
      ungroup() %>%  
      mutate(share=value/total,
             month=month(date))
    
    monthly_shares_reg = lm_robust(share~factor(month),monthly_shares %>% group_by(fiscal_year) %>% filter(n()==12))
    
    fcast_df1 = read_csv(paste0("Data/Processing/imputed_data/imputed_data_asof",floor_date(Sys.Date(),"year")-1,".csv")) %>% 
      arrange(date) %>%
      mutate(year=year(date),
             month=month(date)) %>%
      select(-c(PCE,PRS85006112)) %>%
      select(-one_of("ADPMNUSNERSA")) %>% 
      left_join(mandatory %>% 
                  mutate(record_date=floor_date(record_date,"month"),
                         current_month_net_rcpt_amt=as.numeric(current_month_rcpt_outly_amt)/1000000000) %>% 
                  select(record_date,current_month_net_rcpt_amt) %>% 
                  rename(date=record_date,
                         value=current_month_net_rcpt_amt)) %>% # join the yvariable
      arrange(date) %>%
      left_join(national_econ %>% 
                  filter(series_id=="GDPC1") %>% 
                  select(date,GDPC1=value)) %>% 
      group_by(year,quarter(date)) %>% 
      mutate(GDPC1=GDPC1[1])  %>% 
      ungroup() %>% 
      select(-`quarter(date)`) %>% 
      mutate(GDPC1 = (GDPC1/dplyr::lag(GDPC1,3)-1)*100) %>% 
      rowwise() %>% 
      mutate(GDPC1=ifelse(floor_date(date,"quarter")%in%gdp_data$date&is.na(GDPC1),
                          tail(gdp_data$gdp[gdp_data$date==floor_date(date,"quarter")],1),
                          GDPC1)) %>% 
      ungroup() %>% 
      mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10),~((./dplyr::lag(.,1)-1)*100)) %>%
      mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,gt_1003:gt_999),~(.-dplyr::lag(.,1))) %>%
      mutate(lag1=dplyr::lag(value,1),
             lag2=dplyr::lag(value,2),
             lag3=dplyr::lag(value,3),
             lag4=dplyr::lag(value,4)) %>%
      ungroup() %>% 
      mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)))  %>% 
      left_join(cbo_proj %>% 
                  filter(subcategory=="Defense Discretionary") %>% 
                  group_by(projected_fiscal_year) %>% 
                  slice(n()) %>% 
                  select(projected_fiscal_year,value) %>% 
                  rename(cbo_proj=value,
                         fiscal_year=projected_fiscal_year))
    fcast_df1$cbo_proj_month = as.numeric(predict(monthly_shares_reg,data.frame(month=fcast_df1$month)))*fcast_df1$cbo_proj
    fcast_df1 = fcast_df1 %>% 
      mutate(cbo_proj_diff=(value/cbo_proj_month-1)*100) %>% 
      mutate(lag1_cbo_proj_diff=dplyr::lag(cbo_proj_diff,1),
             lag2_cbo_proj_diff=dplyr::lag(cbo_proj_diff,2))
    
    fcast_df2 = read_csv(paste0("Data/Processing/imputed_data/imputed_data_asof",Sys.Date(),".csv")) %>% 
      arrange(date) %>%
      mutate(year=year(date),
             month=month(date)) %>%
      select(-c(PCE,PRS85006112)) %>%
      select(-one_of("ADPMNUSNERSA")) %>% 
      left_join(mandatory %>% 
                  mutate(record_date=floor_date(record_date,"month"),
                         current_month_net_rcpt_amt=as.numeric(current_month_rcpt_outly_amt)/1000000000) %>% 
                  select(record_date,current_month_net_rcpt_amt) %>% 
                  rename(date=record_date,
                         value=current_month_net_rcpt_amt)) %>% # join the yvariable
      arrange(date) %>%
      left_join(national_econ %>% 
                  filter(series_id=="GDPC1") %>% 
                  select(date,GDPC1=value)) %>% 
      group_by(year,quarter(date)) %>% 
      mutate(GDPC1=GDPC1[1])  %>% 
      ungroup() %>% 
      select(-`quarter(date)`) %>% 
      mutate(GDPC1 = (GDPC1/dplyr::lag(GDPC1,3)-1)*100) %>% 
      rowwise() %>% 
      mutate(GDPC1=ifelse(floor_date(date,"quarter")%in%gdp_data$date&is.na(GDPC1),
                          tail(gdp_data$gdp[gdp_data$date==floor_date(date,"quarter")],1),
                          GDPC1)) %>% 
      ungroup() %>% 
      mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10),~((./dplyr::lag(.,1)-1)*100)) %>%
      mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,gt_1003:gt_999),~(.-dplyr::lag(.,1))) %>%
      mutate(lag1=dplyr::lag(value,1),
             lag2=dplyr::lag(value,2),
             lag3=dplyr::lag(value,3),
             lag4=dplyr::lag(value,4)) %>%
      ungroup() %>% 
      mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)))  %>% 
      left_join(cbo_proj %>% 
                  filter(subcategory=="Defense Discretionary") %>% 
                  group_by(projected_fiscal_year) %>% 
                  slice(n()) %>% 
                  select(projected_fiscal_year,value) %>% 
                  rename(cbo_proj=value,
                         fiscal_year=projected_fiscal_year))
    fcast_df2$cbo_proj_month = as.numeric(predict(monthly_shares_reg,data.frame(month=fcast_df2$month)))*fcast_df2$cbo_proj
    fcast_df2 = fcast_df2 %>% 
      mutate(cbo_proj_diff=(value/cbo_proj_month-1)*100) %>% 
      mutate(lag1_cbo_proj_diff=dplyr::lag(cbo_proj_diff,1),
             lag2_cbo_proj_diff=dplyr::lag(cbo_proj_diff,2))
  }
  
  if(cbo_category=="Net Interest"){
    mandatory = spending_by_function %>% 
      filter(classification_desc=="Net Interest") %>% 
      select(record_date,current_month_rcpt_outly_amt) %>% 
      mutate(current_month_rcpt_outly_amt=as.numeric(current_month_rcpt_outly_amt))
    
    monthly_shares = mandatory %>% 
      mutate(record_date=floor_date(record_date,"month"),
             current_month_net_rcpt_amt=as.numeric(current_month_rcpt_outly_amt)/1000000000) %>% 
      select(record_date,current_month_net_rcpt_amt) %>% 
      rename(date=record_date,
             value=current_month_net_rcpt_amt) %>% 
      mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10))) %>% 
      group_by(fiscal_year) %>% 
      mutate(total=sum(value,na.rm=TRUE)) %>% 
      ungroup() %>%  
      mutate(share=value/total,
             month=month(date))
    
    monthly_shares_reg = lm_robust(share~factor(month),monthly_shares %>% group_by(fiscal_year) %>% filter(n()==12))
    
    fcast_df1 = read_csv(paste0("Data/Processing/imputed_data/imputed_data_asof",floor_date(Sys.Date(),"year")-1,".csv")) %>% 
      arrange(date) %>%
      mutate(year=year(date),
             month=month(date)) %>%
      select(-c(PCE,PRS85006112)) %>%
      select(-one_of("ADPMNUSNERSA")) %>% 
      left_join(mandatory %>% 
                  mutate(record_date=floor_date(record_date,"month"),
                         current_month_net_rcpt_amt=as.numeric(current_month_rcpt_outly_amt)/1000000000) %>% 
                  select(record_date,current_month_net_rcpt_amt) %>% 
                  rename(date=record_date,
                         value=current_month_net_rcpt_amt)) %>% # join the yvariable
      arrange(date) %>%
      left_join(national_econ %>% 
                  filter(series_id=="GDPC1") %>% 
                  select(date,GDPC1=value)) %>% 
      group_by(year,quarter(date)) %>% 
      mutate(GDPC1=GDPC1[1])  %>% 
      ungroup() %>% 
      select(-`quarter(date)`) %>% 
      mutate(GDPC1 = (GDPC1/dplyr::lag(GDPC1,3)-1)*100) %>% 
      rowwise() %>% 
      mutate(GDPC1=ifelse(floor_date(date,"quarter")%in%gdp_data$date&is.na(GDPC1),
                          tail(gdp_data$gdp[gdp_data$date==floor_date(date,"quarter")],1),
                          GDPC1)) %>% 
      ungroup() %>% 
      mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10),~((./dplyr::lag(.,1)-1)*100)) %>%
      mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,gt_1003:gt_999),~(.-dplyr::lag(.,1))) %>%
      mutate(lag1=dplyr::lag(value,1),
             lag2=dplyr::lag(value,2),
             lag3=dplyr::lag(value,3),
             lag4=dplyr::lag(value,4)) %>%
      ungroup() %>% 
      mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)))  %>% 
      left_join(cbo_proj %>% 
                  filter(subcategory=="Net Interest") %>% 
                  group_by(projected_fiscal_year) %>% 
                  slice(n()) %>% 
                  select(projected_fiscal_year,value) %>% 
                  rename(cbo_proj=value,
                         fiscal_year=projected_fiscal_year))
    fcast_df1$cbo_proj_month = as.numeric(predict(monthly_shares_reg,data.frame(month=fcast_df1$month)))*fcast_df1$cbo_proj
    fcast_df1 = fcast_df1 %>% 
      mutate(cbo_proj_diff=(value/cbo_proj_month-1)*100) %>% 
      mutate(lag1_cbo_proj_diff=dplyr::lag(cbo_proj_diff,1),
             lag2_cbo_proj_diff=dplyr::lag(cbo_proj_diff,2))
    
    fcast_df2 = read_csv(paste0("Data/Processing/imputed_data/imputed_data_asof",Sys.Date(),".csv")) %>% 
      arrange(date) %>%
      mutate(year=year(date),
             month=month(date)) %>%
      select(-c(PCE,PRS85006112)) %>%
      select(-one_of("ADPMNUSNERSA")) %>% 
      left_join(mandatory %>% 
                  mutate(record_date=floor_date(record_date,"month"),
                         current_month_net_rcpt_amt=as.numeric(current_month_rcpt_outly_amt)/1000000000) %>% 
                  select(record_date,current_month_net_rcpt_amt) %>% 
                  rename(date=record_date,
                         value=current_month_net_rcpt_amt)) %>% # join the yvariable
      arrange(date) %>%
      left_join(national_econ %>% 
                  filter(series_id=="GDPC1") %>% 
                  select(date,GDPC1=value)) %>% 
      group_by(year,quarter(date)) %>% 
      mutate(GDPC1=GDPC1[1])  %>% 
      ungroup() %>% 
      select(-`quarter(date)`) %>% 
      mutate(GDPC1 = (GDPC1/dplyr::lag(GDPC1,3)-1)*100) %>% 
      rowwise() %>% 
      mutate(GDPC1=ifelse(floor_date(date,"quarter")%in%gdp_data$date&is.na(GDPC1),
                          tail(gdp_data$gdp[gdp_data$date==floor_date(date,"quarter")],1),
                          GDPC1)) %>% 
      ungroup() %>% 
      mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10),~((./dplyr::lag(.,1)-1)*100)) %>%
      mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,gt_1003:gt_999),~(.-dplyr::lag(.,1))) %>%
      mutate(lag1=dplyr::lag(value,1),
             lag2=dplyr::lag(value,2),
             lag3=dplyr::lag(value,3),
             lag4=dplyr::lag(value,4)) %>%
      ungroup() %>% 
      mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)))  %>% 
      left_join(cbo_proj %>% 
                  filter(subcategory=="Net Interest") %>% 
                  group_by(projected_fiscal_year) %>% 
                  slice(n()) %>% 
                  select(projected_fiscal_year,value) %>% 
                  rename(cbo_proj=value,
                         fiscal_year=projected_fiscal_year))
    fcast_df2$cbo_proj_month = as.numeric(predict(monthly_shares_reg,data.frame(month=fcast_df2$month)))*fcast_df2$cbo_proj
    fcast_df2 = fcast_df2 %>% 
      mutate(cbo_proj_diff=(value/cbo_proj_month-1)*100) %>% 
      mutate(lag1_cbo_proj_diff=dplyr::lag(cbo_proj_diff,1),
             lag2_cbo_proj_diff=dplyr::lag(cbo_proj_diff,2))
  }
  
  if(cbo_category=="Other Spending"){
    
    mandatory = bind_rows(
      # Health mandatory spending
      spending_by_function %>% 
        filter((classification_desc%in%c("Income Security","Health","Transportation","Community and Regional Development",
                                         "Education, Training, Employment, and Social Services",
                                         "Veterans Benefits and Services","Administration of Justice",
                                         "General Government","Undistributed Offsetting Receipts",
                                         "International Affairs","General Science, Space, and Technology",
                                         "Energy","Natural Resources and Environment","Agriculture",
                                         "Commerce and Housing Credit","Medicare","Social Security"))) %>% 
        select(record_date,classification_desc,current_month_rcpt_outly_amt) %>% 
        mutate(current_month_net_outly_amt=(as.numeric(current_month_rcpt_outly_amt)),
               current_month_net_outly_amt=ifelse(classification_desc=="Social Security",current_month_net_outly_amt*(1-0.9941),current_month_net_outly_amt),
               current_month_net_outly_amt=ifelse(classification_desc=="Medicare",current_month_net_outly_amt*(1-.988828),current_month_net_outly_amt)) %>% 
        select(-c(current_month_rcpt_outly_amt))) %>% 
      mutate(fiscal_year=as.integer(quarter(record_date, with_year = TRUE, fiscal_start = 10))) %>% 
      group_by(record_date) %>% 
      summarize(current_month_net_outly_amt=sum(current_month_net_outly_amt,na.rm=TRUE),
                fiscal_year=fiscal_year[n()]) %>% 
      left_join(outlays %>% 
                  filter(classification_desc=="Grants to States for Medicaid") %>% 
                  select(record_date,current_month_net_outly_amt) %>% 
                  mutate(medicaid=as.numeric(current_month_net_outly_amt)) %>% 
                  select(-current_month_net_outly_amt)) %>% 
      mutate(current_month_rcpt_outly_amt=current_month_net_outly_amt-medicaid) %>% 
      select(-c(current_month_net_outly_amt,medicaid))
    
    monthly_shares = mandatory %>% 
      mutate(record_date=floor_date(record_date,"month"),
             current_month_net_rcpt_amt=as.numeric(current_month_rcpt_outly_amt)/1000000000) %>% 
      select(record_date,current_month_net_rcpt_amt) %>% 
      rename(date=record_date,
             value=current_month_net_rcpt_amt) %>% 
      mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10))) %>% 
      group_by(fiscal_year) %>% 
      mutate(total=sum(value,na.rm=TRUE)) %>% 
      ungroup() %>%  
      mutate(share=value/total,
             month=month(date))
    
    monthly_shares_reg = lm_robust(share~factor(month),monthly_shares %>% group_by(fiscal_year) %>% filter(n()==12))
    
    fcast_df1 = read_csv(paste0("Data/Processing/imputed_data/imputed_data_asof",floor_date(Sys.Date(),"year")-1,".csv")) %>% 
      arrange(date) %>%
      mutate(year=year(date),
             month=month(date)) %>%
      select(-c(PCE,PRS85006112)) %>%
      select(-one_of("ADPMNUSNERSA")) %>% 
      left_join(mandatory %>% 
                  mutate(record_date=floor_date(record_date,"month"),
                         current_month_net_rcpt_amt=as.numeric(current_month_rcpt_outly_amt)/1000000000) %>% 
                  select(record_date,current_month_net_rcpt_amt) %>% 
                  rename(date=record_date,
                         value=current_month_net_rcpt_amt)) %>% # join the yvariable
      arrange(date) %>%
      left_join(national_econ %>% 
                  filter(series_id=="GDPC1") %>% 
                  select(date,GDPC1=value)) %>% 
      group_by(year,quarter(date)) %>% 
      mutate(GDPC1=GDPC1[1])  %>% 
      ungroup() %>% 
      select(-`quarter(date)`) %>% 
      mutate(GDPC1 = (GDPC1/dplyr::lag(GDPC1,3)-1)*100) %>% 
      rowwise() %>% 
      mutate(GDPC1=ifelse(floor_date(date,"quarter")%in%gdp_data$date&is.na(GDPC1),
                          tail(gdp_data$gdp[gdp_data$date==floor_date(date,"quarter")],1),
                          GDPC1)) %>% 
      ungroup() %>% 
      mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10),~((./dplyr::lag(.,1)-1)*100)) %>%
      mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,gt_1003:gt_999),~(.-dplyr::lag(.,1))) %>%
      mutate(lag1=dplyr::lag(value,1),
             lag2=dplyr::lag(value,2),
             lag3=dplyr::lag(value,3),
             lag4=dplyr::lag(value,4)) %>%
      ungroup() %>% 
      mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)))  %>% 
      left_join(cbo_proj %>% 
                  filter(subcategory%in%c("Nondefense Discretionary","Other Mandatory")) %>% 
                  group_by(projected_fiscal_year,subcategory) %>% 
                  slice(n()) %>% 
                  group_by(projected_fiscal_year) %>% 
                  summarize(value=sum(value,na.rm=TRUE)) %>% 
                  rename(cbo_proj=value,
                         fiscal_year=projected_fiscal_year))
    fcast_df1$cbo_proj_month = as.numeric(predict(monthly_shares_reg,data.frame(month=fcast_df1$month)))*fcast_df1$cbo_proj
    fcast_df1 = fcast_df1 %>% 
      mutate(cbo_proj_diff=(value/cbo_proj_month-1)*100) %>% 
      mutate(lag1_cbo_proj_diff=dplyr::lag(cbo_proj_diff,1),
             lag2_cbo_proj_diff=dplyr::lag(cbo_proj_diff,2))
    
    fcast_df1 = read_csv(paste0("Data/Processing/imputed_data/imputed_data_asof",floor_date(Sys.Date(),"year")-1,".csv")) %>% 
      arrange(date) %>%
      mutate(year=year(date),
             month=month(date)) %>%
      select(-c(PCE,PRS85006112)) %>%
      select(-one_of("ADPMNUSNERSA")) %>% 
      left_join(mandatory %>% 
                  mutate(record_date=floor_date(record_date,"month"),
                         current_month_net_rcpt_amt=as.numeric(current_month_rcpt_outly_amt)/1000000000) %>% 
                  select(record_date,current_month_net_rcpt_amt) %>% 
                  rename(date=record_date,
                         value=current_month_net_rcpt_amt)) %>% # join the yvariable
      arrange(date) %>%
      left_join(national_econ %>% 
                  filter(series_id=="GDPC1") %>% 
                  select(date,GDPC1=value)) %>% 
      group_by(year,quarter(date)) %>% 
      mutate(GDPC1=GDPC1[1])  %>% 
      ungroup() %>% 
      select(-`quarter(date)`) %>% 
      mutate(GDPC1 = (GDPC1/dplyr::lag(GDPC1,3)-1)*100) %>% 
      rowwise() %>% 
      mutate(GDPC1=ifelse(floor_date(date,"quarter")%in%gdp_data$date&is.na(GDPC1),
                          tail(gdp_data$gdp[gdp_data$date==floor_date(date,"quarter")],1),
                          GDPC1)) %>% 
      ungroup() %>% 
      mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10),~((./dplyr::lag(.,1)-1)*100)) %>%
      mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,gt_1003:gt_999),~(.-dplyr::lag(.,1))) %>%
      mutate(lag1=dplyr::lag(value,1),
             lag2=dplyr::lag(value,2),
             lag3=dplyr::lag(value,3),
             lag4=dplyr::lag(value,4)) %>%
      ungroup() %>% 
      mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)))  %>% 
      left_join(cbo_proj %>% 
                  filter(subcategory%in%c("Nondefense Discretionary","Other Mandatory")) %>% 
                  group_by(projected_fiscal_year,subcategory) %>% 
                  slice(n()) %>% 
                  group_by(projected_fiscal_year) %>% 
                  summarize(value=sum(value,na.rm=TRUE)) %>% 
                  rename(cbo_proj=value,
                         fiscal_year=projected_fiscal_year))
    fcast_df1$cbo_proj_month = as.numeric(predict(monthly_shares_reg,data.frame(month=fcast_df1$month)))*fcast_df1$cbo_proj
    fcast_df1 = fcast_df1 %>% 
      mutate(cbo_proj_diff=(value/cbo_proj_month-1)*100) %>% 
      mutate(lag1_cbo_proj_diff=dplyr::lag(cbo_proj_diff,1),
             lag2_cbo_proj_diff=dplyr::lag(cbo_proj_diff,2))
    
    fcast_df2 = read_csv(paste0("Data/Processing/imputed_data/imputed_data_asof",Sys.Date(),".csv")) %>% 
      arrange(date) %>%
      mutate(year=year(date),
             month=month(date)) %>%
      select(-c(PCE,PRS85006112)) %>%
      select(-one_of("ADPMNUSNERSA")) %>% 
      left_join(mandatory %>% 
                  mutate(record_date=floor_date(record_date,"month"),
                         current_month_net_rcpt_amt=as.numeric(current_month_rcpt_outly_amt)/1000000000) %>% 
                  select(record_date,current_month_net_rcpt_amt) %>% 
                  rename(date=record_date,
                         value=current_month_net_rcpt_amt)) %>% # join the yvariable
      arrange(date) %>%
      left_join(national_econ %>% 
                  filter(series_id=="GDPC1") %>% 
                  select(date,GDPC1=value)) %>% 
      group_by(year,quarter(date)) %>% 
      mutate(GDPC1=GDPC1[1])  %>% 
      ungroup() %>% 
      select(-`quarter(date)`) %>% 
      mutate(GDPC1 = (GDPC1/dplyr::lag(GDPC1,3)-1)*100) %>% 
      rowwise() %>% 
      mutate(GDPC1=ifelse(floor_date(date,"quarter")%in%gdp_data$date&is.na(GDPC1),
                          tail(gdp_data$gdp[gdp_data$date==floor_date(date,"quarter")],1),
                          GDPC1)) %>% 
      ungroup() %>% 
      mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10),~((./dplyr::lag(.,1)-1)*100)) %>%
      mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,gt_1003:gt_999),~(.-dplyr::lag(.,1))) %>%
      mutate(lag1=dplyr::lag(value,1),
             lag2=dplyr::lag(value,2),
             lag3=dplyr::lag(value,3),
             lag4=dplyr::lag(value,4)) %>%
      ungroup() %>% 
      mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)))  %>% 
      left_join(cbo_proj %>% 
                  filter(subcategory%in%c("Nondefense Discretionary","Other Mandatory")) %>% 
                  group_by(projected_fiscal_year,subcategory) %>% 
                  slice(n()) %>% 
                  group_by(projected_fiscal_year) %>% 
                  summarize(value=sum(value,na.rm=TRUE)) %>% 
                  rename(cbo_proj=value,
                         fiscal_year=projected_fiscal_year))
    fcast_df2$cbo_proj_month = as.numeric(predict(monthly_shares_reg,data.frame(month=fcast_df2$month)))*fcast_df2$cbo_proj
    fcast_df2 = fcast_df2 %>% 
      mutate(cbo_proj_diff=(value/cbo_proj_month-1)*100) %>% 
      mutate(lag1_cbo_proj_diff=dplyr::lag(cbo_proj_diff,1),
             lag2_cbo_proj_diff=dplyr::lag(cbo_proj_diff,2))
    
  }
  
  X = model.matrix(as.formula(paste0("value","~",paste(colnames(fcast_df1)[c(2:252)],collapse="+"))),
                   fcast_df1 %>% filter(date<"2024-01-01"&year(date)>=2006&!is.na(value)))[, -1]
  y = (fcast_df1 %>% filter(date<"2024-01-01"&year(date)>=2006&!is.na(value)))[['value']]
  
  weight = (1:nrow(X))/nrow(X)
  weight = ifelse(weight<.5,.5,weight)
  fit_lasso_state = glmnet(X, y, alpha = 1,pmax=20,weights = weight)
  # weight by how recent the data is
  
  selected_coefs_state = data.frame(varImp(fit_lasso_state,lambda=min(fit_lasso_state$lambda), scale = FALSE)) %>% filter(Overall!=0)
  selected_coefs_state$var = as.numeric(gsub("gt_","",rownames(selected_coefs_state)))
  coef_value_state = coef(fit_lasso_state,s=min(fit_lasso_state$lambda))[,1][-1]
  coef_value_state = coef_value_state[coef_value_state!=0]
  selected_coefs_state = cbind(selected_coefs_state,coef_value_state)
  selected_coefs_state$category = sapply(selected_coefs_state$var,which_category)
  selected_coefs_state = selected_coefs_state %>% arrange(-Overall)
  
  test = lm_robust(as.formula(paste0("value","~lag1+lag2+lag3+lag4+cbo_proj_month+GDPC1+",paste(c(rownames(selected_coefs_state)),collapse="+"))),
                   data = fcast_df1 %>% filter(date<='2024-01-01') %>% mutate(weight=(1:n())/n()))
  
  if(is.infinite(max(abs(which(is.na(tail(fcast_df2$value,5)))-6)))){
    next
  }else{
    for(dat in tail(fcast_df2$date,max(abs(which(is.na(tail(fcast_df2$value,5)))-6)))){
      
      fcast_df2$value[fcast_df2$date==dat] = predict(test,fcast_df2 %>% filter(date==dat))
      
      fcast_df2 = fcast_df2 %>% 
        mutate(lag1=dplyr::lag(value,1),
               lag2=dplyr::lag(value,2),
               lag3=dplyr::lag(value,3),
               lag4=dplyr::lag(value,4))
      
    }
  }
  
  
  pred_df = data.frame(
    date=fcast_df2[['date']],
    var=cbo_category,
    pred=predict(test,fcast_df2),
    actual=fcast_df2[['value']],
    cbo_proj=fcast_df2[['cbo_proj_month']]
  )
  
  return(list(
    'data'=fcast_df2,
    'reg'=test,
    'pred_df'=pred_df,
    'monthly_shares_reg'=monthly_shares_reg
  ))
  
}


#' 
#' forecast_component
#' \code{forecast_component}
#' 
#' 

forecast_component = function(nowcast_object,nowcast_total_object,daily_df,cbo_monthly_proj_col,component_abbrev,arima_mode='auto'){
  
  tst = nowcast_object[[3]] %>% 
    mutate(year=year(date),
           month=month(date)) %>% 
    select(date,year,month,pred,actual) %>% 
    left_join(nowcast_total_object[[3]] %>% select(date,total_actual=actual)) %>% 
    mutate(share=actual/total_actual,
           record_fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10))) %>% 
    left_join(daily_df %>% 
                select(record_fiscal_year,record_calendar_month,total_pred=pred,extrap_total) %>%
                group_by(record_fiscal_year,record_calendar_month) %>% 
                mutate(extrap_total=extrap_total[n()]) %>% 
                slice(1),
              by=c("record_fiscal_year"="record_fiscal_year","month"="record_calendar_month")) %>% 
    select(date,record_calendar_year=year,record_calendar_month=month,share,actual,total_actual,pred,total_pred,extrap_total) %>% 
    group_by(record_calendar_month) %>% 
    mutate(pred_share=mean(share,na.rm=TRUE)) %>% 
    ungroup() %>% 
    left_join(data.frame(date=cbo_monthly_proj %>% mutate(date=as.Date(paste0(year,"-",month,"-01"))) %>% select(date) %>% pull(),cbo_outlay=rowSums(cbo_monthly_proj %>% select(outlay_Medicaid:outlay_Other)),cbo_revenue=rowSums(cbo_monthly_proj %>% select(`revenue_Corporate Income Taxes`:`revenue_Payroll Taxes`)))) %>% 
    left_join(cbo_monthly_proj %>% mutate(date=as.Date(paste0(year,"-",month,"-01"))) %>% select(date,!!sym(cbo_monthly_proj_col))) %>% 
    mutate(ch_component=actual-!!sym(cbo_monthly_proj_col),
           ch_cbo=total_actual-!!sym(paste0("cbo_",strsplit(cbo_monthly_proj_col,"_")[[1]][1])))
  
  
  tst$actual[tst$date==max(nowcast_object[[3]]$date)] = tst$pred[tst$date==max(nowcast_object[[3]]$date)]+predict(lm_robust(ch_component~ch_cbo*factor(record_calendar_month)-1,
                                                                                                                            tst),tst %>% slice(n()) %>% mutate(ch_cbo=extrap_total-total_pred))
  
  # make this into a regression that factors in month and prediction
  reg_df_all = tst %>% 
    arrange(date) %>% 
    mutate(record_fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10))) %>% 
    ungroup() %>% 
    select(date,record_fiscal_year,actual,pred) %>% 
    mutate(error=(pred/actual-1)*100,
           error=ifelse(date==max(date),NA,error),
           avg_error=rollmedian(error,6,align="right",partial=TRUE),
           month=month(date)) %>% 
    full_join(cbo_monthly_proj %>% 
                select(year,month,cbo_monthly_proj_col),
              by=c("record_fiscal_year"="year","month"="month")) %>% 
    filter(!is.na(record_fiscal_year)) %>% 
    mutate(date=ifelse(month>=10,paste0(as.numeric(record_fiscal_year)-1,"-",month,"-01"),paste0(record_fiscal_year,"-",month,"-01")),
           date=as.Date(date)) %>%
    left_join(nowcast_object[[1]] %>% select(date,names(nowcast_object[[2]]$coefficients)[-c(1:6)])) %>% 
    arrange(date)
  
  reg_df = reg_df_all %>% 
    fill(colnames(reg_df_all)[6:ncol(reg_df_all)],.direction="down") %>% 
    select(-c(record_fiscal_year,pred,error,month)) %>% 
    rename(cbo_proj=!!sym(cbo_monthly_proj_col))
  
  y=reg_df %>% select(actual) %>% drop_na(actual) %>% pull()
  if(arima_mode[1]=='auto'){
    arima1 = auto.arima(y,
                        xreg= as.matrix(reg_df %>% drop_na(actual) %>% select(colnames(reg_df)[3:4])),
                        seasonal=TRUE,
                        biasadj=TRUE)
  }
  if(arima_mode[1]!='auto'){
    arima1 = Arima(y,order=arima_mode,
                   xreg= as.matrix(reg_df %>% drop_na(actual) %>% select(colnames(reg_df)[3:4])),
                   seasonal=TRUE,
                   biasadj=TRUE)
  }
  
  arima1 %>% 
    forecast(xreg=
               as.matrix(reg_df %>% filter(date>=max(nowcast_object[[3]]$date)&date<=max(nowcast_object[[3]]$date)%m+%years(2)) %>% 
                           select(colnames(reg_df)[3:4]))) %>% 
    autoplot()
  
  fcast = arima1 %>% 
    forecast(xreg=
               as.matrix(reg_df %>% filter(date>max(nowcast_object[[3]]$date)&date<=max(nowcast_object[[3]]$date)%m+%years(2)) %>% 
                           select(colnames(reg_df)[3:4])),
             level=c(30))
  
  forecast_data = bind_rows(reg_df %>%
                              filter(date<=max(nowcast_object[[3]]$date)) %>% 
                              select(date,actual),
                            data.frame(
                              date=reg_df %>% filter(date>max(nowcast_object[[3]]$date)&date<=max(nowcast_object[[3]]$date)%m+%years(2)) %>% select(date) %>% pull(),
                              mean=as.numeric(fcast$mean),
                              lower=as.numeric(fcast$lower[,1]),
                              upper=as.numeric(fcast$upper[,1])
                            )
  ) %>% 
    mutate(var=cbo_monthly_proj_col) %>% 
    filter(date>="2015-03-01")
  
  return(list(
    "forecast_data"=forecast_data,
    "tst"=tst)
  )
  
}

impute_function_mice = function(df,dat){
  
  conflicted::conflicts_prefer(dplyr::first)
  conflicted::conflicts_prefer(dplyr::between)
  conflicted::conflicts_prefer(dplyr::last)
  conflicted::conflicts_prefer(lubridate::year)
  conflicted::conflicts_prefer(lubridate::quarter)
  conflicted::conflicts_prefer(lubridate::month)
  conflicted::conflicts_prefer(lubridate::quarter)
  
  require(ranger)
  require(mlr)
  require(tuneRanger)
  require(miceRanger)
  require(parsnip)
  
  test_dineof=df
  
  all_df = data.frame()
  
  cols = colnames(test_dineof %>% select(-one_of("PRS85006112","A261RX1Q020SBEA","A261RX1Q020SBEA", "GDPC1","PCECC96","DGDSRX1Q020SBEA",
                                                 "PCDGCC96","PCNDGC96","PCESVC96","GPDIC1", "FPIC1",          
                                                 "PNFIC1","PRFIC1" ,"EXPGSC1" ,"IMPGSC1","GCEC1" ,         
                                                 "FGCEC1", "SLCEC1","W006RC1Q027SBEA", "A074RC1Q027SBEA", "W007RC1Q027SBEA" ,"B234RC1Q027SBEA" ,"B235RC1Q027SBEA", "B075RC1Q027SBEA",
                                                 "W780RC1Q027SBEA" ,"W009RC1Q027SBEA" ,"B094RC1Q027SBEA" ,"W053RC1Q027SBEA" ,"B1040C1Q027SBEA" ,"W011RC1Q027SBEA",
                                                 "W012RC1Q027SBEA" ,"B233RC1Q027SBEA" ,"B097RC1Q027SBEA" ,"FGEXPND"         ,"A957RC1Q027SBEA" ,"W014RC1Q027SBEA",
                                                 "W015RC1Q027SBEA" ,"B087RC1Q027SBEA" ,"FGSL"            ,"W017RC1Q027SBEA" ,"A091RC1Q027SBEA" ,"B096RC1Q027SBEA",
                                                 "B243RC1Q027SBEA" ,"W018RC1Q027SBEA" ,"W019RCQ027SBEA"  ,"AD02RC1Q027SBEA","year","qtr","date")))
  
  miceObj <- miceRanger(
    test_dineof %>% 
      select(cols) %>% 
      select_if(~!all(is.na(.))),
    valueSelector = "meanMatch",
    returnModels = TRUE
  )
  
  dataList <- completeData(miceObj)
  
  tmp = data.frame(date=test_dineof$date)
  for(col1 in cols){
    
    tmp[[col1]] = rowMeans(sapply(dataList,function(x) x[[col1]]))
    
    if(length(test_dineof$date[is.na(test_dineof[[col1]])])==0){next}
    
    test_dineof[[col1]][is.na(test_dineof[[col1]])] = tmp[[col1]][is.na(test_dineof[[col1]])] 
    
  }
  
  return(test_dineof)
  
}


impute_function_kalman = function(df,dat){
  
  require(imputeTS)
  
  test_dineof=df
  
  value = data.frame()
  cols = colnames(test_dineof %>% select(-one_of("PRS85006112","A261RX1Q020SBEA","A261RX1Q020SBEA", "GDPC1","PCECC96","DGDSRX1Q020SBEA",
                                                 "PCDGCC96","PCNDGC96","PCESVC96","GPDIC1", "FPIC1",          
                                                 "PNFIC1","PRFIC1" ,"EXPGSC1" ,"IMPGSC1","GCEC1" ,         
                                                 "FGCEC1", "SLCEC1","W006RC1Q027SBEA", "A074RC1Q027SBEA", "W007RC1Q027SBEA" ,"B234RC1Q027SBEA" ,"B235RC1Q027SBEA", "B075RC1Q027SBEA",
                                                 "W780RC1Q027SBEA" ,"W009RC1Q027SBEA" ,"B094RC1Q027SBEA" ,"W053RC1Q027SBEA" ,"B1040C1Q027SBEA" ,"W011RC1Q027SBEA",
                                                 "W012RC1Q027SBEA" ,"B233RC1Q027SBEA" ,"B097RC1Q027SBEA" ,"FGEXPND"         ,"A957RC1Q027SBEA" ,"W014RC1Q027SBEA",
                                                 "W015RC1Q027SBEA" ,"B087RC1Q027SBEA" ,"FGSL"            ,"W017RC1Q027SBEA" ,"A091RC1Q027SBEA" ,"B096RC1Q027SBEA",
                                                 "B243RC1Q027SBEA" ,"W018RC1Q027SBEA" ,"W019RCQ027SBEA"  ,"AD02RC1Q027SBEA","year","qtr","date")))
                  
  test =   na_kalman(test_dineof[,cols])
  tmp=sapply(test_dineof[,cols],function(x) which(is.na(x)))
  value1 = bind_rows(lapply(names(tmp[which(as.numeric(sapply(tmp,function(x) length(x)))>0)]),
                            function(x) data.frame(prediction_date = dat,
                                                   variable=x,
                                                   date=test_dineof[as.numeric(tmp[x][[1]]),"date"],
                                                   replacement=as.numeric(unlist(test[as.numeric(tmp[x][[1]]),x])))))
  
  for(i in 1:nrow(value1)){
    
    test_dineof[[value1$variable[i]]][test_dineof$date==value1$date[i]] = value1$replacement[i]
    
  }
  
  return(test_dineof)
  
}

get_imputed_data = function(dat,col,testing){
  
  fcast_df1 = read_csv(paste0("Data/Processing/imputed_data/imputed_data_asof",dat,".csv")) %>% 
    arrange(date) %>%
    mutate(year=year(date),
           qtr=quarter(date)) %>%
    select(-c(PCE,PRS85006112)) %>%
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
    mutate(IHLIDXUS=ifelse(is.nan(IHLIDXUS),0,IHLIDXUS)) # bring back when not testing
  
  if(testing){
    
    fcast_df1 = fcast_df1 %>% 
      select(-one_of("ADPMNUSNERSA","IHLIDXUS"))
    
  }
  
  return(fcast_df1)
}

fcast_gdp_ols = function(dat,col,testing=FALSE){
  
  set.seed(178)
  
  fcast_df1 = get_imputed_data(floor_date(as.Date(dat),"quarter")-1,col,testing)
  
  X = model.matrix(as.formula(paste0(col,"~",paste(colnames(fcast_df1 %>% select(PAYEMS:gt_999)),collapse="+"))),
                   fcast_df1 %>% filter(date<max(c(floor_date(as.Date(dat),"year") %m-% years(1),'2007-01-01'))&year(date)>=2006&!is.na(!!sym(col))))[, -1]
  y = (fcast_df1 %>% filter(date<max(c(floor_date(as.Date(dat),"year") %m-% years(1),'2007-01-01'))&year(date)>=2006&!is.na(!!sym(col))))[[col]]
  
  if(length(y)<4){next}
  
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
                   data = fcast_df1 %>% filter(date<=max(c(floor_date(as.Date(dat),"year") %m-% years(1),'2007-01-01'))))
  
  fcast_df1 = get_imputed_data(dat,col,testing)
  
  dates = tail(fcast_df1,10) %>% filter(is.na(!!sym(col))) %>% pull(date)
  
  i=2
  if(length(dates)>1){
    
    fcast_df1$lag1[fcast_df1$date==dates[i]] = predict(test,fcast_df1 %>% filter(date==dates[i-1]))
    
  }
  
  gdp_pred_df = data.frame(
    prediction_date=dat,
    date=dates,
    var=col,
    pred=predict(test,fcast_df1 %>% filter(date%in%dates))
  ) %>% 
    left_join(national_econ %>% filter(series_id==col) %>% select(date,value) %>% mutate(value=(value/dplyr::lag(value,1)-1)*100))
  
  # Create an explainer
  explainer <- DALEX::explain(test, 
                       data = fcast_df1 %>% filter(date<dates[1]&!is.na(!!sym(col))&!is.na(lag2)) %>% select(names(test$coefficients)[-1]), 
                       y = (fcast_df1 %>% filter(date<dates[1]&!is.na(!!sym(col))&!is.na(lag2)))[[col]])
  
  tmp = lapply(dates,function(x) predict_parts(explainer, new_observation = fcast_df1 %>% filter(date%in%x), type = "break_down") %>% mutate(date=x,prediction_date=dat,var=col))
  breakdown = bind_rows(tmp)
  
  return(list(gdp_pred_df,explainer,breakdown))
  
}

