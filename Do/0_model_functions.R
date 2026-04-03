is_bad = function(x){
  
  return(is.na(x)|is.nan(x)|is.infinite(x))
  
}


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

make_df = function(end_date,bad_vars,national_econ,most_recent=TRUE){
  df = national_econ %>%
    filter(date<=end_date) %>%
    mutate(value=ifelse(release_date>end_date,NA,value)) %>%
    pivot_wider(id_cols=c('date'),names_from='series_id',values_from='value') %>%
    mutate(year=year(date),
           qtr=quarter(date)) %>%
    relocate(date,.before=1) %>%
    # other state variables
    full_join(make_state_trends(end_date,bad_vars,most_recent) %>%
                filter(!(category%in%bad_vars$category)) %>%
                group_by(category) %>%
                complete(date = full_seq(c(date,as.Date(end_date)), 1)) %>%
                fill(value:release_date,.direction="down") %>%
                mutate(date=date+6,
                       series_id=paste0("gt_",category),
                       month=month(date),
                       year=year(date)) %>%
                group_by(year,month,series_id) %>%
                summarize(deviation=mean(deviation,na.rm=TRUE)) %>%
                mutate(date=as.Date(paste0(year,"-",month,"-","01"),format="%Y-%m-%d")) %>%
                filter(date<=end_date) %>% 
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
#'  @param repeats number of times the prediction is iterated. > 1 and predicted values will be used a second time to inform imputation.
#'  @param exclude_google_var drop specific variables from imputation method
#'  
#'  @return imputed data frame


impute_function_old = function(df,dat){
  
  set.seed(178)
  
  test_dineof=df
  
  while(!is.infinite(min(tail(test_dineof,10) %>% filter(if_any(everything(), is.na)) %>% pull(date)))){
    predict_date = min(tail(test_dineof,10) %>% filter(if_any(everything(), is.na)) %>% pull(date))
    flag = 0
    while(flag<3){
    
      for(col1 in colnames(test_dineof)[c(2:ncol(test_dineof))]){
        
        if(col1==tail(colnames(test_dineof),1)){
          flag = flag+1
        }
        if(!is.na(df[df$date==predict_date,col1])){next} # if already have value, dont need to project
        
        system(sprintf('echo "\n%s\n"', paste0(as.character(predict_date)," ",col1, collapse="")))
        
        if(!(col1%in%colnames(test_dineof))){next}
        if(col1%in%c("ADPMNUSNERSA")&as.Date(dat)<"2010-01-01"){next}
        if(col1=="IHLIDXUS"&as.Date(dat)<"2021-01-01"){next}
        
        value = data.frame(date=test_dineof$date)
        for(i in 1:30){
          if("IHLIDXUS"%in%colnames(test_dineof)&"ADPMNUSNERSA"%in%colnames(test_dineof)){
            if(col1=="IHLIDXUS"){potential_cols = colnames(test_dineof %>% filter(date==predict_date) %>% select(-c(col1,IHLIDXUS,ADPMNUSNERSA,grep("gt_",colnames(test_dineof),value=TRUE))) %>% filter(date==max(date)) %>% select(-date) %>% select_if(!is.na(.)))}else{
              potential_cols = colnames(test_dineof %>% filter(date==predict_date) %>% select(-c(col1,IHLIDXUS,ADPMNUSNERSA,grep("gt_",colnames(test_dineof),value=TRUE))) %>% select(-date) %>% select_if(!is.na(.)))
            }
          } else{
            potential_cols = colnames(test_dineof %>% filter(date==predict_date) %>% select(-c(col1,grep("gt_",colnames(test_dineof),value=TRUE))) %>% select(-one_of("ADPMNUSNERSA","IHLIDXUS")) %>% select(-date) %>% select_if(!is.na(.)))
          }
          cols = c(sample(potential_cols,min(c(15,floor(length(potential_cols)/2)))),sample(colnames(test_dineof %>% select(grep("gt_",colnames(test_dineof),value=TRUE))),15))
          test = lm_robust(as.formula(paste0(paste0(col1,"~lag+lag2+"),paste(cols,collapse="+"))),
                           data=test_dineof %>% filter(date<predict_date) %>% select(col1,cols) %>% 
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
          
          value=suppressMessages(bind_cols(value,imp))
        }
        
        value1 = data.frame(
          date=value$date,
          replacement=rowMeans(value[,2:ncol(value)],na.rm=TRUE)
        ) %>% 
          filter(date<=predict_date)
        
        for(i in 1:nrow(value1)){
          
          if(!is.na(value1$replacement[i]&is.na(df[i,col1]))){
            test_dineof[i,col1]  = value1$replacement[i]
          }
        }
        
      }
    }
  }
  
  return(test_dineof)
  
}

impute_function_new = function(df,dat,repeats,sample_vars=FALSE,exclude_google_var="deviation_perc"){
  
  set.seed(178)
  
  test_dineof=df
  
  gt_cor_df = df
  
  flag = 0
  while(flag<repeats){ # iterate over three times
    for(col1 in colnames(df)[c(2:ncol(df))]){
      
      # system(sprintf('echo "\n%s\n"', paste0(as.character(dat)," ",col1, collapse="")))
      
      test_dineof[[col1]] = df[[col1]]
      
      gt_vars = cor(gt_cor_df[[col1]],gt_cor_df %>% select(starts_with("gt_")),use="complete.obs")
      xvars = grep(paste(c("date",exclude_google_var,paste(setdiff(colnames(gt_cor_df %>% select(starts_with("gt_"))),colnames(gt_cor_df %>% select(starts_with("gt")))[order(abs(gt_vars))[1:10]]),collapse="|")),collapse="|"),colnames(gt_cor_df),value=TRUE,invert = TRUE)
      
      # TODO: figure out way to incoporate ADP and Indeed into regression framework
      gt_cor_df1 = gt_cor_df
      if(dat<"2011-01-01"){
        xvars = grep("ADPMNUSNERSA",xvars,value=TRUE,invert = TRUE)
        if(col1!="ADPMNUSNERSA"){
          gt_cor_df1 = gt_cor_df1 %>% 
            select(-any_of("ADPMNUSNERSA"))
        }
      }else{
        
        adp_reg = lm_robust(ADPMNUSNERSA~PAYEMS,test_dineof)
        test_dineof$ADPMNUSNERSA[test_dineof$date<"2011-01-01"] = bind_cols(test_dineof %>% select(date,ADPMNUSNERSA),
                                                                            pred=predict(adp_reg,test_dineof)) %>% 
          mutate(ADPMNUSNERSA=coalesce(ADPMNUSNERSA,pred)) %>% 
          filter(date<"2011-01-01") %>% 
          pull(ADPMNUSNERSA)
        
      }
      
      if(dat<"2021-01-01"){
        xvars = grep("IHLIDXUS",xvars,value=TRUE,invert = TRUE)
        if(col1!="IHLIDXUS"){
          gt_cor_df1 = gt_cor_df1 %>% 
            select(-any_of("IHLIDXUS"))
        }
      }else{
        
        indeed_reg = lm_robust(IHLIDXUS~JTSJOL,test_dineof)
        test_dineof$IHLIDXUS[test_dineof$date<"2021-01-01"] = bind_cols(test_dineof %>% select(date,IHLIDXUS),
                                                                        pred=predict(indeed_reg,test_dineof)) %>% 
          mutate(IHLIDXUS=coalesce(IHLIDXUS,pred)) %>% 
          filter(date<"2021-01-01") %>% 
          pull(IHLIDXUS)
        
      }
      xvars = sample(xvars,floor(min(length(xvars),max(15,nrow(gt_cor_df1 %>% select(col1,xvars) %>% drop_na())/2))))
      
      if(length(which(is.na(df[c((nrow(df)-10):nrow(df)),col1])))==0){ next }
      if(!(col1%in%colnames(df))){next}
      
      value = data.frame(date=test_dineof$date)
      
      if(col1%in%c("ADPMNUSNERSA")&as.Date(dat)<"2011-01-01"){
        
        value1 = data.frame(
          date=value$date,
          replacement=mean(tail(test_dineof$ADPMNUSNERSA[!is.na(test_dineof$ADPMNUSNERSA)],3))
        )
        
        for(i in tail(1:nrow(value),12)){
          
          if(is.na(tmp[i,col1])&tmp$date[i]>="2020-01-01"){
            test_dineof[i,col1] = value1[i,"replacement"]
          }
        }
        
        next
        
      }
      if(col1=="IHLIDXUS"&as.Date(dat)<"2021-01-01"){
        
        value1 = data.frame(
          date=value$date,
          replacement=mean(tail(test_dineof$IHLIDXUS[!is.na(test_dineof$IHLIDXUS)],3))
        )
        
        for(i in tail(1:nrow(value),12)){
          
          if(is.na(tmp[i,col1])&tmp$date[i]>="2020-01-01"){
            test_dineof[i,col1] = value1[i,"replacement"]
          }
        }
        
        next
        
      }
      
      test_dineof = test_dineof %>% 
        mutate_at(vars(!!col1),list(lag1=~dplyr::lag(.,1),
                                    lag12=~dplyr::lag(.,12)))
      
      for(i in 1:30){
        
        tmp = test_dineof
        
        pred_df = data.frame()
        for(j in which(is.na(tail(tmp[[col1]],12)))){
          
          xvars1 = colnames(tmp %>% filter(date==tail(tmp$date,12)[j]) %>% select(xvars) %>% select_if(!is.na(.)))
          if(sample_vars){
            xvars1 = sample(xvars1,min(c(length(xvars1),10)))
          }
          
          reg1 = lm_robust(as.formula(paste0(col1,"~",paste(c("lag1","lag12",xvars1),collapse="+"))),
                           tmp)
          
          tmp1 = tmp %>% 
            filter(date==tail(tmp$date,12)[j]) %>% 
            mutate(var=predict(reg1,.)) %>% 
            select(date,var) %>% 
            mutate(test_date=dat,
                   name=col1)
          
          if(i<12) tmp$lag1[tmp$date==tail(tmp$date,12)[j+1]] = tmp1$var
          
          pred_df = bind_rows(pred_df,
                              tmp1
          )
          
        }
        
        imp <- tmp %>% 
          select(date,!!col1) %>% 
          left_join(pred_df %>% 
                      select(date,var),by="date") %>% 
          mutate("{col1}":=coalesce(!!sym(col1),var)) %>% 
          pull(!!col1)
        
        value=suppressMessages(bind_cols(value,imp))
      }
      
      value1 = data.frame(
        date=value$date,
        replacement=rowMeans(value[,2:ncol(value)],na.rm=TRUE)
      )
      
      for(i in tail(1:nrow(value),12)){
        
        if(is.na(tmp[i,col1])){
          test_dineof[i,col1] = value1[i,"replacement"]
        }
      }
    }
    if(col1==tail(colnames(df),1)){
      flag = flag+1
    }
  }
  
  return(test_dineof)
  
}

impute_function = function(df,dat,repeats,sample_vars=TRUE,exclude_google_var="deviation_perc"){
  
  
  old = impute_function_old(df,dat)
  
  new = impute_function_new(df,dat,repeats,sample_vars,exclude_google_var)
  
  final_df = bind_cols(
    old %>% select(-any_of(c("CPILFESL","TOTBUSIMNSA","UNRATE","WHLSLRIMSA","ADPMNUSNERSA","IHLIDXUS"))),
    new %>% select(any_of(c("CPILFESL","TOTBUSIMNSA","UNRATE","WHLSLRIMSA","ADPMNUSNERSA","IHLIDXUS")))
  ) %>% 
    relocate(colnames(df))
  
  return(final_df)
  
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
    select(-any_of(paste0("gt_",bad_vars$category))) %>% 
    arrange(date) %>%
    mutate(year=year(date),
           month=month(date)) %>%
    select(-c(PCE,PRS85006112)) %>%
    select(-one_of("ADPMNUSNERSA")) %>% 
    left_join(dataset %>% 
                select(date,value)) %>% # join the yvariable
    arrange(date) %>%
    # left_join(national_econ %>% 
    #             filter(series_id=="GDPC1") %>% 
    #             select(date,GDPC1=value)) %>% 
    # group_by(year,quarter(date)) %>% 
    # mutate(GDPC1=GDPC1[1])  %>% 
    # ungroup() %>% 
    # select(-`quarter(date)`) %>% 
    # mutate(GDPC1 = (GDPC1/dplyr::lag(GDPC1,3)-1)*100) %>% 
    # rowwise() %>% 
    # mutate(GDPC1=ifelse(floor_date(date,"quarter")%in%gdp_data$date&is.na(GDPC1),
    #                     tail(gdp_data$gdp[gdp_data$date==floor_date(date,"quarter")],1),
    #                     GDPC1)) %>% 
    # ungroup() %>% 
    mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10),~((./dplyr::lag(.,1)-1)*100)) %>%
    mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,grep("gt_",colnames(.),value=TRUE)),~(.-dplyr::lag(.,1))) %>%
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

nowcast_headline = function(dataset,end_date,cbo_category){
  
  model_headline = readRDS(paste0("Data/Processing/Models/nowcast_headline_",cbo_category,".RDS"))
  
  x_data = read_csv(paste0("Data/Processing/imputed_data/imputed_data_asof",end_date,".csv"))  %>% 
    select(-any_of(paste0("gt_",bad_vars$category))) %>% 
    arrange(date) %>%
    ungroup() %>% 
    mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10),.funs=list(ch12m=~((./dplyr::lag(.,12)-1)*100),ch1m=~((./dplyr::lag(.,1)-1)*100))) %>%
    mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,grep("gt_",colnames(.),value=TRUE)),.funs=list(ch12m=~.-dplyr::lag(.,12),ch1m=~.-dplyr::lag(.,1))) %>%
    mutate_at(vars(PAYEMS:gt_999_ch1m),.funs=list(lag1=~dplyr::lag(.,1),lag2=~dplyr::lag(.,2),lag3=~dplyr::lag(.,3),lag4=~dplyr::lag(.,4))) %>% 
    mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)))  %>% 
    left_join(cbo_proj %>% 
                filter(component==cbo_category&category=="Total") %>% 
                group_by(projected_fiscal_year) %>% 
                slice(n()) %>% 
                select(projected_fiscal_year,value) %>% 
                rename(cbo_proj=value,
                       fiscal_year=projected_fiscal_year)) %>% 
    mutate(month=month(date)) %>% 
    left_join(dataset %>% 
                select(date,value)) # join the yvariable
  x_data$cbo_proj_month = as.numeric(predict(model_headline$monthly_shares_reg,x_data))*x_data$cbo_proj
  x_data = x_data %>% 
    mutate(cbo_proj_diff=(value/cbo_proj_month-1)*100) %>% 
    mutate(lag1_cbo_proj_diff=dplyr::lag(cbo_proj_diff,1),
           lag2_cbo_proj_diff=dplyr::lag(cbo_proj_diff,2)) %>% 
    mutate(lag1=dplyr::lag(value,1),
           lag2=dplyr::lag(value,2),
           lag3=dplyr::lag(value,3),
           lag4=dplyr::lag(value,4),
           actual=value)
  
  for(dat in tail(x_data,10) %>% filter(is.na(value)) %>% pull(date)){
    
    x_data$value[x_data$date==dat] = predict(test,x_data %>% filter(date==dat)) 
    
    x_data = x_data %>% 
      mutate(lag1=dplyr::lag(value,1),
             lag2=dplyr::lag(value,2),
             lag3=dplyr::lag(value,3),
             lag4=dplyr::lag(value,4))
    
  }
  
  pred_df = data.frame(
    date=x_data[['date']],
    var=cbo_category,
    pred=predict(model_headline$model,x_data),
    actual=x_data[['actual']],
    cbo_proj=x_data[['cbo_proj_month']]
  )
  
  return(list(
    'data'=x_data,
    'reg'=model_headline$model,
    'pred_df'=pred_df,
    'monthly_shares_reg'=model_headline$monthly_shares_reg
  ))
  
}


nowcast_daily_budget_receipt = function(dts,mts_dataset,end_date,col,col_mts,testing=NA){
  
  models_daily = readRDS(paste0("Data/Processing/Models/nowcast_daily_",col,".RDS"))
  models_monthly = readRDS(paste0("Data/Processing/Models/nowcast_",col,".RDS"))
  
  overlays = overlay_df %>% 
    filter(date_active<=end_date&(is.na(date_inactive)|date_inactive<end_date)&category==col)
  
  overlays_daily = overlay_daily_df %>% 
    filter(date_active<=end_date&(is.na(date_inactive)|date_inactive<end_date)&category==col)
  
  # get monthly predicted share at outset to avoid copying code over and over
  monthly_share_pred = data.frame(date=seq.Date(floor_date(min(dts$record_date),"month"),
                                                as.Date(paste0(max(cbo_proj$projected_fiscal_year[cbo_proj$baseline_date<=end_date]),"-09-01")),
                                                by="month")) %>% 
    mutate(month=month(date),
           fy_month=case_when(
             month%in%c(10:12)~month-9,
             month%in%c(1:9)~month+3
           ),
           fed_remittances_suspended=ifelse(date>="2022-09-01",1,0)) %>%  # keep this activated unless they go back to a low interest environment, but given the path of interest payments, unlikely to ever happen
    mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)),
           tax_due=case_when(
             !(fiscal_year%in%c(2020,2021))&month==4&col=="Individual Income Taxes"~1,
             fiscal_year==2020&month==7&col=="Individual Income Taxes"~1,
             fiscal_year==2021&month==5&col=="Individual Income Taxes"~1,
             !(fiscal_year%in%c(2020))&month==4&col=="Corporate Income Taxes"~1,
             fiscal_year==2020&month==7&col=="Corporate Income Taxes"~1,
             fiscal_year==2020&month==9&col=="Excise Taxes"~1,
             TRUE~0
           ),
           quarter_end=case_when(col=="Corporate Income Taxes"&month%in%c(12,4,6,9)~1,
                                 col=="Individual Income Taxes"&month%in%c(1,4,6,9)~1,
                                 !grepl("Income",col)&month%in%c(1,4,6,9)~1,
                                 TRUE~0)) %>% 
    rowwise() %>% 
    mutate(first_day_thismonth_weekend=(weekdays(date,abbreviate=TRUE)%in%c("Sat","Sun")|date%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=1,
           first_day_nextmonth_weekend=(weekdays(date %m+% months(1),abbreviate=TRUE)%in%c("Sat","Sun")|(date %m+% months(1))%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12,
           last_day_thismonth_weekend=(weekdays((date %m+% months(1) )- 1,abbreviate=TRUE)%in%c("Sat","Sun")|((date %m+% months(1) )- 1)%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12) %>% 
    ungroup() %>% 
    {if(col%in%c("Individual Income Taxes","Corporate Income Taxes")) bind_cols(.,data.frame(predict(models_monthly$share,data=.,type="quantiles",quantiles=c(0.5,.1,.9))) %>% 
                                                                                  rename(pred_cumshare=1,pred_cumshare_lwr=2,pred_cumshare_upper=3)) %>%  
        group_by(fiscal_year) %>% mutate_at(vars(pred_cumshare_lwr:pred_cumshare_upper),~(.-pred_cumshare)) %>% 
        arrange(date) %>% mutate_at(vars(pred_cumshare),~cumsum(.)) else bind_cols(.,predict(models_monthly$share,.,interval="confidence",alpha=0.1) %>% 
                                                                                     as.data.frame() %>% 
                                                                                     mutate_all(~ifelse(is.nan(.),1,.)) %>% 
                                                                                     rename(pred_cumshare=1,pred_cumshare_lwr=2,pred_cumshare_upper=3)) %>% 
        group_by(fiscal_year) %>% 
        arrange(date) %>% 
        mutate_at(vars(pred_cumshare:pred_cumshare_upper),list(diff=~ifelse(fy_month==1,.,.-dplyr::lag(.,1)))) %>%
        rowwise() %>% 
        mutate(pred_cumshare_lwr=min(c(pred_cumshare_lwr_diff-pred_cumshare_diff,pred_cumshare_upper_diff-pred_cumshare_diff)),
               pred_cumshare_upper=max(c(pred_cumshare_lwr_diff-pred_cumshare_diff,pred_cumshare_upper_diff-pred_cumshare_diff))) %>% 
        ungroup() %>% 
        select(-c(pred_cumshare_diff:pred_cumshare_upper_diff))} %>% 
    group_by(fiscal_year) %>% 
    mutate(pred_cumshare=pred_cumshare/pred_cumshare[n()]) %>% 
    ungroup()
  
  monthly_df = get_monthly_shares_df_revenue(receipts,col_mts,"revenue",col) %>% 
    arrange(date) %>% 
    mutate(actual=value)
  
  MAX_DATE = ifelse(!is.na(testing),testing,as.character(max(dts$record_date,floor_date(end_date,"month"))))
  
  monthly_df = monthly_df %>% 
    mutate_at(vars(value,total,share,cum_total,cum_share),~ifelse(date>=floor_date(as.Date(MAX_DATE),"month"),NA,.)) %>% 
    mutate(actual=value) 
  
  tmp = data.frame(date=seq.Date(min(monthly_share_pred$date),min(monthly_df$date)-1,by="1 month")) %>% 
    mutate(month=month(date),
           fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)),
           fy_month=case_when(
             month%in%c(10:12)~month-9,
             month%in%c(1:9)~month+3
           ),
           quarter_end=case_when(col=="Corporate Income Taxes"&month%in%c(12,4,6,9)~1,
                                 col=="Individual Income Taxes"&month%in%c(1,4,6,9)~1,
                                 !grepl("Income",col)&month%in%c(1,4,6,9)~1,
                                 TRUE~0),
           tax_due=case_when(
             !(fiscal_year%in%c(2020,2021))&month==4&col=="Individual Income Taxes"~1,
             fiscal_year==2020&month==7&col=="Individual Income Taxes"~1,
             fiscal_year==2021&month==5&col=="Individual Income Taxes"~1,
             !(fiscal_year%in%c(2020))&month==4&col=="Corporate Income Taxes"~1,
             fiscal_year==2020&month==7&col=="Corporate Income Taxes"~1,
             fiscal_year==2020&month==9&col=="Excise Taxes"~1,
             TRUE~0
           ),
           fed_remittances_suspended=ifelse(date>="2022-09-01",1,0)) %>% 
    left_join(cbo_actual %>% 
                filter(category==col) %>% 
                ungroup() %>% 
                select(fiscal_year,value=actual_value)) %>% 
    group_by(fiscal_year) %>% 
    {if(col%in%c("Individual Income Taxes","Corporate Income Taxes")) bind_cols(.,data.frame(predict(models_monthly$share,data=.,type="quantiles",quantiles=c(0.5))) %>% 
                                                                                  rename(cbo_proj=1)) %>%  
        arrange(date) %>% mutate_at(vars(cbo_proj),~cumsum(.)) else bind_cols(.,predict(models_monthly$share,.) %>% 
                                                                                as.data.frame() %>% 
                                                                                mutate_all(~ifelse(is.nan(.),1,.)) %>% 
                                                                                rename(cbo_proj=1))} %>% 
    group_by(fiscal_year) %>% 
    mutate(cbo_proj=ifelse(fy_month==1,cbo_proj,cbo_proj-dplyr::lag(cbo_proj,1)),
           tmp=cbo_proj/sum(cbo_proj),
           num=n(),
           cbo_proj=ifelse(num==12,tmp,cbo_proj)) %>% 
    select(-c(tmp,num)) %>% 
    mutate(total=value,
           value=cbo_proj*total,
           share=cbo_proj,
           actual=value,
           error=1,
           error_ly=1,
           cum_total=cumsum(value),
           cum_share=cumsum(share)) %>% 
    select(-cbo_proj) %>% 
    left_join(cbo_proj %>% 
                {if(col=="Other Spending") filter(.,subcategory %in% c("Nondefense Discretionary","Other Mandatory")) else if(col=="National Defense") filter(.,subcategory=="Defense Discretionary") else filter(.,subcategory%in%col)} %>% 
                group_by(projected_fiscal_year,subcategory) %>% 
                filter(baseline_date<=MAX_DATE) %>% 
                slice(n()) %>% 
                group_by(projected_fiscal_year) %>% 
                summarize(value=sum(value,na.rm=TRUE)) %>% 
                select(projected_fiscal_year,value) %>% 
                rename(cbo_proj=value,
                       fiscal_year=projected_fiscal_year)) 
  
  monthly_df = bind_rows(monthly_df,tmp) %>% 
    arrange(date) %>% 
    fill(error,error_ly,.direction="down")
  
  refund_share = bind_rows(receipts %>% 
                             filter(grepl("Total -- Individual Income Taxes",classification_desc)) %>% 
                             select(record_date,refund_amt=current_month_refund_amt) %>% 
                             mutate(var="Non-refundable",refund_amt=as.numeric(refund_amt)),
                           outlays %>% filter(grepl("Payment Where|Refund|Build America",classification_desc)|
                                                (parent_id%in%outlays$classification_id[outlays$classification_desc=="Internal Revenue Service:"]&classification_desc=="Other")) %>% 
                             select(record_date,refund_amt=current_month_net_outly_amt) %>% 
                             group_by(record_date) %>% 
                             summarize(refund_amt=sum(as.numeric(refund_amt)),var="Refundable")) %>% 
    group_by(record_date) %>% 
    mutate(share=refund_amt/sum(refund_amt,na.rm=TRUE),
           share=ifelse(is.na(share),0,share),
           fiscal_year=as.integer(quarter(record_date, with_year = TRUE, fiscal_start = 10)),
           month=month(record_date)) %>% 
    mutate(tax_due=case_when(
      !(fiscal_year%in%c(2020,2021))&month==4~1,
      fiscal_year==2020&month==7~1,
      fiscal_year==2021&month==5~1,
      TRUE~0
    ),
    quarter_end=ifelse(month%in%c(1,4,6,9),1,0)) %>% 
    filter(record_date<=floor_date(as.Date(MAX_DATE),"month")) 
  
  daily_df = dts %>% 
    filter(record_date<=MAX_DATE) %>% #TODO: REMOVE WHEN DONE TESTING 
    filter(cbo_category%in%case_when(col%in%c("Individual Income Taxes","Payroll Taxes")~c("Individual Income Taxes","Payroll Taxes"),
                                     TRUE~col)&!grepl("from Depositaries",transaction_catg)&record_date<=end_date) %>% 
    {if(col%in%c("Individual Income Taxes","Payroll Taxes")) mutate(.,date=floor_date(record_date,"month")) %>% 
        left_join(refund_share %>% filter(var=="Refundable") %>% select(date=record_date,refund_share=share) %>% mutate(date=floor_date(date,"month"))) %>% 
        mutate(fiscal_year=as.integer(quarter(record_date, with_year = TRUE, fiscal_start = 10)),
               month=month(record_date)) %>% 
        mutate(tax_due=case_when(
          !(fiscal_year%in%c(2020,2021))&month==4~1,
          fiscal_year==2020&month==7~1,
          fiscal_year==2021&month==5~1,
          TRUE~0
        ),
        quarter_end=ifelse(month%in%c(1,4,6,9),1,0)) %>% 
        mutate(refund_share=case_when(
          is.na(refund_share)~predict(models_daily$refund_reg,.)$predictions,
          TRUE~refund_share
        ),
        refund_share=1-refund_share,
        transaction_today_amt=case_when(
          grepl("Individual Tax Refunds|Tax Refunds Individual",transaction_catg)~transaction_today_amt*refund_share,
          TRUE~transaction_today_amt
        )) %>% select(-c(quarter_end,tax_due,fiscal_year,refund_share)) else . } %>% 
    mutate(cbo_category=col) %>% 
    group_by(record_fiscal_year,record_calendar_month,record_calendar_day) %>% 
    summarize(record_date=record_date[1],
              cbo_category=cbo_category[1],
              total_day=sum(transaction_today_amt/1000,na.rm=TRUE)) %>% 
    mutate(total_day_lwr=total_day,
           total_day_upper=total_day) %>% 
    ungroup() %>% 
    arrange(record_date) %>% 
    complete(record_date = seq.Date(floor_date(min(dts$record_date),"month"), as.Date(MAX_DATE), by = "day")) %>% 
    mutate(record_fiscal_year=as.integer(quarter(record_date, with_year = TRUE, fiscal_start = 10)),
           record_calendar_month=month(record_date),
           record_calendar_year=year(record_date),
           record_calendar_day=sprintf("%02d", day(record_date))) %>% 
    mutate_at(vars(total_day,total_day_lwr,total_day_upper),~case_when(!is.na(.)~.,
                                                                       record_date<=as.Date(MAX_DATE)&is.na(.)~0,
                                                                       record_date>as.Date(MAX_DATE)~NA)) %>% 
    fill(cbo_category,.direction="downup") %>% 
    mutate(date=floor_date(record_date,"month"))
  
  
  x_data = read_csv(paste0("Data/Processing/imputed_data/imputed_data_asof",end_date,".csv"))  %>% 
    select(-any_of(paste0("gt_",bad_vars$category))) %>% 
    arrange(date) %>%
    ungroup() %>% 
    mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10),.funs=list(ch12m=~((./dplyr::lag(.,12)-1)*100),ch1m=~((./dplyr::lag(.,1)-1)*100))) %>%
    mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,grep("gt_",colnames(.),value=TRUE)),.funs=list(ch12m=~.-dplyr::lag(.,12),ch1m=~.-dplyr::lag(.,1))) %>%
    mutate_at(vars(PAYEMS:gt_999_ch1m),.funs=list(lag1=~dplyr::lag(.,1),lag2=~dplyr::lag(.,2),lag3=~dplyr::lag(.,3),lag4=~dplyr::lag(.,4))) %>% 
    left_join(monthly_df,by="date") %>% 
    mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10))) %>% 
    mutate(month=month(date),
           fy_month=case_when(
             month%in%c(10:12)~month-9,
             month%in%c(1:9)~month+3
           )) %>% 
    select(-cbo_proj) %>% 
    left_join(cbo_proj %>% 
                {if(col=="Other Spending") filter(.,subcategory %in% c("Nondefense Discretionary","Other Mandatory")) else if(col=="National Defense") filter(.,subcategory=="Defense Discretionary") else filter(.,subcategory%in%col)} %>% 
                group_by(projected_fiscal_year,subcategory) %>% 
                filter(baseline_date<=MAX_DATE) %>% 
                slice(n()) %>% 
                group_by(projected_fiscal_year) %>% 
                summarize(value=sum(value,na.rm=TRUE)) %>% 
                select(projected_fiscal_year,value) %>% 
                rename(cbo_proj=value,
                       fiscal_year=projected_fiscal_year)) %>% 
    ungroup() %>% 
    mutate(value_lwr=value,
           value_upper=value)
  
  dates = x_data %>% filter(date<=MAX_DATE&date>max(monthly_df$date[!is.na(monthly_df$value)])) %>% arrange(date) %>% pull(date)
  fys = unique(as.integer(quarter(dates, with_year = TRUE, fiscal_start = 10)))
  
  for(dat in as.character(dates)){
    
    x_data1 = x_data %>% 
      filter(date<=dat) %>% 
      left_join(monthly_share_pred %>% select(date,pred_cumshare:pred_cumshare_upper),by="date")
    
    x_data1 = x_data1 %>% 
      group_by(fiscal_year) %>% 
      mutate(cum_total=cumsum(value),
             cum_total_lwr=cumsum(value_lwr),
             cum_total_upper=cumsum(value_upper)) %>% 
      ungroup()
    
    x_data1$pred_total = x_data1$cum_total/x_data1$pred_cumshare
    # x_data1$pred_total_lwr = x_data1$cum_total_lwr/x_data1$pred_cumshare_upper
    # x_data1$pred_total_upper = x_data1$cum_total_upper/x_data1$pred_cumshare_lwr
    x_data1  = x_data1 %>% 
      group_by(fiscal_year) %>% 
      fill(pred_total,.direction="down")
    
    if(tail(x_data1$fiscal_year,1)!=fys[1]){
      
      scalar = x_data1 %>% 
        ungroup() %>% 
        filter(fiscal_year==fys[1]) %>% 
        mutate(cbo_pred_month=cbo_proj*pred_cumshare,
               cbo_pred_month=case_when(fy_month==1~cbo_pred_month,
                                        TRUE~cbo_pred_month-dplyr::lag(cbo_pred_month,1))) %>% 
        select(date,actual,cbo_pred_month) %>% 
        summarize(num=mean(actual/cbo_pred_month,na.rm=TRUE)) %>% 
        pull(num)
      
      x_data1 = x_data1 %>% 
        mutate(cbo_proj=case_when(fiscal_year==tail(x_data1$fiscal_year,1)~cbo_proj*scalar,
                                  TRUE~cbo_proj))
      
    }else{
      scalar=1
    }
    
    x_data1 = x_data1 %>% 
      group_by(fiscal_year) %>% 
      mutate_at(vars(pred_total),~.*(max(c(tail(fy_month[!is.na(value)&fiscal_year>=fys[1]],1),0),na.rm=TRUE)/12)) %>% 
      mutate(cbo_proj_month=cbo_proj*(1-max(c(tail(fy_month[!is.na(value)&fiscal_year>=fys[1]],1),0),na.rm=TRUE)/12)) %>% 
      rowwise() %>% 
      mutate_at(vars(pred_total),~sum(c(.,cbo_proj_month),na.rm=TRUE)) %>% 
      ungroup() %>% 
      mutate(final_pred_month=pred_total*pred_cumshare)
    
    x_data1 = x_data1 %>% 
      mutate(cbo_proj_month=cbo_proj*pred_cumshare) %>% 
      group_by(fiscal_year) %>% 
      mutate_at(vars(final_pred_month,cbo_proj_month),~case_when(fy_month==1~.,TRUE~.-dplyr::lag(.,1))) %>% 
      ungroup() %>% 
      mutate(final_pred_month_lwr=final_pred_month+pred_cumshare_lwr*pred_total,
             final_pred_month_upper=final_pred_month+pred_cumshare_upper*pred_total)
    
    if(nrow(overlays)>0){
      
      x_data1 = x_data1 %>% 
        mutate(up_diff=final_pred_month_upper-final_pred_month,
               down_diff=final_pred_month_lwr-final_pred_month) %>% 
        left_join(overlays %>% select(date,overlay_value=value)) %>% 
        mutate(final_pred_month=ifelse(is.na(overlay_value),final_pred_month,overlay_value),
               final_pred_month_lwr=ifelse(is.na(overlay_value),final_pred_month_lwr,overlay_value+down_diff),
               final_pred_month_upper=ifelse(is.na(overlay_value),final_pred_month_upper,overlay_value+up_diff)) %>% 
        select(-c(up_diff,down_diff,overlay_value))
      
    }
    
    tmp = predict(models_monthly$res_shrunk,x_data1 %>% filter(date==dat) %>% mutate(cbo_proj_month=final_pred_month),se.fit=TRUE)
    x_data$value[x_data$date==dat] = tmp$fit
    tmp = predict(models_monthly$res_shrunk,x_data1 %>% filter(date==dat) %>% mutate(cbo_proj_month=final_pred_month_lwr),se.fit=TRUE)
    x_data$value_lwr[x_data$date==dat] = tmp$fit - 1.64*tmp$se.fit
    tmp = predict(models_monthly$res_shrunk,x_data1 %>% filter(date==dat) %>% mutate(cbo_proj_month=final_pred_month_upper),se.fit=TRUE)
    x_data$value_upper[x_data$date==dat] = tmp$fit + 1.64*tmp$se.fit
    x_data$cbo_proj_month=NA
    x_data$cbo_proj_month[x_data$date<=dat] = x_data1$cbo_proj_month
    
    monthly_nowcast = x_data %>% filter(date<=dat) %>% select(date,actual,pred=value,fit.lwr=value_lwr,fit.upr=value_upper,cbo_proj=cbo_proj_month)
    
    if((max(daily_df$record_date[!is.na(daily_df$total_day)],na.rm=TRUE))<(ceiling_date(as.Date(dat),"month")-1)){ # testing if we have the last day of the month. If we have the last day of the month then we dont need to add the missing days
      
      daily_df = daily_df %>% 
        ungroup() %>% 
        complete(record_date = seq.Date(min(record_date), (ceiling_date(as.Date(dat),"month")-1), by = "day")) %>% 
        mutate(record_fiscal_year=as.integer(quarter(record_date, with_year = TRUE, fiscal_start = 10)),
               record_calendar_month=month(record_date),
               record_calendar_year=year(record_date),
               record_calendar_day=sprintf("%02d", day(record_date)),
               date=floor_date(record_date,"month")) %>% 
        mutate_at(vars(total_day,total_day_lwr,total_day_upper),~case_when(!is.na(.)~.,
                                                                           record_date<=MAX_DATE&is.na(.)~0,
                                                                           record_date>MAX_DATE~NA)) %>% 
        fill(cbo_category,.direction="down")
      
    }
    
    daily_df1 = daily_df %>% 
      filter(record_date<=(ceiling_date(as.Date(dat),"month")-1)) %>% 
      mutate(imputed=ifelse(record_date<=MAX_DATE&record_date>=head(record_date[total_day!=0],1),0,1)) %>% 
      group_by(record_fiscal_year,record_calendar_month) %>% 
      arrange(record_calendar_day) %>% 
      mutate(cum_total_day=cumsum(total_day),
             cum_total_day_lwr=cumsum(total_day_lwr),
             cum_total_day_upper=cumsum(total_day_upper),
             total_month=sum(total_day,na.rm=TRUE),
             total_month_lwr=sum(total_day_lwr,na.rm=TRUE),
             total_month_upper=sum(total_day_upper,na.rm=TRUE),
             record_calendar_day_perc=(as.numeric(record_calendar_day))/as.numeric(days_in_month(record_date)),
             inv_record_calendar_day=1-record_calendar_day_perc) %>% 
      mutate(fy_month=case_when(
        record_calendar_month%in%c(10:12)~record_calendar_month-9,
        record_calendar_month%in%c(1:9)~record_calendar_month+3
      )) %>% 
      group_by(record_fiscal_year) %>% 
      arrange(fy_month) %>% 
      mutate(cum_total_month=cumsum(total_day),
             cum_total_month_lwr=cumsum(total_day_lwr),
             cum_total_month_upper=cumsum(total_day_upper),
             total_year=sum(total_month),
             total_year_lwr=sum(total_month_lwr),
             total_year_upper=sum(total_month_upper)) %>% 
      ungroup() %>% 
      mutate(date=floor_date(record_date,"month")) %>% 
      left_join(monthly_nowcast,by="date") %>% 
      arrange(record_date) %>% 
      left_join(tax_days,by=c("record_date"="date")) %>% 
      mutate(cum_share=cum_total_day/total_month,
             cum_share_lwr=cum_total_day_lwr/total_month_lwr,
             cum_share_upper=cum_total_day_upper/total_month_upper,
             quarter_end=case_when(
               record_calendar_month==4&tax_day==1~1,
               col=="Corporate Income Taxes"&record_calendar_month%in%c(12,6,9)&record_calendar_day==15&!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))~1,
               col=="Corporate Income Taxes"&record_calendar_month%in%c(12,6,9)&record_calendar_day%in%c(16,17)&(weekdays(record_date,abbreviate = TRUE)%in%c("Mon"))~1, # only use 16 or 17 IF the 15th had fallen on a weekend
               col=="Individual Income Taxes"&record_calendar_month%in%c(1,6,9)&record_calendar_day==15&!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))~1,
               col=="Individual Income Taxes"&record_calendar_month%in%c(1,6,9)&record_calendar_day%in%c(16,17)&(weekdays(record_date,abbreviate = TRUE)%in%c("Mon"))~1)
      ) %>% 
      group_by(date) %>% 
      fill(tax_day,quarter_end,.direction="down") %>% 
      mutate(tax_day=ifelse(is.na(tax_day),0,tax_day),
             quarter_end=ifelse(is.na(quarter_end),0,quarter_end),
             settlement_period=case_when(
               record_date>=max(record_date[!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))])~"EOM",
               record_date>=min(record_date[day(record_date)>=15&!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))])~"Second Settlement",
               TRUE~"First Settlement"
             )) %>% 
      group_by(date) %>% 
      mutate(weekend=weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun")) %>% 
      ungroup()
    
    daily_df1 = daily_df1 %>% 
      left_join(daily_df1 %>% 
                  distinct(date) %>% 
                  mutate(dat=1:n(),
                         month=month(date)) %>% 
                  rowwise() %>% 
                  mutate(first_day_thismonth_weekend=(weekdays(date,abbreviate=TRUE)%in%c("Sat","Sun")|date%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=1,
                         first_day_nextmonth_weekend=(weekdays(date %m+% months(1),abbreviate=TRUE)%in%c("Sat","Sun")|(date %m+% months(1))%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12,
                         last_day_thismonth_weekend=(weekdays((date %m+% months(1) )- 1,abbreviate=TRUE)%in%c("Sat","Sun")|((date %m+% months(1) )- 1)%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12) %>% 
                  ungroup() %>% 
                  mutate(date_group=case_when(
                    date<="2020-03-01"~"Before 2020-4",
                    date<="2023-11-01"~"Before 2023-12",
                    date>"2023-11-01"~"After 2023-12"
                  ))) # for the scalar reg
    
    daily_df1 = daily_df1 %>% 
      bind_cols(data.frame(predict(models_daily$share,
                                   data=.,type="quantiles",
                                   quantiles=c(0.5,.1,.9))) %>% 
                  rename("pred_share"=1,"pred_share_lwr"=2,"pred_share_upper"=3)) %>% 
      group_by(date) %>% 
      mutate(pred_cumshare=cumsum(pred_share),
             pred_cumshare_lwr=cumsum(pred_share_lwr),
             pred_cumshare_upper=cumsum(pred_share_upper),
             row=1:n()) %>% 
      mutate_at(vars(pred_cumshare:pred_cumshare_upper),~case_when(row>=max(row[weekend==FALSE])&.<=0~1,
                                                                   TRUE~.)) %>% 
      mutate_at(vars(pred_cumshare:pred_cumshare_upper),~./.[n()]) %>% 
      mutate(month=month(record_date)) %>% 
      bind_cols(data.frame(predict(models_daily$scalar,
                                   newdata = .,
                                   se.fit=TRUE, 
                                   interval="confidence", 
                                   alpha=0.10)) %>% 
                  rename("scalar"=1,"scalar_lwr"=2,"scalar_upper"=3,"scalar_se_fit"=4)) %>% 
      select(-dat)
    
    daily_df1 = daily_df1 %>%
      group_by(date) %>%
      mutate(pred_month_total=cum_total_day/pred_cumshare*scalar,
             pred_month_total=ifelse(is.nan(pred_month_total)|is.infinite(pred_month_total)|pred_month_total==0,pred,pred_month_total),
             pred_month_total_lwr=cum_total_day_lwr/pred_cumshare_upper*scalar_lwr,
             pred_month_total_lwr=ifelse(is.nan(pred_month_total_lwr)|is.infinite(pred_month_total_lwr)|pred_month_total_lwr==0,fit.lwr,pred_month_total_lwr),
             pred_month_total_upper=cum_total_day_upper/pred_cumshare_lwr*scalar_upper,
             pred_month_total_upper=ifelse(is.nan(pred_month_total_upper)|is.infinite(pred_month_total_upper)|pred_month_total_upper==0,fit.upr,pred_month_total_upper)) %>% 
      ungroup() %>% 
      mutate_at(vars(pred_month_total,pred_month_total_lwr,pred_month_total_upper),~ifelse(.<(min(daily_df1$actual,na.rm=TRUE)-(5*sd(daily_df1$actual,na.rm=TRUE))),min(daily_df1$actual,na.rm=TRUE)-(5*sd(daily_df1$actual,na.rm=TRUE)),.)) %>% 
      mutate_at(vars(pred_month_total,pred_month_total_lwr,pred_month_total_upper),~ifelse(.>(max(daily_df1$actual,na.rm=TRUE)+(5*sd(daily_df1$actual,na.rm=TRUE))),max(daily_df1$actual,na.rm=TRUE)+(5*sd(daily_df1$actual,na.rm=TRUE)),.))
    
    
    if(col%in%c("Individual Income Taxes","Payroll Taxes")){
      
      preds = data.frame(predict(models_daily$disagg_reg,daily_df1 %>% 
                                   mutate(month=month(record_date),
                                          fiscal_year=year(record_date),
                                          quarter_end=case_when(col=="Corporate Income Taxes"&month%in%c(12,4,6,9)~1,
                                                                col=="Individual Income Taxes"&month%in%c(1,4,6,9)~1,
                                                                !grepl("Income",col)&month%in%c(1,4,6,9)~1,
                                                                TRUE~0),
                                          tax_due=case_when(
                                            !(fiscal_year%in%c(2020,2021))&month==4&col=="Individual Income Taxes"~1,
                                            fiscal_year==2020&month==7&col=="Individual Income Taxes"~1,
                                            fiscal_year==2021&month==5&col=="Individual Income Taxes"~1,
                                            !(fiscal_year%in%c(2020))&month==4&col=="Corporate Income Taxes"~1,
                                            fiscal_year==2020&month==7&col=="Corporate Income Taxes"~1,
                                            fiscal_year==2020&month==9&col=="Excise Taxes"~1,
                                            TRUE~0
                                          )),
                                 se.fit=TRUE, interval="confidence", alpha=0.10))
      colnames(preds)=c("scalar_adj","scalar_lwr_adj","scalar_upper_adj","scalar_adj_se_fit")
      daily_df1 = bind_cols(daily_df1,preds) %>% 
        rowwise() %>% 
        mutate(scalar_lwr=ifelse(col=="Individual Income Taxes",scalar_lwr*scalar_lwr_adj,scalar_lwr*(1-scalar_lwr_adj)),
               scalar_upper=ifelse(col=="Individual Income Taxes",scalar_upper*scalar_upper_adj,scalar_upper*(1-scalar_upper_adj)),
               scalar=ifelse(col=="Individual Income Taxes",scalar*scalar_adj,scalar*(1-scalar_adj)))
      
    }
    
    if(col%in%c("Miscellaneous Receipts")){
      
      daily_df1 = daily_df1 %>% 
        group_by(date) %>% 
        mutate(pred_month_total=cum_total_day/pred_cumshare*scalar,
               pred_month_total_lwr=cum_total_day/pred_cumshare_upper*scalar_lwr,
               pred_month_total_upper=cum_total_day/pred_cumshare_lwr*scalar_upper) %>% 
        group_by(date) %>% 
        fill(pred_month_total,pred_month_total_lwr,pred_month_total_upper,.direction="down") %>% 
        mutate(pred_total1=pred,
               pred_total1_lwr=fit.lwr,
               pred_total1_upper=fit.upr)
      
    } else{
      
      daily_df1 = daily_df1 %>% 
        rowwise() %>% 
        mutate(pred_month_total=cum_total_day/pred_cumshare*scalar,
               pred_month_total_lwr=cum_total_day/pred_cumshare_upper*scalar_lwr,
               pred_month_total_upper=cum_total_day/pred_cumshare_lwr*scalar_upper) %>% 
        group_by(date) %>% 
        mutate(pred_total1=pred_month_total*record_calendar_day_perc+pred*(1-record_calendar_day_perc),
               pred_total1_lwr=pred_month_total_lwr*record_calendar_day_perc+fit.lwr*(1-record_calendar_day_perc),
               pred_total1_upper=pred_month_total_upper*record_calendar_day_perc+fit.upr*(1-record_calendar_day_perc),
               pred_total1=case_when((col=="Corporate Income Taxes"&MAX_DATE<"2023-10-01")~pred,
                                     is_bad(pred_total1)&is_bad(pred_month_total)&!is_bad(pred)~pred,
                                     is_bad(pred_total1)&!is_bad(pred_month_total)&is_bad(pred)~pred_month_total,
                                     TRUE~pred_total1),
               pred_total1_lwr=case_when((col=="Corporate Income Taxes"&MAX_DATE<"2023-10-01")~fit.lwr,
                                         is_bad(pred_total1_lwr)&is_bad(pred_month_total_lwr)&!is_bad(fit.lwr)~fit.lwr,
                                         is_bad(pred_total1_lwr)&!is_bad(pred_month_total_lwr)&is_bad(fit.lwr)~pred_month_total_lwr,
                                         TRUE~pred_total1_lwr),
               pred_total1_upper=case_when((col=="Corporate Income Taxes"&MAX_DATE<"2023-10-01")~fit.upr,
                                           is_bad(pred_total1_upper)&is_bad(pred_month_total_upper)&!is_bad(fit.upr)~fit.upr,
                                           is_bad(pred_total1_upper)&!is_bad(pred_month_total_upper)&is_bad(fit.upr)~pred_month_total_upper,
                                           TRUE~pred_total1_upper)) %>% 
        fill(pred_month_total,pred_month_total_lwr,pred_month_total_upper,pred_total1,pred_total1_lwr,pred_total1_upper,.direction="down")
      
    }
    
    if(nrow(overlays)>0){
      
      daily_df1 = daily_df1 %>% 
        mutate(up_diff=pred_total1_upper-pred_total1,
               down_diff=pred_total1_lwr-pred_total1) %>% 
        left_join(overlays %>% select(date,overlay_value=value)) %>% 
        mutate(pred_total1=ifelse(is.na(overlay_value),pred_total1,overlay_value),
               pred_total1_lwr=ifelse(is.na(overlay_value),pred_total1_lwr,overlay_value+down_diff),
               pred_total1_upper=ifelse(is.na(overlay_value),pred_total1_upper,overlay_value+up_diff)) %>% 
        select(-c(up_diff,down_diff,overlay_value))
      
    }
    
    daily_df1 = daily_df1 %>% 
      rowwise() %>% 
      mutate(min=min(c(pred_total1_lwr,pred_total1_upper)),
             max=max(c(pred_total1_lwr,pred_total1_upper)),
             pred_total1_lwr=min,
             pred_total1_upper=max) %>% 
      select(-c(min,max))
    
    # TODO: THINK ABOUT HOW TO GET CLOSER WHEN DISAGGREGATING
    # see if it improves things for any category at one date and with proper backtesting
    # if doesnt improve, just keep as a separate data series
    
    daily_df1 = daily_df1 %>% 
      group_by(date) %>% 
      mutate(final_pred_day_cum=case_when(
        all(!is.na(actual))&all(!is_bad(cum_share))&!(col%in%c("Payroll Taxes"))~actual*cum_share, # distribute by observed pattern
        (all(!is.na(actual))&!all(!is_bad(cum_share)))|(all(!is.na(actual))&all(!is_bad(cum_share))&(col%in%c("Payroll Taxes")))~actual*pred_cumshare,
        date<dat&!((col=="Corporate Income Taxes"&MAX_DATE<"2023-10-01"))~cum_total_day,
        (date<dat|imputed==0)&((col=="Corporate Income Taxes"&MAX_DATE<"2023-10-01"))~pred_total1[n()]*pred_cumshare,
        date>=dat&(col%in%c("Individual Income Taxes","Payroll Taxes","Miscellaneous Receipts"))~pred_total1[n()]*pred_cumshare,
        imputed==0&!((col=="Corporate Income Taxes"&MAX_DATE<"2023-10-01"))~cum_total_day*scalar,
        imputed==1~pred_total1[n()]*pred_cumshare
      ),
      final_pred_day_cum_lwr=case_when(
        all(!is.na(actual))&all(!is_bad(cum_share))&!(col%in%c("Payroll Taxes"))~actual*cum_share, # distribute by observed pattern
        (all(!is.na(actual))&!all(!is_bad(cum_share)))|(all(!is.na(actual))&all(!is_bad(cum_share))&(col%in%c("Payroll Taxes")))~actual*pred_cumshare,
        date<dat&!((col=="Corporate Income Taxes"&MAX_DATE<"2023-10-01"))~cum_total_day_lwr,
        (date<dat|imputed==0)&((col=="Corporate Income Taxes"&MAX_DATE<"2023-10-01"))~pred_total1_lwr[n()]*pred_cumshare_lwr,
        date>=dat&(col%in%c("Individual Income Taxes","Payroll Taxes","Miscellaneous Receipts"))~pred_total1_lwr[n()]*pred_cumshare_lwr,
        imputed==0&!((col=="Corporate Income Taxes"&MAX_DATE<"2023-10-01"))~cum_total_day*scalar_lwr,
        imputed==1~pred_total1_lwr[n()]*pred_cumshare_lwr
      ),
      final_pred_day_cum_upper=case_when(
        all(!is.na(actual))&all(!is_bad(cum_share))&!(col%in%c("Payroll Taxes"))~actual*cum_share, # distribute by observed pattern
        (all(!is.na(actual))&!all(!is_bad(cum_share)))|(all(!is.na(actual))&all(!is_bad(cum_share))&(col%in%c("Payroll Taxes")))~actual*pred_cumshare,
        date<dat&!((col=="Corporate Income Taxes"&MAX_DATE<"2023-10-01"))~cum_total_day_upper,
        (date<dat|imputed==0)&((col=="Corporate Income Taxes"&MAX_DATE<"2023-10-01"))~pred_total1_upper[n()]*pred_cumshare_upper,
        date>=dat&(col%in%c("Individual Income Taxes","Payroll Taxes","Miscellaneous Receipts"))~pred_total1_upper[n()]*pred_cumshare_upper,
        imputed==0&!((col=="Corporate Income Taxes"&MAX_DATE<"2023-10-01"))~cum_total_day*scalar_upper,
        imputed==1~pred_total1_upper[n()]*pred_cumshare_upper
      )) %>% 
      select(record_date,record_fiscal_year,fy_month,imputed,total_day,scalar,total_month,
             date,pred,fit.lwr,fit.upr,actual,cbo_proj,scalar,
             intermediate_pred=pred_total1,intermediate_pred_lwr=pred_total1_lwr,intermediate_pred_upper=pred_total1_upper,
             final_pred_day_cum,final_pred_day_cum_lwr,final_pred_day_cum_upper) %>% 
      mutate(cbo_category=col,
             final_pred_day=case_when(
               record_date==record_date[1]~final_pred_day_cum,
               TRUE~final_pred_day_cum-dplyr::lag(final_pred_day_cum,1)
             ),
             final_pred_day_lwr=case_when(
               record_date==record_date[1]~final_pred_day_cum_lwr,
               TRUE~final_pred_day_cum_lwr-dplyr::lag(final_pred_day_cum_lwr,1)
             ),
             final_pred_day_upper=case_when(
               record_date==record_date[1]~final_pred_day_cum_upper,
               TRUE~final_pred_day_cum_upper-dplyr::lag(final_pred_day_cum_upper,1)
             ),
             total_day_imp=case_when(
               imputed==0~total_day*scalar,
               imputed==1~((final_pred_day_cum[n()]-(total_month[n()]*scalar[n()]))/sum(final_pred_day[imputed==1]))*final_pred_day
             )) %>% 
      ungroup() %>% 
      relocate(total_day_imp,.after=total_day) %>% 
      select(-scalar)
    
    daily_df[daily_df$date<=dat,c("total_day","total_day_lwr","total_day_upper")] = daily_df1 %>% filter(date<=dat) %>% select(final_pred_day:final_pred_day_upper)
    x_data[x_data$date==dat,c("value","value_lwr","value_upper")] = daily_df1 %>% filter(date==dat) %>% slice(n()) %>% select(final_pred_day_cum:final_pred_day_cum_upper)
    
  }
  
  daily_df = daily_df %>% 
    mutate(final_pred_day=total_day,
           final_pred_day_lwr=total_day_lwr,
           final_pred_day_upper=total_day_upper) %>% 
    left_join(daily_df1 %>% select(record_date,cbo_proj)) %>% 
    group_by(date) %>% 
    mutate(final_pred_day_cum=cumsum(final_pred_day),
           final_pred_day_cum_lwr=cumsum(final_pred_day_lwr),
           final_pred_day_cum_upper=cumsum(final_pred_day_upper)) %>% 
    ungroup()
  
  x_data = x_data  %>% 
    select(-c(value:cbo_proj))
  
  if(!(max(daily_df$record_date)==paste0(max(daily_df$record_fiscal_year),"-09-30"))){
    
    dates = seq(max(daily_df$record_date,na.rm=TRUE)+1,as.Date(paste0(max(daily_df$record_fiscal_year),"-09-30")),by=1)
    months = unique(month(dates))
    
    daily_df1 = daily_df %>% 
      bind_rows(data.frame(record_date=seq(max(daily_df$record_date,na.rm=TRUE)+1,max(dates),by=1))) %>% 
      group_by(record_fiscal_year) %>% 
      arrange(record_date) %>% 
      mutate(cum_total_fy=cumsum(final_pred_day),
             cum_total_fy_lwr=cumsum(final_pred_day_lwr),
             cum_total_fy_upper=cumsum(final_pred_day_upper),
             record_fiscal_year=as.integer(quarter(record_date, with_year = TRUE, fiscal_start = 10)),
             date=floor_date(record_date,"month"),
             month=month(record_date),
             fy_month=case_when(
               month%in%c(10:12)~month-9,
               month%in%c(1:9)~month+3
             ),
             imputed=ifelse(record_date>MAX_DATE,1,0)) %>% 
      left_join(tax_days,by=c("record_date"="date")) %>% 
      group_by(date) %>% 
      mutate(day=day(record_date),
             quarter_end=case_when(
               record_calendar_month==4&tax_day==1~1,
               col=="Corporate Income Taxes"&record_calendar_month%in%c(12,6,9)&record_calendar_day==15&!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))~1,
               col=="Corporate Income Taxes"&record_calendar_month%in%c(12,6,9)&record_calendar_day%in%c(16,17)&(weekdays(record_date,abbreviate = TRUE)%in%c("Mon"))~1, # only use 16 or 17 IF the 15th had fallen on a weekend
               col=="Individual Income Taxes"&record_calendar_month%in%c(1,6,9)&record_calendar_day==15&!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))~1,
               col=="Individual Income Taxes"&record_calendar_month%in%c(1,6,9)&record_calendar_day%in%c(16,17)&(weekdays(record_date,abbreviate = TRUE)%in%c("Mon"))~1)
      ) %>% 
      fill(tax_day,quarter_end,.direction="down") %>% 
      group_by(date) %>% 
      mutate(tax_day=ifelse(is.na(tax_day),0,tax_day),
             quarter_end=ifelse(is.na(quarter_end),0,quarter_end),
             settlement_period=case_when(
               record_date>=max(record_date[!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))])~"EOM",
               record_date>=min(record_date[day(record_date)>=15&!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))])~"Second Settlement",
               TRUE~"First Settlement"
             )) %>% 
      ungroup() %>% 
      mutate(fiscal_year=as.integer(quarter(record_date, with_year = TRUE, fiscal_start = 10)),
             tax_due=case_when(
               !(fiscal_year%in%c(2020,2021))&month==4&col=="Individual Income Taxes"~1,
               fiscal_year==2020&month==7&col=="Individual Income Taxes"~1,
               fiscal_year==2021&month==5&col=="Individual Income Taxes"~1,
               !(fiscal_year%in%c(2020))&month==4&col=="Corporate Income Taxes"~1,
               fiscal_year==2020&month==7&col=="Corporate Income Taxes"~1,
               fiscal_year==2020&month==9&col=="Excise Taxes"~1,
               TRUE~0
             )) %>% 
      group_by(date) %>% 
      mutate(weekend=weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"),
             record_calendar_month=month(record_date),
             record_calendar_day=sprintf("%02d", day(record_date)),
             fed_remittances_suspended=ifelse(date>="2022-09-01",1,0)) %>% 
      ungroup() %>% 
      fill(cbo_category,.direction = "down")
    
    daily_df1 = daily_df1 %>% 
      left_join(daily_df1 %>% 
                  distinct(date) %>% 
                  mutate(dat=1:n(),
                         month=month(date)) %>% 
                  rowwise() %>% 
                  mutate(first_day_thismonth_weekend=(weekdays(date,abbreviate=TRUE)%in%c("Sat","Sun")|date%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=1,
                         first_day_nextmonth_weekend=(weekdays(date %m+% months(1),abbreviate=TRUE)%in%c("Sat","Sun")|(date %m+% months(1))%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12,
                         last_day_thismonth_weekend=(weekdays((date %m+% months(1) )- 1,abbreviate=TRUE)%in%c("Sat","Sun")|((date %m+% months(1) )- 1)%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12) %>% 
                  ungroup() %>% 
                  mutate(date_group=case_when(
                    date<="2020-03-01"~"Before 2020-4",
                    date<="2023-11-01"~"Before 2023-12",
                    date>"2023-11-01"~"After 2023-12"
                  ))) # for the scalar reg
    
    daily_df1 = daily_df1 %>% 
      left_join(monthly_share_pred %>% select(date,
                                              pred_cumshare_fy=pred_cumshare,
                                              pred_cumshare_fy_lwr=pred_cumshare_lwr,
                                              pred_cumshare_fy_upper=pred_cumshare_upper))
    
    daily_df1 = daily_df1 %>% 
      left_join(cbo_proj %>% 
                  {if(col=="Other Spending") filter(.,subcategory %in% c("Nondefense Discretionary","Other Mandatory")) else if(col=="National Defense") filter(.,subcategory=="Defense Discretionary") else filter(.,subcategory%in%col)} %>% 
                  group_by(projected_fiscal_year,subcategory) %>% 
                  filter(baseline_date<=MAX_DATE) %>% 
                  slice(n()) %>% 
                  group_by(projected_fiscal_year) %>% 
                  summarize(value=sum(value,na.rm=TRUE)) %>% 
                  select(projected_fiscal_year,value) %>% 
                  rename(cbo_proj_fy=value,
                         fiscal_year=projected_fiscal_year),
                by=c("record_fiscal_year"="fiscal_year")) %>% 
      mutate(cbo_proj_fmonth = cbo_proj_fy*pred_cumshare_fy,
             proj_fy = cum_total_fy/pred_cumshare_fy) %>% 
      fill(proj_fy,.direction="down") %>% 
      group_by(date) %>% 
      mutate(record_calendar_day_perc=(day(record_date))/as.numeric(days_in_month(record_date)),
             total_pred=proj_fy) %>% 
      group_by(record_fiscal_year) %>% 
      fill(total_pred,.direction="down") %>% 
      mutate(pred_month=total_pred*pred_cumshare_fy)
    
    daily_df1 = daily_df1 %>% 
      select(-c(pred_month)) %>% 
      left_join(daily_df1 %>% 
                  group_by(date) %>% 
                  slice(n()) %>% 
                  ungroup() %>% 
                  mutate(pred_month=case_when(fy_month==1~pred_month,
                                              TRUE~pred_month-dplyr::lag(pred_month,1)),
                         pred_month_lwr=pred_month+pred_cumshare_fy_lwr*total_pred,
                         pred_month_upper=pred_month+pred_cumshare_fy_upper*total_pred,
                         cbo_pred_month=case_when(fy_month==1~cbo_proj_fmonth,
                                                  TRUE~cbo_proj_fmonth-dplyr::lag(cbo_proj_fmonth,1)),
                         cbo_pred_month_lwr=cbo_pred_month+pred_cumshare_fy_lwr*cbo_proj_fy,
                         cbo_pred_month_upper=cbo_pred_month+pred_cumshare_fy_upper*cbo_proj_fy) %>% 
                  select(date,pred_month,pred_month_lwr,pred_month_upper,cbo_pred_month,cbo_pred_month_lwr,cbo_pred_month_upper),by="date") %>% 
      mutate(record_calendar_day=sprintf("%02d", day(record_date)),
             record_calendar_month=month(record_date),
             record_calendar_year=year(record_date),
             record_fiscal_year=as.integer(quarter(record_date, with_year = TRUE, fiscal_start = 10))) %>% 
      ungroup() %>% 
      mutate(pred_month1=pred_month*(tail(daily_df1$fy_month[!is.na(daily_df1$total_day)],1)/12)+cbo_pred_month*(1-tail(daily_df1$fy_month[!is.na(daily_df1$total_day)],1)/12),
             pred_month1_lwr=pred_month_lwr*(tail(daily_df1$fy_month[!is.na(daily_df1$total_day)],1)/12)+cbo_pred_month_lwr*(1-tail(daily_df1$fy_month[!is.na(daily_df1$total_day)],1)/12),
             pred_month1_upper=pred_month_upper*(tail(daily_df1$fy_month[!is.na(daily_df1$total_day)],1)/12)+cbo_pred_month_upper*(1-tail(daily_df1$fy_month[!is.na(daily_df1$total_day)],1)/12))
    
    if(nrow(overlays)>0){
      daily_df1 = daily_df1 %>% 
        mutate(up_diff=pred_month1_upper-pred_month1,
               down_diff=pred_month1_lwr-pred_month1) %>% 
        left_join(overlays %>% select(date,overlay_value=value)) %>% 
        mutate(pred_month1=ifelse(is.na(overlay_value),pred_month1,overlay_value),
               pred_month1_lwr=ifelse(is.na(overlay_value),pred_month1_lwr,overlay_value+down_diff),
               pred_month1_upper=ifelse(is.na(overlay_value),pred_month1_upper,overlay_value+up_diff)) %>% 
        select(-c(up_diff,down_diff,overlay_value))
    }
    
    preds = data.frame(predict(models_daily$share,data=daily_df1,type="quantiles",quantiles=c(0.5,.1,.9)))
    colnames(preds)=c("pred_share","pred_share_lwr","pred_share_upper")
    daily_df1 = bind_cols(daily_df1,preds) %>% 
      group_by(date) %>% 
      mutate(pred_cumshare_daily=cumsum(pred_share),
             pred_cumshare_daily_lwr=cumsum(pred_share_lwr),
             pred_cumshare_daily_upper=cumsum(pred_share_upper),
             row=1:n()) %>% 
      mutate_at(vars(pred_cumshare_daily:pred_cumshare_daily_upper),~case_when(row>=max(row[weekend==FALSE])&.<=0~1,
                                                                               TRUE~.)) %>% 
      mutate_at(vars(pred_cumshare_daily:pred_cumshare_daily_upper),~./.[n()])
    
    preds = data.frame(predict(models_daily$scalar,daily_df1,se.fit=TRUE, interval="confidence", alpha=0.10))
    colnames(preds)=c("scalar","scalar_lwr","scalar_upper","scalar_se_fit")
    daily_df1 = bind_cols(daily_df1,preds)
    
    daily_df1 = daily_df1 %>% 
      mutate(pred_day=pred_cumshare_daily*pred_month1,
             pred_day_lwr=pred_cumshare_daily_lwr*pred_month1_lwr,
             pred_day_upper=pred_cumshare_daily_upper*pred_month1_upper) %>% 
      group_by(date) %>% 
      mutate(pred_day_cum=pred_day,
             pred_day_cum_lwr=pred_day_lwr,
             pred_day_cum_upper=pred_day_upper,
             pred_day=case_when(
               record_date==min(record_date)~pred_day,
               TRUE~pred_day-dplyr::lag(pred_day,1)
             ),
             pred_day_lwr=case_when(
               record_date==min(record_date)~pred_day_lwr,
               TRUE~pred_day_lwr-dplyr::lag(pred_day_lwr,1)
             ),
             pred_day_upper=case_when(
               record_date==min(record_date)~pred_day_upper,
               TRUE~pred_day_upper-dplyr::lag(pred_day_upper,1)
             ),
             final_pred_day=ifelse(is.na(final_pred_day),pred_day,final_pred_day),
             final_pred_day_lwr=ifelse(is.na(final_pred_day_lwr),pred_day_lwr,final_pred_day_lwr),
             final_pred_day_upper=ifelse(is.na(final_pred_day_upper),pred_day_upper,final_pred_day_upper),
             final_pred_day_cum=ifelse(is.na(final_pred_day_cum),pred_day_cum,final_pred_day_cum),
             final_pred_day_cum_lwr=ifelse(is.na(final_pred_day_cum_lwr),pred_day_cum_lwr,final_pred_day_cum_lwr),
             final_pred_day_cum_upper=ifelse(is.na(final_pred_day_cum_upper),pred_day_cum_upper,final_pred_day_cum_upper),
             cbo_proj=ifelse(imputed==1&is.na(cbo_proj),cbo_pred_month,cbo_proj)) %>% 
      select(any_of(c(colnames(daily_df),"pred_month1","pred_month1_lwr","pred_month1_upper"))) %>% 
      ungroup() %>% 
      fill(cbo_category,.direction="down")
    
    daily_df = daily_df1
    
  }
  
  dates = seq(max(daily_df$record_date,na.rm=TRUE)+1,as.Date(paste0(max(cbo_proj$projected_fiscal_year[cbo_proj$baseline_date<=end_date]),"-09-30")),by=1)
  
  daily_df2 = bind_rows(data.frame(record_date=dates)) %>% 
    mutate(record_fiscal_year=as.integer(quarter(record_date, with_year = TRUE, fiscal_start = 10)),
           month=month(record_date),
           fy_month=case_when(
             month%in%c(10:12)~month-9,
             month%in%c(1:9)~month+3
           ),
           imputed=1,
           total_day=NA,
           total_day_imp=NA,
           total_month=NA,
           date=floor_date(record_date,"month")) %>% 
    group_by(record_fiscal_year) %>% 
    arrange(record_date) %>% 
    left_join(tax_days,by=c("record_date"="date")) %>% 
    group_by(date) %>% 
    mutate(day=day(record_date),
           month=month(record_date),
           record_calendar_day=sprintf("%02d", day(record_date)),
           record_calendar_month=month(record_date),
           quarter_end=case_when(
             record_calendar_month==4&tax_day==1~1,
             col=="Corporate Income Taxes"&month%in%c(12,6,9)&record_calendar_day==15&!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))~1,
             col=="Corporate Income Taxes"&record_calendar_month%in%c(12,6,9)&record_calendar_day%in%c(16,17)&(weekdays(record_date,abbreviate = TRUE)%in%c("Mon"))~1, # only use 16 or 17 IF the 15th had fallen on a weekend
             col=="Individual Income Taxes"&record_calendar_month%in%c(1,6,9)&record_calendar_day==15&!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))~1,
             col=="Individual Income Taxes"&record_calendar_month%in%c(1,6,9)&record_calendar_day%in%c(16,17)&(weekdays(record_date,abbreviate = TRUE)%in%c("Mon"))~1),
           settlement_period=case_when(
             record_date>=max(record_date[!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))])~"EOM",
             record_date>=min(record_date[day(record_date)>=15&!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))])~"Second Settlement",
             TRUE~"First Settlement"
           )) %>% 
    group_by(date) %>% 
    fill(tax_day,quarter_end,.direction="down") %>% 
    mutate(tax_day=ifelse(is.na(tax_day),0,tax_day),
           quarter_end=ifelse(is.na(quarter_end),0,quarter_end),
           fy_month=case_when(
             month%in%c(10:12)~month-9,
             month%in%c(1:9)~month+3
           )) %>% 
    ungroup() %>% 
    mutate(fiscal_year=as.integer(quarter(record_date, with_year = TRUE, fiscal_start = 10)),
           tax_due=case_when(
             !(fiscal_year%in%c(2020,2021))&month==4&col=="Individual Income Taxes"~1,
             fiscal_year==2020&month==7&col=="Individual Income Taxes"~1,
             fiscal_year==2021&month==5&col=="Individual Income Taxes"~1,
             !(fiscal_year%in%c(2020))&month==4&col=="Corporate Income Taxes"~1,
             fiscal_year==2020&month==7&col=="Corporate Income Taxes"~1,
             fiscal_year==2020&month==9&col=="Excise Taxes"~1,
             TRUE~0
           )) %>% 
    left_join(cbo_proj %>% 
                {if(col=="Other Spending") filter(.,subcategory %in% c("Nondefense Discretionary","Other Mandatory")) else if(col=="National Defense") filter(.,subcategory=="Defense Discretionary") else filter(.,subcategory%in%col)} %>% 
                group_by(projected_fiscal_year,subcategory) %>% 
                filter(baseline_date<=MAX_DATE) %>% 
                slice(n()) %>% 
                group_by(projected_fiscal_year) %>% 
                summarize(value=sum(value,na.rm=TRUE)) %>% 
                select(projected_fiscal_year,value) %>% 
                rename(cbo_proj_fy=value,
                       fiscal_year=projected_fiscal_year) %>% 
                mutate(change_cbo_proj_fy=cbo_proj_fy/cbo_proj_fy[fiscal_year==max(daily_df$record_fiscal_year)]),
              by=c("record_fiscal_year"="fiscal_year")) %>% 
    bind_cols(daily_df %>% 
                filter(record_fiscal_year==max(record_fiscal_year)) %>% 
                mutate(month=month(record_date)) %>% 
                group_by(month) %>% 
                slice(n()) %>% 
                ungroup() %>% 
                summarize(pred_total=sum(final_pred_day_cum),
                          pred_total_lwr=sum(final_pred_day_cum_lwr),
                          pred_total_upper=sum(final_pred_day_cum_upper)) %>% 
                select(pred_total:pred_total_upper)) %>% 
    mutate_at(vars(pred_total:pred_total_upper),~.*change_cbo_proj_fy) %>% 
    mutate(weekend=weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"),
           record_calendar_month=month(record_date),
           record_calendar_day=sprintf("%02d", day(record_date)),
           fed_remittances_suspended=ifelse(date>="2022-09-01",1,0)) %>% 
    ungroup()
  
  daily_df2 = daily_df2 %>% 
    left_join(daily_df2 %>% 
                distinct(date) %>% 
                mutate(month=month(date)) %>% 
                rowwise() %>% 
                mutate(first_day_thismonth_weekend=(weekdays(date,abbreviate=TRUE)%in%c("Sat","Sun")|date%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=1,
                       first_day_nextmonth_weekend=(weekdays(date %m+% months(1),abbreviate=TRUE)%in%c("Sat","Sun")|(date %m+% months(1))%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12,
                       last_day_thismonth_weekend=(weekdays((date %m+% months(1) )- 1,abbreviate=TRUE)%in%c("Sat","Sun")|((date %m+% months(1) )- 1)%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12) %>% 
                ungroup())
  
  daily_df2 = daily_df2 %>% 
    left_join(monthly_share_pred %>% select(date,
                                            pred_cumshare_fy=pred_cumshare,
                                            pred_cumshare_fy_lwr=pred_cumshare_lwr,
                                            pred_cumshare_fy_upper=pred_cumshare_upper))
  
  daily_df2 = daily_df2 %>% 
    mutate(cbo_proj_fmonth = cbo_proj_fy*pred_cumshare_fy,
           pred_month=pred_total*pred_cumshare_fy)
  
  daily_df2 = daily_df2 %>% 
    select(-c(pred_month)) %>% 
    left_join(daily_df2 %>% 
                group_by(date) %>% 
                slice(n()) %>% 
                ungroup() %>% 
                mutate(pred_month=case_when(fy_month==1~pred_month,
                                            TRUE~pred_month-dplyr::lag(pred_month,1)),
                       pred_month_lwr=pred_month+pred_cumshare_fy_lwr*pred_total,
                       pred_month_upper=pred_month+pred_cumshare_fy_upper*pred_total,
                       cbo_pred_month=case_when(fy_month==1~cbo_proj_fmonth,
                                                TRUE~cbo_proj_fmonth-dplyr::lag(cbo_proj_fmonth,1)),
                       cbo_pred_month_lwr=cbo_pred_month+pred_cumshare_fy_lwr*cbo_proj_fy,
                       cbo_pred_month_upper=cbo_pred_month+pred_cumshare_fy_upper*cbo_proj_fy) %>% 
                select(date,pred_month,pred_month_lwr,pred_month_upper,cbo_pred_month,cbo_pred_month_lwr,cbo_pred_month_upper),by="date") %>% 
    mutate(record_calendar_year=year(record_date)) %>% 
    ungroup() %>% 
    mutate(pred_month1=pred_month*(.5)+cbo_pred_month*(.5),
           pred_month1_lwr=pred_month_lwr*(.5)+cbo_pred_month_lwr*(.5),
           pred_month1_upper=pred_month_upper*(.5)+cbo_pred_month_upper*(.5))
  
  if(nrow(overlays)>0){
    daily_df2 = daily_df2 %>% 
      mutate(up_diff=pred_month1_upper-pred_month1,
             down_diff=pred_month1_lwr-pred_month1) %>% 
      left_join(overlays %>% select(date,overlay_value=value)) %>% 
      mutate(pred_month1=ifelse(is.na(overlay_value),pred_month1,overlay_value),
             pred_month1_lwr=ifelse(is.na(overlay_value),pred_month1_lwr,overlay_value+down_diff),
             pred_month1_upper=ifelse(is.na(overlay_value),pred_month1_upper,overlay_value+up_diff)) %>% 
      select(-c(up_diff,down_diff,overlay_value))
  }
  
  preds = data.frame(predict(models_daily$share,data=daily_df2,type="quantiles",quantiles=c(0.5,.1,.9)))
  colnames(preds)=c("pred_share","pred_share_lwr","pred_share_upper")
  daily_df2 = bind_cols(daily_df2,preds) %>% 
    group_by(date) %>% 
    mutate(pred_cumshare_daily=cumsum(pred_share),
           pred_cumshare_daily_lwr=cumsum(pred_share_lwr),
           pred_cumshare_daily_upper=cumsum(pred_share_upper),
           row=1:n()) %>% 
    mutate_at(vars(pred_cumshare_daily:pred_cumshare_daily_upper),~case_when(row>=max(row[weekend==FALSE])&.<=0~1,
                                                                             TRUE~.)) %>% 
    mutate_at(vars(pred_cumshare_daily:pred_cumshare_daily_upper),~./.[n()]) %>% 
    mutate(pred_month2=pred_month1,
           pred_month2_lwr=pred_month1_lwr,
           pred_month2_upper=pred_month1_upper)
  
  daily_df2= daily_df2 %>% 
    mutate(pred_day=pred_cumshare_daily*pred_month2,
           pred_day_lwr=pred_cumshare_daily_lwr*pred_month2_lwr,
           pred_day_upper=pred_cumshare_daily_upper*pred_month2_upper,
           final_pred_day=NA,
           final_pred_day_lwr=NA,
           final_pred_day_upper=NA,
           final_pred_day_cum=NA,
           final_pred_day_cum_lwr=NA,
           final_pred_day_cum_upper=NA) %>% 
    group_by(date) %>% 
    mutate(pred_day_cum=pred_day,
           pred_day_cum_lwr=pred_day_lwr,
           pred_day_cum_upper=pred_day_upper,
           pred_day=case_when(
             record_date==min(record_date)~pred_day,
             TRUE~pred_day-dplyr::lag(pred_day,1)
           ),
           pred_day_lwr=case_when(
             record_date==min(record_date)~pred_day_lwr,
             TRUE~pred_day_lwr-dplyr::lag(pred_day_lwr,1)
           ),
           pred_day_upper=case_when(
             record_date==min(record_date)~pred_day_upper,
             TRUE~pred_day_upper-dplyr::lag(pred_day_upper,1)
           ),
           final_pred_day=ifelse(is.na(final_pred_day),pred_day,final_pred_day),
           final_pred_day_lwr=ifelse(is.na(final_pred_day_lwr),pred_day_lwr,final_pred_day_lwr),
           final_pred_day_upper=ifelse(is.na(final_pred_day_upper),pred_day_upper,final_pred_day_upper),
           final_pred_day_cum=ifelse(is.na(final_pred_day_cum),pred_day_cum,final_pred_day_cum),
           final_pred_day_cum_lwr=ifelse(is.na(final_pred_day_cum_lwr),pred_day_cum_lwr,final_pred_day_cum_lwr),
           final_pred_day_cum_upper=ifelse(is.na(final_pred_day_cum_upper),pred_day_cum_upper,final_pred_day_cum_upper)) %>% 
    mutate(pred_month1=pred_month2,
           pred_month1_lwr=pred_month2_lwr,
           pred_month1_upper=pred_month2_upper,
           cbo_category=col,
           cbo_proj=cbo_pred_month) %>% 
    select(any_of(c(colnames(daily_df)))) %>% 
    ungroup()
  
  daily_df = bind_rows(daily_df,daily_df2)
  
  if(nrow(overlays_daily)>0){
    
    daily_df = daily_df %>% 
      mutate(up_diff=final_pred_day_upper-final_pred_day,
             down_diff=final_pred_day_lwr-final_pred_day) %>% 
      left_join(overlays_daily %>% select(date,overlay_value=value)) %>% 
      group_by(date) %>% 
      mutate(final_pred_day=ifelse(is.na(overlay_value),final_pred_day,overlay_value),
             final_pred_day_lwr=ifelse(is.na(overlay_value),final_pred_day_lwr,overlay_value+down_diff),
             final_pred_day_upper=ifelse(is.na(overlay_value),final_pred_day_upper,overlay_value+up_diff),
             final_pred_day_cum=cumsum(final_pred_day),
             final_pred_day_cum_lwr=cumsum(final_pred_day_lwr),
             final_pred_day_cum_upper=cumsum(final_pred_day_upper)) %>% 
      select(-c(up_diff,down_diff,overlay_value))
    
  }
  
  return(list(daily_df=daily_df,nowcast=monthly_nowcast))
  
}

nowcast_daily_budget_outlay = function(dts,mts_dataset,end_date,col,col_mts,testing=NA){
  
  models_daily = readRDS(paste0("Data/Processing/Models/nowcast_daily_",col,".RDS"))
  models_monthly = readRDS(paste0("Data/Processing/Models/nowcast_",col,".RDS"))
  
  overlays = overlay_df %>% 
    filter(date_active<=end_date&(is.na(date_inactive)|date_inactive<end_date)&category==col)
  
  overlays_daily = overlay_daily_df %>% 
    filter(date_active<=end_date&(is.na(date_inactive)|date_inactive<end_date)&category==col)
  
  # get monthly predicted share at outset to avoid copying code over and over
  monthly_share_pred = data.frame(date=seq.Date(floor_date(min(dts$record_date),"month"),
                                                as.Date(paste0(max(cbo_proj$projected_fiscal_year[cbo_proj$baseline_date<=end_date]),"-09-01")),
                                                by="month")) %>% 
    mutate(month=month(date),
           fy_month=case_when(
             month%in%c(10:12)~month-9,
             month%in%c(1:9)~month+3
           ),
           fed_remittances_suspended=ifelse(date>="2022-09-01",1,0)) %>%  # keep this activated unless they go back to a low interest environment, but given the path of interest payments, unlikely to ever happen
    mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10))) %>% 
    rowwise() %>% 
    mutate(first_day_thismonth_weekend=(weekdays(date,abbreviate=TRUE)%in%c("Sat","Sun")|date%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=1,
           first_day_nextmonth_weekend=(weekdays(date %m+% months(1),abbreviate=TRUE)%in%c("Sat","Sun")|(date %m+% months(1))%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12,
           last_day_thismonth_weekend=(weekdays((date %m+% months(1) )- 1,abbreviate=TRUE)%in%c("Sat","Sun")|((date %m+% months(1) )- 1)%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12) %>% 
    ungroup() %>% 
    bind_cols(predict(models_monthly$share,.,interval="confidence",alpha=0.1) %>% 
                as.data.frame() %>% 
                mutate_all(~ifelse(is.nan(.),1,.)) %>% 
                rename(pred_cumshare=1,pred_cumshare_lwr=2,pred_cumshare_upper=3)) %>% 
    {if(col=="Medicare")  mutate(.,pred_cumshare=ifelse(fy_month==1,pred_cumshare,pred_cumshare-dplyr::lag(pred_cumshare,1)),
                                    pred_cumshare_lwr=ifelse(fy_month==1,pred_cumshare_lwr,pred_cumshare_lwr-dplyr::lag(pred_cumshare_lwr,1)),
                                    pred_cumshare_upper=ifelse(fy_month==1,pred_cumshare_upper,pred_cumshare_upper-dplyr::lag(pred_cumshare_upper,1))) %>%
        bind_cols(.,predict(models_monthly$scalar,.,interval="confidence",alpha=0.1) %>% 
                                     as.data.frame() %>% 
                                     rename(scalar=1,scalar_lwr=2,scalar_upper=3)) %>% 
        
        group_by(fiscal_year) %>% 
        mutate(pred_cumshare2=pred_cumshare*scalar,
               pred_cumshare2_lwr=pred_cumshare_lwr*scalar_lwr,
               pred_cumshare2_upper=pred_cumshare_upper*scalar_upper,
               pred_cumshare=cumsum(pred_cumshare2),
               pred_cumshare_lwr=cumsum(pred_cumshare2_lwr),
               pred_cumshare_upper=cumsum(pred_cumshare2_upper)) %>% 
        select(-c(pred_cumshare2:pred_cumshare2_upper)) else . } %>% 
    mutate_at(vars(pred_cumshare:pred_cumshare_upper),list(diff=~ifelse(fy_month==1,.,.-dplyr::lag(.,1)))) %>%
    rowwise() %>% 
    mutate(pred_cumshare_lwr=min(c(pred_cumshare_lwr_diff-pred_cumshare_diff,pred_cumshare_upper_diff-pred_cumshare_diff)),
           pred_cumshare_upper=max(c(pred_cumshare_lwr_diff-pred_cumshare_diff,pred_cumshare_upper_diff-pred_cumshare_diff))) %>% 
    ungroup() %>% 
    select(-c(pred_cumshare_diff:pred_cumshare_upper_diff)) %>% 
    group_by(fiscal_year) %>% 
    mutate(pred_cumshare=pred_cumshare/pred_cumshare[n()]) %>% 
    ungroup()
  
  monthly_df = get_monthly_shares_df_spending(col_mts,col) %>% 
    arrange(date) %>% 
    mutate(actual=value)
  
  MAX_DATE = ifelse(!is.na(testing),testing,as.character(max(dts$record_date,floor_date(end_date,"month"))))
  
  monthly_df = monthly_df %>% 
    mutate_at(vars(value,total,share,cum_total,cum_share),~ifelse(date>=floor_date(as.Date(MAX_DATE),"month"),NA,.)) %>% 
    mutate(actual=value) 
  
  tmp = data.frame(date=seq.Date(min(monthly_share_pred$date),min(monthly_df$date)-1,by="1 month")) %>% 
    mutate(month=month(date),
           fy_month=case_when(
             month%in%c(10:12)~month-9,
             month%in%c(1:9)~month+3
           ),
           fed_remittances_suspended=ifelse(date>="2022-09-01",1,0)) %>%  # keep this activated unless they go back to a low interest environment, but given the path of interest payments, unlikely to ever happen
    mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10))) %>% 
    rowwise() %>% 
    mutate(first_day_thismonth_weekend=(weekdays(date,abbreviate=TRUE)%in%c("Sat","Sun")|date%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=1,
           first_day_nextmonth_weekend=(weekdays(date %m+% months(1),abbreviate=TRUE)%in%c("Sat","Sun")|(date %m+% months(1))%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12,
           last_day_thismonth_weekend=(weekdays((date %m+% months(1) )- 1,abbreviate=TRUE)%in%c("Sat","Sun")|((date %m+% months(1) )- 1)%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12) %>% 
    ungroup() %>% 
    left_join(cbo_actual %>% 
                {if(col=="Other Spending") filter(.,subcategory %in% c("Nondefense Discretionary","Other Mandatory")) else if(col=="National Defense") filter(.,subcategory=="Defense Discretionary") else filter(.,subcategory%in%col)} %>% 
                group_by(fiscal_year) %>% 
                summarize(value=sum(actual_value,na.rm=TRUE)) %>% 
                select(fiscal_year,value)) %>% 
    mutate(cbo_proj=predict(models_monthly$share,.)) %>% 
    {if(col=="Medicare")  mutate(.,cbo_proj=ifelse(fy_month==1,cbo_proj,cbo_proj-dplyr::lag(cbo_proj,1))) %>%
        bind_cols(.,predict(models_monthly$scalar,.) %>% 
                    as.data.frame() %>% 
                    rename(scalar=1)) %>% 
        group_by(fiscal_year) %>% 
        mutate(cbo_proj2=cbo_proj*scalar,
               cbo_proj=cumsum(cbo_proj)) %>% 
        select(-c(cbo_proj2)) else . } %>% 
    group_by(fiscal_year) %>% 
    mutate(cbo_proj=ifelse(fy_month==1,cbo_proj,cbo_proj-dplyr::lag(cbo_proj,1)),
           tmp=cbo_proj/sum(cbo_proj),
           num=n(),
           cbo_proj=ifelse(num==12,tmp,cbo_proj)) %>% 
    select(-c(tmp,num)) %>% 
    mutate(total=value,
           value=cbo_proj*total,
           share=cbo_proj,
           actual=value,
           error=1,
           error_ly=1,
           cum_total=cumsum(value),
           cum_share=cumsum(share)) %>% 
    left_join(cbo_proj %>% 
                {if(col=="Other Spending") filter(.,subcategory %in% c("Nondefense Discretionary","Other Mandatory")) else if(col=="National Defense") filter(.,subcategory=="Defense Discretionary") else filter(.,subcategory%in%col)} %>% 
                group_by(projected_fiscal_year,subcategory) %>% 
                filter(baseline_date<=MAX_DATE) %>% 
                slice(n()) %>% 
                group_by(projected_fiscal_year) %>% 
                summarize(value=sum(value,na.rm=TRUE)) %>% 
                select(projected_fiscal_year,value) %>% 
                rename(cbo_proj=value,
                       fiscal_year=projected_fiscal_year)) 
  
  monthly_df = bind_rows(monthly_df,tmp) %>% 
    arrange(date) %>% 
    fill(error,error_ly,.direction="down")
  
  refund_share = bind_rows(receipts %>% 
                             filter(grepl("Total -- Individual Income Taxes",classification_desc)) %>% 
                             select(record_date,refund_amt=current_month_refund_amt) %>% 
                             mutate(var="Non-refundable",refund_amt=as.numeric(refund_amt)),
                           outlays %>% filter(grepl("Payment Where|Refund|Build America",classification_desc)|
                                                (parent_id%in%outlays$classification_id[outlays$classification_desc=="Internal Revenue Service:"]&classification_desc=="Other")) %>% 
                             select(record_date,refund_amt=current_month_net_outly_amt) %>% 
                             group_by(record_date) %>% 
                             summarize(refund_amt=sum(as.numeric(refund_amt)),var="Refundable")) %>% 
    group_by(record_date) %>% 
    mutate(share=refund_amt/sum(refund_amt,na.rm=TRUE),
           share=ifelse(is.na(share),0,share),
           fiscal_year=as.integer(quarter(record_date, with_year = TRUE, fiscal_start = 10)),
           month=month(record_date)) %>% 
    mutate(tax_due=case_when(
      !(fiscal_year%in%c(2020,2021))&month==4~1,
      fiscal_year==2020&month==7~1,
      fiscal_year==2021&month==5~1,
      TRUE~0
    ),
    quarter_end=ifelse(month%in%c(1,4,6,9),1,0)) %>% 
    filter(record_date<=floor_date(as.Date(MAX_DATE),"month")) 
  
  daily_df = dts %>% 
    filter(((cbo_category%in%col&!grepl("from Depositaries",transaction_catg))|(col=="Other Spending"&grepl("Individual Tax Refunds|Tax Refunds Individual",transaction_catg)))&record_date<=MAX_DATE) %>%
    { if(col=="Other Spending")     mutate(.,date=floor_date(record_date,"month")) %>% 
        left_join(refund_share %>% filter(var=="Refundable") %>% select(date=record_date,refund_share=share) %>% mutate(date=floor_date(date,"month"))) %>% 
    mutate(fiscal_year=as.integer(quarter(record_date, with_year = TRUE, fiscal_start = 10)),
           month=month(record_date)) %>% 
    mutate(tax_due=case_when(
      !(fiscal_year%in%c(2020,2021))&month==4~1,
      fiscal_year==2020&month==7~1,
      fiscal_year==2021&month==5~1,
      TRUE~0
    ),
    quarter_end=ifelse(month%in%c(1,4,6,9),1,0)) %>% 
    mutate(refund_share=case_when(
      is.na(refund_share)~predict(models_daily$refund_reg,.)$predictions,
      TRUE~refund_share
    ),
    transaction_today_amt=case_when(
      grepl("Individual Tax Refunds|Tax Refunds Individual",transaction_catg)~transaction_today_amt*refund_share,
      TRUE~transaction_today_amt
    )) %>% # refundable tax credit is counted in Other Spending (under IRS subheading)
    select(-c(quarter_end,tax_due,fiscal_year)) else .} %>% 
    mutate(cbo_category=col) %>% 
    group_by(record_fiscal_year,record_calendar_month,record_calendar_day) %>% 
    summarize(record_date=record_date[1],
              cbo_category=cbo_category[1],
              total_day=sum(transaction_today_amt/1000,na.rm=TRUE)) %>% 
    mutate(total_day_lwr=total_day,
           total_day_upper=total_day) %>% 
    ungroup() %>% 
    arrange(record_date) %>% 
    complete(record_date = seq.Date(floor_date(record_date[1],"month"), as.Date(MAX_DATE), by = "day")) %>% 
    mutate(record_fiscal_year=as.integer(quarter(record_date, with_year = TRUE, fiscal_start = 10)),
           record_calendar_month=month(record_date),
           record_calendar_year=year(record_date),
           record_calendar_day=sprintf("%02d", day(record_date))) %>% 
    mutate_at(vars(total_day,total_day_lwr,total_day_upper),~case_when(!is.na(.)~.,
                                                                 record_date<=as.Date(MAX_DATE)&is.na(.)~0,
                                                                 record_date>as.Date(MAX_DATE)~NA)) %>% 
    fill(cbo_category,.direction="downup") %>% 
    mutate(date=floor_date(record_date,"month")) %>% 
    mutate_at(vars(total_day:total_day_upper),~.*-1)  # put it in positive terms
  
  x_data = read_csv(paste0("Data/Processing/imputed_data/imputed_data_asof",end_date,".csv"))  %>% 
    select(-any_of(paste0("gt_",bad_vars$category))) %>% 
    arrange(date) %>%
    ungroup() %>% 
    mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10),.funs=list(ch12m=~((./dplyr::lag(.,12)-1)*100),ch1m=~((./dplyr::lag(.,1)-1)*100))) %>%
    mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,grep("gt_",colnames(.),value=TRUE)),.funs=list(ch12m=~.-dplyr::lag(.,12),ch1m=~.-dplyr::lag(.,1))) %>%
    mutate_at(vars(PAYEMS:gt_999_ch1m),.funs=list(lag1=~dplyr::lag(.,1),lag2=~dplyr::lag(.,2),lag3=~dplyr::lag(.,3),lag4=~dplyr::lag(.,4))) %>% 
    left_join(monthly_df,by="date") %>% 
    mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10))) %>% 
    mutate(month=month(date),
           fy_month=case_when(
             month%in%c(10:12)~month-9,
             month%in%c(1:9)~month+3
           )) %>% 
    select(-cbo_proj) %>% 
    left_join(cbo_proj %>% 
                {if(col=="Other Spending") filter(.,subcategory %in% c("Nondefense Discretionary","Other Mandatory")) else if(col=="National Defense") filter(.,subcategory=="Defense Discretionary") else filter(.,subcategory%in%col)} %>% 
                group_by(projected_fiscal_year,subcategory) %>% 
                filter(baseline_date<=MAX_DATE) %>% 
                slice(n()) %>% 
                group_by(projected_fiscal_year) %>% 
                summarize(value=sum(value,na.rm=TRUE)) %>% 
                select(projected_fiscal_year,value) %>% 
                rename(cbo_proj=value,
                       fiscal_year=projected_fiscal_year)) %>% 
    ungroup() %>% 
    rowwise() %>% 
    mutate(first_day_thismonth_weekend=(weekdays(date,abbreviate=TRUE)%in%c("Sat","Sun")|date%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=1,
           first_day_nextmonth_weekend=(weekdays(date %m+% months(1),abbreviate=TRUE)%in%c("Sat","Sun")|(date %m+% months(1))%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12,
           last_day_thismonth_weekend=(weekdays((date %m+% months(1) )- 1,abbreviate=TRUE)%in%c("Sat","Sun")|((date %m+% months(1) )- 1)%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12) %>% 
    ungroup() %>% 
    mutate(value_lwr=value,
           value_upper=value)
  
  dates = x_data %>% filter(date<=MAX_DATE&date>max(monthly_df$date[!is.na(monthly_df$value)])) %>% arrange(date) %>% pull(date)
  fys = unique(as.integer(quarter(dates, with_year = TRUE, fiscal_start = 10)))
  
  for(dat in as.character(dates)){
    
    x_data1 = x_data %>% 
      filter(date<=dat) %>% 
      left_join(monthly_share_pred %>% select(date,pred_cumshare:pred_cumshare_upper),by="date")
    
    x_data1 = x_data1 %>% 
      group_by(fiscal_year) %>% 
      mutate(cum_total=cumsum(value),
             cum_total_lwr=cumsum(value_lwr),
             cum_total_upper=cumsum(value_upper)) %>% 
      ungroup()
    
    x_data1$pred_total = x_data1$cum_total/x_data1$pred_cumshare
    # x_data1$pred_total_lwr = x_data1$cum_total_lwr/x_data1$pred_cumshare_upper
    # x_data1$pred_total_upper = x_data1$cum_total_upper/x_data1$pred_cumshare_lwr
    x_data1  = x_data1 %>% 
      group_by(fiscal_year) %>% 
      fill(pred_total,.direction="down")
    
    if(tail(x_data1$fiscal_year,1)!=fys[1]){
      
      scalar = x_data1 %>% 
        ungroup() %>% 
        filter(fiscal_year==fys[1]) %>% 
        mutate(cbo_pred_month=cbo_proj*pred_cumshare,
               cbo_pred_month=case_when(fy_month==1~cbo_pred_month,
                                        TRUE~cbo_pred_month-dplyr::lag(cbo_pred_month,1))) %>% 
        select(date,actual,cbo_pred_month) %>% 
        summarize(num=mean(actual/cbo_pred_month,na.rm=TRUE)) %>% 
        pull(num)
      
      x_data1 = x_data1 %>% 
        mutate(cbo_proj=case_when(fiscal_year==tail(x_data1$fiscal_year,1)~cbo_proj*scalar,
                                  TRUE~cbo_proj))
      
    }else{
      scalar=1
    }
    
    x_data1 = x_data1 %>% 
      group_by(fiscal_year) %>% 
      mutate_at(vars(pred_total),~.*(max(c(tail(fy_month[!is.na(value)&fiscal_year>=fys[1]],1),0),na.rm=TRUE)/12)) %>% 
      mutate(cbo_proj_month=cbo_proj*(1-max(c(tail(fy_month[!is.na(value)&fiscal_year>=fys[1]],1),0),na.rm=TRUE)/12)) %>% 
      rowwise() %>% 
      mutate_at(vars(pred_total),~sum(c(.,cbo_proj_month),na.rm=TRUE)) %>% 
      ungroup() %>% 
      mutate(final_pred_month=pred_total*pred_cumshare)
    
    x_data1 = x_data1 %>% 
      mutate(cbo_proj_month=cbo_proj*pred_cumshare) %>% 
      group_by(fiscal_year) %>% 
      mutate_at(vars(final_pred_month,cbo_proj_month),~case_when(fy_month==1~.,TRUE~.-dplyr::lag(.,1))) %>% 
      ungroup() %>% 
      mutate(final_pred_month_lwr=final_pred_month+pred_cumshare_lwr*pred_total,
             final_pred_month_upper=final_pred_month+pred_cumshare_upper*pred_total)
    
    
    if(col=="Social Security"&month(as.Date(dat))%in%c(12,1)){
      
      if(month(as.Date(dat))==12&weekdays(as.Date(dat) %m+% months(1),abbreviate = TRUE)=="Fri"){
        
        x_data1 = x_data1 %>% 
          mutate_at(vars(final_pred_month,final_pred_month_lwr,final_pred_month_upper,cbo_proj_month),~.+.*0.3579050)
        
      }
      
      if(month(as.Date(dat))==1&weekdays(as.Date(dat),abbreviate = TRUE)=="Fri"){
        
        x_data1 = x_data1 %>% 
          mutate_at(vars(final_pred_month,final_pred_month_lwr,final_pred_month_upper,cbo_proj_month),~.-.*0.3579050)
        
      }
      
    }
    
    if(nrow(overlays)>0){
      
      x_data1 = x_data1 %>% 
        mutate(up_diff=final_pred_month_upper-final_pred_month,
               down_diff=final_pred_month_lwr-final_pred_month) %>% 
        left_join(overlays %>% select(date,overlay_value=value)) %>% 
        mutate(final_pred_month=ifelse(is.na(overlay_value),final_pred_month,overlay_value),
               final_pred_month_lwr=ifelse(is.na(overlay_value),final_pred_month_lwr,overlay_value+down_diff),
               final_pred_month_upper=ifelse(is.na(overlay_value),final_pred_month_upper,overlay_value+up_diff)) %>% 
        select(-c(up_diff,down_diff,overlay_value))
      
    }
    
    tmp = predict(models_monthly$res_shrunk,x_data1 %>% filter(date==dat) %>% mutate(cbo_proj_month=final_pred_month),se.fit=TRUE)
    x_data$value[x_data$date==dat] = tmp$fit
    tmp = predict(models_monthly$res_shrunk,x_data1 %>% filter(date==dat) %>% mutate(cbo_proj_month=final_pred_month_lwr),se.fit=TRUE)
    x_data$value_lwr[x_data$date==dat] = tmp$fit - 1.64*tmp$se.fit
    tmp = predict(models_monthly$res_shrunk,x_data1 %>% filter(date==dat) %>% mutate(cbo_proj_month=final_pred_month_upper),se.fit=TRUE)
    x_data$value_upper[x_data$date==dat] = tmp$fit + 1.64*tmp$se.fit
    x_data$cbo_proj_month=NA
    x_data$cbo_proj_month[x_data$date<=dat] = x_data1$cbo_proj_month
    
    monthly_nowcast = x_data %>% filter(date<=dat) %>% select(date,actual,pred=value,fit.lwr=value_lwr,fit.upr=value_upper,cbo_proj=cbo_proj_month)

    if((max(daily_df$record_date[!is.na(daily_df$total_day)],na.rm=TRUE))<(ceiling_date(as.Date(dat),"month")-1)){ # testing if we have the last day of the month. If we have the last day of the month then we dont need to add the missing days
      
      daily_df = daily_df %>% 
        ungroup() %>% 
        complete(record_date = seq.Date(min(record_date), (ceiling_date(as.Date(dat),"month")-1), by = "day")) %>% 
        mutate(record_fiscal_year=as.integer(quarter(record_date, with_year = TRUE, fiscal_start = 10)),
               record_calendar_month=month(record_date),
               record_calendar_year=year(record_date),
               record_calendar_day=sprintf("%02d", day(record_date)),
               date=floor_date(record_date,"month")) %>% 
        mutate_at(vars(total_day,total_day_lwr,total_day_upper),~case_when(!is.na(.)~.,
                                                         record_date<=MAX_DATE&is.na(.)~0,
                                                         record_date>MAX_DATE~NA)) %>% 
        fill(cbo_category,.direction="down")
      
    }
    
    daily_df1 = daily_df %>% 
      filter(record_date<=(ceiling_date(as.Date(dat),"month")-1)) %>% 
      mutate(imputed=ifelse(record_date<=MAX_DATE&record_date>=head(record_date[total_day!=0],1),0,1)) %>% 
      group_by(record_fiscal_year,record_calendar_month) %>% 
      arrange(record_calendar_day) %>% 
      mutate(cum_total_day=cumsum(total_day),
             cum_total_day_lwr=cumsum(total_day_lwr),
             cum_total_day_upper=cumsum(total_day_upper),
             total_month=sum(total_day,na.rm=TRUE),
             total_month_lwr=sum(total_day_lwr,na.rm=TRUE),
             total_month_upper=sum(total_day_upper,na.rm=TRUE),
             record_calendar_day_perc=(as.numeric(record_calendar_day))/as.numeric(days_in_month(record_date)),
             inv_record_calendar_day=1-record_calendar_day_perc) %>% 
      mutate(fy_month=case_when(
        record_calendar_month%in%c(10:12)~record_calendar_month-9,
        record_calendar_month%in%c(1:9)~record_calendar_month+3
      )) %>% 
      group_by(record_fiscal_year) %>% 
      arrange(fy_month) %>% 
      mutate(cum_total_month=cumsum(total_day),
             cum_total_month_lwr=cumsum(total_day_lwr),
             cum_total_month_upper=cumsum(total_day_upper),
             total_year=sum(total_month),
             total_year_lwr=sum(total_month_lwr),
             total_year_upper=sum(total_month_upper)) %>% 
      ungroup() %>% 
      mutate(date=floor_date(record_date,"month")) %>% 
      left_join(monthly_nowcast,by="date") %>% 
      arrange(record_date) %>% 
      left_join(tax_days,by=c("record_date"="date")) %>% 
      mutate(cum_share=cum_total_day/total_month,
             cum_share_lwr=cum_total_day_lwr/total_month_lwr,
             cum_share_upper=cum_total_day_upper/total_month_upper,
             quarter_end=case_when(
               record_calendar_month==4&tax_day==1~1,
               record_calendar_month%in%c(1,6,9)&record_calendar_day==15&!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))~1,
               record_calendar_month%in%c(1,6,9)&record_calendar_day%in%c(16,17)&(weekdays(record_date,abbreviate = TRUE)%in%c("Mon"))~1 # only use 16 or 17 IF the 15th had fallen on a weekend
             )) %>% 
      group_by(date) %>% 
      fill(tax_day,quarter_end,.direction="down") %>% 
      mutate(tax_day=ifelse(is.na(tax_day),0,tax_day),
             quarter_end=ifelse(is.na(quarter_end),0,quarter_end),
             settlement_period=case_when(
               record_date==max(record_date[!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))])~"EOM",
               record_date==min(record_date[day(record_date)>=15&!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))])~"Second Settlement",
               record_date==min(record_date[day(record_date)<=7&!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))])~"First Settlement",
               TRUE~"Regular Day"
             )) %>% 
      group_by(date) %>% 
      mutate(weekend=weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"),
             dotw=weekdays(record_date,abbreviate=TRUE),
             holiday=record_date%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"),
             ssi_day=case_when(
               record_calendar_day=="01"&!weekend&!holiday~1,
               (weekdays(date%m+%months(1),abbreviate = TRUE)%in%c("Sat","Sun")|(date%m+%months(1))%in%as.Date(as.character(tis::holidays(year(date%m+%months(1)))),,format="%Y%m%d"))&record_date==last(record_date[!weekend&!holiday])~1,
               TRUE~0
             ),
             ss_ssi_day=case_when(
               weekdays(date%m+%months(1),abbreviate=TRUE)=="Fri"&record_calendar_month==12&record_calendar_day=="31"~1,
               record_calendar_day=="03"&!weekend&!holiday~1,
               weekend[record_calendar_day=="03"]==TRUE&record_date==last(record_date[!weekend&!holiday&day(record_date)<3])~1,
               TRUE~0
             ),
             ss_day=case_when(
               record_date%in%record_date[dotw=="Wed"][2:4]&!weekend&!holiday~1,
               record_date%in%(as.Date(intersect(as.character(record_date[dotw=="Wed"][2:4]),as.character(record_date[holiday]))) %m-% days(1))~1,
               TRUE~0
             )) %>% 
      ungroup()
    
    daily_df1 = daily_df1 %>% 
      left_join(daily_df1 %>% 
                  distinct(date) %>% 
                  mutate(dat=1:n(),
                         month=month(date)) %>% 
                  rowwise() %>% 
                  mutate(first_day_thismonth_weekend=(weekdays(date,abbreviate=TRUE)%in%c("Sat","Sun")|date%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=1,
                         first_day_nextmonth_weekend=(weekdays(date %m+% months(1),abbreviate=TRUE)%in%c("Sat","Sun")|(date %m+% months(1))%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12,
                         last_day_thismonth_weekend=(weekdays((date %m+% months(1) )- 1,abbreviate=TRUE)%in%c("Sat","Sun")|((date %m+% months(1) )- 1)%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12) %>% 
                  ungroup() %>% 
                  mutate(date_group=case_when(
                    date<="2020-03-01"~"Before 2020-4",
                    date<="2023-11-01"~"Before 2023-12",
                    date>"2023-11-01"~"After 2023-12"
                  ))) # for the scalar reg
    
    daily_df1 = daily_df1 %>% 
      bind_cols(data.frame(predict(models_daily$share,
                                   data=.,type="quantiles",
                                   quantiles=c(0.5,.1,.9))) %>% 
                  rename("pred_share"=1,"pred_share_lwr"=2,"pred_share_upper"=3)) %>% 
      group_by(date) %>% 
      mutate(pred_cumshare=cumsum(pred_share),
             pred_cumshare_lwr=cumsum(pred_share_lwr),
             pred_cumshare_upper=cumsum(pred_share_upper),
             row=1:n()) %>% 
      mutate_at(vars(pred_cumshare:pred_cumshare_upper),~case_when(row>=max(row[weekend==FALSE])&.<=0~1,
                                                                   TRUE~.)) %>% 
      mutate_at(vars(pred_cumshare:pred_cumshare_upper),~./.[n()]) %>% 
      mutate(month=month(record_date)) %>% 
      bind_cols(data.frame(predict(models_daily$scalar,
                                   newdata = .,
                                   se.fit=TRUE, 
                                   interval="confidence", 
                                   alpha=0.10)) %>% 
                  rename("scalar"=1,"scalar_lwr"=2,"scalar_upper"=3,"scalar_se_fit"=4)) %>% 
      select(-dat)
    
    daily_df1 = daily_df1 %>%
      group_by(date) %>%
      mutate(pred_month_total=cum_total_day/pred_cumshare*scalar,
             pred_month_total=ifelse(is.nan(pred_month_total)|is.infinite(pred_month_total)|pred_month_total==0,pred,pred_month_total),
             pred_month_total_lwr=cum_total_day_lwr/pred_cumshare_lwr*scalar_lwr,
             pred_month_total_lwr=ifelse(is.nan(pred_month_total_lwr)|is.infinite(pred_month_total_lwr)|pred_month_total_lwr==0,fit.lwr,pred_month_total_lwr),
             pred_month_total_upper=cum_total_day_upper/pred_cumshare_upper*scalar_upper,
             pred_month_total_upper=ifelse(is.nan(pred_month_total_upper)|is.infinite(pred_month_total_upper)|pred_month_total_upper==0,fit.upr,pred_month_total_upper)) %>% 
      ungroup() %>% 
      mutate_at(vars(pred_month_total,pred_month_total_lwr,pred_month_total_upper),~ifelse(.<(min(daily_df1$actual,na.rm=TRUE)-(5*sd(daily_df1$actual,na.rm=TRUE))),min(daily_df1$actual,na.rm=TRUE)-(5*sd(daily_df1$actual,na.rm=TRUE)),.)) %>% 
      mutate_at(vars(pred_month_total,pred_month_total_lwr,pred_month_total_upper),~ifelse(.>(max(daily_df1$actual,na.rm=TRUE)+(5*sd(daily_df1$actual,na.rm=TRUE))),max(daily_df1$actual,na.rm=TRUE)+(5*sd(daily_df1$actual,na.rm=TRUE)),.))
    
    # if(col%in%c("Individual Income Taxes","Payroll Taxes")){
    #   
    #   preds = data.frame(predict(models_daily$disagg_reg,daily_df %>% mutate(month=month(record_date)),se.fit=TRUE, interval="confidence", alpha=0.10))
    #   colnames(preds)=c("scalar_adj","scalar_lwr_adj","scalar_upper_adj","scalar_adj_se_fit")
    #   daily_df = bind_cols(daily_df,preds) %>% 
    #     ungroup() %>% 
    #     mutate(scalar_lwr=ifelse(col=="Individual Income Taxes",scalar_lwr*scalar_lwr_adj,scalar_lwr*(1-scalar_upper_adj)),
    #            scalar_upper=ifelse(col=="Individual Income Taxes",scalar_upper*scalar_upper_adj,scalar_upper*(1-scalar_lwr_adj)),
    #            scalar=ifelse(col=="Individual Income Taxes",scalar*scalar_adj,scalar*(1-scalar_adj)))
    #   
    # }
    # 
    if(col%in%c("Other Spending","Net Interest")){
      
      daily_df1 = daily_df1 %>% 
        group_by(date) %>%
        fill(pred_month_total,pred_month_total_lwr,pred_month_total_upper,.direction="down") %>%
        mutate(pred_total1=pred,
               pred_total1_lwr=fit.lwr,
               pred_total1_upper=fit.upr)
      
    } else{
      
      daily_df1 = daily_df1 %>% 
        group_by(date) %>% 
        mutate(pred_total1=pred_month_total*record_calendar_day_perc+pred*(1-record_calendar_day_perc),
               pred_total1_lwr=pred_month_total_lwr*record_calendar_day_perc+fit.lwr*(1-record_calendar_day_perc),
               pred_total1_upper=pred_month_total_upper*record_calendar_day_perc+fit.upr*(1-record_calendar_day_perc),
               pred_total1=case_when(MAX_DATE<"2015-10-01"~pred,
                                     is_bad(pred_total1)&is_bad(pred_month_total)&!is_bad(pred)~pred,
                                     is_bad(pred_total1)&!is_bad(pred_month_total)&is_bad(pred)~pred_month_total,
                                     TRUE~pred_total1),
               pred_total1_lwr=case_when(MAX_DATE<"2015-10-01"~pred,
                                         is_bad(pred_total1_lwr)&is_bad(pred_month_total_lwr)&!is_bad(fit.lwr)~fit.lwr,
                                         is_bad(pred_total1_lwr)&!is_bad(pred_month_total_lwr)&is_bad(fit.lwr)~pred_month_total_lwr,
                                         TRUE~pred_total1_lwr),
               pred_total1_upper=case_when(MAX_DATE<"2015-10-01"~pred,
                                           is_bad(pred_total1_upper)&is_bad(pred_month_total_upper)&!is_bad(fit.upr)~fit.upr,
                                           is_bad(pred_total1_upper)&!is_bad(pred_month_total_upper)&is_bad(fit.upr)~pred_month_total_upper,
                                           TRUE~pred_total1_upper)) %>% 
        fill(pred_month_total,pred_month_total_lwr,pred_month_total_upper,pred_total1,pred_total1_lwr,pred_total1_upper,.direction="down")
      
    }
    
    if(nrow(overlays)>0){
      
      daily_df1 = daily_df1 %>% 
        mutate(up_diff=pred_total1_upper-pred_total1,
               down_diff=pred_total1_lwr-pred_total1) %>% 
        left_join(overlays %>% select(date,overlay_value=value)) %>% 
        mutate(pred_total1=ifelse(is.na(overlay_value),pred_total1,overlay_value),
               pred_total1_lwr=ifelse(is.na(overlay_value),pred_total1_lwr,overlay_value+down_diff),
               pred_total1_upper=ifelse(is.na(overlay_value),pred_total1_upper,overlay_value+up_diff)) %>% 
        select(-c(up_diff,down_diff,overlay_value))
      
    }
    
    daily_df1 = daily_df1 %>% 
      rowwise() %>% 
      mutate(min=min(c(pred_total1_lwr,pred_total1_upper)),
             max=max(c(pred_total1_lwr,pred_total1_upper)),
             pred_total1_lwr=min,
             pred_total1_upper=max) %>% 
      select(-c(min,max))
    
    # TODO: THINK ABOUT HOW TO GET CLOSER WHEN DISAGGREGATING
    # see if it improves things for any category at one date and with proper backtesting
    # if doesnt improve, just keep as a separate data series
    
    daily_df1 = daily_df1 %>% 
      group_by(date) %>% 
      mutate(final_pred_day_cum=case_when(
        all(!is.na(actual))&all(!is_bad(cum_share))~actual*cum_share, # distribute by observed pattern
        all(!is.na(actual))&!all(!is_bad(cum_share))~actual*pred_cumshare,
        date<dat&!(MAX_DATE<"2015-10-01")~cum_total_day,
        date<dat&(MAX_DATE<"2015-10-01")~pred_total1[n()]*pred_cumshare,
        imputed==0&!(col%in%c("Other Spending","Net Interest"))&!(MAX_DATE<"2015-10-01")~cum_total_day*scalar,
        imputed==1|col%in%c("Other Spending","Net Interest")~pred_total1[n()]*pred_cumshare
      ),
      final_pred_day_cum_lwr=case_when(
        all(!is.na(actual))&all(!is_bad(cum_share))~actual*cum_share, # distribute by observed pattern
        all(!is.na(actual))&!all(!is_bad(cum_share))~actual*pred_cumshare,
        date<dat&!(MAX_DATE<"2015-10-01")~cum_total_day_lwr,
        date<dat&(MAX_DATE<"2015-10-01")~pred_total1[n()]*pred_cumshare_lwr,
        imputed==0&!(col%in%c("Other Spending","Net Interest"))&!(MAX_DATE<"2015-10-01")~cum_total_day*scalar_lwr,
        imputed==1|col%in%c("Other Spending","Net Interest")~pred_total1_lwr[n()]*pred_cumshare_lwr
      ),
      final_pred_day_cum_upper=case_when(
        all(!is.na(actual))&all(!is_bad(cum_share))~actual*cum_share, # distribute by observed pattern
        all(!is.na(actual))&!all(!is_bad(cum_share))~actual*pred_cumshare,
        date<dat&!(MAX_DATE<"2015-10-01")~cum_total_day_upper,
        date<dat&(MAX_DATE<"2015-10-01")~pred_total1[n()]*pred_cumshare_upper,
        imputed==0&!(col%in%c("Other Spending","Net Interest"))&!(MAX_DATE<"2015-10-01")~cum_total_day*scalar_upper,
        imputed==1|col%in%c("Other Spending","Net Interest")~pred_total1_upper[n()]*pred_cumshare_upper
      )) %>% 
      select(record_date,record_fiscal_year,fy_month,imputed,total_day,scalar,total_month,
             date,pred,fit.lwr,fit.upr,actual,cbo_proj,scalar,
             intermediate_pred=pred_total1,intermediate_pred_lwr=pred_total1_lwr,intermediate_pred_upper=pred_total1_upper,
             final_pred_day_cum,final_pred_day_cum_lwr,final_pred_day_cum_upper) %>% 
      mutate(cbo_category=col,
             final_pred_day=case_when(
               record_date==record_date[1]~final_pred_day_cum,
               TRUE~final_pred_day_cum-dplyr::lag(final_pred_day_cum,1)
             ),
             final_pred_day_lwr=case_when(
               record_date==record_date[1]~final_pred_day_cum_lwr,
               TRUE~final_pred_day_cum_lwr-dplyr::lag(final_pred_day_cum_lwr,1)
             ),
             final_pred_day_upper=case_when(
               record_date==record_date[1]~final_pred_day_cum_upper,
               TRUE~final_pred_day_cum_upper-dplyr::lag(final_pred_day_cum_upper,1)
             ),
             total_day_imp=case_when(
               imputed==0~total_day*scalar,
               imputed==1~((final_pred_day_cum[n()]-(total_month[n()]*scalar[n()]))/sum(final_pred_day[imputed==1]))*final_pred_day
             )) %>% 
      ungroup() %>% 
      relocate(total_day_imp,.after=total_day) %>% 
      select(-scalar)
    
    daily_df[daily_df$date<=dat,c("total_day","total_day_lwr","total_day_upper")] = daily_df1 %>% filter(date<=dat) %>% select(final_pred_day:final_pred_day_upper)
    x_data[x_data$date==dat,c("value","value_lwr","value_upper")] = daily_df1 %>% filter(date==dat) %>% slice(n()) %>% select(final_pred_day_cum:final_pred_day_cum_upper)
    
  }
  
  daily_df = daily_df %>% 
    mutate(final_pred_day=total_day,
           final_pred_day_lwr=total_day_lwr,
           final_pred_day_upper=total_day_upper) %>% 
    left_join(daily_df1 %>% select(record_date,cbo_proj)) %>% 
    group_by(date) %>% 
    mutate(final_pred_day_cum=cumsum(final_pred_day),
           final_pred_day_cum_lwr=cumsum(final_pred_day_lwr),
           final_pred_day_cum_upper=cumsum(final_pred_day_upper)) %>% 
    ungroup()
  
  x_data = x_data  %>% 
    select(-c(value:cbo_proj))
  
  if(!(max(daily_df$record_date)==paste0(max(daily_df$record_fiscal_year),"-09-30"))){
    
    dates = seq(max(daily_df$record_date,na.rm=TRUE)+1,as.Date(paste0(max(daily_df$record_fiscal_year),"-09-30")),by=1)
    months = unique(month(dates))
    
    daily_df1 = daily_df %>% 
      bind_rows(data.frame(record_date=seq(max(daily_df$record_date,na.rm=TRUE)+1,max(dates),by=1))) %>% 
      group_by(record_fiscal_year) %>% 
      arrange(record_date) %>% 
      mutate(cum_total_fy=cumsum(final_pred_day),
             cum_total_fy_lwr=cumsum(final_pred_day_lwr),
             cum_total_fy_upper=cumsum(final_pred_day_upper),
             record_fiscal_year=as.integer(quarter(record_date, with_year = TRUE, fiscal_start = 10)),
             date=floor_date(record_date,"month"),
             month=month(record_date),
             fy_month=case_when(
               month%in%c(10:12)~month-9,
               month%in%c(1:9)~month+3
             ),
             imputed=ifelse(record_date>MAX_DATE,1,0)) %>% 
      left_join(tax_days,by=c("record_date"="date")) %>% 
      group_by(date) %>% 
      mutate(day=day(record_date),
             quarter_end=case_when(
               month==4&tax_day==1~1,
               month%in%c(1,6,9)&day==15&!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))~1,
               month%in%c(1,6,9)&day%in%c(16,17)&(weekdays(record_date,abbreviate = TRUE)%in%c("Mon"))~1 # only use 16 or 17 IF the 15th had fallen on a weekend
             )) %>% 
      group_by(date) %>% 
      fill(tax_day,quarter_end,.direction="down") %>% 
      mutate(tax_day=ifelse(is.na(tax_day),0,tax_day),
             quarter_end=ifelse(is.na(quarter_end),0,quarter_end),
             settlement_period=case_when(
               record_date>=max(record_date[!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))])~"EOM",
               record_date>=min(record_date[day(record_date)>=15&!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))])~"Second Settlement",
               TRUE~"First Settlement"
             )) %>% 
      ungroup() %>% 
      mutate(fiscal_year=as.integer(quarter(record_date, with_year = TRUE, fiscal_start = 10)),
             tax_due=case_when(
        !(fiscal_year%in%c(2020,2021))&month==4&col=="Individual Income Taxes"~1,
        fiscal_year==2020&month==7&col=="Individual Income Taxes"~1,
        fiscal_year==2021&month==5&col=="Individual Income Taxes"~1,
        !(fiscal_year%in%c(2020))&month==4&col=="Corporate Income Taxes"~1,
        fiscal_year==2020&month==7&col=="Corporate Income Taxes"~1,
        fiscal_year==2020&month==9&col=="Excise Taxes"~1,
        TRUE~0
      )) %>% 
      group_by(date) %>% 
      mutate(weekend=weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"),
             record_calendar_month=month(record_date),
             record_calendar_day=sprintf("%02d", day(record_date)),
             fed_remittances_suspended=ifelse(date>="2022-09-01",1,0),
             dotw=weekdays(record_date,abbreviate=TRUE),
             holiday=record_date%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"),
             ssi_day=case_when(
               record_calendar_day=="01"&!weekend&!holiday~1,
               (weekdays(date%m+%months(1),abbreviate = TRUE)%in%c("Sat","Sun")|(date%m+%months(1))%in%as.Date(as.character(tis::holidays(year(date%m+%months(1)))),,format="%Y%m%d"))&record_date==last(record_date[!weekend&!holiday])~1,
               TRUE~0
             ),
             ss_ssi_day=case_when(
               weekdays(date%m+%months(1),abbreviate=TRUE)=="Fri"&record_calendar_month==12&record_calendar_day=="31"~1,
               record_calendar_day=="03"&!weekend&!holiday~1,
               weekend[record_calendar_day=="03"]==TRUE&record_date==last(record_date[!weekend&!holiday&day(record_date)<3])~1,
               TRUE~0
             ),
             ss_day=case_when(
               record_date%in%record_date[dotw=="Wed"][2:4]&!weekend&!holiday~1,
               record_date%in%(as.Date(intersect(as.character(record_date[dotw=="Wed"][2:4]),as.character(record_date[holiday]))) %m-% days(1))~1,
               TRUE~0
             )) %>% 
      ungroup() %>% 
      fill(cbo_category,.direction = "down")
    
    daily_df1 = daily_df1 %>% 
      left_join(daily_df1 %>% 
                  distinct(date) %>% 
                  mutate(dat=1:n(),
                         month=month(date)) %>% 
                  rowwise() %>% 
                  mutate(first_day_thismonth_weekend=(weekdays(date,abbreviate=TRUE)%in%c("Sat","Sun")|date%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=1,
                         first_day_nextmonth_weekend=(weekdays(date %m+% months(1),abbreviate=TRUE)%in%c("Sat","Sun")|(date %m+% months(1))%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12,
                         last_day_thismonth_weekend=(weekdays((date %m+% months(1) )- 1,abbreviate=TRUE)%in%c("Sat","Sun")|((date %m+% months(1) )- 1)%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12) %>% 
                  ungroup() %>% 
                  mutate(date_group=case_when(
                    date<="2020-03-01"~"Before 2020-4",
                    date<="2023-11-01"~"Before 2023-12",
                    date>"2023-11-01"~"After 2023-12"
                  ))) # for the scalar reg
    
    daily_df1 = daily_df1 %>% 
      left_join(monthly_share_pred %>% select(date,
                                              pred_cumshare_fy=pred_cumshare,
                                              pred_cumshare_fy_lwr=pred_cumshare_lwr,
                                              pred_cumshare_fy_upper=pred_cumshare_upper))
    
    daily_df1 = daily_df1 %>% 
      left_join(cbo_proj %>% 
                  {if(col=="Other Spending") filter(.,subcategory %in% c("Nondefense Discretionary","Other Mandatory")) else if(col=="National Defense") filter(.,subcategory=="Defense Discretionary") else filter(.,subcategory%in%col)} %>% 
                  group_by(projected_fiscal_year,subcategory) %>% 
                  filter(baseline_date<=MAX_DATE) %>% 
                  slice(n()) %>% 
                  group_by(projected_fiscal_year) %>% 
                  summarize(value=sum(value,na.rm=TRUE)) %>% 
                  select(projected_fiscal_year,value) %>% 
                  rename(cbo_proj_fy=value,
                         fiscal_year=projected_fiscal_year),
                by=c("record_fiscal_year"="fiscal_year")) %>% 
      mutate(cbo_proj_fmonth = cbo_proj_fy*pred_cumshare_fy,
             proj_fy = cum_total_fy/pred_cumshare_fy) %>% 
      fill(proj_fy,.direction="down") %>% 
      group_by(date) %>% 
      mutate(record_calendar_day_perc=(day(record_date))/as.numeric(days_in_month(record_date)),
             total_pred=proj_fy) %>% 
      group_by(record_fiscal_year) %>% 
      fill(total_pred,.direction="down") %>% 
      mutate(pred_month=total_pred*pred_cumshare_fy)
    
    daily_df1 = daily_df1 %>% 
      select(-c(pred_month)) %>% 
      left_join(daily_df1 %>% 
                  group_by(date) %>% 
                  slice(n()) %>% 
                  ungroup() %>% 
                  mutate(pred_month=case_when(fy_month==1~pred_month,
                                              TRUE~pred_month-dplyr::lag(pred_month,1)),
                         pred_month_lwr=pred_month+pred_cumshare_fy_lwr*total_pred,
                         pred_month_upper=pred_month+pred_cumshare_fy_upper*total_pred,
                         cbo_pred_month=case_when(fy_month==1~cbo_proj_fmonth,
                                                  TRUE~cbo_proj_fmonth-dplyr::lag(cbo_proj_fmonth,1)),
                         cbo_pred_month_lwr=cbo_pred_month+pred_cumshare_fy_lwr*cbo_proj_fy,
                         cbo_pred_month_upper=cbo_pred_month+pred_cumshare_fy_upper*cbo_proj_fy) %>% 
                  select(date,pred_month,pred_month_lwr,pred_month_upper,cbo_pred_month,cbo_pred_month_lwr,cbo_pred_month_upper),by="date") %>% 
      {if(col=="Social Security") mutate_at(.,vars(pred_month,pred_month_lwr,pred_month_upper,cbo_pred_month,cbo_pred_month_lwr,cbo_pred_month_upper),
                                            ~case_when(month(date)==12&weekdays(date %m+% months(1),abbreviate = TRUE)=="Fri"~.+.*0.3579050,
                                                       month(date)==1&weekdays(date,abbreviate = TRUE)=="Fri"~.-.*0.3579050,
                                                       TRUE~.)) else .} %>% 
      mutate(record_calendar_day=sprintf("%02d", day(record_date)),
             record_calendar_month=month(record_date),
             record_calendar_year=year(record_date),
             record_fiscal_year=as.integer(quarter(record_date, with_year = TRUE, fiscal_start = 10))) %>% 
      ungroup() %>% 
      mutate(pred_month1=pred_month*(tail(daily_df1$fy_month[!is.na(daily_df1$total_day)],1)/12)+cbo_pred_month*(1-tail(daily_df1$fy_month[!is.na(daily_df1$total_day)],1)/12),
             pred_month1_lwr=pred_month_lwr*(tail(daily_df1$fy_month[!is.na(daily_df1$total_day)],1)/12)+cbo_pred_month_lwr*(1-tail(daily_df1$fy_month[!is.na(daily_df1$total_day)],1)/12),
             pred_month1_upper=pred_month_upper*(tail(daily_df1$fy_month[!is.na(daily_df1$total_day)],1)/12)+cbo_pred_month_upper*(1-tail(daily_df1$fy_month[!is.na(daily_df1$total_day)],1)/12))
    
    if(nrow(overlays)>0){
      daily_df1 = daily_df1 %>% 
        mutate(up_diff=pred_month1_upper-pred_month1,
               down_diff=pred_month1_lwr-pred_month1) %>% 
        left_join(overlays %>% select(date,overlay_value=value)) %>% 
        mutate(pred_month1=ifelse(is.na(overlay_value),pred_month1,overlay_value),
               pred_month1_lwr=ifelse(is.na(overlay_value),pred_month1_lwr,overlay_value+down_diff),
               pred_month1_upper=ifelse(is.na(overlay_value),pred_month1_upper,overlay_value+up_diff)) %>% 
        select(-c(up_diff,down_diff,overlay_value))
    }
    
    preds = data.frame(predict(models_daily$share,data=daily_df1,type="quantiles",quantiles=c(0.5,.1,.9)))
    colnames(preds)=c("pred_share","pred_share_lwr","pred_share_upper")
    daily_df1 = bind_cols(daily_df1,preds) %>% 
      group_by(date) %>% 
      mutate(pred_cumshare_daily=cumsum(pred_share),
             pred_cumshare_daily_lwr=cumsum(pred_share_lwr),
             pred_cumshare_daily_upper=cumsum(pred_share_upper),
             row=1:n()) %>% 
      mutate_at(vars(pred_cumshare_daily:pred_cumshare_daily_upper),~case_when(row>=max(row[weekend==FALSE])&.<=0~1,
                                                                   TRUE~.)) %>% 
      mutate_at(vars(pred_cumshare_daily:pred_cumshare_daily_upper),~./.[n()]) %>% 
      mutate_at(vars(pred_cumshare_daily:pred_cumshare_daily_upper),~ifelse(is_bad(.),0,.)) %>% 
      mutate(num=1:n()) %>% 
      mutate_at(vars(pred_cumshare_daily:pred_cumshare_daily_upper),~case_when(num==max(num)&.==0~1,
                                                                               TRUE~.)) %>% 
      select(-num)
    
    preds = data.frame(predict(models_daily$scalar,daily_df1,se.fit=TRUE, interval="confidence", alpha=0.10))
    colnames(preds)=c("scalar","scalar_lwr","scalar_upper","scalar_se_fit")
    daily_df1 = bind_cols(daily_df1,preds)
    
    daily_df1 = daily_df1 %>% 
      mutate(pred_day=pred_cumshare_daily*pred_month1,
             pred_day_lwr=pred_cumshare_daily_lwr*pred_month1_lwr,
             pred_day_upper=pred_cumshare_daily_upper*pred_month1_upper) %>% 
      group_by(date) %>% 
      mutate(pred_day_cum=pred_day,
             pred_day_cum_lwr=pred_day_lwr,
             pred_day_cum_upper=pred_day_upper,
             pred_day=case_when(
               record_date==min(record_date)~pred_day,
               TRUE~pred_day-dplyr::lag(pred_day,1)
             ),
             pred_day_lwr=case_when(
               record_date==min(record_date)~pred_day_lwr,
               TRUE~pred_day_lwr-dplyr::lag(pred_day_lwr,1)
             ),
             pred_day_upper=case_when(
               record_date==min(record_date)~pred_day_upper,
               TRUE~pred_day_upper-dplyr::lag(pred_day_upper,1)
             ),
             final_pred_day=ifelse(is.na(final_pred_day),pred_day,final_pred_day),
             final_pred_day_lwr=ifelse(is.na(final_pred_day_lwr),pred_day_lwr,final_pred_day_lwr),
             final_pred_day_upper=ifelse(is.na(final_pred_day_upper),pred_day_upper,final_pred_day_upper),
             final_pred_day_cum=ifelse(is.na(final_pred_day_cum),pred_day_cum,final_pred_day_cum),
             final_pred_day_cum_lwr=ifelse(is.na(final_pred_day_cum_lwr),pred_day_cum_lwr,final_pred_day_cum_lwr),
             final_pred_day_cum_upper=ifelse(is.na(final_pred_day_cum_upper),pred_day_cum_upper,final_pred_day_cum_upper),
             cbo_proj=ifelse(imputed==1&is.na(cbo_proj),cbo_pred_month,cbo_proj)) %>% 
      select(any_of(c(colnames(daily_df),"pred_month1","pred_month1_lwr","pred_month1_upper"))) %>% 
      ungroup() %>% 
      fill(cbo_category,.direction="down")
    
    daily_df = daily_df1
    
  }
  
  dates = seq(max(daily_df$record_date,na.rm=TRUE)+1,as.Date(paste0(max(cbo_proj$projected_fiscal_year[cbo_proj$baseline_date<=end_date]),"-09-30")),by=1)
  
  daily_df2 = bind_rows(data.frame(record_date=dates)) %>% 
    mutate(record_fiscal_year=as.integer(quarter(record_date, with_year = TRUE, fiscal_start = 10)),
           month=month(record_date),
           fy_month=case_when(
             month%in%c(10:12)~month-9,
             month%in%c(1:9)~month+3
           ),
           imputed=1,
           total_day=NA,
           total_day_imp=NA,
           total_month=NA,
           date=floor_date(record_date,"month")) %>% 
    group_by(record_fiscal_year) %>% 
    arrange(record_date) %>% 
    left_join(tax_days,by=c("record_date"="date")) %>% 
    group_by(date) %>% 
    mutate(day=day(record_date),
           month=month(record_date),
           quarter_end=case_when(
             month==4&tax_day==1~1,
             month%in%c(1,6,9)&day==15&!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))~1,
             month%in%c(1,6,9)&day%in%c(16,17)&(weekdays(record_date,abbreviate = TRUE)%in%c("Mon"))~1 # only use 16 or 17 IF the 15th had fallen on a weekend
           ),
           settlement_period=case_when(
             record_date>=max(record_date[!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))])~"EOM",
             record_date>=min(record_date[day(record_date)>=15&!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))])~"Second Settlement",
             TRUE~"First Settlement"
           )) %>% 
    group_by(date) %>% 
    fill(tax_day,quarter_end,.direction="down") %>% 
    mutate(tax_day=ifelse(is.na(tax_day),0,tax_day),
           quarter_end=ifelse(is.na(quarter_end),0,quarter_end),
           record_calendar_day=sprintf("%02d", day(record_date)),
           record_calendar_month=month(record_date),
           fy_month=case_when(
             month%in%c(10:12)~month-9,
             month%in%c(1:9)~month+3
           )) %>% 
    ungroup() %>% 
    mutate(fiscal_year=as.integer(quarter(record_date, with_year = TRUE, fiscal_start = 10)),
           tax_due=case_when(
      !(fiscal_year%in%c(2020,2021))&month==4&col=="Individual Income Taxes"~1,
      fiscal_year==2020&month==7&col=="Individual Income Taxes"~1,
      fiscal_year==2021&month==5&col=="Individual Income Taxes"~1,
      !(fiscal_year%in%c(2020))&month==4&col=="Corporate Income Taxes"~1,
      fiscal_year==2020&month==7&col=="Corporate Income Taxes"~1,
      fiscal_year==2020&month==9&col=="Excise Taxes"~1,
      TRUE~0
    )) %>% 
    left_join(cbo_proj %>% 
                {if(col=="Other Spending") filter(.,subcategory %in% c("Nondefense Discretionary","Other Mandatory")) else if(col=="National Defense") filter(.,subcategory=="Defense Discretionary") else filter(.,subcategory%in%col)} %>% 
                group_by(projected_fiscal_year,subcategory) %>% 
                filter(baseline_date<=MAX_DATE) %>% 
                slice(n()) %>% 
                group_by(projected_fiscal_year) %>% 
                summarize(value=sum(value,na.rm=TRUE)) %>% 
                select(projected_fiscal_year,value) %>% 
                rename(cbo_proj_fy=value,
                       fiscal_year=projected_fiscal_year) %>% 
                mutate(change_cbo_proj_fy=cbo_proj_fy/cbo_proj_fy[fiscal_year==max(daily_df$record_fiscal_year)]),
              by=c("record_fiscal_year"="fiscal_year")) %>% 
    bind_cols(daily_df %>% 
                filter(record_fiscal_year==max(record_fiscal_year)) %>% 
                mutate(month=month(record_date)) %>% 
                group_by(month) %>% 
                slice(n()) %>% 
                ungroup() %>% 
                summarize(pred_total=sum(final_pred_day_cum),
                          pred_total_lwr=sum(final_pred_day_cum_lwr),
                          pred_total_upper=sum(final_pred_day_cum_upper)) %>% 
                select(pred_total:pred_total_upper)) %>% 
    mutate_at(vars(pred_total:pred_total_upper),~.*change_cbo_proj_fy) %>% 
    group_by(date) %>% 
    mutate(weekend=weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"),
           record_calendar_month=month(record_date),
           record_calendar_day=sprintf("%02d", day(record_date)),
           fed_remittances_suspended=ifelse(date>="2022-09-01",1,0),
           dotw=weekdays(record_date,abbreviate=TRUE),
           holiday=record_date%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"),
           ssi_day=case_when(
             record_calendar_day=="01"&!weekend&!holiday~1,
             (weekdays(date%m+%months(1),abbreviate = TRUE)%in%c("Sat","Sun")|(date%m+%months(1))%in%as.Date(as.character(tis::holidays(year(date%m+%months(1)))),,format="%Y%m%d"))&record_date==last(record_date[!weekend&!holiday])~1,
             TRUE~0
           ),
           ss_ssi_day=case_when(
             weekdays(date%m+%months(1),abbreviate=TRUE)=="Fri"&record_calendar_month==12&record_calendar_day=="31"~1,
             record_calendar_day=="03"&!weekend&!holiday~1,
             weekend[record_calendar_day=="03"]==TRUE&record_date==last(record_date[!weekend&!holiday&day(record_date)<3])~1,
             TRUE~0
           ),
           ss_day=case_when(
             record_date%in%record_date[dotw=="Wed"][2:4]&!weekend&!holiday~1,
             record_date%in%(as.Date(intersect(as.character(record_date[dotw=="Wed"][2:4]),as.character(record_date[holiday]))) %m-% days(1))~1,
             TRUE~0
           )) %>% 
    ungroup()
  
  daily_df2 = daily_df2 %>% 
    left_join(daily_df2 %>% 
                distinct(date) %>% 
                mutate(month=month(date)) %>% 
                rowwise() %>% 
                mutate(first_day_thismonth_weekend=(weekdays(date,abbreviate=TRUE)%in%c("Sat","Sun")|date%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=1,
                       first_day_nextmonth_weekend=(weekdays(date %m+% months(1),abbreviate=TRUE)%in%c("Sat","Sun")|(date %m+% months(1))%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12,
                       last_day_thismonth_weekend=(weekdays((date %m+% months(1) )- 1,abbreviate=TRUE)%in%c("Sat","Sun")|((date %m+% months(1) )- 1)%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12) %>% 
                ungroup())
  
  daily_df2 = daily_df2 %>% 
    left_join(monthly_share_pred %>% select(date,
                                            pred_cumshare_fy=pred_cumshare,
                                            pred_cumshare_fy_lwr=pred_cumshare_lwr,
                                            pred_cumshare_fy_upper=pred_cumshare_upper))
  
  daily_df2 = daily_df2 %>% 
    mutate(cbo_proj_fmonth = cbo_proj_fy*pred_cumshare_fy,
           pred_month=pred_total*pred_cumshare_fy)
  
  daily_df2 = daily_df2 %>% 
    select(-c(pred_month)) %>% 
    left_join(daily_df2 %>% 
                group_by(date) %>% 
                slice(n()) %>% 
                ungroup() %>% 
                mutate(pred_month=case_when(fy_month==1~pred_month,
                                            TRUE~pred_month-dplyr::lag(pred_month,1)),
                       pred_month_lwr=pred_month+pred_cumshare_fy_lwr*pred_total,
                       pred_month_upper=pred_month+pred_cumshare_fy_upper*pred_total,
                       cbo_pred_month=case_when(fy_month==1~cbo_proj_fmonth,
                                                TRUE~cbo_proj_fmonth-dplyr::lag(cbo_proj_fmonth,1)),
                       cbo_pred_month_lwr=cbo_pred_month+pred_cumshare_fy_lwr*cbo_proj_fy,
                       cbo_pred_month_upper=cbo_pred_month+pred_cumshare_fy_upper*cbo_proj_fy) %>% 
                select(date,pred_month,pred_month_lwr,pred_month_upper,cbo_pred_month,cbo_pred_month_lwr,cbo_pred_month_upper),by="date") %>% 
    {if(col=="Social Security") mutate_at(.,vars(pred_month,pred_month_lwr,pred_month_upper,cbo_pred_month,cbo_pred_month_lwr,cbo_pred_month_upper),
                                          ~case_when(month(date)==12&weekdays(date %m+% months(1),abbreviate = TRUE)=="Fri"~.+.*0.3579050,
                                                     month(date)==1&weekdays(date,abbreviate = TRUE)=="Fri"~.-.*0.3579050,
                                                     TRUE~.)) else .} %>% 
    mutate(record_calendar_year=year(record_date)) %>% 
    ungroup() %>% 
    mutate(pred_month1=pred_month*(.5)+cbo_pred_month*(.5),
           pred_month1_lwr=pred_month_lwr*(.5)+cbo_pred_month_lwr*(.5),
           pred_month1_upper=pred_month_upper*(.5)+cbo_pred_month_upper*(.5))
  
  if(nrow(overlays)>0){
    daily_df2 = daily_df2 %>% 
      mutate(up_diff=pred_month1_upper-pred_month1,
             down_diff=pred_month1_lwr-pred_month1) %>% 
      left_join(overlays %>% select(date,overlay_value=value)) %>% 
      mutate(pred_month1=ifelse(is.na(overlay_value),pred_month1,overlay_value),
             pred_month1_lwr=ifelse(is.na(overlay_value),pred_month1_lwr,overlay_value+down_diff),
             pred_month1_upper=ifelse(is.na(overlay_value),pred_month1_upper,overlay_value+up_diff)) %>% 
      select(-c(up_diff,down_diff,overlay_value))
  }
  
  preds = data.frame(predict(models_daily$share,data=daily_df2,type="quantiles",quantiles=c(0.5,.1,.9)))
  colnames(preds)=c("pred_share","pred_share_lwr","pred_share_upper")
  daily_df2 = bind_cols(daily_df2,preds) %>% 
    group_by(date) %>% 
    mutate(pred_cumshare_daily=cumsum(pred_share),
           pred_cumshare_daily_lwr=cumsum(pred_share_lwr),
           pred_cumshare_daily_upper=cumsum(pred_share_upper),
           row=1:n()) %>% 
    mutate_at(vars(pred_cumshare_daily:pred_cumshare_daily_upper),~case_when(row>=max(row[weekend==FALSE])&.<=0~1,
                                                                 TRUE~.)) %>% 
    mutate_at(vars(pred_cumshare_daily:pred_cumshare_daily_upper),~./.[n()]) %>% 
    mutate_at(vars(pred_cumshare_daily:pred_cumshare_daily_upper),~ifelse(is_bad(.),0,.)) %>% 
    mutate(num=1:n()) %>% 
    mutate_at(vars(pred_cumshare_daily:pred_cumshare_daily_upper),~case_when(num==max(num)&.==0~1,
                                                                             TRUE~.)) %>% 
    select(-num) %>% 
    mutate(pred_month2=pred_month1,
           pred_month2_lwr=pred_month1_lwr,
           pred_month2_upper=pred_month1_upper)
  
  daily_df2= daily_df2 %>% 
    mutate(pred_day=pred_cumshare_daily*pred_month2,
           pred_day_lwr=pred_cumshare_daily_lwr*pred_month2_lwr,
           pred_day_upper=pred_cumshare_daily_upper*pred_month2_upper,
           final_pred_day=NA,
           final_pred_day_lwr=NA,
           final_pred_day_upper=NA,
           final_pred_day_cum=NA,
           final_pred_day_cum_lwr=NA,
           final_pred_day_cum_upper=NA) %>% 
    group_by(date) %>% 
    mutate(pred_day_cum=pred_day,
           pred_day_cum_lwr=pred_day_lwr,
           pred_day_cum_upper=pred_day_upper,
           pred_day=case_when(
             record_date==min(record_date)~pred_day,
             TRUE~pred_day-dplyr::lag(pred_day,1)
           ),
           pred_day_lwr=case_when(
             record_date==min(record_date)~pred_day_lwr,
             TRUE~pred_day_lwr-dplyr::lag(pred_day_lwr,1)
           ),
           pred_day_upper=case_when(
             record_date==min(record_date)~pred_day_upper,
             TRUE~pred_day_upper-dplyr::lag(pred_day_upper,1)
           ),
           final_pred_day=ifelse(is.na(final_pred_day),pred_day,final_pred_day),
           final_pred_day_lwr=ifelse(is.na(final_pred_day_lwr),pred_day_lwr,final_pred_day_lwr),
           final_pred_day_upper=ifelse(is.na(final_pred_day_upper),pred_day_upper,final_pred_day_upper),
           final_pred_day_cum=ifelse(is.na(final_pred_day_cum),pred_day_cum,final_pred_day_cum),
           final_pred_day_cum_lwr=ifelse(is.na(final_pred_day_cum_lwr),pred_day_cum_lwr,final_pred_day_cum_lwr),
           final_pred_day_cum_upper=ifelse(is.na(final_pred_day_cum_upper),pred_day_cum_upper,final_pred_day_cum_upper)) %>% 
    mutate(pred_month1=pred_month2,
           pred_month1_lwr=pred_month2_lwr,
           pred_month1_upper=pred_month2_upper,
           cbo_category=col,
           cbo_proj=cbo_pred_month) %>% 
    select(any_of(c(colnames(daily_df)))) %>% 
    ungroup()
  
  daily_df = bind_rows(daily_df,daily_df2)
  
  if(nrow(overlays_daily)>0){
    
    daily_df = daily_df %>% 
      mutate(up_diff=final_pred_day_upper-final_pred_day,
             down_diff=final_pred_day_lwr-final_pred_day) %>% 
      left_join(overlays_daily %>% select(date,overlay_value=value)) %>% 
      group_by(date) %>% 
      mutate(final_pred_day=ifelse(is.na(overlay_value),final_pred_day,overlay_value),
             final_pred_day_lwr=ifelse(is.na(overlay_value),final_pred_day_lwr,overlay_value+down_diff),
             final_pred_day_upper=ifelse(is.na(overlay_value),final_pred_day_upper,overlay_value+up_diff),
             final_pred_day_cum=cumsum(final_pred_day),
             final_pred_day_cum_lwr=cumsum(final_pred_day_lwr),
             final_pred_day_cum_upper=cumsum(final_pred_day_upper)) %>% 
      select(-c(up_diff,down_diff,overlay_value))
    
  }
  
  return(list(daily_df=daily_df,nowcast=monthly_nowcast))
  
}

get_budget_outlay_df = function(cbo_category){
  
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
             month=month(date)) %>% 
      rowwise() %>% 
      mutate(first_day_thismonth_weekend=(weekdays(date,abbreviate=TRUE)%in%c("Sat","Sun")|date%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=1,
             first_day_nextmonth_weekend=(weekdays(date %m+% months(1),abbreviate=TRUE)%in%c("Sat","Sun")|(date %m+% months(1))%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12,
             last_day_thismonth_weekend=(weekdays((date %m+% months(1) )- 1,abbreviate=TRUE)%in%c("Sat","Sun")|((date %m+% months(1) )- 1)%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12) %>% 
      ungroup()
    
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
    
  }
  

  return(monthly_shares)
  
}

get_monthly_shares_df_spending = function(col_mts,col_cbo){
  monthly_shares = get_budget_outlay_df(col_cbo) %>% 
    mutate(share=value/total,
           month=month(date),
           fy_month=case_when(
             month%in%c(10:12)~month-9,
             month%in%c(1:9)~month+3
           )) %>% 
    group_by(fiscal_year) %>% 
    arrange(fy_month) %>% 
    mutate(cum_total=cumsum(value),
           cum_share=cumsum(share),
           num=n()) %>% 
    left_join(cbo_proj %>% 
                {if(col_cbo=="Other Spending") filter(.,subcategory %in% c("Nondefense Discretionary","Other Mandatory")) else if(col_cbo=="National Defense") filter(.,subcategory=="Defense Discretionary") else filter(.,subcategory%in%col_cbo)} %>% 
                group_by(projected_fiscal_year,subcategory) %>% 
                filter(baseline_date<=as.Date(paste0(projected_fiscal_year,"-09-30"))) %>% 
                slice(n()) %>% 
                group_by(projected_fiscal_year) %>% 
                summarize(value=sum(value,na.rm=TRUE)) %>% 
                select(projected_fiscal_year,value) %>% 
                rename(cbo_proj=value,
                       fiscal_year=projected_fiscal_year)) %>% 
    ungroup() %>%
    rowwise() %>% 
    mutate(first_day_thismonth_weekend=(weekdays(date,abbreviate=TRUE)%in%c("Sat","Sun")|date%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=1,
           first_day_nextmonth_weekend=(weekdays(date %m+% months(1),abbreviate=TRUE)%in%c("Sat","Sun")|(date %m+% months(1))%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12,
           last_day_thismonth_weekend=(weekdays((date %m+% months(1) )- 1,abbreviate=TRUE)%in%c("Sat","Sun")|((date %m+% months(1) )- 1)%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12) %>% 
    ungroup()
  
  return(monthly_shares)
}

get_monthly_shares_df_revenue = function(mts_dataset,col_mts,cbo_component,cbo_category){
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
           month=month(date),
           fy_month=case_when(
             month%in%c(10:12)~month-9,
             month%in%c(1:9)~month+3
           )) %>% 
    group_by(fiscal_year) %>% 
    arrange(fy_month) %>% 
    mutate(cum_total=cumsum(value),
           cum_share=cumsum(share),
           num=n()) %>% 
    left_join(cbo_proj %>% 
                filter(component==cbo_component&category==cbo_category) %>% 
                group_by(projected_fiscal_year) %>% 
                filter(baseline_date<=as.Date(paste0(projected_fiscal_year,"-09-30"))) %>% 
                slice(n()) %>% 
                select(projected_fiscal_year,value) %>% 
                rename(cbo_proj=value,
                       fiscal_year=projected_fiscal_year)) %>% 
    mutate(error=total/cbo_proj) %>% 
    ungroup() %>% 
    arrange(fiscal_year,fy_month) %>% 
    mutate(error_ly=dplyr::lag(error,12),
           error_ly=ifelse(fiscal_year==2016,error[fiscal_year==2015][1],error_ly)) %>% 
    ungroup() %>%
    select(-num) 
  
  return(monthly_shares)
  
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
    select(-any_of(paste0("gt_",bad_vars$category))) %>% 
    arrange(date) %>%
    mutate(year=year(date),
           qtr=quarter(date)) %>%
    select(-c(PCE,PRS85006112)) %>%
    group_by(year,qtr) %>%
    mutate_at(vars(PAYEMS:tail(grep("gt_",colnames(.),value=TRUE),1)),~mean(.,na.rm=TRUE)) %>%
    summarize_all(~.[1]) %>%
    ungroup() %>% 
    left_join(national_econ %>% 
                filter(release_date<=dat) %>% 
                select(date,series_id,value) %>%
                pivot_wider(names_from=series_id,values_from=value) %>% 
                select(date,A261RX1Q020SBEA:SLCEC1)) %>%
    arrange(date) %>%
    mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10,col),~((./dplyr::lag(.,1)-1)*100)) %>%
    mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,grep("gt_",colnames(.),value=TRUE)),~(.-dplyr::lag(.,1))) %>%
    mutate(lag1=dplyr::lag(!!sym(col),1),
           lag2=dplyr::lag(!!sym(col),2),
           lag3=dplyr::lag(!!sym(col),3),
           lag4=dplyr::lag(!!sym(col),4)) %>%
    ungroup() 
  
  if(testing){
    
    fcast_df1 = fcast_df1 %>% 
      select(-one_of("ADPMNUSNERSA","IHLIDXUS"))
    
  } else{
    fcast_df1 = fcast_df1 %>% 
      mutate(IHLIDXUS=ifelse(is.nan(IHLIDXUS),0,IHLIDXUS)) # bring back when not testing
  }
  
  return(fcast_df1)
}

fcast_gdp_ols = function(dat,col,testing=FALSE){
  
  conflicted::conflicts_prefer(dplyr::lag)
  
  set.seed(178)
  
  fcast_df1 = get_imputed_data(max(c(((ceiling_date(as.Date(dat),"quarter"))-1) %m-% years(1),'2010-03-31')),col,testing)
  
  X = model.matrix(as.formula(paste0(col,"~",paste(colnames(fcast_df1 %>% select(PAYEMS:gt_999)),collapse="+"))),
                   fcast_df1 %>% filter(year(date)>=2006&!is.na(!!sym(col))))[, -1]
  y = (fcast_df1 %>% filter(year(date)>=2006&!is.na(!!sym(col))))[[col]]
  
  if(length(y)<4){next}
  
  fit_lasso_state = glmnet(X, y, alpha = 1,pmax=min(15,nrow(fcast_df1)/2),weights=1:nrow(X))
  # weight by how recent the data is
  
  selected_coefs_state = data.frame(varImp(fit_lasso_state,lambda=min(fit_lasso_state$lambda), scale = FALSE)) %>% filter(Overall!=0)
  selected_coefs_state$var = as.numeric(gsub("gt_","",rownames(selected_coefs_state)))
  coef_value_state = coef(fit_lasso_state,s=min(fit_lasso_state$lambda))[,1][-1]
  coef_value_state = coef_value_state[coef_value_state!=0]
  selected_coefs_state = cbind(selected_coefs_state,coef_value_state)
  selected_coefs_state$category = sapply(selected_coefs_state$var,which_category)
  selected_coefs_state = selected_coefs_state %>% arrange(-Overall)
  
  # make adjustments for non-intuitive coefficients
  
  checked = FALSE
  vars = c("lag2",rownames(selected_coefs_state))
  i = 0
  while(checked==FALSE){
    
    print(i)
    
    reg_df = fcast_df1 %>% 
      mutate(num=(1:n())/n(),
             num=ifelse(date>="2020-01-01"&date<="2021-06-30",0,num)) %>% 
      rowwise() %>%  
      mutate(num=max(c(.5,num))) %>% 
      ungroup() %>% 
      mutate(num=hardhat::importance_weights(num)) %>% 
      filter(year(date)>=2006&!is.na(!!sym(col)))
    
    # tuning_mod = cv.glmnet(x= model.matrix(as.formula(paste0(col,"~lag1+lag2+",paste(vars,collapse="+"))),
    #                                        reg_df)[, -1],
    #                        y = reg_df[[col]],
    #                        alpha=0,
    #                        weights=reg_df$num
    # )

    # tuning_mod = glmnet(x= model.matrix(as.formula(paste0(col,"~lag1+lag2+",paste(vars,collapse="+"))),
    #                                     reg_df)[, -1],
    #                     y = reg_df[[col]],
    #                     weights=reg_df$num,
    #                     alpha=0,
    #                     lambda = 10
    # )
    # 
    # tuning_mod = lm_robust(as.formula(paste0(col,"~",paste(tidy(tuning_mod)$term[-1],collapse="+"))),
    #                        reg_df %>% filter(!(year%in%c(2020:2021))),
    #                        weights=as.numeric(num))
    
    tuning_mod = lm_robust(as.formula(paste0(col,"~lag1+",paste(vars,collapse="+"))),
                           reg_df,
                           weights=as.numeric(num))

    tidy_lm = tidy(tuning_mod)
    
    if(col=="GDPC1"){
      
      not_allowed = data.frame(
        term=c("RRSFS", "CE16OV", "PAYEMS", "gt_145", "gt_672",       
               "gt_340", "gt_531" , "TOTBUSIMNSA", "gt_670", "gt_899",        
               "gt_718", "IR", "CPILFESL", "gt_652", "IHLIDXUS",
               "gt_1268", "DTCDISA066MSFRBNY", "GACDISA066MSFRBNY", "ICSA","gt_671"),
        estimate=c(-1, -1, -1, -1, -1,       
                   -1, -1, -1, -1, -1,        
                   -1, -1, -1, -1, -1,
                   -1, 1, -1, 1,-1)
      )
      
      check_df = bind_rows(tidy_lm %>% select(term,estimate) %>% mutate(estimate=sign(estimate)),
                           not_allowed)
      
      check_df$flag = duplicated(check_df)
      
      checked=all(check_df$flag==FALSE)
      
      vars = tidy_lm %>% filter(!(term%in%check_df$term[check_df$flag==TRUE])&!grepl("Intercept|lag1",term)&p.value<.3) %>% distinct(term) %>% pull(term)
      
      
    }
    if(col=="PCECC96"){
      
      not_allowed = data.frame(
        term=c("RRSFS", "CE16OV", "PAYEMS", "gt_145", "gt_672",       
                "gt_340", "gt_531" , "TOTBUSIMNSA", "gt_670", "gt_899",        
                "gt_718", "IR", "CPILFESL", "gt_652", "IHLIDXUS",
                "gt_1268", "DTCDISA066MSFRBNY", "GACDISA066MSFRBNY", "ICSA","gt_671"),
        estimate=c(-1, -1, -1, -1, -1,       
                   -1, -1, -1, -1, -1,        
                   -1, -1, -1, -1, -1,
                   -1, 1, -1, 1,-1)
      )
      
      check_df = bind_rows(tidy_lm %>% select(term,estimate) %>% mutate(estimate=sign(estimate)),
                           not_allowed)
      
      check_df$flag = duplicated(check_df)
      
      checked=all(check_df$flag==FALSE)
      
      vars = tidy_lm %>% filter(!(term%in%check_df$term[check_df$flag==TRUE])&!grepl("Intercept|lag1",term)&p.value<.3) %>% distinct(term) %>% pull(term)
      
      
    }
    if(col=="GPDIC1"){
      
      not_allowed = data.frame(
        term=c("AMDMVS","gt_999","gt_989","gt_1339","gt_983","gt_255",
               "DSPIC96","gt_814","gt_229","INDPRO","gt_813","PERMIT",
               "IQ","gt_312","gt_957","BOPTIMP","HSN1F","DFF","GACDISA066MSFRBNY","AMDMVS"),
        estimate=c(-1,-1,-1,1,-1,-1,
                   -1,-1,-1,-1,-1,-1,
                   1,-1,-1,-1,1,1,-1,1)
      )
      
      check_df = bind_rows(tidy_lm %>% select(term,estimate) %>% mutate(estimate=sign(estimate)),
                           not_allowed)
      
      check_df$flag = duplicated(check_df)
      
      checked=all(check_df$flag==FALSE)
      
      vars = tidy_lm %>% filter(!(term%in%check_df$term[check_df$flag==TRUE])&!grepl("Intercept|lag1",term)&p.value<.3) %>% distinct(term) %>% pull(term)
      
      
    }
    if(col=="EXPGSC1"){
      
      not_allowed = data.frame(
        term=c("BOPTEXP","IQ","CPILFESL","gt_432","gt_1166",
               "gt_989","gt_340","gt_206","gt_1178","gt_107",
               "gt_255","PAYEMS","gt_665","DTCDISA066MSFRBNY","ADPMNUSNERSA"),
        estimate=c(-1,1,-1,1,-1,
                   -1,1,-1,-1,1,
                   -1,-1,-1,1,-1)
      )
      
      check_df = bind_rows(tidy_lm %>% select(term,estimate) %>% mutate(estimate=sign(estimate)),
                           not_allowed)
      
      check_df$flag = duplicated(check_df)
      
      checked=all(check_df$flag==FALSE)
      
      vars = tidy_lm %>% filter(!(term%in%check_df$term[check_df$flag==TRUE])&!grepl("Intercept|lag1|lag2",term)) %>% distinct(term) %>% pull(term)
      
      
    }
    if(col=="IMPGSC1"){
      
      not_allowed = data.frame(
        term=c("BOPTIMP","UNRATE","IR","gt_670","gt_999","AMDMVS",
               "gt_671","DSPIC96","BOPTEXP","gt_696","RRSFS","gt_1171",
               "PERMIT","gt_989","CE16OV","DTCDISA066MSFRBNY","gt_994","gt_229","gt_340"),
        estimate=c(-1,1,1,1,-1,-1,
                   -1,-1,-1,-1,-1,-1,
                   -1,-1,-1,1,-1,-1,-1)
      )
      
      check_df = bind_rows(tidy_lm %>% select(term,estimate) %>% mutate(estimate=sign(estimate)),
                           not_allowed)
      
      check_df$flag = duplicated(check_df)
      
      checked=all(check_df$flag==FALSE)
      
      vars = tidy_lm %>% filter(!(term%in%check_df$term[check_df$flag==TRUE])&!grepl("Intercept|lag1|lag2",term)) %>% distinct(term) %>% pull(term)
      
      
    }
    if(col=="GCEC1"){
      
      not_allowed = data.frame(
        term=c("WHLSLRIMSA","DSPIC96","gt_466","CPILFESL","gt_739",
               "gt_650","gt_991","AMDMVS","gt_665","gt_1269","AMTMUO",
               "IQ","gt_1003","JTSJOL","PERMIT","HOUST","UMCSENT","gt_1076"),
        estimate=c(1,-1,-1,-1,-1,
                   1,1,1,-1,1,1,
                   1,1,1,1,1,-1,-1)
      )
      
      check_df = bind_rows(tidy_lm %>% select(term,estimate) %>% mutate(estimate=sign(estimate)),
                           not_allowed)
      
      check_df$flag = duplicated(check_df)
      
      checked=all(check_df$flag==FALSE)
      
      vars = tidy_lm %>% filter(!(term%in%check_df$term[check_df$flag==TRUE])&!grepl("Intercept|lag1|lag2",term)) %>% distinct(term) %>% pull(term)
      
      
    }
    
    i=i+1
    
  }
  
  fcast_df1 = get_imputed_data(dat,col,testing)
  
  dates = tail(fcast_df1,10) %>% filter(is.na(!!sym(col))) %>% pull(date)
  
  i=2
  if(length(dates)>1){
    
    fcast_df1$lag1[fcast_df1$date==dates[i]] = predict(tuning_mod,fcast_df1 %>% filter(date==dates[i-1]))
    
  }
  
  gdp_pred_df = data.frame(
    prediction_date=dat,
    date=dates,
    var=col,
    pred=as.numeric(predict(tuning_mod,(fcast_df1 %>% filter(date%in%dates) %>% select(any_of(tidy_lm$term)))))
  ) %>% 
    left_join(national_econ %>% filter(series_id==col) %>% select(date,value) %>% mutate(value=(value/dplyr::lag(value,1)-1)*100))
  
  # Create an explainer
  explainer <- DALEX::explain(tuning_mod, 
                       data = reg_df %>% select(-!!sym(col)), 
                       y = reg_df[[col]],
                       weights=reg_df$num)
  
  tmp = lapply(dates,function(x) predict_parts(explainer, new_observation = fcast_df1 %>% filter(date%in%x) %>% select(any_of(tidy_lm$term)), type = "break_down") %>% mutate(date=x,prediction_date=dat,var=col))
  breakdown = bind_rows(tmp)
  
  return(list(gdp_pred_df,explainer,breakdown))
  
}

fcast_gdp_ols2 = function(dat,col,testing=FALSE){
  
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
  
  # make adjustments for non-intuitive coefficients
  
  
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


make_state_trends = function(end_date,bad_vars,most_recent=TRUE){
  if(most_recent){
    state_trends = read_csv(paste0("Data/Processing/gt_data/",list.files("Data/Processing/gt_data")[which.max(gsub("trends_full_sa_|.csv","",list.files("Data/Processing/gt_data")))])) %>% # get first file that would include this date
      mutate(release_date=date+6)
  }else{
    if(is.na(list.files("Data/Processing/gt_data")[which(gsub("trends_full_sa_|.csv","",list.files("Data/Processing/gt_data"))>=gsub("-","",end_date))[1]])){ stop("No google trends data with this date.")}
    state_trends = read_csv(paste0("Data/Processing/gt_data/",list.files("Data/Processing/gt_data")[which(gsub("trends_full_sa_|.csv","",list.files("Data/Processing/gt_data"))>=gsub("-","",end_date))[1]])) %>% # get first file that would include this date
      mutate(release_date=date+6)
  }
  state_trends = state_trends %>%
    filter(!(category%in%bad_vars$category)&date<=(as.Date(end_date)+6))
  return(state_trends)
}

