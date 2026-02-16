# test imputation

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
          if(col1=="IHLIDXUS"){potential_cols = colnames(test_dineof %>% select(-c(col1,IHLIDXUS,ADPMNUSNERSA,grep("gt_",colnames(test_dineof),value=TRUE))) %>% filter(date==max(date)) %>% select(-date) %>% select_if(!is.na(.)))}else{
            potential_cols = colnames(test_dineof %>% select(-c(col1,IHLIDXUS,ADPMNUSNERSA,grep("gt_",colnames(test_dineof),value=TRUE))) %>% filter(date==test_dineof$date[(nrow(test_dineof)-3):nrow(test_dineof)][head(which(is.na(test_dineof[[col1]][(nrow(test_dineof)-3):nrow(test_dineof)])),1)]) %>% select(-date) %>% select_if(!is.na(.)))
          }
        } else{
          potential_cols = colnames(test_dineof %>% select(-c(col1,grep("gt_",colnames(test_dineof),value=TRUE))) %>% select(-one_of("ADPMNUSNERSA","IHLIDXUS")) %>% filter(date==test_dineof$date[(nrow(test_dineof)-3):nrow(test_dineof)][max(head(which(is.na(test_dineof[[col1]][(nrow(test_dineof)-3):nrow(test_dineof)])),1),1)]) %>% select(-date) %>% select_if(!is.na(.)))
        }
        cols = c(sample(potential_cols,min(c(15,floor(length(potential_cols)/2)))),sample(colnames(test_dineof %>% select(grep("gt_",colnames(test_dineof),value=TRUE))),15))
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
    if(col1==tail(colnames(test_dineof),1)){
      flag = flag+1
    }
  }
  
  return(test_dineof)
  
}


df = make_df(end_date,bad_vars,most_recent = FALSE) %>% 
  group_by(year,qtr) %>%
  fill(PRS85006112,.direction="down") %>% 
  ungroup() %>% 
  select(-c(MTSR133FMS:W017RC1Q027SBEA,A261RX1Q020SBEA:SLCEC1,B096RC1Q027SBEA:A091RC1Q027SBEA,B243RC1Q027SBEA:AD02RC1Q027SBEA,year,qtr)) %>%  # remove indeed and retail variables to speed up code, even though they do improve the model fit
  mutate_at(vars(-c(date)),~ifelse(is.infinite(.)|is.nan(.),NA,.)) %>% 
  select_if(~sum(!is.na(.))>0|is.character(.)|is.Date(.)) %>% 
  select_if(~sd(.,na.rm=TRUE)!=0|is.character(.)|is.Date(.)) %>% 
  filter(date>="2004-01-01")

write_csv(df,paste0("Data/Processing/raw_data/data_asof",end_date,".csv"))

set.seed(178)

imputed_df = impute_function(df,end_date)

mape = function(pred,obs){
  
  return(mean(abs((obs-pred)/obs),na.rm=TRUE))
  
}

vars_to_impute = df %>% slice((n()-11):n()) %>% pivot_longer(cols=2:ncol(df)) %>% group_by(name) %>% summarize(num=sum(is.na(value))) %>% filter(num>0) %>% pull(name)

# MOST SIMPLE METHOD: simple mean
simple_avg_test = function(col,test_dates,exclude_google_var="deviation_perc"){
  
  if(test_dates[1]<"2010-01-01"&col%in%c("ADPMNUSNERSA")){
    break
  }
  if(test_dates[1]<"2020-01-01"&col%in%c("IHLIDXUS")){
    break
  }
  
  df = make_df('2025-08-10',bad_vars,most_recent = FALSE) %>% 
    group_by(year,qtr) %>%
    fill(PRS85006112,.direction="down") %>% 
    ungroup() %>% 
    select(-c(MTSR133FMS:W017RC1Q027SBEA,A261RX1Q020SBEA:SLCEC1,B096RC1Q027SBEA:A091RC1Q027SBEA,B243RC1Q027SBEA:AD02RC1Q027SBEA,year,qtr))
  
  xvars = grep(paste0("date|",col,"|",exclude_google_var),colnames(df),value=TRUE,invert = TRUE)
  if(test_dates[1]<"2010-01-01"){
    xvars = grep("ADPMNUSNERSA",colnames(df),value=TRUE,invert = TRUE)
  }
  if(test_dates[1]<"2020-01-01"&col%in%c("IHLIDXUS")){
    xvars = grep("IHLIDXUS",colnames(df),value=TRUE,invert = TRUE)
  }
  
  pred_df = data.frame()
  for(dat in as.character(test_dates)){
    
    system(sprintf('echo "\n%s\n"', paste0(c(as.character(dat),col), collapse="")))
    
    df = make_df(dat,bad_vars,most_recent = FALSE) %>% 
      group_by(year,qtr) %>%
      fill(PRS85006112,.direction="down") %>% 
      ungroup() %>% 
      select(-c(MTSR133FMS:W017RC1Q027SBEA,A261RX1Q020SBEA:SLCEC1,B096RC1Q027SBEA:A091RC1Q027SBEA,B243RC1Q027SBEA:AD02RC1Q027SBEA,year,qtr)) %>% 
      filter(date>="2004-01-01")
    
    avg = mean(df[[col]],na.rm=TRUE)
    tmp = df %>% 
      select(date,!!col) %>% 
      filter(is.na(get(col))) %>% 
      mutate({{col}} := avg) %>% 
      mutate(test_date=dat) %>% 
      left_join(national_econ %>% filter(series_id==col),by="date") %>% 
      rename(pred=!!col)
    
    pred_df = bind_rows(pred_df,tmp)
    
  }
  
  return(pred_df)
  
}

simple_avg_preds_full = data.frame()
for(var in vars_to_impute){
  
  simple_avg_preds = mclapply(list(data.frame(date=as.Date(paste0(2009,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                                   data.frame(date=as.Date(paste0(2015,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                                   data.frame(date=as.Date(paste0(2020,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                                   data.frame(date=as.Date(paste0(2024,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date)),
                              simple_avg_test,
                              exclude_google_var="deviation_perc",
                              col=var,
                              mc.cores=4)
  
  simple_avg_preds_full = bind_rows(simple_avg_preds_full,bind_rows(simple_avg_preds[which(sapply(simple_avg_preds,length)>1)]))
  
}
RMSE(simple_avg_preds_full$pred,simple_avg_preds_full$value)
mape(simple_avg_preds_full$pred[simple_avg_preds_full$series_id=="PAYEMS"],simple_avg_preds_full$value[simple_avg_preds_full$series_id=="PAYEMS"])
# PAYEMS MAPE: 0.057
mape(simple_avg_preds_full$pred[simple_avg_preds_full$value!=0],simple_avg_preds_full$value[simple_avg_preds_full$value!=0])
# Overall MAPE: 0.27
sapply(c(2009,2015,2020,2024),function(x) mape(simple_avg_preds_full$pred[year(simple_avg_preds_full$test_date)==x&simple_avg_preds_full$series_id=="PAYEMS"],simple_avg_preds_full$value[year(simple_avg_preds_full$test_date)==x&simple_avg_preds_full$series_id=="PAYEMS"]))

sapply(vars_to_impute,function(x) mape(simple_avg_preds_full$pred[simple_avg_preds_full$series_id==x&simple_avg_preds_full$value!=0],simple_avg_preds_full$value[simple_avg_preds_full$series_id==x&simple_avg_preds_full$value!=0]))

# rolling mean
rolling_avg_test = function(col,test_dates,exclude_google_var="deviation_perc"){
  
  if(test_dates[1]<"2010-01-01"&col%in%c("ADPMNUSNERSA")){
    break
  }
  if(test_dates[1]<"2020-01-01"&col%in%c("IHLIDXUS")){
    break
  }
  
  df = make_df('2025-08-10',bad_vars,most_recent = FALSE) %>% 
    group_by(year,qtr) %>%
    fill(PRS85006112,.direction="down") %>% 
    ungroup() %>% 
    select(-c(MTSR133FMS:W017RC1Q027SBEA,A261RX1Q020SBEA:SLCEC1,B096RC1Q027SBEA:A091RC1Q027SBEA,B243RC1Q027SBEA:AD02RC1Q027SBEA,year,qtr))
  
  xvars = grep(paste0("date|",col,"|",exclude_google_var),colnames(df),value=TRUE,invert = TRUE)
  if(test_dates[1]<"2010-01-01"){
    xvars = grep("ADPMNUSNERSA",colnames(df),value=TRUE,invert = TRUE)
  }
  if(test_dates[1]<"2020-01-01"&col%in%c("IHLIDXUS")){
    xvars = grep("IHLIDXUS",colnames(df),value=TRUE,invert = TRUE)
  }
  
  pred_df = data.frame()
  for(dat in as.character(test_dates)){
    
    system(sprintf('echo "\n%s\n"', paste0(c(as.character(dat),col), collapse="")))
    
    df = make_df(dat,bad_vars,most_recent = FALSE) %>% 
      group_by(year,qtr) %>%
      fill(PRS85006112,.direction="down") %>% 
      ungroup() %>% 
      select(-c(MTSR133FMS:W017RC1Q027SBEA,A261RX1Q020SBEA:SLCEC1,B096RC1Q027SBEA:A091RC1Q027SBEA,B243RC1Q027SBEA:AD02RC1Q027SBEA,year,qtr))
    
    avg = mean(tail(df[[col]][!is.na(df[[col]])],52*3),na.rm=TRUE)
    tmp = df %>% 
      filter(date>="2004-01-01") %>% 
      select(date,!!col) %>% 
      filter(is.na(get(col))) %>% 
      mutate({{col}} := avg) %>% 
      mutate(test_date=dat) %>% 
      left_join(national_econ %>% filter(series_id==col),by="date")
    
    pred_df = bind_rows(pred_df,tmp)
    
  }
  
  return(pred_df)
  
}

rolling_avg_preds_full = data.frame()
for(var in vars_to_impute){
rolling_avg_preds = mclapply(list(data.frame(date=as.Date(paste0(2009,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                                  data.frame(date=as.Date(paste0(2015,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                                  data.frame(date=as.Date(paste0(2020,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                                  data.frame(date=as.Date(paste0(2024,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date)),
                            rolling_avg_test,
                            exclude_google_var="deviation_perc",
                            col=var,
                            mc.cores=4)

rolling_avg_preds_full = bind_rows(rolling_avg_preds_full,bind_rows(rolling_avg_preds[which(sapply(rolling_avg_preds,length)>1)]))

}

rolling_avg_preds_full = bind_rows(rolling_avg_preds)
rolling_avg_preds_full1 = rolling_avg_preds_full %>% 
  select(-c(series_id,release_date,title)) %>% 
  rename(actual=value) %>% 
  pivot_longer(cols=intersect(colnames(rolling_avg_preds_full),xvars)) %>% 
  filter(!is.na(actual)&!is.na(value))

RMSE(rolling_avg_preds_full$PAYEMS,rolling_avg_preds_full$value)
mape(rolling_avg_preds_full$PAYEMS,rolling_avg_preds_full$value)
# PAYEMS MAPE: 0.047, better performance during and after COVID
sapply(c(2009,2015,2020,2024),function(x) mape(rolling_avg_preds_full$PAYEMS[year(rolling_avg_preds_full$test_date)==x],rolling_avg_preds_full$value[year(rolling_avg_preds_full$test_date)==x]))

RMSE(rolling_avg_preds_full1$value[rolling_avg_preds_full1$actual!=0],rolling_avg_preds_full1$actual[rolling_avg_preds_full1$actual!=0])
mape(rolling_avg_preds_full1$value[rolling_avg_preds_full1$actual!=0&rolling_avg_preds_full1$name=="PAYEMS"],rolling_avg_preds_full1$actual[rolling_avg_preds_full1$actual!=0&rolling_avg_preds_full1$name=="PAYEMS"])
# PAYEMS MAPE: 0.047
mape(rolling_avg_preds_full1$value[rolling_avg_preds_full1$actual!=0],rolling_avg_preds_full1$actual[rolling_avg_preds_full1$actual!=0])
# Overall MAPE:0.30
sapply(c(2009,2015,2020,2024),function(x) mape(rolling_avg_preds_full1$value[year(rolling_avg_preds_full1$test_date)==x&rolling_avg_preds_full1$name=="PAYEMS"],rolling_avg_preds_full1$actual[year(rolling_avg_preds_full1$test_date)==x&rolling_avg_preds_full1$name=="PAYEMS"]))
sapply(vars_to_impute,function(x) mape(rolling_avg_preds_full1$value[rolling_avg_preds_full1$name==x&rolling_avg_preds_full1$actual!=0],rolling_avg_preds_full1$actual[rolling_avg_preds_full1$name==x&rolling_avg_preds_full1$actual!=0]))


# mice
mice_level_test = function(test_dates,exclude_google_var="deviation_perc"){
  
  library(mice)
  
  df = make_df('2025-08-10',bad_vars,most_recent = FALSE) %>% 
    group_by(year,qtr) %>%
    fill(PRS85006112,.direction="down") %>% 
    ungroup() %>% 
    select(-c(MTSR133FMS:W017RC1Q027SBEA,A261RX1Q020SBEA:SLCEC1,B096RC1Q027SBEA:A091RC1Q027SBEA,B243RC1Q027SBEA:AD02RC1Q027SBEA,year,qtr))
  
  xvars = grep(paste(c("date",exclude_google_var,paste(head(sample(grep("gt_",colnames(df),value=TRUE)),-20),collapse="|")),collapse="|"),colnames(df),value=TRUE,invert = TRUE)
  if(test_dates<"2010-01-01"){
    xvars = grep("ADPMNUSNERSA",xvars,value=TRUE,invert = TRUE)
    
    df = df %>% 
      select(-any_of("ADPMNUSNERSA"))
  }
  if(test_dates<"2020-02-01"){
    xvars = grep("IHLIDXUS",xvars,value=TRUE,invert = TRUE)
    
    df = df %>% 
      select(-any_of("IHLIDXUS"))
  }
  
  pred_df = data.frame()
  for(dat in as.character(test_dates)){
    
    system(sprintf('echo "\n%s\n"', paste0(as.character(dat), collapse="")))
    
    df = make_df(dat,bad_vars,most_recent = FALSE) %>% 
      group_by(year,qtr) %>%
      fill(PRS85006112,.direction="down") %>% 
      ungroup() %>% 
      select(-c(MTSR133FMS:W017RC1Q027SBEA,A261RX1Q020SBEA:SLCEC1,B096RC1Q027SBEA:A091RC1Q027SBEA,B243RC1Q027SBEA:AD02RC1Q027SBEA,year,qtr)) %>% 
      filter(date>="2004-01-01")
    
    if(test_dates<"2010-01-01"){
      df = df %>% 
        select(-any_of("ADPMNUSNERSA"))
    }
    if(test_dates<"2020-02-01"){
      df = df %>% 
        select(-any_of("IHLIDXUS"))
    }
    
    
    tempData <- ((mice(df %>% select(xvars),meth='rf',seed=178)))
    tmp=complete(tempData) %>% 
      bind_cols(df %>% select(date)) %>% 
      relocate(date,1)
    
    tmp = df %>% 
      pivot_longer(cols=colnames(df[2:ncol(df)])) %>% 
      filter(is.na(value)) %>% 
      left_join(tmp %>% pivot_longer(cols=colnames(tmp)[-1]),by=c('date','name')) %>% 
      mutate(test_date=dat) %>% 
      left_join(national_econ,by=c("date"="date","name"="series_id"))
    
    pred_df = bind_rows(pred_df,tmp)
    
  }
  
  return(pred_df)
  
}

mice_level_preds = mclapply(c(data.frame(date=as.Date(paste0(2009,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                                  data.frame(date=as.Date(paste0(2015,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                                  data.frame(date=as.Date(paste0(2020,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                                  data.frame(date=as.Date(paste0(2024,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date)),
                             mice_level_test,
                             exclude_google_var="deviation_perc",
                             mc.cores=8)

mice_level_preds_full = bind_rows(mice_level_preds)

RMSE(mice_level_preds_full$value.y[mice_level_preds_full$value!=0],mice_level_preds_full$value[mice_level_preds_full$value!=0])
mape(mice_level_preds_full$value.y[mice_level_preds_full$value!=0&mice_level_preds_full$name=="PAYEMS"],mice_level_preds_full$value[mice_level_preds_full$value!=0&mice_level_preds_full$name=="PAYEMS"])
# PAYEMS MAPE: 0.051
mape(mice_level_preds_full$value.y[mice_level_preds_full$value!=0],mice_level_preds_full$value[mice_level_preds_full$value!=0])
# Overall MAPE:0.44
sapply(c(2009,2015,2020,2024),function(x) mape(mice_level_preds_full$value.y[year(mice_level_preds_full$test_date)==x&mice_level_preds_full$value!=0],mice_level_preds_full$value[year(mice_level_preds_full$test_date)==x&mice_level_preds_full$value!=0]))
sapply(vars_to_impute,function(x) mape(mice_level_preds_full$value.y[mice_level_preds_full$name==x&mice_level_preds_full$value!=0],mice_level_preds_full$value[mice_level_preds_full$name==x&mice_level_preds_full$value!=0]))


#### Regression level ####
reg_level_test = function(col,test_dates,exclude_google_var="deviation_perc"){
  
  library(mice)
  
  df = make_df('2025-08-10',bad_vars,most_recent = FALSE) %>% 
    group_by(year,qtr) %>%
    fill(PRS85006112,.direction="down") %>% 
    ungroup() %>% 
    select(-c(MTSR133FMS:W017RC1Q027SBEA,A261RX1Q020SBEA:SLCEC1,B096RC1Q027SBEA:A091RC1Q027SBEA,B243RC1Q027SBEA:AD02RC1Q027SBEA,year,qtr))
  
  gt_vars = cor(df[[col]],df %>% select(starts_with("gt_")),use="complete.obs")
  xvars = grep(paste(c("date",exclude_google_var,paste(setdiff(colnames(df %>% select(starts_with("gt_"))),colnames(df %>% select(starts_with("gt")))[order(abs(gt_vars))[1:10]]),collapse="|")),collapse="|"),colnames(df),value=TRUE,invert = TRUE)
  xvars = grep("ADPMNUSNERSA",xvars,value=TRUE,invert = TRUE)
  if(col!="ADPMNUSNERSA"){
    df = df %>% 
    select(-any_of("ADPMNUSNERSA"))
  }
  xvars = grep("IHLIDXUS",xvars,value=TRUE,invert = TRUE)
  if(col!="IHLIDXUS"){
    df = df %>% 
      select(-any_of("IHLIDXUS"))
  }
  xvars = sample(xvars,floor(min(length(xvars),max(15,nrow(df %>% select(col,xvars) %>% drop_na())/2))))
  
  pred_df = data.frame()
  for(dat in as.character(test_dates)){
    
    system(sprintf('echo "\n%s\n"', paste0(c(as.character(dat),col), collapse="")))
    
    df = make_df(dat,bad_vars,most_recent = FALSE) %>% 
      group_by(year,qtr) %>%
      fill(PRS85006112,.direction="down") %>% 
      ungroup() %>% 
      select(-c(MTSR133FMS:W017RC1Q027SBEA,A261RX1Q020SBEA:SLCEC1,B096RC1Q027SBEA:A091RC1Q027SBEA,B243RC1Q027SBEA:AD02RC1Q027SBEA,year,qtr)) %>% 
      filter(date>="2004-01-01")
    
    if(col!="ADPMNUSNERSA"){
      df = df %>% 
        select(-any_of("ADPMNUSNERSA"))
    }
    if(col!="IHLIDXUS"){
      df = df %>% 
        select(-any_of("IHLIDXUS"))
    }
    
    for(i in which(is.na(tail(df[[col]],12)))){
      
      xvars1 = colnames(df %>% filter(date==tail(df$date,12)[i]) %>% select(xvars) %>% select_if(!is.na(.)))
      
      reg1 = lm_robust(as.formula(paste0(col,"~",paste(xvars1,collapse="+"))),
                       df %>% select(col,xvars1))
      
      pred_df = bind_rows(pred_df,
                          df %>% 
                            filter(date==tail(df$date,12)[i]) %>% 
                            mutate(var=predict(reg1,.)) %>% 
                            select(date,var) %>% 
                            mutate(test_date=dat,
                                   name=col)
                          )
      
    }
    
  }
  
  pred_df = pred_df %>%     
    left_join(national_econ,by=c("date"="date","name"="series_id"))
  
  return(pred_df)
  
}

reg_level_preds_full = data.frame()
for(var in vars_to_impute){
  
  reg_level_preds = mclapply(c(data.frame(date=as.Date(paste0(2009,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                               data.frame(date=as.Date(paste0(2015,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                               data.frame(date=as.Date(paste0(2020,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                               data.frame(date=as.Date(paste0(2024,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date)),
                             reg_level_test,
                             col=var,
                             exclude_google_var="deviation_perc",
                             mc.cores=8)
  
  reg_level_preds_full = bind_rows(reg_level_preds_full,bind_rows(reg_level_preds[which(sapply(reg_level_preds,length)>1)]))
  
}

RMSE(reg_level_preds_full$var[reg_level_preds_full$name=="PAYEMS"],reg_level_preds_full$value[reg_level_preds_full$name=="PAYEMS"])
mape(reg_level_preds_full$var[reg_level_preds_full$name=="PAYEMS"],reg_level_preds_full$value[reg_level_preds_full$name=="PAYEMS"])
# PAYEMS MAPE: 0.042
sapply(c(2009,2015,2020,2024),function(x) mape(reg_level_preds_full$var[reg_level_preds_full$name=="PAYEMS"&year(reg_level_preds_full$test_date)==x],reg_level_preds_full$value[reg_level_preds_full$name=="PAYEMS"&year(reg_level_preds_full$test_date)==x]))

RMSE(reg_level_preds_full$var[reg_level_preds_full$value!=0],reg_level_preds_full$value[reg_level_preds_full$value!=0])
mape(reg_level_preds_full$var[reg_level_preds_full$value!=0&reg_level_preds_full$name=="PAYEMS"],reg_level_preds_full$value[reg_level_preds_full$value!=0&reg_level_preds_full$name=="PAYEMS"])
# PAYEMS MAPE: 0.042
mape(reg_level_preds_full$var[reg_level_preds_full$value!=0], reg_level_preds_full$value[reg_level_preds_full$value!=0])
# Overall MAPE:0.24
sapply(c(2009,2015,2020,2024),function(x) mape(reg_level_preds_full$var[year(reg_level_preds_full$test_date)==x&reg_level_preds_full$value!=0],reg_level_preds_full$value[year(reg_level_preds_full$test_date)==x&reg_level_preds_full$value!=0]))
sapply(vars_to_impute,function(x) mape(reg_level_preds_full$var[reg_level_preds_full$name==x&reg_level_preds_full$value!=0],reg_level_preds_full$value[reg_level_preds_full$name==x&reg_level_preds_full$value!=0]))


#### simple avg growth rates ####
simple_avg_gr_test = function(col,test_dates,exclude_google_var="deviation_perc"){
  
  if(test_dates[1]<"2010-01-01"&col%in%c("ADPMNUSNERSA")){
    break
  }
  if(test_dates[1]<"2020-01-01"&col%in%c("IHLIDXUS")){
    break
  }
  
  df = make_df('2025-08-10',bad_vars,most_recent = FALSE) %>% 
    group_by(year,qtr) %>%
    fill(PRS85006112,.direction="down") %>% 
    ungroup() %>% 
    select(-c(MTSR133FMS:W017RC1Q027SBEA,A261RX1Q020SBEA:SLCEC1,B096RC1Q027SBEA:A091RC1Q027SBEA,B243RC1Q027SBEA:AD02RC1Q027SBEA,year,qtr))
  
  xvars = grep(paste0("date|",col,"|",exclude_google_var),colnames(df),value=TRUE,invert = TRUE)
  if(test_dates[1]<"2010-01-01"){
    xvars = grep("ADPMNUSNERSA",colnames(df),value=TRUE,invert = TRUE)
  }
  if(test_dates[1]<"2020-01-01"&col%in%c("IHLIDXUS")){
    xvars = grep("IHLIDXUS",colnames(df),value=TRUE,invert = TRUE)
  }
  
  pred_df = data.frame()
  for(dat in as.character(test_dates)){
    
    system(sprintf('echo "\n%s\n"', paste0(c(as.character(dat),var), collapse="")))
    
    df = make_df(dat,bad_vars,most_recent = FALSE) %>% 
      group_by(year,qtr) %>%
      fill(PRS85006112,.direction="down") %>% 
      ungroup() %>% 
      select(-c(MTSR133FMS:W017RC1Q027SBEA,A261RX1Q020SBEA:SLCEC1,B096RC1Q027SBEA:A091RC1Q027SBEA,B243RC1Q027SBEA:AD02RC1Q027SBEA,year,qtr)) %>% 
      filter(date>="2004-01-01")
    
    avg = mean((df %>% mutate_at(col,~(./dplyr::lag(.,12))))[[col]],na.rm=TRUE)
    tmp = df %>% 
      select(date,!!col) %>% 
      mutate_at(col,list(lag=~(dplyr::lag(.,12)))) %>% 
      filter(is.na(get(col))) %>% 
      mutate({{col}} := avg*lag) %>% 
      mutate(test_date=dat) %>% 
      left_join(national_econ %>% filter(series_id==col),by="date")
    
    pred_df = bind_rows(pred_df,tmp)
    
  }
  
  return(pred_df)
  
}

simple_avg_gr_preds_full = data.frame()
for(var in vars_to_impute){
  
  simple_avg_gr_preds = mclapply(list(data.frame(date=as.Date(paste0(2009,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                                      data.frame(date=as.Date(paste0(2015,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                                      data.frame(date=as.Date(paste0(2020,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                                      data.frame(date=as.Date(paste0(2024,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date)),
                                 simple_avg_gr_test,
                                 exclude_google_var="deviation_perc",
                                 col=var,
                                 mc.cores=4)
  
  simple_avg_gr_preds_full = bind_rows(simple_avg_gr_preds_full,bind_rows(simple_avg_gr_preds[which(sapply(simple_avg_gr_preds,length)>1)]))
  
}

simple_avg_gr_preds_full = simple_avg_gr_preds_full %>% 
  pivot_longer(AMDMVS:WTISPLC,names_to="proj_col",values_to="proj_value") %>% 
  filter(series_id==proj_col)

RMSE(simple_avg_gr_preds_full$proj_value[simple_avg_gr_preds_full$proj_col=="PAYEMS"],simple_avg_gr_preds_full$value[simple_avg_gr_preds_full$proj_col=="PAYEMS"])
mape(simple_avg_gr_preds_full$proj_value[simple_avg_gr_preds_full$proj_col=="PAYEMS"],simple_avg_gr_preds_full$value[simple_avg_gr_preds_full$proj_col=="PAYEMS"])
# PAYEMS MAPE: 0.036
sapply(c(2009,2015,2020,2024),function(x) mape(simple_avg_gr_preds_full$proj_value[year(simple_avg_gr_preds_full$test_date)==x&simple_avg_gr_preds_full$proj_col=="PAYEMS"],simple_avg_gr_preds_full$value[year(simple_avg_gr_preds_full$test_date)==x&simple_avg_gr_preds_full$proj_col=="PAYEMS"]))

RMSE(simple_avg_gr_preds_full$proj_value[!(simple_avg_gr_preds_full$proj_value%in%c(-Inf,Inf,0,NA,NaN))&!(simple_avg_gr_preds_full$value%in%c(-Inf,Inf,0,NA,NaN))],simple_avg_gr_preds_full$value[!(simple_avg_gr_preds_full$proj_value%in%c(-Inf,Inf,0,NA,NaN))&!(simple_avg_gr_preds_full$value%in%c(NA,-Inf,Inf,0,NaN))])
mape(simple_avg_gr_preds_full$proj_value[!(simple_avg_gr_preds_full$proj_value%in%c(-Inf,Inf,0,NA,NaN))&!(simple_avg_gr_preds_full$value%in%c(-Inf,Inf,0,NA,NaN))],simple_avg_gr_preds_full$value[!(simple_avg_gr_preds_full$proj_value%in%c(-Inf,Inf,0,NA,NaN))&!(simple_avg_gr_preds_full$value%in%c(NA,-Inf,Inf,0,NaN))])
# Overall MAPE: 0.19
sapply(c(2009,2015,2020,2024),function(x) mape(simple_avg_gr_preds_full$proj_value[year(simple_avg_gr_preds_full$test_date)==x&!(simple_avg_gr_preds_full$proj_value%in%c(-Inf,Inf,0,NA,NaN))&!(simple_avg_gr_preds_full$value%in%c(-Inf,Inf,0,NA,NaN))],simple_avg_gr_preds_full$value[year(simple_avg_gr_preds_full$test_date)==x&!(simple_avg_gr_preds_full$proj_value%in%c(-Inf,Inf,0,NA,NaN))&!(simple_avg_gr_preds_full$value%in%c(NA,-Inf,Inf,0,NaN))]))
sapply(vars_to_impute,function(x) mape(simple_avg_gr_preds_full$proj_value[simple_avg_gr_preds_full$proj_col==x&!(simple_avg_gr_preds_full$proj_value%in%c(-Inf,Inf,0,NA,NaN))&!(simple_avg_gr_preds_full$value%in%c(-Inf,Inf,0,NA,NaN))],simple_avg_gr_preds_full$value[simple_avg_gr_preds_full$proj_col==x&!(simple_avg_gr_preds_full$proj_value%in%c(-Inf,Inf,0,NA,NaN))&!(simple_avg_gr_preds_full$value%in%c(NA,-Inf,Inf,0,NaN))]))


#### rolling avg growth rates ####
rolling_avg_gr_test = function(col,test_dates,exclude_google_var="deviation_perc"){
  
  if(test_dates[1]<"2010-01-01"&col%in%c("ADPMNUSNERSA")){
    break
  }
  if(test_dates[1]<"2020-01-01"&col%in%c("IHLIDXUS")){
    break
  }
  
  df = make_df('2025-08-10',bad_vars,most_recent = FALSE) %>% 
    group_by(year,qtr) %>%
    fill(PRS85006112,.direction="down") %>% 
    ungroup() %>% 
    select(-c(MTSR133FMS:W017RC1Q027SBEA,A261RX1Q020SBEA:SLCEC1,B096RC1Q027SBEA:A091RC1Q027SBEA,B243RC1Q027SBEA:AD02RC1Q027SBEA,year,qtr))
  
  xvars = grep(paste0("date|",col,"|",exclude_google_var),colnames(df),value=TRUE,invert = TRUE)
  if(test_dates[1]<"2010-01-01"){
    xvars = grep("ADPMNUSNERSA",colnames(df),value=TRUE,invert = TRUE)
  }
  if(test_dates[1]<"2020-01-01"&col%in%c("IHLIDXUS")){
    xvars = grep("IHLIDXUS",colnames(df),value=TRUE,invert = TRUE)
  }
  
  pred_df = data.frame()
  for(dat in as.character(test_dates)){
    
    system(sprintf('echo "\n%s\n"', paste0(as.character(dat), collapse="")))
    
    df = make_df(dat,bad_vars,most_recent = FALSE) %>% 
      group_by(year,qtr) %>%
      fill(PRS85006112,.direction="down") %>% 
      ungroup() %>% 
      select(-c(MTSR133FMS:W017RC1Q027SBEA,A261RX1Q020SBEA:SLCEC1,B096RC1Q027SBEA:A091RC1Q027SBEA,B243RC1Q027SBEA:AD02RC1Q027SBEA,year,qtr)) %>% 
      filter(date>="2004-01-01")
    
    avg = mean((df %>% filter(date>=(as.Date(dat) %m-% years(3))) %>% mutate_at(col,~(./dplyr::lag(.,12))))[[col]],na.rm=TRUE)
    tmp = df %>% 
      select(date,!!col) %>% 
      mutate_at(col,list(lag=~(dplyr::lag(.,12)))) %>% 
      filter(is.na(get(col))) %>% 
      mutate({{col}} := avg*lag) %>% 
      mutate(test_date=dat) %>% 
      left_join(national_econ %>% filter(series_id==col),by="date")
    
    pred_df = bind_rows(pred_df,tmp)
    
  }
  
  return(pred_df)
  
  
}

rolling_avg_gr_preds_full = data.frame()
for(var in vars_to_impute){
  
  rolling_avg_gr_preds = mclapply(list(data.frame(date=as.Date(paste0(2009,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                                      data.frame(date=as.Date(paste0(2015,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                                      data.frame(date=as.Date(paste0(2020,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                                      data.frame(date=as.Date(paste0(2024,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date)),
                                  rolling_avg_gr_test,
                                 exclude_google_var="deviation_perc",
                                 col=var,
                                 mc.cores=4)
  
  rolling_avg_gr_preds_full = bind_rows(rolling_avg_gr_preds_full,bind_rows(rolling_avg_gr_preds[which(sapply(rolling_avg_gr_preds,length)>1)]))
  
}

rolling_avg_gr_preds_full = rolling_avg_gr_preds_full %>% 
  pivot_longer(AMDMVS:WTISPLC,names_to="proj_col",values_to="proj_value") %>% 
  filter(series_id==proj_col)

y=rolling_avg_gr_preds_full
RMSE(y$proj_value[!(y$proj_value%in%c(-Inf,Inf,0,NA,NaN))&!(y$value%in%c(-Inf,Inf,0,NA,NaN))],y$value[!(y$proj_value%in%c(-Inf,Inf,0,NA,NaN))&!(y$value%in%c(NA,-Inf,Inf,0,NaN))])
mape(y$proj_value[!(y$proj_value%in%c(-Inf,Inf,0,NA,NaN))&!(y$value%in%c(-Inf,Inf,0,NA,NaN))],y$value[!(y$proj_value%in%c(-Inf,Inf,0,NA,NaN))&!(y$value%in%c(NA,-Inf,Inf,0,NaN))])
# Overall MAPE: 0.39
sapply(c(2009,2015,2020,2024),function(x,y=rolling_avg_gr_preds_full) mape(y$proj_value[year(y$test_date)==x&!(y$proj_value%in%c(-Inf,Inf,0,NA,NaN))&!(y$value%in%c(-Inf,Inf,0,NA,NaN))],y$value[year(y$test_date)==x&!(y$proj_value%in%c(-Inf,Inf,0,NA,NaN))&!(y$value%in%c(NA,-Inf,Inf,0,NaN))]))
sapply(vars_to_impute,function(x,y=rolling_avg_gr_preds_full) mape(y$proj_value[y$proj_col==x&!(y$proj_value%in%c(-Inf,Inf,0,NA,NaN))&!(y$value%in%c(-Inf,Inf,0,NA,NaN))],y$value[y$proj_col==x&!(y$proj_value%in%c(-Inf,Inf,0,NA,NaN))&!(y$value%in%c(NA,-Inf,Inf,0,NaN))]))

#### mice with growth rates ####
mice_gr_test = function(test_dates,exclude_google_var="deviation_perc"){
  
  library(mice)
  
  df = make_df('2025-08-10',bad_vars,most_recent = FALSE) %>% 
    group_by(year,qtr) %>%
    fill(PRS85006112,.direction="down") %>% 
    ungroup() %>% 
    select(-c(MTSR133FMS:W017RC1Q027SBEA,A261RX1Q020SBEA:SLCEC1,B096RC1Q027SBEA:A091RC1Q027SBEA,B243RC1Q027SBEA:AD02RC1Q027SBEA,year,qtr))
  
  xvars = grep(paste(c("date",exclude_google_var,paste(head(sample(grep("gt_",colnames(df),value=TRUE)),-20),collapse="|")),collapse="|"),colnames(df),value=TRUE,invert = TRUE)
  xvars = grep("ADPMNUSNERSA",xvars,value=TRUE,invert = TRUE)
  if(col!="ADPMNUSNERSA"){
    df = df %>% 
      select(-any_of("ADPMNUSNERSA"))
  }
  xvars = grep("IHLIDXUS",xvars,value=TRUE,invert = TRUE)
  if(col!="IHLIDXUS"){
    df = df %>% 
      select(-any_of("IHLIDXUS"))
  }
  
  
  pred_df = data.frame()
  for(dat in as.character(test_dates)){
    
    system(sprintf('echo "\n%s\n"', paste0(as.character(dat), collapse="")))
    
    df = make_df(dat,bad_vars,most_recent = FALSE) %>% 
      group_by(year,qtr) %>%
      fill(PRS85006112,.direction="down") %>% 
      ungroup() %>% 
      select(-c(MTSR133FMS:W017RC1Q027SBEA,A261RX1Q020SBEA:SLCEC1,B096RC1Q027SBEA:A091RC1Q027SBEA,B243RC1Q027SBEA:AD02RC1Q027SBEA,year,qtr))
    
    tmp = df %>% 
      mutate_at(vars(intersect(xvars,c("UNRATE","PRS85006112","GACDISA066MSFRBNY","DTCDISA066MSFRBNY","GACDFSA066MSFRBPHI","DTCDFSA066MSFRBPHI","DFF","DGS10"))),~(.-dplyr::lag(.,12))) %>% 
      mutate_at(vars(setdiff(xvars,c("UNRATE","PRS85006112","GACDISA066MSFRBNY","DTCDISA066MSFRBNY","GACDFSA066MSFRBPHI","DTCDFSA066MSFRBPHI","DFF","DGS10"))),~(./dplyr::lag(.,12)-1)) %>% 
      filter(date>="2005-01-01") %>% 
      select(-any_of(c("ADPMNUSNERSA","IHLIDXUS")))
    
    for(i in which(rowSums(is.na(tmp %>% select(xvars)))>0)){
      
      tempData <- ((mice(tmp %>% select(xvars) %>% slice(1:i),meth='cart',seed=178)))
      tmp1=complete(tempData)
      tmp[1:i,xvars] = tmp1
      
    }
    
    tmp = tmp %>% 
      select(date,xvars)
    
    tmp = df %>% 
      filter(date>="2004-01-01") %>% 
      select(date,xvars) %>% 
      mutate_at(vars(intersect(xvars,c("UNRATE","PRS85006112","GACDISA066MSFRBNY","DTCDISA066MSFRBNY","GACDFSA066MSFRBPHI","DTCDFSA066MSFRBPHI","DFF","DGS10"))),~(dplyr::lag(.,12))) %>% 
      mutate_at(vars(setdiff(xvars,c("UNRATE","PRS85006112","GACDISA066MSFRBNY","DTCDISA066MSFRBNY","GACDFSA066MSFRBPHI","DTCDFSA066MSFRBPHI","DFF","DGS10"))),~(dplyr::lag(.,12))) %>% 
      pivot_longer(cols=xvars) %>% 
      left_join(tmp %>% pivot_longer(cols=colnames(tmp)[-1]),by=c('date','name')) %>% 
      mutate(value.x=case_when(name%in%c("UNRATE","PRS85006112","GACDISA066MSFRBNY","DTCDISA066MSFRBNY","GACDFSA066MSFRBPHI","DTCDFSA066MSFRBPHI","DFF","DGS10")~(value.x+value.y),
                                  name%in%setdiff(xvars,c("UNRATE","PRS85006112","GACDISA066MSFRBNY","DTCDISA066MSFRBNY","GACDFSA066MSFRBPHI","DTCDFSA066MSFRBPHI","DFF","DGS10"))~(value.x*(value.y+1)))) %>% 
      mutate(test_date=dat) %>% 
      inner_join(df %>% 
                   select(date,xvars) %>% 
                   pivot_longer(cols=xvars) %>% 
                   filter(is.na(value)) %>% 
                   select(date,name)) %>% 
      left_join(national_econ,by=c("date"="date","name"="series_id"))
    
    pred_df = bind_rows(pred_df,tmp)
    
  }
  
  return(pred_df)
  
}

mice_gr_preds = mclapply(c(data.frame(date=as.Date(paste0(2009,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                              data.frame(date=as.Date(paste0(2015,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                              data.frame(date=as.Date(paste0(2020,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                              data.frame(date=as.Date(paste0(2024,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date)),
                            mice_gr_test,
                            exclude_google_var="deviation_perc",
                            mc.cores=8)

mice_gr_preds_full = bind_rows(mice_gr_preds)
RMSE(mice_gr_preds_full$value.x[mice_gr_preds_full$name=="PAYEMS"],mice_gr_preds_full$value[mice_gr_preds_full$name=="PAYEMS"])
mape(mice_gr_preds_full$value.x[mice_gr_preds_full$name=="PAYEMS"],mice_gr_preds_full$value[mice_gr_preds_full$name=="PAYEMS"])
# PAYEMS MAPE: 0.033
sapply(c(2009,2015,2020,2024),function(x) mape(mice_gr_preds_full$value.x[mice_gr_preds_full$name=="PAYEMS"&year(mice_gr_preds_full$test_date)==x],mice_gr_preds_full$value[mice_gr_preds_full$name=="PAYEMS"&year(mice_gr_preds_full$test_date)==x]))

y=mice_gr_preds_full
RMSE(y$value.x[!(y$value.x%in%c(-Inf,Inf,0,NA,NaN))&!(y$value%in%c(-Inf,Inf,0,NA,NaN))],y$value[!(y$value.x%in%c(-Inf,Inf,0,NA,NaN))&!(y$value%in%c(NA,-Inf,Inf,0,NaN))])
mape(y$value.x[!(y$value.x%in%c(-Inf,Inf,0,NA,NaN))&!(y$value%in%c(-Inf,Inf,0,NA,NaN))],y$value[!(y$value.x%in%c(-Inf,Inf,0,NA,NaN))&!(y$value%in%c(NA,-Inf,Inf,0,NaN))])
# Overall MAPE: 0.43
sapply(c(2009,2015,2020,2024),function(x,y=mice_gr_preds_full) mape(y$value.x[year(y$test_date)==x&!(y$value.x%in%c(-Inf,Inf,0,NA,NaN))&!(y$value%in%c(-Inf,Inf,0,NA,NaN))],y$value[year(y$test_date)==x&!(y$value.x%in%c(-Inf,Inf,0,NA,NaN))&!(y$value%in%c(NA,-Inf,Inf,0,NaN))]))
sapply(vars_to_impute,function(x,y=mice_gr_preds_full) mape(y$value.x[y$name==x&!(y$value.x%in%c(-Inf,Inf,0,NA,NaN))&!(y$value%in%c(-Inf,Inf,0,NA,NaN))],y$value[y$name==x&!(y$value.x%in%c(-Inf,Inf,0,NA,NaN))&!(y$value%in%c(NA,-Inf,Inf,0,NaN))]))


#### reg with growth rates ####
reg_gr_test = function(col,test_dates,exclude_google_var="deviation_perc"){
  
  library(mice)
  
  df = make_df('2025-08-10',bad_vars,most_recent = FALSE) %>% 
    group_by(year,qtr) %>%
    fill(PRS85006112,.direction="down") %>% 
    ungroup() %>% 
    select(-c(MTSR133FMS:W017RC1Q027SBEA,A261RX1Q020SBEA:SLCEC1,B096RC1Q027SBEA:A091RC1Q027SBEA,B243RC1Q027SBEA:AD02RC1Q027SBEA,year,qtr))
  
  gt_vars = cor(df[[col]],df %>% select(starts_with("gt_")),use="complete.obs")
  xvars = grep(paste(c("date",exclude_google_var,paste(setdiff(colnames(df %>% select(starts_with("gt_"))),colnames(df %>% select(starts_with("gt")))[order(abs(gt_vars))[1:10]]),collapse="|")),collapse="|"),colnames(df),value=TRUE,invert = TRUE)
  xvars = grep("ADPMNUSNERSA",xvars,value=TRUE,invert = TRUE)
  if(col!="ADPMNUSNERSA"){
    df = df %>% 
      select(-any_of("ADPMNUSNERSA"))
  }
  xvars = grep("IHLIDXUS",xvars,value=TRUE,invert = TRUE)
  if(col!="IHLIDXUS"){
    df = df %>% 
      select(-any_of("IHLIDXUS"))
  }
  xvars = sample(xvars,floor(min(length(xvars),max(15,nrow(df %>% select(col,xvars) %>% drop_na())/2))))
  
  pred_df = data.frame()
  for(dat in as.character(test_dates)){
    
    system(sprintf('echo "\n%s\n"', paste0(as.character(dat), collapse="")))
    
    df = make_df(dat,bad_vars,most_recent = FALSE) %>% 
      group_by(year,qtr) %>%
      fill(PRS85006112,.direction="down") %>% 
      ungroup() %>% 
      select(-c(MTSR133FMS:W017RC1Q027SBEA,A261RX1Q020SBEA:SLCEC1,B096RC1Q027SBEA:A091RC1Q027SBEA,B243RC1Q027SBEA:AD02RC1Q027SBEA,year,qtr))
    
    tmp = df %>% 
      mutate_at(vars(intersect(xvars,c("UNRATE","PRS85006112","GACDISA066MSFRBNY","DTCDISA066MSFRBNY","GACDFSA066MSFRBPHI","DTCDFSA066MSFRBPHI","DFF","DGS10"))),~(.-dplyr::lag(.,12))) %>% 
      mutate_at(vars(setdiff(xvars,c("UNRATE","PRS85006112","GACDISA066MSFRBNY","DTCDISA066MSFRBNY","GACDFSA066MSFRBPHI","DTCDFSA066MSFRBPHI","DFF","DGS10"))),~(./dplyr::lag(.,12)-1)) %>% 
      filter(date>="2005-01-01") %>% 
      select(-any_of(c("ADPMNUSNERSA","IHLIDXUS")))    
    
    if(col!="ADPMNUSNERSA"){
      tmp = tmp %>% 
        select(-any_of("ADPMNUSNERSA"))
    }
    if(col!="IHLIDXUS"){
      tmp = tmp %>% 
        select(-any_of("IHLIDXUS"))
    }
    
    for(i in which(is.na(tail(tmp[[col]],12)))){
      
      xvars1 = colnames(tmp %>% filter(date==tail(tmp$date,12)[i]) %>% select(xvars) %>% select_if(!is.na(.)))
      
      reg1 = lm_robust(as.formula(paste0(col,"~",paste(xvars1,collapse="+"))),
                       tmp %>% select(col,xvars1))
      
      tmp1 = tmp %>% 
        filter(date==tail(tmp$date,12)[i]) %>% 
        mutate(var=predict(reg1,.)) %>% 
        select(date,var) %>% 
        mutate(test_date=dat,
               name=col) %>%
        left_join(df %>% 
                    mutate_at(vars(intersect(xvars,c("UNRATE","PRS85006112","GACDISA066MSFRBNY","DTCDISA066MSFRBNY","GACDFSA066MSFRBPHI","DTCDFSA066MSFRBPHI","DFF","DGS10"))),~(dplyr::lag(.,12))) %>% 
                    mutate_at(vars(setdiff(xvars,c("UNRATE","PRS85006112","GACDISA066MSFRBNY","DTCDISA066MSFRBNY","GACDFSA066MSFRBPHI","DTCDFSA066MSFRBPHI","DFF","DGS10"))),~(dplyr::lag(.,12))) %>% 
                    select(date,value=!!col)
                  ) %>% 
        mutate(pred=case_when(name%in%c("UNRATE","PRS85006112","GACDISA066MSFRBNY","DTCDISA066MSFRBNY","GACDFSA066MSFRBPHI","DTCDFSA066MSFRBPHI","DFF","DGS10")~(var+value),
                                 name%in%setdiff(xvars,c("UNRATE","PRS85006112","GACDISA066MSFRBNY","DTCDISA066MSFRBNY","GACDFSA066MSFRBPHI","DTCDFSA066MSFRBPHI","DFF","DGS10"))~(value*(var+1)))) %>% 
        mutate(test_date=dat) %>% 
        left_join(national_econ,by=c("date"="date","name"="series_id")) %>% 
        select(date,test_date,name,pred,value.y,release_date,title)
      
      pred_df = bind_rows(pred_df,
                          tmp1
      )
      
    }
    
  }

  return(pred_df)
  
}

reg_gr_preds_full = data.frame()
for(var in vars_to_impute){
  
  reg_gr_preds = mclapply(c(data.frame(date=as.Date(paste0(2009,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                            data.frame(date=as.Date(paste0(2015,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                            data.frame(date=as.Date(paste0(2020,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                            data.frame(date=as.Date(paste0(2024,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date)),
                          reg_gr_test,
                          col=var,
                          exclude_google_var="deviation_perc",
                          mc.cores=8)
  
  reg_gr_preds_full = bind_rows(reg_gr_preds_full,bind_rows(reg_gr_preds[which(sapply(reg_gr_preds,length)>1)]))
  
}

RMSE(reg_gr_preds_full$pred[reg_gr_preds_full$name=="PAYEMS"],reg_gr_preds_full$value.y[reg_gr_preds_full$name=="PAYEMS"])
mape(reg_gr_preds_full$pred[reg_gr_preds_full$name=="PAYEMS"],reg_gr_preds_full$value.y[reg_gr_preds_full$name=="PAYEMS"])
# PAYEMS MAPE: 0.024
sapply(c(2009,2015,2020,2024),function(x) mape(reg_gr_preds_full$pred[reg_gr_preds_full$name=="PAYEMS"&year(reg_gr_preds_full$test_date)==x],reg_gr_preds_full$value.y[reg_gr_preds_full$name=="PAYEMS"&year(reg_gr_preds_full$test_date)==x]))

y=reg_gr_preds_full
RMSE(y$pred[!(y$pred%in%c(-Inf,Inf,0,NA,NaN))&!(y$value.y%in%c(-Inf,Inf,0,NA,NaN))],y$value.y[!(y$pred%in%c(-Inf,Inf,0,NA,NaN))&!(y$value.y%in%c(NA,-Inf,Inf,0,NaN))])
mape(y$pred[!(y$pred%in%c(-Inf,Inf,0,NA,NaN))&!(y$value.y%in%c(-Inf,Inf,0,NA,NaN))],y$value.y[!(y$pred%in%c(-Inf,Inf,0,NA,NaN))&!(y$value.y%in%c(NA,-Inf,Inf,0,NaN))])
# Overall MAPE: 0.49
sapply(c(2009,2015,2020,2024),function(x,y=reg_gr_preds_full) mape(y$pred[year(y$test_date)==x&!(y$pred%in%c(-Inf,Inf,0,NA,NaN))&!(y$value.y%in%c(-Inf,Inf,0,NA,NaN))],y$value.y[year(y$test_date)==x&!(y$pred%in%c(-Inf,Inf,0,NA,NaN))&!(y$value.y%in%c(NA,-Inf,Inf,0,NaN))]))
sapply(vars_to_impute,function(x,y=reg_gr_preds_full) mape(y$pred[y$name==x&!(y$pred%in%c(-Inf,Inf,0,NA,NaN))&!(y$value.y%in%c(-Inf,Inf,0,NA,NaN))],y$value.y[y$name==x&!(y$pred%in%c(-Inf,Inf,0,NA,NaN))&!(y$value.y%in%c(NA,-Inf,Inf,0,NaN))]))

#### reg with lags ####
reg_lag_test = function(col,test_dates,exclude_google_var="deviation_perc"){
  
  library(mice)
  
  df = make_df('2025-08-10',bad_vars,most_recent = FALSE) %>% 
    group_by(year,qtr) %>%
    fill(PRS85006112,.direction="down") %>% 
    ungroup() %>% 
    select(-c(MTSR133FMS:W017RC1Q027SBEA,A261RX1Q020SBEA:SLCEC1,B096RC1Q027SBEA:A091RC1Q027SBEA,B243RC1Q027SBEA:AD02RC1Q027SBEA,year,qtr))
  
  gt_vars = cor(df[[col]],df %>% select(starts_with("gt_")),use="complete.obs")
  xvars = grep(paste(c("date",exclude_google_var,paste(setdiff(colnames(df %>% select(starts_with("gt_"))),colnames(df %>% select(starts_with("gt")))[order(abs(gt_vars))[1:10]]),collapse="|")),collapse="|"),colnames(df),value=TRUE,invert = TRUE)
  xvars = grep("ADPMNUSNERSA",xvars,value=TRUE,invert = TRUE)
  if(col!="ADPMNUSNERSA"){
    df = df %>% 
      select(-any_of("ADPMNUSNERSA"))
  }
  xvars = grep("IHLIDXUS",xvars,value=TRUE,invert = TRUE)
  if(col!="IHLIDXUS"){
    df = df %>% 
      select(-any_of("IHLIDXUS"))
  }
  xvars = sample(xvars,floor(min(length(xvars),max(15,nrow(df %>% select(col,xvars) %>% drop_na())/2))))
  
  pred_df = data.frame()
  for(dat in as.character(test_dates)){
    
    system(sprintf('echo "\n%s\n"', paste0(as.character(dat)," ",which(vars_to_impute==col)/length(vars_to_impute), collapse="")))
    
    df = make_df(dat,bad_vars,most_recent = FALSE) %>% 
      group_by(year,qtr) %>%
      fill(PRS85006112,.direction="down") %>% 
      ungroup() %>% 
      select(-c(MTSR133FMS:W017RC1Q027SBEA,A261RX1Q020SBEA:SLCEC1,B096RC1Q027SBEA:A091RC1Q027SBEA,B243RC1Q027SBEA:AD02RC1Q027SBEA,year,qtr)) %>% 
      filter(date>="2004-01-01")
    
    if(col!="ADPMNUSNERSA"){
      df = df %>% 
        select(-any_of("ADPMNUSNERSA"))
    }
    if(col!="IHLIDXUS"){
      df = df %>% 
        select(-any_of("IHLIDXUS"))
    }
    
    df = df %>% 
      mutate_at(vars(!!col),list(lag1=~dplyr::lag(.,1),
                               lag12=~dplyr::lag(.,12)))
    
    for(i in which(is.na(tail(df[[col]],12)))){
      
      xvars1 = colnames(df %>% filter(date==tail(df$date,12)[i]) %>% select(xvars) %>% select_if(!is.na(.)))
      xvars1 = append(xvars1,c("lag1","lag12"))
      
      reg1 = lm_robust(as.formula(paste0(col,"~",paste(xvars1,collapse="+"))),
                       df %>% select(col,xvars1))
      
      tmp1 = df %>% 
        filter(date==tail(df$date,12)[i]) %>% 
        mutate(var=predict(reg1,.)) %>% 
        select(date,var) %>% 
        mutate(test_date=dat,
               name=col)
      
     if(i<12) df$lag1[df$date==tail(df$date,12)[i+1]] = tmp1$var
      
      pred_df = bind_rows(pred_df,
                          tmp1
      )
      
    }
    
  }
  
  pred_df = pred_df %>%     
    left_join(national_econ,by=c("date"="date","name"="series_id"))
  
  return(pred_df)
  
}

reg_lag_preds_full = data.frame()
for(var in vars_to_impute){
  
  reg_lag_preds = mclapply(c(data.frame(date=as.Date(paste0(2009,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                             data.frame(date=as.Date(paste0(2015,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                             data.frame(date=as.Date(paste0(2020,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                             data.frame(date=as.Date(paste0(2024,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date)),
                           reg_lag_test,
                           col=var,
                           exclude_google_var="deviation_perc",
                           mc.cores=8)
  
  reg_lag_preds_full = bind_rows(reg_lag_preds_full,bind_rows(reg_lag_preds[which(sapply(reg_lag_preds,length)>1)]))
  
}

RMSE(reg_lag_preds_full$var[reg_lag_preds_full$name=="PAYEMS"],reg_lag_preds_full$value[reg_lag_preds_full$name=="PAYEMS"])
mape(reg_lag_preds_full$var[reg_lag_preds_full$name=="PAYEMS"],reg_lag_preds_full$value[reg_lag_preds_full$name=="PAYEMS"])
# PAYEMS MAPE: 0.009
sapply(c(2009,2015,2020,2024),function(x) mape(reg_lag_preds_full$var[reg_lag_preds_full$name=="PAYEMS"&year(reg_lag_preds_full$test_date)==x],reg_lag_preds_full$value[reg_lag_preds_full$name=="PAYEMS"&year(reg_lag_preds_full$test_date)==x]))

y=reg_lag_preds_full
RMSE(y$var[!(y$var%in%c(-Inf,Inf,0,NA,NaN))&!(y$value%in%c(-Inf,Inf,0,NA,NaN))],y$value[!(y$var%in%c(-Inf,Inf,0,NA,NaN))&!(y$value%in%c(NA,-Inf,Inf,0,NaN))])
mape(y$var[!(y$var%in%c(-Inf,Inf,0,NA,NaN))&!(y$value%in%c(-Inf,Inf,0,NA,NaN))],y$value[!(y$var%in%c(-Inf,Inf,0,NA,NaN))&!(y$value%in%c(NA,-Inf,Inf,0,NaN))])
# Overall MAPE: 0.19
sapply(c(2009,2015,2020,2024),function(x,y=reg_lag_preds_full) mape(y$var[year(y$test_date)==x&!(y$var%in%c(-Inf,Inf,0,NA,NaN))&!(y$value%in%c(-Inf,Inf,0,NA,NaN))],y$value[year(y$test_date)==x&!(y$var%in%c(-Inf,Inf,0,NA,NaN))&!(y$value%in%c(NA,-Inf,Inf,0,NaN))]))
sapply(vars_to_impute,function(x,y=reg_lag_preds_full) mape(y$var[y$name==x&!(y$var%in%c(-Inf,Inf,0,NA,NaN))&!(y$value%in%c(-Inf,Inf,0,NA,NaN))],y$value[y$name==x&!(y$var%in%c(-Inf,Inf,0,NA,NaN))&!(y$value%in%c(NA,-Inf,Inf,0,NaN))]))


compare_df = 
  #simple_avg_preds_full %>% 
  # filter(!is.na(series_id)) %>% 
  # select(date,test_date,series_id,pred,obs=value) %>% 
  # mutate(model="simple_avg") %>% 
  # bind_rows(
  #   rolling_avg_preds_full1 %>% 
  #     filter(!is.na(name)) %>% 
  #     select(date,test_date,series_id=name,pred=value,obs=actual) %>% 
  #     mutate(model="rolling_avg")
  # ) %>% 
  # bind_rows(
  #   mice_level_preds_full %>% 
  #     filter(!is.na(name)) %>% 
  #     select(date,test_date,series_id=name,pred=value.y,obs=value) %>% 
  #     mutate(model="mice_level")
  # ) %>% 
  # bind_rows(
  #   reg_level_preds_full %>% 
  #     filter(!is.na(name)) %>% 
  #     select(date,test_date,series_id=name,pred=var,obs=value) %>% 
  #     mutate(model="reg_level")
  # ) %>% 
  #bind_rows(
    simple_avg_gr_preds_full %>% 
      filter(!is.na(series_id)) %>% 
      select(date,test_date,series_id,pred=proj_value,obs=value) %>% 
      mutate(model="simple_avg_gr") %>% 
  #) %>% 
  bind_rows(
    rolling_avg_gr_preds_full %>% 
      filter(!is.na(series_id)) %>% 
      select(date,test_date,series_id,pred=proj_value,obs=value) %>% 
      mutate(model="rolling_avg_gr")
  ) %>% 
  bind_rows(
    mice_gr_preds_full %>% 
      filter(!is.na(name)) %>% 
      select(date,test_date,series_id=name,pred=value.x,obs=value) %>% 
      mutate(model="mice_gr")
  ) %>% 
  # bind_rows(
  #   reg_gr_preds_full %>% 
  #     filter(!is.na(name)) %>% 
  #     select(date,test_date,series_id=name,pred,obs=value.y) %>% 
  #     mutate(model="reg_gr")
  # ) %>% 
  bind_rows(
    reg_lag_preds_full %>% 
      filter(!is.na(name)) %>% 
      select(date,test_date,series_id=name,pred=var,obs=value) %>% 
      mutate(model="reg_lag")
  ) %>% 
  pivot_wider(id_cols=c(date,test_date,series_id),values_from=c(pred,obs),names_from=model)


compare_df %>% 
  filter(complete.cases(.)) %>% 
  filter(across(4:11, is.finite)) %>% 
  pivot_longer(pred_simple_avg_gr:obs_reg_lag) %>% 
  mutate(pred_flg=ifelse(grepl("pred",name),"pred","obs"),
         model=gsub("pred_|obs_","",name)) %>% 
  select(-name) %>% 
  pivot_wider(values_from=value,names_from=pred_flg) %>% 
  filter(test_date>="2010-01-01") %>% 
  group_by(model,series_id) %>% 
  mutate(mape=mape(pred,obs)) %>% 
  group_by(series_id) %>% 
  summarize(mape_avg=mape(pred,obs),
            scaled=mape[model=="reg_lag"]/mape_avg)

impute_function_old = function(dat){
  
  set.seed(178)
  
  df = make_df(dat,bad_vars,most_recent = FALSE) %>%
    group_by(year,qtr) %>%
    fill(PRS85006112,.direction="down") %>%
    ungroup() %>%
    select(-c(MTSR133FMS:W017RC1Q027SBEA,A261RX1Q020SBEA:SLCEC1,B096RC1Q027SBEA:A091RC1Q027SBEA,B243RC1Q027SBEA:AD02RC1Q027SBEA,year,qtr)) %>%  # remove indeed and retail variables to speed up code, even though they do improve the model fit
    mutate_at(vars(-c(date)),~ifelse(is.infinite(.)|is.nan(.),NA,.)) %>%
    select_if(~sum(!is.na(.))>0|is.character(.)|is.Date(.)) %>%
    select_if(~sd(.,na.rm=TRUE)!=0|is.character(.)|is.Date(.)) %>%
    filter(date>="2004-01-01")
  
  test_dineof=df
  
  flag = 0
  while(flag<3){
    for(col1 in colnames(test_dineof)[c(2:ncol(test_dineof))]){
      
      system(sprintf('echo "\n%s\n"', paste0(as.character(dat)," ",col1, collapse="")))
      
      if(length(which(is.na(test_dineof[c((nrow(test_dineof)-10):nrow(test_dineof)),col1])))==0&col1!="IHLIDXUS"){ next }
      if(!(col1%in%colnames(test_dineof))){next}
      if(col1%in%c("ADPMNUSNERSA")&as.Date(dat)<"2010-01-01"){next}
      if(col1=="IHLIDXUS"&as.Date(dat)<"2021-01-01"){next}
      
      value = data.frame(date=test_dineof$date)
      for(i in 1:30){
        if("IHLIDXUS"%in%colnames(test_dineof)&"ADPMNUSNERSA"%in%colnames(test_dineof)){
          if(col1=="IHLIDXUS"){potential_cols = colnames(test_dineof %>% select(-c(col1,IHLIDXUS,ADPMNUSNERSA,grep("gt_",colnames(test_dineof),value=TRUE))) %>% filter(date==max(date)) %>% select(-date) %>% select_if(!is.na(.)))}else{
            potential_cols = colnames(test_dineof %>% select(-c(col1,IHLIDXUS,ADPMNUSNERSA,grep("gt_",colnames(test_dineof),value=TRUE))) %>% filter(date==test_dineof$date[(nrow(test_dineof)-3):nrow(test_dineof)][head(which(is.na(test_dineof[[col1]][(nrow(test_dineof)-3):nrow(test_dineof)])),1)]) %>% select(-date) %>% select_if(!is.na(.)))
          }
        } else{
          potential_cols = colnames(test_dineof %>% select(-c(col1,grep("gt_",colnames(test_dineof),value=TRUE))) %>% select(-one_of("ADPMNUSNERSA","IHLIDXUS")) %>% filter(date==test_dineof$date[(nrow(test_dineof)-3):nrow(test_dineof)][max(head(which(is.na(test_dineof[[col1]][(nrow(test_dineof)-3):nrow(test_dineof)])),1),1)]) %>% select(-date) %>% select_if(!is.na(.)))
        }
        cols = c(sample(potential_cols,min(c(15,floor(length(potential_cols)/2)))),sample(colnames(test_dineof %>% select(grep("gt_",colnames(test_dineof),value=TRUE))),15))
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
    if(col1==tail(colnames(test_dineof),1)){
      flag = flag+1
    }
  }
  
  return(test_dineof)
  
}

impute_function_new = function(dat,repeats,sample_vars=FALSE,exclude_google_var="deviation_perc"){
  
  set.seed(178)
  
  df = make_df(dat,bad_vars,most_recent = FALSE) %>%
    group_by(year,qtr) %>%
    fill(PRS85006112,.direction="down") %>%
    ungroup() %>%
    select(-c(MTSR133FMS:W017RC1Q027SBEA,A261RX1Q020SBEA:SLCEC1,B096RC1Q027SBEA:A091RC1Q027SBEA,B243RC1Q027SBEA:AD02RC1Q027SBEA,year,qtr)) %>%  # remove indeed and retail variables to speed up code, even though they do improve the model fit
    mutate_at(vars(-c(date)),~ifelse(is.infinite(.)|is.nan(.),NA,.)) %>%
    select_if(~sum(!is.na(.))>0|is.character(.)|is.Date(.)) %>%
    select_if(~sd(.,na.rm=TRUE)!=0|is.character(.)|is.Date(.)) %>%
    filter(date>="2004-01-01")
  
  test_dineof=df
  
  gt_cor_df = make_df(dat,bad_vars,most_recent = FALSE) %>% 
    group_by(year,qtr) %>%
    fill(PRS85006112,.direction="down") %>% 
    ungroup() %>% 
    select(-c(MTSR133FMS:W017RC1Q027SBEA,A261RX1Q020SBEA:SLCEC1,B096RC1Q027SBEA:A091RC1Q027SBEA,B243RC1Q027SBEA:AD02RC1Q027SBEA,year,qtr))
  
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
      xvars = sample(xvars,floor(min(length(xvars),max(15,nrow(gt_cor_df1 %>% select(col,xvars) %>% drop_na())/2))))
      
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
        for(i in which(is.na(tail(tmp[[col1]],12)))){
          
          xvars1 = colnames(tmp %>% filter(date==tail(tmp$date,12)[i]) %>% select(xvars) %>% select_if(!is.na(.)))
          if(sample_vars){
            xvars1 = sample(xvars1,min(c(length(xvars1),10)))
          }
          
          reg1 = lm_robust(as.formula(paste0(col1,"~",paste(c("lag1","lag12",xvars1),collapse="+"))),
                           tmp)
          
          tmp1 = tmp %>% 
            filter(date==tail(tmp$date,12)[i]) %>% 
            mutate(var=predict(reg1,.)) %>% 
            select(date,var) %>% 
            mutate(test_date=dat,
                   name=col)
          
          if(i<12) tmp$lag1[tmp$date==tail(tmp$date,12)[i+1]] = tmp1$var
          
          pred_df = bind_rows(pred_df,
                              tmp1
          )
          
        }
        
        imp <- tmp %>% 
          select(date,!!col1) %>% 
          left_join(pred_df %>% 
                      select(date,var)) %>% 
          mutate("{col1}":=coalesce(!!sym(col1),var)) %>% 
          pull(!!col1)
        
        value=bind_cols(value,imp)
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


new_test = mclapply(c(data.frame(date=as.Date(paste0(2009,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                           data.frame(date=as.Date(paste0(2015,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                           data.frame(date=as.Date(paste0(2020,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                           data.frame(date=as.Date(paste0(2024,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date)),
                         impute_function_new,
                         exclude_google_var="deviation_perc",
                         sample_vars=FALSE,
                         repeats=3,
                         mc.cores=8)

new_test_w_sample = mclapply(c(data.frame(date=as.Date(paste0(2009,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                      data.frame(date=as.Date(paste0(2015,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                      data.frame(date=as.Date(paste0(2020,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                      data.frame(date=as.Date(paste0(2024,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date)),
                    impute_function_new,
                    exclude_google_var="deviation_perc",
                    sample_vars=TRUE,
                    repeats=1,
                    mc.cores=8)

new_test_wo_sample_1repeat = mclapply(c(data.frame(date=as.Date(paste0(2009,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                               data.frame(date=as.Date(paste0(2015,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                               data.frame(date=as.Date(paste0(2020,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                               data.frame(date=as.Date(paste0(2024,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date)),
                             impute_function_new,
                             exclude_google_var="deviation_perc",
                             sample_vars=FALSE,
                             repeats=1,
                             mc.cores=8)


old_test = mclapply(c(data.frame(date=as.Date(paste0(2009,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                      data.frame(date=as.Date(paste0(2015,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                      data.frame(date=as.Date(paste0(2020,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
                      data.frame(date=as.Date(paste0(2024,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date)),
                    impute_function_old,
                    mc.cores=8)

dates = c(data.frame(date=as.Date(paste0(2009,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
          data.frame(date=as.Date(paste0(2015,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
          data.frame(date=as.Date(paste0(2020,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date),
          data.frame(date=as.Date(paste0(2024,"-",rep(1:12,each=3),"-01"))) %>% group_by(date) %>% mutate(num=1:n()) %>% ungroup() %>% mutate(date=case_when(num==1~date,num==2~date+14,num==3~ceiling_date(date,"month")-1)) %>% pull(date))
names(old_test) = dates
names(new_test) = dates
names(new_test_w_sample) = dates
names(new_test_wo_sample_1repeat) = dates

compare_funs = data.frame()

for(dat in names(old_test)){
  
  print(dat)
  
  get_na = make_df(dat,bad_vars,most_recent = FALSE) %>%
    group_by(year,qtr) %>%
    fill(PRS85006112,.direction="down") %>%
    ungroup() %>%
    select(-c(MTSR133FMS:W017RC1Q027SBEA,A261RX1Q020SBEA:SLCEC1,B096RC1Q027SBEA:A091RC1Q027SBEA,B243RC1Q027SBEA:AD02RC1Q027SBEA,year,qtr)) %>%  # remove indeed and retail variables to speed up code, even though they do improve the model fit
    mutate_at(vars(-c(date)),~ifelse(is.infinite(.)|is.nan(.),NA,.)) %>%
    select_if(~sum(!is.na(.))>0|is.character(.)|is.Date(.)) %>%
    select_if(~sd(.,na.rm=TRUE)!=0|is.character(.)|is.Date(.)) %>%
    filter(date>="2004-01-01") %>% 
    tail(.,12) %>% 
    pivot_longer(cols=PAYEMS:gt_999) %>% 
    filter(is.na(value)) %>% 
    left_join(old_test[[dat]] %>% 
                pivot_longer(cols=PAYEMS:gt_999) %>% 
                rename(old_test=value),
              by=c("date","name")) %>% 
    left_join(new_test[[dat]] %>% 
                pivot_longer(cols=PAYEMS:gt_999) %>% 
                rename(new_test=value) %>% 
                select(-c(lag1,lag12)),
              by=c("date","name")) %>% 
    left_join(new_test_w_sample[[dat]] %>% 
                pivot_longer(cols=PAYEMS:gt_999) %>% 
                rename(new_test_w_sample=value) %>% 
                select(-c(lag1,lag12)),
              by=c("date","name")) %>% 
    left_join(new_test_wo_sample_1repeat[[dat]] %>% 
                pivot_longer(cols=PAYEMS:gt_999) %>% 
                rename(new_test_wo_sample_1repeat=value) %>% 
                select(-c(lag1,lag12)),
              by=c("date","name")) %>% 
    left_join(national_econ,by=c("date"="date","name"="series_id")) %>% 
    mutate(test_date=dat)
  
  compare_funs = bind_rows(compare_funs,get_na)
  
}

compare_funs1 = compare_funs %>% 
  group_by(name,test_date) %>% 
  filter(!any(is.infinite(c(old_test,new_test,new_test_w_sample,new_test_wo_sample_1repeat)))) %>% 
  filter(!is.na(value.y)&value.y!=0) %>% 
  ungroup()

compare_funs1 %>% 
  summarize_at(vars(old_test:new_test_wo_sample_1repeat),~mape(.,value.y))

compare_funs1 %>% 
  group_by(year(test_date)) %>% 
  summarize_at(vars(old_test:new_test_wo_sample_1repeat),~mape(.,value.y))

compare_funs1 %>% 
  group_by(name) %>% 
  summarize_at(vars(old_test:new_test_wo_sample_1repeat),~mape(.,value.y))

# name               old_test new_test new_test_w_sample new_test_wo_sample_1repeat
# <chr>                 <dbl>    <dbl>             <dbl>                      <dbl>
#   1 ADPMNUSNERSA        0.00766  0.00556           0.00549                    0.00613
# 2 AMDMVS              0.0224   0.0353            0.0237                     0.0305 
# 3 AMTMUO              0.00854  0.0124            0.0103                     0.0108 
# 4 BOPTEXP             0.0285   0.0386            0.0268                     0.0355 
# 5 BOPTIMP             0.0284   0.0315            0.0281                     0.0314 
# 6 CE16OV              0.0161   0.0115            0.0101                     0.00937
# 7 CPIAUCSL            0.00445  0.00639           0.00402                    0.00609
# 8 CPILFESL            0.00145  0.00247           0.00134                    0.00175
# 9 DFF                 0.458    1.79              0.657                      1.42   
# 10 DGORDER             0.0444   0.0806            0.0523                     0.0659 
# 11 DGS10               0.0900   0.194             0.0974                     0.118  
# 12 DSPIC96             0.0146   0.0245            0.0151                     0.0174 
# 13 DTCDFSA066MSFRBPHI  2.43     4.12              3.27                       3.94   
# 14 DTCDISA066MSFRBNY   1.39     2.43              1.33                       1.35   
# 15 GACDFSA066MSFRBPHI  2.49     4.57              2.91                       2.83   
# 16 GACDISA066MSFRBNY   2.85     5.43              3.07                       2.39   
# 17 HOUST               0.0956   0.172             0.110                      0.158  
# 18 HSN1F               0.0877   0.163             0.0930                     0.132  
# 19 ICSA                0.299    0.671             0.273                      0.390  
# 20 IHLIDXUS            0.0116   0.0819            0.0833                     0.0785 
# 21 INDPRO              0.0151   0.0200            0.0164                     0.0181 
# 22 IQ                  0.00945  0.0137            0.0101                     0.0120 
# 23 IR                  0.0148   0.0325            0.0205                     0.0282 
# 24 JTSJOL              0.0714   0.100             0.0753                     0.0904 
# 25 PAYEMS              0.0165   0.00946           0.00905                    0.00938
# 26 PCE                 0.0128   0.0128            0.0119                     0.0136 
# 27 PCEPI               0.00257  0.00381           0.00288                    0.00347
# 28 PCEPILFE            0.00115  0.00217           0.00124                    0.00189
# 29 PERMIT              0.0638   0.130             0.0653                     0.106  
# 30 PRS85006112         2.13     4.74              2.41                       2.94   
# 31 RRSFS               0.0212   0.0242            0.0228                     0.0208 
# 32 TOTALSA             0.0772   0.119             0.0675                     0.107  
# 33 TOTBUSIMNSA         0.00687  0.00792           0.00617                    0.00909
# 34 TTLCONS             0.0136   0.0189            0.0112                     0.0170 
# 35 UMCSENT             0.0489   0.0863            0.0520                     0.0853 
# 36 UNRATE              0.102    0.0978            0.0892                     0.0848 
# 37 WHLSLRIMSA          0.00798  0.0143            0.00756                    0.00901
# 38 WTISPLC             0.181    0.312             0.219                      0.299  

# get list of variables where new_test_w_sample does better than old_test in 2009 and 2020
compare_funs1 %>% filter(year(test_date)%in%c(2009,2020)) %>% 
  group_by(year(test_date),name) %>% 
  summarize_at(vars(old_test:new_test_wo_sample_1repeat),~mape(.,value.y)) %>% 
  mutate_at(vars(new_test:new_test_wo_sample_1repeat),~.-old_test) %>% 
  group_by(name) %>% 
  filter(all(new_test_w_sample<0))


impute_function_old = function(df,dat){
  
  set.seed(178)
  
  test_dineof=df
  
  flag = 0
  while(flag<3){
    for(col1 in colnames(test_dineof)[c(2:ncol(test_dineof))]){
      
      system(sprintf('echo "\n%s\n"', paste0(as.character(dat)," ",col1, collapse="")))
      
      if(length(which(is.na(test_dineof[c((nrow(test_dineof)-10):nrow(test_dineof)),col1])))==0&col1!="IHLIDXUS"){ next }
      if(!(col1%in%colnames(test_dineof))){next}
      if(col1%in%c("ADPMNUSNERSA")&as.Date(dat)<"2010-01-01"){next}
      if(col1=="IHLIDXUS"&as.Date(dat)<"2021-01-01"){next}
      
      value = data.frame(date=test_dineof$date)
      for(i in 1:30){
        if("IHLIDXUS"%in%colnames(test_dineof)&"ADPMNUSNERSA"%in%colnames(test_dineof)){
          if(col1=="IHLIDXUS"){potential_cols = colnames(test_dineof %>% select(-c(col1,IHLIDXUS,ADPMNUSNERSA,grep("gt_",colnames(test_dineof),value=TRUE))) %>% filter(date==max(date)) %>% select(-date) %>% select_if(!is.na(.)))}else{
            potential_cols = colnames(test_dineof %>% select(-c(col1,IHLIDXUS,ADPMNUSNERSA,grep("gt_",colnames(test_dineof),value=TRUE))) %>% filter(date==test_dineof$date[(nrow(test_dineof)-3):nrow(test_dineof)][head(which(is.na(test_dineof[[col1]][(nrow(test_dineof)-3):nrow(test_dineof)])),1)]) %>% select(-date) %>% select_if(!is.na(.)))
          }
        } else{
          potential_cols = colnames(test_dineof %>% select(-c(col1,grep("gt_",colnames(test_dineof),value=TRUE))) %>% select(-one_of("ADPMNUSNERSA","IHLIDXUS")) %>% filter(date==test_dineof$date[(nrow(test_dineof)-3):nrow(test_dineof)][max(head(which(is.na(test_dineof[[col1]][(nrow(test_dineof)-3):nrow(test_dineof)])),1),1)]) %>% select(-date) %>% select_if(!is.na(.)))
        }
        cols = c(sample(potential_cols,min(c(15,floor(length(potential_cols)/2)))),sample(colnames(test_dineof %>% select(grep("gt_",colnames(test_dineof),value=TRUE))),15))
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
    if(col1==tail(colnames(test_dineof),1)){
      flag = flag+1
    }
  }
  
  return(test_dineof)
  
}

impute_function_new = function(df,dat,repeats,sample_vars=FALSE,exclude_google_var="deviation_perc"){
  
  set.seed(178)
  
  test_dineof=df
  
  gt_cor_df = make_df(dat,bad_vars,most_recent = FALSE) %>% 
    group_by(year,qtr) %>%
    fill(PRS85006112,.direction="down") %>% 
    ungroup() %>% 
    select(-c(MTSR133FMS:W017RC1Q027SBEA,A261RX1Q020SBEA:SLCEC1,B096RC1Q027SBEA:A091RC1Q027SBEA,B243RC1Q027SBEA:AD02RC1Q027SBEA,year,qtr))
  
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
      xvars = sample(xvars,floor(min(length(xvars),max(15,nrow(gt_cor_df1 %>% select(col,xvars) %>% drop_na())/2))))
      
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
        for(i in which(is.na(tail(tmp[[col1]],12)))){
          
          xvars1 = colnames(tmp %>% filter(date==tail(tmp$date,12)[i]) %>% select(xvars) %>% select_if(!is.na(.)))
          if(sample_vars){
            xvars1 = sample(xvars1,min(c(length(xvars1),10)))
          }
          
          reg1 = lm_robust(as.formula(paste0(col1,"~",paste(c("lag1","lag12",xvars1),collapse="+"))),
                           tmp)
          
          tmp1 = tmp %>% 
            filter(date==tail(tmp$date,12)[i]) %>% 
            mutate(var=predict(reg1,.)) %>% 
            select(date,var) %>% 
            mutate(test_date=dat,
                   name=col)
          
          if(i<12) tmp$lag1[tmp$date==tail(tmp$date,12)[i+1]] = tmp1$var
          
          pred_df = bind_rows(pred_df,
                              tmp1
          )
          
        }
        
        imp <- tmp %>% 
          select(date,!!col1) %>% 
          left_join(pred_df %>% 
                      select(date,var)) %>% 
          mutate("{col1}":=coalesce(!!sym(col1),var)) %>% 
          pull(!!col1)
        
        value=bind_cols(value,imp)
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
    old %>% select(-c(CPILFESL,TOTBUSIMNSA,UNRATE,WHLSLRIMSA,ADPMNUSNERSA,IHLIDXUS)),
    new %>% select(c(CPILFESL,TOTBUSIMNSA,UNRATE,WHLSLRIMSA,ADPMNUSNERSA,IHLIDXUS))
  ) %>% 
    relocate(colnames(df))
  
  return(final_df)
  
}


tst = df %>% 
  tail(.,12) %>% 
  pivot_longer(cols=PAYEMS:gt_999) %>% 
  filter(is.na(value)) %>% 
  left_join(imputed_df %>% 
              tail(.,12) %>% 
              pivot_longer(cols=PAYEMS:gt_999),
            by=c("date","name")) %>% 
  left_join(national_econ,by=c("date"="date","name"="series_id"))




