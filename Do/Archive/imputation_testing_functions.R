
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
#' impute_function
#' \code{impute_function} takes an incomplete data frame of economic data and makes it full based on the available observations
#' 
#'  @param df data.frame with monthly economic data
#'  @param dat is the date that is the 'end date' of the data
#'  
#'  @return imputed data frame


impute_function_ols = function(df,dat){
  test_dineof=df
  
  flag = 0
  all_df = data.frame()
  while(flag<3){
    for(col1 in colnames(test_dineof %>% select(-one_of("PRS85006112","A261RX1Q020SBEA","A261RX1Q020SBEA", "GDPC1","PCECC96","DGDSRX1Q020SBEA",
                                                        "PCDGCC96","PCNDGC96","PCESVC96","GPDIC1", "FPIC1",          
                                                        "PNFIC1","PRFIC1" ,"EXPGSC1" ,"IMPGSC1","GCEC1" ,         
                                                        "FGCEC1", "SLCEC1","W006RC1Q027SBEA", "A074RC1Q027SBEA", "W007RC1Q027SBEA" ,"B234RC1Q027SBEA" ,"B235RC1Q027SBEA", "B075RC1Q027SBEA",
                                                        "W780RC1Q027SBEA" ,"W009RC1Q027SBEA" ,"B094RC1Q027SBEA" ,"W053RC1Q027SBEA" ,"B1040C1Q027SBEA" ,"W011RC1Q027SBEA",
                                                        "W012RC1Q027SBEA" ,"B233RC1Q027SBEA" ,"B097RC1Q027SBEA" ,"FGEXPND"         ,"A957RC1Q027SBEA" ,"W014RC1Q027SBEA",
                                                        "W015RC1Q027SBEA" ,"B087RC1Q027SBEA" ,"FGSL"            ,"W017RC1Q027SBEA" ,"A091RC1Q027SBEA" ,"B096RC1Q027SBEA",
                                                        "B243RC1Q027SBEA" ,"W018RC1Q027SBEA" ,"W019RCQ027SBEA"  ,"AD02RC1Q027SBEA","year","qtr","date")) %>% select(-starts_with("gt_")))){      
      print(paste0(col1))
      
      if(length(which(is.na(test_dineof[c((nrow(test_dineof)-10):nrow(test_dineof)),col1])))==0&col1!="IHLIDXUS"){ next }
      if(!(col1%in%colnames(test_dineof))){next}
      if(col1%in%c("ADPMNUSNERSA")&as.Date(dat)<"2010-01-01"){next}
      if(col1=="IHLIDXUS"&as.Date(dat)<"2021-01-01"){next}
      
      value = data.frame(date=test_dineof$date)
      for(i in 1:5){
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
      
      for(i in which(test_dineof$date%in%tail(test_dineof$date,10)[which(is.na(tail(test_dineof[[col1]],10)))])){
        
        if(is.nan(value1[i,"replacement"])|is.na(value1[i,"replacement"])){
          next
        }
        
        test_df = data.frame(
          prediction_date = dat,
          variable=col1,
          date=value1$date[i],
          replacement=value1[i,"replacement"]
        )
        
        all_df = bind_rows(all_df,test_df)
        
      }
      
    }
      flag = flag+1

  }
  
  all_df = all_df %>% 
    group_by(variable,prediction_date) %>% 
    slice(n()) %>% 
    ungroup()
  
  return(all_df)
  
}

impute_function_arima = function(df,dat){
  
    test_dineof=df
  
    value = data.frame()
    for(col1 in colnames(test_dineof %>% select(-one_of("PRS85006112","A261RX1Q020SBEA","A261RX1Q020SBEA", "GDPC1","PCECC96","DGDSRX1Q020SBEA",
                                                        "PCDGCC96","PCNDGC96","PCESVC96","GPDIC1", "FPIC1",          
                                                       "PNFIC1","PRFIC1" ,"EXPGSC1" ,"IMPGSC1","GCEC1" ,         
                                                        "FGCEC1", "SLCEC1","W006RC1Q027SBEA", "A074RC1Q027SBEA", "W007RC1Q027SBEA" ,"B234RC1Q027SBEA" ,"B235RC1Q027SBEA", "B075RC1Q027SBEA",
                                                       "W780RC1Q027SBEA" ,"W009RC1Q027SBEA" ,"B094RC1Q027SBEA" ,"W053RC1Q027SBEA" ,"B1040C1Q027SBEA" ,"W011RC1Q027SBEA",
                                                       "W012RC1Q027SBEA" ,"B233RC1Q027SBEA" ,"B097RC1Q027SBEA" ,"FGEXPND"         ,"A957RC1Q027SBEA" ,"W014RC1Q027SBEA",
                                                       "W015RC1Q027SBEA" ,"B087RC1Q027SBEA" ,"FGSL"            ,"W017RC1Q027SBEA" ,"A091RC1Q027SBEA" ,"B096RC1Q027SBEA",
                                                       "B243RC1Q027SBEA" ,"W018RC1Q027SBEA" ,"W019RCQ027SBEA"  ,"AD02RC1Q027SBEA","year","qtr","date")) %>% select(-starts_with("gt_")))){
      
      if(sum(!is.na(test_dineof[[col1]]))<15){
        next
      }
      rmse = c()
      for(i in c(25,50,100,200,Inf)){
        test =   auto.arima(tail(test_dineof[[col1]],i),
                            seasonal=FALSE,
                            biasadj=TRUE,
                            max.p=10,max.q=10,max.d=2)
        
        check_df = as.data.frame(cbind(
          pred=as.numeric(tail(test$fitted,25)),
          actual=as.numeric(na.omit(tail(test_dineof[[col1]],25)))
        ))
        
        rmse = append(rmse,RMSE(check_df$pred,check_df$actual,na.rm=TRUE))
      }
      
      test =   auto.arima(tail(test_dineof[[col1]],c(25,50,100,200,Inf)[which.min(rmse)]),
                          seasonal=FALSE,
                          biasadj=TRUE,
                          max.p=10,max.q=10,max.d=2)
      
        if(sum(is.na(tail(test_dineof[[col1]],10)))==0){
          next
        }
      
        fcast_sav = test %>% 
          forecast(h=sum(is.na(tail(test_dineof[[col1]],10))))
    
      
      value1 = data.frame(
        prediction_date = dat,
        variable=col1,
        date=tail(test_dineof[['date']],sum(is.na(tail(test_dineof[[col1]],10)))),
        replacement=as.numeric(fcast_sav$mean)
      )
      
      value = bind_rows(value,value1)
      
    }
  
  return(value)
  
}

impute_function_rf = function(df,dat,col2){
  
  conflicted::conflicts_prefer(dplyr::first)
  conflicted::conflicts_prefer(dplyr::between)
  conflicted::conflicts_prefer(dplyr::last)
  conflicted::conflicts_prefer(miceRanger::impute)
  
  require(ranger)
  require(mlr)
  require(tuneRanger)
  require(miceRanger)
  require(parsnip)
  
  
  
  test_dineof=df
  
  flag = 0
  all_df = data.frame()
  
    for(col1 in col2){      
      print(paste0(col1))
      
      if(length(which(is.na(test_dineof[c((nrow(test_dineof)-10):nrow(test_dineof)),col1])))==0&col1!="IHLIDXUS"){ next }
      if(!(col1%in%colnames(test_dineof))){next}
      if(col1%in%c("ADPMNUSNERSA")&as.Date(dat)<"2010-01-01"){next}
      if(col1=="IHLIDXUS"&as.Date(dat)<"2021-01-01"){next}
      
      rf_data = test_dineof %>% select(-one_of('IHLIDXUS')) %>% select(-c(col1,PRS85006112,A261RX1Q020SBEA:SLCEC1,W006RC1Q027SBEA:AD02RC1Q027SBEA,year,qtr,date)) 
      rf_data = missRanger(rf_data, pmm.k = 5, num.trees = 100,num.threads=1)
      # rf_data = completeData(miceRanger(
      #   rf_data
      #   , m=1,
      #   maxiter=1,
      #   num.threads=1,
      #   parallel=FALSE,
      #   , returnModels = TRUE
      #   , verbose=TRUE
      # ))$Dataset_1
      rf_data = rf_data %>% 
        bind_cols(test_dineof %>% select(col1)) %>% 
        mutate(lag=dplyr::lag(!!sym(col1),1),
               lag2=dplyr::lag(!!sym(col1),2),
               lag3=dplyr::lag(!!sym(col1),3),
               lag4=dplyr::lag(!!sym(col1),4),
               lag5=dplyr::lag(!!sym(col1),5),
               lag6=dplyr::lag(!!sym(col1),6))
      
      rf_data_scaled = scale(rf_data) %>% 
        as.data.frame() 
      
      col1.task = makeRegrTask(data = rf_data_scaled %>% drop_na(), target = col1)
      
      # Rough Estimation of the Tuning time
      estimateTimeTuneRanger(col1.task,num.threads = 1)
      
      # Tuning process (takes around 1 minute); Tuning measure is the multiclass brier score
      res = tuneRanger(col1.task, num.trees = 500, iters = 10,num.threads = 1)
      
      
      rf_2 = ranger(
        as.formula(paste0(col1,"~.")), 
        data = rf_data_scaled %>% drop_na(), 
        mtry = res$recommended.pars$mtry, 
        min.node.size = res$recommended.pars$min.node.size,
        num.trees = 500, 
        importance = "impurity",
        regularization.factor = 0.3,
        num.threads = 1
      )
      
      for(i in which(test_dineof$date%in%tail(test_dineof$date,10)[which(is.na(tail(test_dineof[[col1]],10)))])){
     
        rf_data_scaled[i,col1] =  predict(rf_2,data=rf_data_scaled)$predictions[i]/predict(rf_2,data=rf_data_scaled)$predictions[i-1]*rf_data_scaled[[col1]][i-1]
        
        if(i!=nrow(rf_data_scaled)){
          
          rf_data_scaled = rf_data_scaled %>% 
            mutate(lag=dplyr::lag(!!sym(col1),1),
                   lag2=dplyr::lag(!!sym(col1),2),
                   lag3=dplyr::lag(!!sym(col1),3),
                   lag4=dplyr::lag(!!sym(col1),4),
                   lag5=dplyr::lag(!!sym(col1),5),
                   lag6=dplyr::lag(!!sym(col1),6)) %>% 
            mutate_all(as.numeric)
          
        }
        
        if(i==nrow(rf_data_scaled)){

          rf_data = unscale(rf_data_scaled,scale(rf_data))

        }

      }
      
      for(i in which(test_dineof$date%in%tail(test_dineof$date,10)[which(is.na(tail(test_dineof[[col1]],10)))])){
        
        if(is.nan(rf_data[i,col1])|is.na(rf_data[i,col1])){
          next
        }
        
        test_df = data.frame(
          prediction_date = dat,
          variable=col1,
          date=test_dineof$date[i],
          replacement=rf_data[i,col1]
        )
        
        all_df = bind_rows(all_df,test_df)
        
      }
    
  }
  
  return(all_df)
  
}

impute_function_lasso = function(df,dat){
  test_dineof=df
  
  flag = 0
  all_df = data.frame()
  while(flag<3){
    for(col1 in colnames(test_dineof %>% select(-one_of("PRS85006112","A261RX1Q020SBEA","A261RX1Q020SBEA", "GDPC1","PCECC96","DGDSRX1Q020SBEA",
                                                        "PCDGCC96","PCNDGC96","PCESVC96","GPDIC1", "FPIC1",          
                                                        "PNFIC1","PRFIC1" ,"EXPGSC1" ,"IMPGSC1","GCEC1" ,         
                                                        "FGCEC1", "SLCEC1","W006RC1Q027SBEA", "A074RC1Q027SBEA", "W007RC1Q027SBEA" ,"B234RC1Q027SBEA" ,"B235RC1Q027SBEA", "B075RC1Q027SBEA",
                                                        "W780RC1Q027SBEA" ,"W009RC1Q027SBEA" ,"B094RC1Q027SBEA" ,"W053RC1Q027SBEA" ,"B1040C1Q027SBEA" ,"W011RC1Q027SBEA",
                                                        "W012RC1Q027SBEA" ,"B233RC1Q027SBEA" ,"B097RC1Q027SBEA" ,"FGEXPND"         ,"A957RC1Q027SBEA" ,"W014RC1Q027SBEA",
                                                        "W015RC1Q027SBEA" ,"B087RC1Q027SBEA" ,"FGSL"            ,"W017RC1Q027SBEA" ,"A091RC1Q027SBEA" ,"B096RC1Q027SBEA",
                                                        "B243RC1Q027SBEA" ,"W018RC1Q027SBEA" ,"W019RCQ027SBEA"  ,"AD02RC1Q027SBEA","year","qtr","date")) %>% select(-starts_with("gt_")))){      
      print(paste0(col1))
      
      if(length(which(is.na(test_dineof[c((nrow(test_dineof)-10):nrow(test_dineof)),col1])))==0&col1!="IHLIDXUS"){ next }
      if(!(col1%in%colnames(test_dineof))){next}
      if(col1%in%c("ADPMNUSNERSA")&as.Date(dat)<"2011-01-01"){next}
      if(col1=="IHLIDXUS"&as.Date(dat)<"2022-01-01"){next}
      
      value = data.frame(date=test_dineof$date)

        if("IHLIDXUS"%in%colnames(test_dineof)&"ADPMNUSNERSA"%in%colnames(test_dineof)){
          if(col1=="IHLIDXUS"){potential_cols = colnames(test_dineof %>% select(-c(col1,IHLIDXUS,ADPMNUSNERSA,year,qtr)) %>% filter(date==max(date)) %>% select(-date) %>% select_if(!is.na(.)))}else{
            potential_cols = colnames(test_dineof %>% select(-c(col1,IHLIDXUS,ADPMNUSNERSA,year,qtr)) %>% filter(date==test_dineof$date[(nrow(test_dineof)-3):nrow(test_dineof)][head(which(is.na(test_dineof[[col1]][(nrow(test_dineof)-3):nrow(test_dineof)])),1)]) %>% select(-date) %>% select_if(!is.na(.)))
          }
        } else{
          potential_cols = colnames(test_dineof %>% select(-c(col1,year,qtr)) %>% select(-one_of("ADPMNUSNERSA","IHLIDXUS")) %>% filter(date==test_dineof$date[(nrow(test_dineof)-3):nrow(test_dineof)][max(head(which(is.na(test_dineof[[col1]][(nrow(test_dineof)-3):nrow(test_dineof)])),1),1)]) %>% select(-date) %>% select_if(!is.na(.)))
        }
      
      cv_model <- cv.glmnet(as.matrix(test_dineof %>% select(col1,potential_cols) %>% 
                              mutate(lag=dplyr::lag(!!sym(col1),1),
                                     lag2=dplyr::lag(!!sym(col1),2),
                                     lag3=dplyr::lag(!!sym(col1),3),
                                     lag4=dplyr::lag(!!sym(col1),4),
                                     lag5=dplyr::lag(!!sym(col1),5),
                                     lag6=dplyr::lag(!!sym(col1),6)) %>% 
                              filter(!is.na(lag6)&!is.na(!!sym(col1))) %>% 
                              select_if(~sum(is.na(.))==0) %>% 
                              select(-col1)), 
                            test_dineof %>% 
                              select(col1,potential_cols) %>% 
                              mutate(lag=dplyr::lag(!!sym(col1),1),
                                     lag2=dplyr::lag(!!sym(col1),2),
                                     lag3=dplyr::lag(!!sym(col1),3),
                                     lag4=dplyr::lag(!!sym(col1),4),
                                     lag5=dplyr::lag(!!sym(col1),5),
                                     lag6=dplyr::lag(!!sym(col1),6)) %>% 
                              filter(!is.na(lag6)&!is.na(!!sym(col1))) %>% 
                              select_if(~sum(is.na(.))==0) %>% 
                              pull(col1), 
                            alpha = 1)
      
      best_lambda <- cv_model$lambda.min
      
      best_model <- glmnet(as.matrix(test_dineof %>% select(col1,potential_cols) %>% 
                                       mutate(lag=dplyr::lag(!!sym(col1),1),
                                              lag2=dplyr::lag(!!sym(col1),2),
                                              lag3=dplyr::lag(!!sym(col1),3),
                                              lag4=dplyr::lag(!!sym(col1),4),
                                              lag5=dplyr::lag(!!sym(col1),5),
                                              lag6=dplyr::lag(!!sym(col1),6)) %>% 
                                       filter(!is.na(lag6)&!is.na(!!sym(col1))) %>% 
                                       select_if(~sum(is.na(.))==0) %>% 
                                       select(-col1)), 
                           test_dineof %>% 
                             select(col1,potential_cols) %>% 
                             mutate(lag=dplyr::lag(!!sym(col1),1),
                                    lag2=dplyr::lag(!!sym(col1),2),
                                    lag3=dplyr::lag(!!sym(col1),3),
                                    lag4=dplyr::lag(!!sym(col1),4),
                                    lag5=dplyr::lag(!!sym(col1),5),
                                    lag6=dplyr::lag(!!sym(col1),6)) %>% 
                             filter(!is.na(lag6)&!is.na(!!sym(col1))) %>% 
                             select_if(~sum(is.na(.))==0) %>% 
                             pull(col1), 
                           alpha = 1, lambda = best_lambda)
      
       imp = predict(best_model, s = best_lambda, newx = as.matrix(test_dineof %>% 
                                                                mutate(lag=dplyr::lag(!!sym(col1),1),
                                                                       lag2=dplyr::lag(!!sym(col1),2),
                                                                       lag3=dplyr::lag(!!sym(col1),3),
                                                                       lag4=dplyr::lag(!!sym(col1),4),
                                                                       lag5=dplyr::lag(!!sym(col1),5),
                                                                       lag6=dplyr::lag(!!sym(col1),6)) %>% 
                                                                select(rownames(coef(best_model))[-1])))
      
        
        value=bind_cols(value,imp)
      
      
      value1 = data.frame(
        date=value$date,
        replacement=value$s1
      )
      
      for(i in which(test_dineof$date%in%tail(test_dineof$date,10)[which(is.na(tail(test_dineof[[col1]],10)))])){
        
        if(is.nan(value1[i,"replacement"])|is.na(value1[i,"replacement"])){
          next
        }
        
        test_df = data.frame(
          prediction_date = dat,
          variable=col1,
          date=value1$date[i],
          replacement=value1[i,"replacement"]
        )
        
        all_df = bind_rows(all_df,test_df)
        
      }
      
    }
    flag = flag+1
    
  }
  
  return(all_df)
  
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
                                                 "B243RC1Q027SBEA" ,"W018RC1Q027SBEA" ,"W019RCQ027SBEA"  ,"AD02RC1Q027SBEA","year","qtr","date")) %>% 
                    select_if(~sum(is.na(.))<15)) 
 
    test =   na_kalman(test_dineof[,cols])
    tmp=sapply(test_dineof[,cols],function(x) which(is.na(x)))
    value1 = bind_rows(lapply(names(tmp[which(as.numeric(sapply(tmp,function(x) length(x)))>0)]),
                              function(x) data.frame(prediction_date = dat,
                                                     variable=x,
                                                     date=test_dineof[as.numeric(tmp[x][[1]]),"date"],
                                                     replacement=as.numeric(unlist(test[as.numeric(tmp[x][[1]]),x])))))
  
  return(value1)
  
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
                                                 "B243RC1Q027SBEA" ,"W018RC1Q027SBEA" ,"W019RCQ027SBEA"  ,"AD02RC1Q027SBEA","year","qtr","date")) %>% select(-starts_with("gt_")))
  
  miceObj <- miceRanger(
    test_dineof %>% 
      select(-one_of("PRS85006112","A261RX1Q020SBEA","A261RX1Q020SBEA", "GDPC1","PCECC96","DGDSRX1Q020SBEA",
                     "PCDGCC96","PCNDGC96","PCESVC96","GPDIC1", "FPIC1",          
                     "PNFIC1","PRFIC1" ,"EXPGSC1" ,"IMPGSC1","GCEC1" ,         
                     "FGCEC1", "SLCEC1","W006RC1Q027SBEA", "A074RC1Q027SBEA", "W007RC1Q027SBEA" ,"B234RC1Q027SBEA" ,"B235RC1Q027SBEA", "B075RC1Q027SBEA",
                     "W780RC1Q027SBEA" ,"W009RC1Q027SBEA" ,"B094RC1Q027SBEA" ,"W053RC1Q027SBEA" ,"B1040C1Q027SBEA" ,"W011RC1Q027SBEA",
                     "W012RC1Q027SBEA" ,"B233RC1Q027SBEA" ,"B097RC1Q027SBEA" ,"FGEXPND"         ,"A957RC1Q027SBEA" ,"W014RC1Q027SBEA",
                     "W015RC1Q027SBEA" ,"B087RC1Q027SBEA" ,"FGSL"            ,"W017RC1Q027SBEA" ,"A091RC1Q027SBEA" ,"B096RC1Q027SBEA",
                     "B243RC1Q027SBEA" ,"W018RC1Q027SBEA" ,"W019RCQ027SBEA"  ,"AD02RC1Q027SBEA","year","qtr","date")) %>% 
      select_if(~!all(is.na(.))),
    valueSelector = "meanMatch"
  )
  
  dataList <- completeData(miceObj)
  
  tmp = data.frame(date=test_dineof$date)
  for(col1 in cols){
    
    tmp[[col1]] = rowMeans(sapply(dataList,function(x) x[[col1]]))
    
    if(length(test_dineof$date[is.na(test_dineof[[col1]])])==0){next}
    
    test_df = data.frame(
      prediction_date = test_dineof$date[is.na(test_dineof[[col1]])],
      variable=col1,
      date=test_dineof$date[is.na(test_dineof[[col1]])],
      replacement=tmp[[col1]][is.na(test_dineof[[col1]])]
    )
    
    all_df = bind_rows(all_df,test_df)
    
  }
  
  return(all_df)
  
}

impute_function_nixtla = function(df,dat){
  
  require(nixtlar)
  nixtla_set_api_key(Sys.getenv("NIXTLA_KEY"))

  
  test_dineof=df
  
  flag = 0
  all_df = data.frame()
  while(flag<3){
    for(col1 in colnames(test_dineof %>% select(-one_of("PRS85006112","A261RX1Q020SBEA","A261RX1Q020SBEA", "GDPC1","PCECC96","DGDSRX1Q020SBEA",
                                                        "PCDGCC96","PCNDGC96","PCESVC96","GPDIC1", "FPIC1",          
                                                        "PNFIC1","PRFIC1" ,"EXPGSC1" ,"IMPGSC1","GCEC1" ,         
                                                        "FGCEC1", "SLCEC1","W006RC1Q027SBEA", "A074RC1Q027SBEA", "W007RC1Q027SBEA" ,"B234RC1Q027SBEA" ,"B235RC1Q027SBEA", "B075RC1Q027SBEA",
                                                        "W780RC1Q027SBEA" ,"W009RC1Q027SBEA" ,"B094RC1Q027SBEA" ,"W053RC1Q027SBEA" ,"B1040C1Q027SBEA" ,"W011RC1Q027SBEA",
                                                        "W012RC1Q027SBEA" ,"B233RC1Q027SBEA" ,"B097RC1Q027SBEA" ,"FGEXPND"         ,"A957RC1Q027SBEA" ,"W014RC1Q027SBEA",
                                                        "W015RC1Q027SBEA" ,"B087RC1Q027SBEA" ,"FGSL"            ,"W017RC1Q027SBEA" ,"A091RC1Q027SBEA" ,"B096RC1Q027SBEA",
                                                        "B243RC1Q027SBEA" ,"W018RC1Q027SBEA" ,"W019RCQ027SBEA"  ,"AD02RC1Q027SBEA","year","qtr","date")) %>% select(-starts_with("gt_")))){      
      print(paste0(col1))
      
      if(length(which(is.na(test_dineof[c((nrow(test_dineof)-10):nrow(test_dineof)),col1])))==0&col1!="IHLIDXUS"){ next }
      if(!(col1%in%colnames(test_dineof))){next}
      if(col1%in%c("ADPMNUSNERSA")&as.Date(dat)<"2010-01-01"){next}
      if(col1=="IHLIDXUS"&as.Date(dat)<"2021-01-01"){next}

        if("IHLIDXUS"%in%colnames(test_dineof)&"ADPMNUSNERSA"%in%colnames(test_dineof)){
          if(col1=="IHLIDXUS"){potential_cols = colnames(test_dineof %>% select(-c(col1,IHLIDXUS,ADPMNUSNERSA,gt_1003:gt_999)) %>% filter(date==max(date)) %>% select(-date) %>% select_if(!is.na(.)))}else{
            potential_cols = colnames(test_dineof %>% select(-c(col1,IHLIDXUS,ADPMNUSNERSA,gt_1003:gt_999)) %>% filter(date==test_dineof$date[(nrow(test_dineof)-3):nrow(test_dineof)][head(which(is.na(test_dineof[[col1]][(nrow(test_dineof)-3):nrow(test_dineof)])),1)]) %>% select(-date) %>% select_if(!is.na(.)))
          }
        } else{
          potential_cols = colnames(test_dineof %>% select(-c(col1,gt_1003:gt_999)) %>% select(-one_of("ADPMNUSNERSA","IHLIDXUS")) %>% filter(date==test_dineof$date[(nrow(test_dineof)-3):nrow(test_dineof)][max(head(which(is.na(test_dineof[[col1]][(nrow(test_dineof)-3):nrow(test_dineof)])),1),1)]) %>% select(-date) %>% select_if(!is.na(.)))
        }
        cols = c(potential_cols,colnames(test_dineof %>% select(gt_1003:gt_999)))
        
        # Model and prediction
        
        m = nixtla_client_forecast(
          test_dineof %>% select(col1,date) %>% filter(!is.na(!!sym(col1))) %>% mutate(id_col=1),
          h = length(tail(test_dineof$date,10)[which(is.na(tail(test_dineof[[col1]],10)))]), 
          freq="M",
          id_col="id_col",
          time_col="date",
          target_col=col1,
          X_df=reg_data %>% filter(!is.na(!!sym(col1))) %>% select(cols)
        )
        
        value1 = data.frame(
          prediction_date=dat,
          date=tail(test_dineof$date,10)[which(is.na(tail(test_dineof[[col1]],10)))],
          variable=col1,
          replacement=m$TimeGPT
        )
      
        test_dineof[[col1]][test_dineof$date%in%as.Date(m$date)] = m$TimeGPT
        
      
      all_df = bind_rows(all_df,value1)
      
    }
    flag = flag+1
  }
  
  return(all_df)
  
}


unscale <- function(vals,norm.data,col.ids) {
  cols <- if (missing(col.ids)) 1:NCOL(vals) else col.ids
  if (length(cols) != NCOL(vals)) stop('Incorrect dimension of data to unscale.')
  centers <- attr(norm.data,'scaled:center')[cols]
  scales <- attr(norm.data,'scaled:scale')[cols]
  unvals <- scale(vals,center=(-centers/scales),scale=1/scales)
  attr(unvals,'scaled:center') <- attr(unvals,'scaled:scale') <- NULL
  unvals
}
