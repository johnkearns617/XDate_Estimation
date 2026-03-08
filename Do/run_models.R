# run models
run_date_cy = "2025-12-31"
run_date_fy = "2025-09-30"
data_date = "2026-01-18"

# Nowcasting headline

#### Nowcast Headline Outlays ####
monthly_shares = outlays_fred %>% 
  filter(fiscal_year>=2002&fiscal_year<=year(as.Date(run_date_fy))) %>% 
  group_by(fiscal_year) %>% 
  mutate(total=sum(value,na.rm=TRUE)) %>% 
  ungroup() %>%  
  mutate(share=value/total,
         month=month(date)) %>% 
  filter(date<=run_date_fy)

monthly_shares_reg = lm_robust(share~factor(month),monthly_shares)

x_data = read_csv(paste0("Data/Processing/imputed_data/imputed_data_asof",data_date,".csv"))  %>% 
  select(-any_of(paste0("gt_",bad_vars$category))) %>% 
  arrange(date) %>%
  ungroup() %>% 
  mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10),.funs=list(ch12m=~((./dplyr::lag(.,12)-1)*100),ch1m=~((./dplyr::lag(.,1)-1)*100))) %>%
  mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,grep("gt_",colnames(.),value=TRUE)),.funs=list(ch12m=~.-dplyr::lag(.,12),ch1m=~.-dplyr::lag(.,1))) %>%
  mutate_at(vars(PAYEMS:gt_999_ch1m),.funs=list(lag1=~dplyr::lag(.,1),lag2=~dplyr::lag(.,2),lag3=~dplyr::lag(.,3),lag4=~dplyr::lag(.,4))) %>% 
  filter(date<=run_date_fy) %>% 
  mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)))  %>% 
  left_join(cbo_proj %>% 
            filter(component=="outlay"&category=="Total") %>% 
            group_by(projected_fiscal_year) %>% 
            slice(n()) %>% 
            select(projected_fiscal_year,value) %>% 
            rename(cbo_proj=value,
                   fiscal_year=projected_fiscal_year)) %>% 
  mutate(month=month(date)) %>% 
  left_join(outlays_fred %>% 
              select(date,value)) # join the yvariable
x_data$cbo_proj_month = as.numeric(predict(monthly_shares_reg,x_data))*x_data$cbo_proj
x_data = x_data %>% 
  mutate(cbo_proj_diff=(value/cbo_proj_month-1)*100) %>% 
  mutate(lag1_cbo_proj_diff=dplyr::lag(cbo_proj_diff,1),
         lag2_cbo_proj_diff=dplyr::lag(cbo_proj_diff,2)) %>% 
  mutate(lag1=dplyr::lag(value,1),
         lag2=dplyr::lag(value,2),
         lag3=dplyr::lag(value,3),
         lag4=dplyr::lag(value,4))

X = model.matrix(as.formula(paste0("value","~",paste(colnames(x_data)[c(2:which(colnames(x_data)=="gt_999_ch1m_lag4"))],collapse="+"))),
                 x_data %>% filter(date<=run_date_fy&year(date)>=2006&!is.na(value)))[, -1]
y = (x_data %>% filter(date<=run_date_fy&year(date)>=2006&!is.na(value)))[["value"]]

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

test = lm_robust(as.formula(paste0("value","~lag1+lag2+lag3+lag4+cbo_proj_month+",paste(c(rownames(selected_coefs_state)),collapse="+"))),
                 data = x_data %>% filter(date<=run_date_fy) %>% mutate(weight=(1:n())/n()))

saveRDS(list(monthly_shares_reg=monthly_shares_reg,model=test),file="Data/Processing/Models/nowcast_headline_outlay.RDS")

#### Nowcast Headline Revenue ####
monthly_shares = receipts_fred %>% 
  filter(fiscal_year>=2002&fiscal_year<=year(as.Date(run_date_fy))) %>% 
  group_by(fiscal_year) %>% 
  mutate(total=sum(value,na.rm=TRUE)) %>% 
  ungroup() %>%  
  mutate(share=value/total,
         month=month(date)) %>% 
  filter(date<=run_date_fy)

monthly_shares_reg = lm_robust(share~factor(month),monthly_shares)

x_data = read_csv(paste0("Data/Processing/imputed_data/imputed_data_asof",data_date,".csv"))  %>% 
  select(-any_of(paste0("gt_",bad_vars$category))) %>% 
  arrange(date) %>%
  ungroup() %>% 
  mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10),.funs=list(ch12m=~((./dplyr::lag(.,12)-1)*100),ch1m=~((./dplyr::lag(.,1)-1)*100))) %>%
  mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,grep("gt_",colnames(.),value=TRUE)),.funs=list(ch12m=~.-dplyr::lag(.,12),ch1m=~.-dplyr::lag(.,1))) %>%
  mutate_at(vars(PAYEMS:gt_999_ch1m),.funs=list(lag1=~dplyr::lag(.,1),lag2=~dplyr::lag(.,2),lag3=~dplyr::lag(.,3),lag4=~dplyr::lag(.,4))) %>% 
  filter(date<=run_date_fy) %>% 
  mutate(fiscal_year=as.integer(quarter(date, with_year = TRUE, fiscal_start = 10)))  %>% 
  left_join(cbo_proj %>% 
              filter(component=="revenue"&category=="Total") %>% 
              group_by(projected_fiscal_year) %>% 
              slice(n()) %>% 
              select(projected_fiscal_year,value) %>% 
              rename(cbo_proj=value,
                     fiscal_year=projected_fiscal_year)) %>% 
  mutate(month=month(date)) %>% 
  left_join(receipts_fred %>% 
              select(date,value)) # join the yvariable
x_data$cbo_proj_month = as.numeric(predict(monthly_shares_reg,x_data))*x_data$cbo_proj
x_data = x_data %>% 
  mutate(cbo_proj_diff=(value/cbo_proj_month-1)*100) %>% 
  mutate(lag1_cbo_proj_diff=dplyr::lag(cbo_proj_diff,1),
         lag2_cbo_proj_diff=dplyr::lag(cbo_proj_diff,2)) %>% 
  mutate(lag1=dplyr::lag(value,1),
         lag2=dplyr::lag(value,2),
         lag3=dplyr::lag(value,3),
         lag4=dplyr::lag(value,4))

X = model.matrix(as.formula(paste0("value","~",paste(colnames(x_data)[c(2:which(colnames(x_data)=="gt_999_ch1m_lag4"))],collapse="+"))),
                 x_data %>% filter(date<=run_date_fy&year(date)>=2006&!is.na(value)))[, -1]
y = (x_data %>% filter(date<=run_date_fy&year(date)>=2006&!is.na(value)))[["value"]]

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

test = lm_robust(as.formula(paste0("value","~lag1+lag2+lag3+lag4+cbo_proj_month+",paste(c(rownames(selected_coefs_state)),collapse="+"))),
                 data = x_data %>% filter(date<=run_date_fy) %>% mutate(weight=(1:n())/n()))

saveRDS(list(monthly_shares_reg=monthly_shares_reg,model=test),file="Data/Processing/Models/nowcast_headline_revenue.RDS")

#### Component Nowcast Models ####
x_data = read_csv(paste0("Data/Processing/imputed_data/imputed_data_asof",data_date,".csv"))  %>% 
  filter(date<=(as.Date(run_date_fy) %m+% months(12))) %>% 
  select(-any_of(paste0("gt_",bad_vars$category))) %>% 
  arrange(date) %>%
  ungroup() %>% 
  mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10),.funs=list(ch12m=~((./dplyr::lag(.,12)-1)*100),ch1m=~((./dplyr::lag(.,1)-1)*100))) %>%
  mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,grep("gt_",colnames(.),value=TRUE)),.funs=list(ch12m=~.-dplyr::lag(.,12),ch1m=~.-dplyr::lag(.,1))) %>%
  mutate_at(vars(PAYEMS:gt_999_ch1m),.funs=list(lag1=~dplyr::lag(.,1),lag2=~dplyr::lag(.,2),lag3=~dplyr::lag(.,3),lag4=~dplyr::lag(.,4)))

#### Individual Income Taxes ####
monthly_shares = get_monthly_shares_df_revenue(receipts,"Total -- Individual Income Taxes","revenue","Individual Income Taxes") %>% 
  mutate(tax_due=case_when(
    !(fiscal_year%in%c(2020,2021))&month==4~1,
    fiscal_year==2020&month==7~1,
    fiscal_year==2021&month==5~1,
    TRUE~0
  ),
  quarter_end=ifelse(month%in%c(1,4,6,9),1,0)) %>% 
  filter(date<=(as.Date(run_date_fy) %m+% months(12))) 

# monthly_shares_reg = lm_robust(cum_share~factor(fy_month),monthly_shares %>% filter(date<=run_date_fy&fiscal_year>2015))

monthly_shares_reg <- ranger(share ~ quarter_end + fy_month + tax_due, 
                             data = monthly_shares %>% filter(date<=run_date_fy&fiscal_year>2015), 
                             importance = 'permutation',
                             scale.permutation.importance = TRUE,
                             quantreg = TRUE,
                             mtry = 3,
                             write.forest = TRUE)

monthly_shares$pred_share=as.numeric(predict(monthly_shares_reg,data=monthly_shares)$predictions)
monthly_shares = monthly_shares %>% 
  group_by(fiscal_year) %>% 
  arrange(date) %>% 
  mutate(pred_cumshare=cumsum(pred_share))
monthly_shares$pred_total = monthly_shares$cum_total/monthly_shares$pred_cumshare

for(i in 0){
  monthly_shares1 = monthly_shares %>% 
    left_join(x_data,by="date")
  
  monthly_shares1 = monthly_shares1 %>% 
    mutate(cbo_proj=cbo_proj*pred_cumshare) %>% 
    group_by(fiscal_year) %>% 
    mutate(tst1=cbo_proj-dplyr::lag(cbo_proj,1),
           cbo_proj_month=ifelse(fy_month==1,cbo_proj,tst1)) %>% 
    ungroup() %>% 
    mutate(current_value=value,          
           total=lead(total,i),
           cbo_proj_month=lead(cbo_proj_month,i),
           value=lead(value,i)) %>% 
    filter(date<=run_date_fy&!is.na(value))
  
  X = model.matrix(as.formula(paste0("total","~",paste(grep("_ch",colnames(monthly_shares1),value=TRUE),collapse="+"))),
                   monthly_shares1 %>% filter(date<=run_date_fy))[, -1]
  y = (monthly_shares1%>% filter(date<=run_date_fy))[['value']]
  
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
  
  res_unshrunk = lm(as.formula(paste0("value~cbo_proj_month-1+",paste(rownames(selected_coefs_state),collapse="+"))),monthly_shares1,x=TRUE,y=TRUE)
  res_shrunk = shrink(res_unshrunk,join=list(rownames(selected_coefs_state)))$postfit
  
}

saveRDS(list(share=monthly_shares_reg,res_unshrunk=res_unshrunk,res_shrunk=res_shrunk,cbo=monthly_shares1 %>% select(date,actual=value,cbo_proj=cbo_proj_month)),file="Data/Processing/Models/nowcast_Individual Income Taxes.RDS")


#### Corporate Income Taxes ####
monthly_shares = get_monthly_shares_df_revenue(receipts,"Corporation Income Taxes","revenue","Corporate Income Taxes") %>% 
  mutate(tax_due=case_when(
    !(fiscal_year%in%c(2020))&month==4~1,
    fiscal_year==2020&month==7~1,
    TRUE~0
  ),
  quarter_end=ifelse(month%in%c(4,6,9,12),1,0)) %>% 
  filter(date<=(as.Date(run_date_fy) %m+% months(12))) 

# monthly_shares_reg = lm_robust(cum_share~factor(fy_month)*factor(tax_due)*factor(quarter_end),monthly_shares %>% filter(date<=run_date_fy&fiscal_year>2015))

monthly_shares_reg <- ranger(share ~ quarter_end + fy_month + tax_due, 
                             data = monthly_shares %>% filter(date<=run_date_fy&fiscal_year>2015), 
                             importance = 'permutation',
                             scale.permutation.importance = TRUE,
                             quantreg = TRUE,
                             mtry = 3,
                             write.forest = TRUE)

monthly_shares$pred_share=as.numeric(predict(monthly_shares_reg,data=monthly_shares)$predictions)
monthly_shares = monthly_shares %>% 
  group_by(fiscal_year) %>% 
  arrange(date) %>% 
  mutate(pred_cumshare=cumsum(pred_share))
monthly_shares$pred_total = monthly_shares$cum_total/monthly_shares$pred_cumshare

for(i in 0){
monthly_shares1 = monthly_shares %>% 
  left_join(x_data,by="date")

monthly_shares1 = monthly_shares1 %>% 
  mutate(cbo_proj=cbo_proj*pred_cumshare) %>% 
  group_by(fiscal_year) %>% 
  mutate(tst1=cbo_proj-dplyr::lag(cbo_proj,1),
         cbo_proj_month=ifelse(fy_month==1,cbo_proj,tst1)) %>% 
  ungroup() %>% 
  mutate(current_value=value,          
         total=lead(total,i),
         cbo_proj_month=lead(cbo_proj_month,i),
         value=lead(value,i)) %>% 
  filter(date<=run_date_fy&!is.na(value))

X = model.matrix(as.formula(paste0("total","~",paste(grep("_ch",colnames(monthly_shares1),value=TRUE),collapse="+"))),
                 monthly_shares1%>% filter(date<=run_date_fy))[, -1]
y = (monthly_shares1%>% filter(date<=run_date_fy))[['value']]

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

res_unshrunk = lm(as.formula(paste0("value~cbo_proj_month-1+",paste(rownames(selected_coefs_state),collapse="+"))),monthly_shares1,x=TRUE,y=TRUE)
res_shrunk = shrink(res_unshrunk,join=list(rownames(selected_coefs_state)))$postfit
}

saveRDS(list(share=monthly_shares_reg,res_unshrunk=res_unshrunk,res_shrunk=res_shrunk,cbo=monthly_shares1 %>% select(date,actual=value,cbo_proj=cbo_proj_month)),file="Data/Processing/Models/nowcast_Corporate Income Taxes.RDS")

#### Payroll Taxes ####
monthly_shares = get_monthly_shares_df_revenue(receipts,"Total -- Social Insurance and Retirement Receipts","revenue","Payroll Taxes") %>% 
  filter(date<=(as.Date(run_date_fy) %m+% months(12)))

monthly_shares_reg = lm_robust(cum_share~factor(fy_month),monthly_shares %>% filter(date<=run_date_fy))

monthly_shares$pred_cumshare=as.numeric(predict(monthly_shares_reg,monthly_shares))

for(i in 0){
monthly_shares1 = monthly_shares %>% 
  left_join(x_data,by="date")

monthly_shares1 = monthly_shares1 %>% 
  mutate(cbo_proj=cbo_proj*pred_cumshare) %>% 
  group_by(fiscal_year) %>% 
  mutate(tst1=cbo_proj-dplyr::lag(cbo_proj,1),
         cbo_proj_month=ifelse(fy_month==1,cbo_proj,tst1)) %>% 
  ungroup() %>% 
  mutate(current_value=value,          
         total=lead(total,i),
         cbo_proj_month=lead(cbo_proj_month,i),
         value=lead(value,i)) %>% 
  filter(date<=run_date_fy&!is.na(value))

X = model.matrix(as.formula(paste0("total","~",paste(grep("_ch",colnames(monthly_shares1),value=TRUE),collapse="+"))),
                 monthly_shares1 %>% filter(date<=run_date_fy))[, -1]
y = (monthly_shares1 %>% filter(date<=run_date_fy))[['value']]

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

res_unshrunk = lm(as.formula(paste0("value~cbo_proj_month-1+",paste(rownames(selected_coefs_state),collapse="+"))),monthly_shares1,x=TRUE,y=TRUE)
res_shrunk = shrink(res_unshrunk,join=list(rownames(selected_coefs_state)))$postfit
}

saveRDS(list(share=monthly_shares_reg,res_unshrunk=res_unshrunk,res_shrunk=res_shrunk,cbo=monthly_shares1 %>% select(date,actual=value,cbo_proj=cbo_proj_month)),file="Data/Processing/Models/nowcast_Payroll Taxes.RDS")

#### Excise Taxes ####
monthly_shares = get_monthly_shares_df_revenue(receipts,"Total -- Excise Taxes","revenue","Excise Taxes") %>% 
  mutate(tax_due=case_when(
    fiscal_year==2020&month==9~1,
    TRUE~0
  )) %>% 
  filter(date<=(as.Date(run_date_fy) %m+% months(12))) 

monthly_shares_reg = lm_robust(cum_share~factor(fy_month)+factor(tax_due),monthly_shares %>% filter(date<=run_date_fy&fiscal_year>2015))

monthly_shares$pred_cumshare=as.numeric(predict(monthly_shares_reg,monthly_shares))

for(i in 0){
monthly_shares1 = monthly_shares %>% 
  left_join(x_data,by="date")

monthly_shares1 = monthly_shares1 %>% 
  mutate(cbo_proj=cbo_proj*pred_cumshare) %>% 
  group_by(fiscal_year) %>% 
  mutate(tst1=cbo_proj-dplyr::lag(cbo_proj,1),
         cbo_proj_month=ifelse(fy_month==1,cbo_proj,tst1)) %>% 
  ungroup() %>% 
  mutate(current_value=value,          
         total=lead(total,i),
         cbo_proj_month=lead(cbo_proj_month,i),
         value=lead(value,i)) %>% 
  filter(date<=run_date_fy&!is.na(value))

X = model.matrix(as.formula(paste0("total","~",paste(grep("_ch",colnames(monthly_shares1),value=TRUE),collapse="+"))),
                 monthly_shares1 %>% filter(date<=run_date_fy))[, -1]
y = (monthly_shares1 %>% filter(date<=run_date_fy))[['value']]

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

res_unshrunk = lm(as.formula(paste0("value~cbo_proj_month-1+",paste(rownames(selected_coefs_state),collapse="+"))),monthly_shares1,x=TRUE,y=TRUE)
res_shrunk = shrink(res_unshrunk,join=list(rownames(selected_coefs_state)))$postfit
}

saveRDS(list(share=monthly_shares_reg,res_unshrunk=res_unshrunk,res_shrunk=res_shrunk,cbo=monthly_shares1 %>% select(date,actual=value,cbo_proj=cbo_proj_month)),file="Data/Processing/Models/nowcast_Excise Taxes.RDS")

#### Estate Taxes ####
monthly_shares = get_monthly_shares_df_revenue(receipts,"Estate and Gift Taxes","revenue","Estate and Gift Taxes") %>% 
  filter(date<=(as.Date(run_date_fy) %m+% months(12))) 

monthly_shares_reg = lm_robust(cum_share~factor(fy_month),monthly_shares %>% filter(date<=run_date_fy&fiscal_year>2015))

monthly_shares$pred_cumshare=as.numeric(predict(monthly_shares_reg,monthly_shares))

for(i in 0){
monthly_shares1 = monthly_shares %>% 
  left_join(x_data,by="date")

monthly_shares1 = monthly_shares1 %>% 
  mutate(cbo_proj=cbo_proj*pred_cumshare) %>% 
  group_by(fiscal_year) %>% 
  mutate(tst1=cbo_proj-dplyr::lag(cbo_proj,1),
         cbo_proj_month=ifelse(fy_month==1,cbo_proj,tst1)) %>% 
  ungroup() %>% 
  mutate(current_value=value,          
         total=lead(total,i),
         cbo_proj_month=lead(cbo_proj_month,i),
         value=lead(value,i)) %>% 
  filter(date<=run_date_fy&!is.na(value))

X = model.matrix(as.formula(paste0("total","~",paste(grep("_ch",colnames(monthly_shares1),value=TRUE),collapse="+"))),
                 monthly_shares1%>% filter(date<=run_date_fy))[, -1]
y = (monthly_shares1%>% filter(date<=run_date_fy))[['value']]

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

res_unshrunk = lm(as.formula(paste0("value~cbo_proj_month-1+",paste(rownames(selected_coefs_state),collapse="+"))),monthly_shares1,x=TRUE,y=TRUE)
res_shrunk = shrink(res_unshrunk,join=list(rownames(selected_coefs_state)))$postfit
}

saveRDS(list(share=monthly_shares_reg,res_unshrunk=res_unshrunk,res_shrunk=res_shrunk,cbo=monthly_shares1 %>% select(date,actual=value,cbo_proj=cbo_proj_month)),file="Data/Processing/Models/nowcast_Estate and Gift Taxes.RDS")

#### Customs Taxes ####
monthly_shares = get_monthly_shares_df_revenue(receipts,"Customs Duties","revenue","Customs Duties") %>% 
  filter(date<=(as.Date(run_date_fy) %m+% months(12))) 

monthly_shares_reg = lm_robust(cum_share~factor(fy_month),monthly_shares%>% filter(date<=run_date_fy&fiscal_year>2015))

monthly_shares$pred_cumshare=as.numeric(predict(monthly_shares_reg,monthly_shares))

for(i in 0){
monthly_shares1 = monthly_shares %>% 
  left_join(x_data,by="date")

monthly_shares1 = monthly_shares1 %>% 
  mutate(cbo_proj=cbo_proj*pred_cumshare) %>% 
  group_by(fiscal_year) %>% 
  mutate(tst1=cbo_proj-dplyr::lag(cbo_proj,1),
         cbo_proj_month=ifelse(fy_month==1,cbo_proj,tst1)) %>% 
  ungroup() %>% 
  mutate(current_value=value,          
         total=lead(total,i),
         cbo_proj_month=lead(cbo_proj_month,i),
         value=lead(value,i)) %>% 
  filter(date<=run_date_fy&!is.na(value))

X = model.matrix(as.formula(paste0("total","~",paste(grep("_ch",colnames(monthly_shares1),value=TRUE),collapse="+"))),
                 monthly_shares1 %>% filter(date<=run_date_fy))[, -1]
y = (monthly_shares1 %>% filter(date<=run_date_fy))[['value']]

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

res_unshrunk = lm(as.formula(paste0("value~cbo_proj_month-1+",paste(rownames(selected_coefs_state),collapse="+"))),monthly_shares1,x=TRUE,y=TRUE)
res_shrunk = shrink(res_unshrunk,join=list(rownames(selected_coefs_state)))$postfit
}

saveRDS(list(share=monthly_shares_reg,res_unshrunk=res_unshrunk,res_shrunk=res_shrunk,cbo=monthly_shares1 %>% select(date,actual=value,cbo_proj=cbo_proj_month)),file="Data/Processing/Models/nowcast_Customs Duties.RDS")

#### Misc Receipts ####
monthly_shares = get_monthly_shares_df_revenue(receipts,"Total -- Miscellaneous Receipts","revenue","Miscellaneous Receipts") %>% 
  filter(date<=(as.Date(run_date_fy) %m+% months(12))) %>% 
  mutate(fed_remittances_suspended=ifelse(date>="2022-09-01",1,0))

monthly_shares_reg = lm_robust(cum_share~factor(fy_month)*factor(fed_remittances_suspended),monthly_shares %>% filter(date<=run_date_fy&fiscal_year>2015))
monthly_shares$pred_cumshare=as.numeric(predict(monthly_shares_reg,monthly_shares))

for(i in 0){
monthly_shares1 = monthly_shares %>% 
  left_join(x_data,by="date")

monthly_shares1 = monthly_shares1 %>% 
  mutate(cbo_proj=cbo_proj*pred_cumshare) %>% 
  group_by(fiscal_year) %>% 
  mutate(tst1=cbo_proj-dplyr::lag(cbo_proj,1),
         cbo_proj_month=ifelse(fy_month==1,cbo_proj,tst1)) %>% 
  ungroup() %>% 
  mutate(current_value=value,          
         total=lead(total,i),
         cbo_proj_month=lead(cbo_proj_month,i),
         value=lead(value,i)) %>% 
  filter(date<=run_date_fy&!is.na(value))

X = model.matrix(as.formula(paste0("total","~",paste(grep("_ch",colnames(monthly_shares1),value=TRUE),collapse="+"))),
                 monthly_shares1 %>% filter(date<=run_date_fy))[, -1]
y = (monthly_shares1 %>% filter(date<=run_date_fy))[['value']]

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

res_unshrunk = lm(as.formula(paste0("value~cbo_proj_month-1+",paste(rownames(selected_coefs_state),collapse="+"))),monthly_shares1,x=TRUE,y=TRUE)
res_shrunk = shrink(res_unshrunk,join=list(rownames(selected_coefs_state)))$postfit
}

saveRDS(list(share=monthly_shares_reg,res_unshrunk=res_unshrunk,res_shrunk=res_shrunk,cbo=monthly_shares1 %>% select(date,actual=value,cbo_proj=cbo_proj_month)),file="Data/Processing/Models/nowcast_Miscellaneous Receipts.RDS")

#### Medicare ####
monthly_shares = get_monthly_shares_df_spending("Medicare","Medicare") %>% 
  filter(date<=(as.Date(run_date_fy) %m+% months(12))&date>="2015-10-01") 

monthly_shares_reg = lm_robust(cum_share~factor(fy_month),monthly_shares %>% filter(date<=run_date_fy&fiscal_year>2015))
monthly_shares$pred_cumshare=as.numeric(predict(monthly_shares_reg,monthly_shares))
monthly_shares= monthly_shares %>% 
  mutate(pred_val=pred_cumshare*cbo_proj) %>% 
  group_by(fiscal_year) %>% 
  mutate(pred_val1=ifelse(fy_month==1,pred_val,pred_val-dplyr::lag(pred_val,1))) %>% 
  group_by(fiscal_year) %>% 
  mutate(scalar=value/mean(value,na.rm=TRUE)) %>% 
  ungroup()
scalar_reg = lm_robust(scalar~factor(first_day_thismonth_weekend)+factor(last_day_thismonth_weekend)+factor(first_day_nextmonth_weekend),monthly_shares %>% filter(date<=run_date_fy))
monthly_shares$scalar = predict(scalar_reg,monthly_shares) 
monthly_shares = monthly_shares %>% 
  mutate(pred_val2=scalar*pred_val1) %>% 
  group_by(fiscal_year) %>%
  arrange(date) %>% 
  mutate(pred_val3=pred_val2*(cbo_proj/sum(pred_val2)),
         tmp=cumsum(pred_val3),
         pred_cumshare=tmp/tmp[n()])

for(i in 0){
  monthly_shares1 = monthly_shares %>% 
    left_join(x_data,by="date")
  
  monthly_shares1 = monthly_shares1 %>% 
    mutate(cbo_proj=cbo_proj*pred_cumshare) %>% 
    group_by(fiscal_year) %>% 
    mutate(tst1=cbo_proj-dplyr::lag(cbo_proj,1),
           cbo_proj_month=ifelse(fy_month==1,cbo_proj,tst1)) %>% 
    ungroup() %>% 
    mutate(current_value=value,          
           total=lead(total,i),
           cbo_proj_month=lead(cbo_proj_month,i),
           value=lead(value,i)) %>% 
    filter(date<=run_date_fy&!is.na(value))
  
  X = model.matrix(as.formula(paste0("total","~",paste(grep("_ch",colnames(monthly_shares1),value=TRUE),collapse="+"))),
                   monthly_shares1 %>% filter(date<=run_date_fy))[, -1]
  y = (monthly_shares1 %>% filter(date<=run_date_fy))[['value']]
  
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
  
  res_unshrunk = lm(as.formula(paste0("value~cbo_proj_month-1+",paste(rownames(selected_coefs_state),collapse="+"))),monthly_shares1,x=TRUE,y=TRUE)
  res_shrunk = shrink(res_unshrunk,join=list(rownames(selected_coefs_state)))$postfit
}

saveRDS(list(share=monthly_shares_reg,scalar=scalar_reg,res_unshrunk=res_unshrunk,res_shrunk=res_shrunk,cbo=monthly_shares1 %>% select(date,actual=value,cbo_proj=cbo_proj_month)),file="Data/Processing/Models/nowcast_Medicare.RDS")

#### Medicaid ####
monthly_shares = get_monthly_shares_df_spending("Grants to States for Medicaid","Medicaid") %>% 
  filter(date<=(as.Date(run_date_fy) %m+% months(12))) 

monthly_shares_reg = lm_robust(cum_share~factor(fy_month),monthly_shares%>% filter(date<=run_date_fy&fiscal_year>2015))
monthly_shares$pred_cumshare=as.numeric(predict(monthly_shares_reg,monthly_shares))

for(i in 0){
  monthly_shares1 = monthly_shares %>% 
    left_join(x_data,by="date")
  
  monthly_shares1 = monthly_shares1 %>% 
    mutate(cbo_proj=cbo_proj*pred_cumshare) %>% 
    group_by(fiscal_year) %>% 
    mutate(tst1=cbo_proj-dplyr::lag(cbo_proj,1),
           cbo_proj_month=ifelse(fy_month==1,cbo_proj,tst1)) %>% 
    ungroup() %>% 
    mutate(current_value=value,          
           total=lead(total,i),
           cbo_proj_month=lead(cbo_proj_month,i),
           value=lead(value,i)) %>% 
    filter(date<=run_date_fy&!is.na(value))
  
  X = model.matrix(as.formula(paste0("total","~",paste(grep("_ch",colnames(monthly_shares1),value=TRUE),collapse="+"))),
                   monthly_shares1 %>% filter(date<=run_date_fy))[, -1]
  y = (monthly_shares1 %>% filter(date<=run_date_fy))[['value']]
  
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
  
  res_unshrunk = lm(as.formula(paste0("value~cbo_proj_month-1+",paste(rownames(selected_coefs_state),collapse="+"))),monthly_shares1,x=TRUE,y=TRUE)
  res_shrunk = shrink(res_unshrunk,join=list(rownames(selected_coefs_state)))$postfit
}
saveRDS(list(share=monthly_shares_reg,res_unshrunk=res_unshrunk,res_shrunk=res_shrunk,cbo=monthly_shares1 %>% select(date,actual=value,cbo_proj=cbo_proj_month)),file="Data/Processing/Models/nowcast_Medicaid.RDS")

#### Social Security ####
monthly_shares = get_monthly_shares_df_spending("Social Security","Social Security") %>% 
  filter(date<=(as.Date(run_date_fy) %m+% months(12))) 

monthly_shares_reg = lm_robust(cum_share~factor(fy_month),monthly_shares%>% filter(date<=run_date_fy&fiscal_year>2015))
monthly_shares$pred_cumshare=as.numeric(predict(monthly_shares_reg,monthly_shares))

for(i in 0){
monthly_shares1 = monthly_shares %>% 
  left_join(x_data,by="date")

monthly_shares1 = monthly_shares1 %>% 
  mutate(cbo_proj=cbo_proj*pred_cumshare) %>% 
  group_by(fiscal_year) %>% 
  mutate(tst1=cbo_proj-dplyr::lag(cbo_proj,1),
         cbo_proj_month=ifelse(fy_month==1,cbo_proj,tst1)) %>% 
  ungroup() %>% 
  mutate(current_value=value,          
         total=lead(total,i),
         cbo_proj_month=lead(cbo_proj_month,i),
         value=lead(value,i)) %>% 
  filter(date<=run_date_fy&!is.na(value))

X = model.matrix(as.formula(paste0("total","~",paste(grep("_ch",colnames(monthly_shares1),value=TRUE),collapse="+"))),
                 monthly_shares1 %>% filter(date<=run_date_fy))[, -1]
y = (monthly_shares1 %>% filter(date<=run_date_fy))[['value']]

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

res_unshrunk = lm(as.formula(paste0("value~cbo_proj_month-1+",paste(rownames(selected_coefs_state),collapse="+"))),monthly_shares1,x=TRUE,y=TRUE)
res_shrunk = shrink(res_unshrunk,join=list(rownames(selected_coefs_state)))$postfit
}
saveRDS(list(share=monthly_shares_reg,scalar=scalar_reg,res_unshrunk=res_unshrunk,res_shrunk=res_shrunk,cbo=monthly_shares1 %>% select(date,actual=value,cbo_proj=cbo_proj_month)),file="Data/Processing/Models/nowcast_Social Security.RDS")

#### Other Spending ####
monthly_shares = get_monthly_shares_df_spending("Other Spending","Other Spending") %>% 
  filter(date<=(as.Date(run_date_fy) %m+% months(12)))  %>% 
  arrange(date)

monthly_shares_reg = lm_robust(cum_share~factor(fy_month),monthly_shares%>% filter(date<=run_date_fy&fiscal_year>2015))
monthly_shares$pred_cumshare=as.numeric(predict(monthly_shares_reg,monthly_shares))

for(i in 0){
monthly_shares1 = monthly_shares %>% 
  left_join(x_data,by="date")

monthly_shares1 = monthly_shares1 %>% 
  mutate(cbo_proj=cbo_proj*pred_cumshare) %>% 
  group_by(fiscal_year) %>% 
  mutate(tst1=cbo_proj-dplyr::lag(cbo_proj,1),
         cbo_proj_month=ifelse(fy_month==1,cbo_proj,tst1)) %>% 
  ungroup() %>% 
  mutate(current_value=value,          
         total=lead(total,i),
         cbo_proj_month=lead(cbo_proj_month,i),
         value=lead(value,i)) %>% 
  filter(date<=run_date_fy&!is.na(value))

X = model.matrix(as.formula(paste0("total","~",paste(grep("_ch",colnames(monthly_shares1),value=TRUE),collapse="+"))),
                 monthly_shares1 %>% filter(date<=run_date_fy))[, -1]
y = (monthly_shares1 %>% filter(date<=run_date_fy))[['value']]

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

res_unshrunk = lm(as.formula(paste0("value~cbo_proj_month-1+",paste(rownames(selected_coefs_state),collapse="+"))),monthly_shares1,x=TRUE,y=TRUE)
res_shrunk = shrink(res_unshrunk,join=list(rownames(selected_coefs_state)))$postfit
}

saveRDS(list(share=monthly_shares_reg,scalar=scalar_reg,res_unshrunk=res_unshrunk,res_shrunk=res_shrunk,cbo=monthly_shares1 %>% select(date,actual=value,cbo_proj=cbo_proj_month)),file="Data/Processing/Models/nowcast_Other Spending.RDS")

#### National Defense ####
monthly_shares = get_monthly_shares_df_spending("National Defense","National Defense") %>% 
  filter(date<=(as.Date(run_date_fy) %m+% months(12))) %>% 
  arrange(date)

monthly_shares_reg = lm_robust(cum_share~factor(fy_month),monthly_shares %>% filter(date<=run_date_fy&fiscal_year>2015))
monthly_shares$pred_cumshare=as.numeric(predict(monthly_shares_reg,monthly_shares))

for(i in 0){
monthly_shares1 = monthly_shares %>% 
  left_join(x_data,by="date")

monthly_shares1 = monthly_shares1 %>% 
  mutate(cbo_proj=cbo_proj*pred_cumshare) %>% 
  group_by(fiscal_year) %>% 
  mutate(tst1=cbo_proj-dplyr::lag(cbo_proj,1),
         cbo_proj_month=ifelse(fy_month==1,cbo_proj,tst1)) %>% 
  ungroup() %>% 
  mutate(current_value=value,          
         total=lead(total,i),
         cbo_proj_month=lead(cbo_proj_month,i),
         value=lead(value,i)) %>% 
  filter(date<=run_date_fy&!is.na(value))

X = model.matrix(as.formula(paste0("total","~",paste(grep("_ch",colnames(monthly_shares1),value=TRUE),collapse="+"))),
                 monthly_shares1 %>% filter(date<=run_date_fy))[, -1]
y = (monthly_shares1 %>% filter(date<=run_date_fy))[['value']]

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

res_unshrunk = lm(as.formula(paste0("value~cbo_proj_month-1+",paste(rownames(selected_coefs_state),collapse="+"))),monthly_shares1,x=TRUE,y=TRUE)
res_shrunk = shrink(res_unshrunk,join=list(rownames(selected_coefs_state)))$postfit
}

saveRDS(list(share=monthly_shares_reg,scalar=scalar_reg,res_unshrunk=res_unshrunk,res_shrunk=res_shrunk,cbo=monthly_shares1 %>% select(date,actual=value,cbo_proj=cbo_proj_month)),file="Data/Processing/Models/nowcast_National Defense.RDS")

#### Net Interest ####
monthly_shares = get_monthly_shares_df_spending("Net Interest","Net Interest") %>% 
  filter(date<=(as.Date(run_date_fy) %m+% months(12))) %>% 
  mutate(fed_remittances_suspended=ifelse(date>="2022-09-01",1,0))

monthly_shares_reg = lm_robust(cum_share~factor(fy_month)*factor(fed_remittances_suspended),monthly_shares %>% filter(date<=run_date_fy&fiscal_year>2015))
monthly_shares$pred_cumshare=as.numeric(predict(monthly_shares_reg,monthly_shares))

for(i in 0){
monthly_shares1 = monthly_shares %>% 
  left_join(x_data,by="date")

monthly_shares1 = monthly_shares1 %>% 
  mutate(cbo_proj=cbo_proj*pred_cumshare) %>% 
  group_by(fiscal_year) %>% 
  mutate(tst1=cbo_proj-dplyr::lag(cbo_proj,1),
         cbo_proj_month=ifelse(fy_month==1,cbo_proj,tst1)) %>% 
  ungroup() %>% 
  mutate(current_value=value,          
         total=lead(total,i),
         cbo_proj_month=lead(cbo_proj_month,i),
         value=lead(value,i)) %>% 
  filter(date<=run_date_fy&!is.na(value))

X = model.matrix(as.formula(paste0("total","~",paste(grep("DGS10|DFF",colnames(monthly_shares1),value=TRUE),collapse="+"))),
                 monthly_shares1 %>% filter(date<=run_date_fy))[, -1]
y = (monthly_shares1 %>% filter(date<=run_date_fy))[['value']]

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

res_unshrunk = lm(as.formula(paste0("value~cbo_proj_month-1+",paste(rownames(selected_coefs_state),collapse="+"))),monthly_shares1,x=TRUE,y=TRUE)
res_shrunk = shrink(res_unshrunk,join=list(rownames(selected_coefs_state)))$postfit
}

saveRDS(list(share=monthly_shares_reg,scalar=scalar_reg,res_unshrunk=res_unshrunk,res_shrunk=res_shrunk,cbo=monthly_shares1 %>% select(date,actual=value,cbo_proj=cbo_proj_month)),file="Data/Processing/Models/nowcast_Net Interest.RDS")

#### Nowcasting Daily Data #####
receipt_daily_df = read_csv("Data/Raw/receipt_daily_df.csv") %>% # from line 71, 8_daily_model.R
  mutate(record_date=as.Date(record_date))
  
tax_days = read_csv("Data/Raw/tax_days_2000_2040.csv") %>% 
  mutate(`Tax Day`=gsub("\\(COVID-19 extension\\)","",`Tax Day`),
         date=paste0(`Tax Day`," ",Year),date=as.Date(date,format="%B %d %Y")) %>% 
  mutate(tax_day=1) %>% 
  select(date,tax_day)

x_data = read_csv(paste0("Data/Processing/imputed_data/imputed_data_asof",data_date,".csv"))  %>% 
  filter(date<=run_date_fy) %>% 
  select(-any_of(paste0("gt_",bad_vars$category))) %>% 
  arrange(date) %>%
  ungroup() %>% 
  mutate_at(vars(PAYEMS:JTSJOL,INDPRO:DGS10),.funs=list(ch12m=~((./dplyr::lag(.,12)-1)*100),ch1m=~((./dplyr::lag(.,1)-1)*100))) %>%
  mutate_at(vars(UNRATE:DTCDFSA066MSFRBPHI,grep("gt_",colnames(.),value=TRUE)),.funs=list(ch12m=~.-dplyr::lag(.,12),ch1m=~.-dplyr::lag(.,1))) %>%
  mutate_at(vars(PAYEMS:gt_999_ch1m),.funs=list(lag1=~dplyr::lag(.,1),lag2=~dplyr::lag(.,2),lag3=~dplyr::lag(.,3),lag4=~dplyr::lag(.,4)))


#### Customs Duties ####
daily_df = receipt_daily_df %>% 
  filter(cbo_category=="Customs Duties"&!grepl("from Depositaries",transaction_catg)&record_date<=run_date_fy) %>% 
  group_by(record_fiscal_year,record_calendar_month,record_calendar_day) %>% 
  summarize(record_date=record_date[1],
            total_day=sum(transaction_today_amt/1000,na.rm=TRUE)) %>% 
  ungroup() %>% 
  complete(record_date = seq.Date(min(record_date), max(record_date), by = "day")) %>% 
  mutate(record_fiscal_year=as.integer(quarter(record_date, with_year = TRUE, fiscal_start = 10)),
         record_calendar_month=month(record_date),
         record_calendar_day=sprintf("%02d", day(record_date)),
         total_day=ifelse(is.na(total_day),0,total_day)) %>% 
  group_by(record_fiscal_year,record_calendar_month) %>% 
  arrange(record_calendar_day) %>% 
  mutate(cum_total_day=cumsum(total_day),
         total_month=sum(total_day,na.rm=TRUE),
         record_calendar_day_perc=(as.numeric(record_calendar_day))/as.numeric(days_in_month(record_date)),
         inv_record_calendar_day=1-record_calendar_day_perc) %>% 
  mutate(fy_month=case_when(
    record_calendar_month%in%c(10:12)~record_calendar_month-9,
    record_calendar_month%in%c(1:9)~record_calendar_month+3
  )) %>% 
  group_by(record_fiscal_year) %>% 
  arrange(fy_month) %>% 
  mutate(cum_total_month=cumsum(total_day),
         total_year=sum(total_month,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(date=floor_date(record_date,"month")) %>% 
  left_join(readRDS(paste0("Data/Processing/Models/nowcast_","Customs Duties",".RDS"))$cbo) %>% 
  arrange(record_date) %>% 
  left_join(tax_days,by=c("record_date"="date")) %>% 
  mutate(cum_share=cum_total_day/total_month,
         share=total_day/total_month,
         quarter_end=case_when(
           record_calendar_month==4&tax_day==1~1,
           record_calendar_month%in%c(1,6,9)&record_calendar_day==15&!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))~1,
           record_calendar_month%in%c(1,6,9)&record_calendar_day%in%c(16,17)&(weekdays(record_date,abbreviate = TRUE)%in%c("Mon"))~1 # only use 16 or 17 IF the 15th had fallen on a weekend
         )) %>% 
  group_by(date) %>% 
  fill(tax_day,quarter_end,.direction="down") %>% 
  mutate(tax_day=ifelse(is.na(tax_day),0,tax_day),
         quarter_end=ifelse(is.na(quarter_end),0,quarter_end)) %>% 
  ungroup() %>% 
  group_by(date) %>% 
  mutate(lag_cum_share = case_when(record_date==min(record_date)~0,
                                   TRUE~dplyr::lag(cum_share)),
         lag_share=case_when(record_date==min(record_date)~0,
                             TRUE~dplyr::lag(share)),
         weekend=weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))

# how much do I have to scale the amounts by?
reg_scalar = lm_robust(value~dat,daily_df %>% group_by(date) %>% slice(n()) %>% summarize(value=actual/total_month) %>% ungroup() %>% mutate(dat=1:n()))

# monthly_shares_reg = lm_robust(cum_share~factor(record_calendar_day)*factor(record_calendar_month)*factor(weekend),daily_df %>% filter(date!="2020-04-01"))

monthly_shares_reg <- ranger(share ~ quarter_end + record_calendar_month + record_calendar_day  + weekend, 
                             data = daily_df, 
                             importance = 'permutation',
                             scale.permutation.importance = TRUE,
                             quantreg = TRUE,
                             mtry = 3,
                             write.forest = TRUE)

daily_df$pred_share=as.numeric(predict(monthly_shares_reg,data=daily_df)$predictions)
daily_df = daily_df %>% 
  group_by(date) %>% 
  mutate(pred_cumshare=cumsum(pred_share))
daily_df$pred_total = daily_df$cum_total_day/daily_df$pred_cumshare

#reg_combine = lm_robust(actual~record_calendar_day_perc*pred_total+record_calendar_day_perc*pred,daily_df)

saveRDS(list(share=monthly_shares_reg,scalar=reg_scalar),file="Data/Processing/Models/nowcast_daily_Customs Duties.RDS")


#### Estate Taxes ####
daily_df = receipt_daily_df %>% 
  filter(cbo_category=="Estate and Gift Taxes"&!grepl("from Depositaries",transaction_catg)&record_date<=run_date_fy) %>% 
  group_by(record_fiscal_year,record_calendar_month,record_calendar_day) %>% 
  summarize(record_date=record_date[1],
            total_day=sum(transaction_today_amt/1000,na.rm=TRUE)) %>% 
  ungroup() %>% 
  complete(record_date = seq.Date(min(record_date), max(record_date), by = "day")) %>% 
  mutate(record_fiscal_year=as.integer(quarter(record_date, with_year = TRUE, fiscal_start = 10)),
         record_calendar_month=month(record_date),
         record_calendar_day=sprintf("%02d", day(record_date)),
         total_day=ifelse(is.na(total_day),0,total_day)) %>% 
  group_by(record_fiscal_year,record_calendar_month) %>% 
  arrange(record_calendar_day) %>% 
  mutate(cum_total_day=cumsum(total_day),
         total_month=sum(total_day,na.rm=TRUE),
         record_calendar_day_perc=(as.numeric(record_calendar_day))/as.numeric(days_in_month(record_date)),
         inv_record_calendar_day=1-record_calendar_day_perc) %>% 
  mutate(fy_month=case_when(
    record_calendar_month%in%c(10:12)~record_calendar_month-9,
    record_calendar_month%in%c(1:9)~record_calendar_month+3
  )) %>% 
  group_by(record_fiscal_year) %>% 
  arrange(fy_month) %>% 
  mutate(cum_total_month=cumsum(total_day),
         total_year=sum(total_month,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(date=floor_date(record_date,"month")) %>% 
  left_join(readRDS(paste0("Data/Processing/Models/nowcast_","Estate and Gift Taxes",".RDS"))$cbo) %>% 
  arrange(record_date) %>% 
  left_join(tax_days,by=c("record_date"="date")) %>% 
  mutate(cum_share=cum_total_day/total_month,
         share=total_day/total_month,
         quarter_end=case_when(
           record_calendar_month==4&tax_day==1~1,
           record_calendar_month%in%c(1,6,9)&record_calendar_day==15&!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))~1,
           record_calendar_month%in%c(1,6,9)&record_calendar_day%in%c(16,17)&(weekdays(record_date,abbreviate = TRUE)%in%c("Mon"))~1 # only use 16 or 17 IF the 15th had fallen on a weekend
         )) %>% 
  group_by(date) %>% 
  fill(tax_day,quarter_end,.direction="down") %>% 
  mutate(tax_day=ifelse(is.na(tax_day),0,tax_day),
         quarter_end=ifelse(is.na(quarter_end),0,quarter_end)) %>% 
  ungroup() %>% 
  group_by(date) %>% 
  mutate(lag_cum_share = case_when(record_date==min(record_date)~0,
                                   TRUE~dplyr::lag(cum_share)),
         lag_share=case_when(record_date==min(record_date)~0,
                             TRUE~dplyr::lag(share)),
         weekend=weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))

# how much do I have to scale the amounts by?
reg_scalar = lm_robust(value~dat,daily_df %>% group_by(date) %>% slice(n()) %>% summarize(value=actual/total_month) %>% ungroup() %>% mutate(dat=1:n()))

# monthly_shares_reg = lm_robust(cum_share~factor(record_calendar_day)*factor(record_calendar_month),daily_df %>% filter(date!="2020-04-01"))

monthly_shares_reg <- ranger(share ~ quarter_end + record_calendar_month + record_calendar_day  + weekend, 
                             data = daily_df, 
                             importance = 'permutation',
                             scale.permutation.importance = TRUE,
                             quantreg = TRUE,
                             mtry = 3,
                             write.forest = TRUE)

daily_df$pred_share=as.numeric(predict(monthly_shares_reg,data=daily_df)$predictions)
daily_df = daily_df %>% 
  group_by(date) %>% 
  mutate(pred_cumshare=cumsum(pred_share))
daily_df$pred_total = daily_df$cum_total_day/daily_df$pred_cumshare

#reg_combine = lm_robust(actual~record_calendar_day_perc*pred_total+record_calendar_day_perc*pred,daily_df)

saveRDS(list(share=monthly_shares_reg,scalar=reg_scalar),file="Data/Processing/Models/nowcast_daily_Estate and Gift Taxes.RDS")

#### Excise Taxes ####
daily_df = receipt_daily_df %>% 
  filter(cbo_category=="Excise Taxes"&!grepl("from Depositaries",transaction_catg)&record_date<=run_date_fy) %>% 
  group_by(record_fiscal_year,record_calendar_month,record_calendar_day) %>% 
  summarize(record_date=record_date[1],
            total_day=sum(transaction_today_amt/1000,na.rm=TRUE)) %>% 
  ungroup() %>% 
  complete(record_date = seq.Date(min(record_date), max(record_date), by = "day")) %>% 
  mutate(record_fiscal_year=as.integer(quarter(record_date, with_year = TRUE, fiscal_start = 10)),
         record_calendar_month=month(record_date),
         record_calendar_day=sprintf("%02d", day(record_date)),
         total_day=ifelse(is.na(total_day),0,total_day)) %>% 
  group_by(record_fiscal_year,record_calendar_month) %>% 
  arrange(record_calendar_day) %>% 
  mutate(cum_total_day=cumsum(total_day),
         total_month=sum(total_day,na.rm=TRUE),
         record_calendar_day_perc=(as.numeric(record_calendar_day))/as.numeric(days_in_month(record_date)),
         inv_record_calendar_day=1-record_calendar_day_perc) %>% 
  mutate(fy_month=case_when(
    record_calendar_month%in%c(10:12)~record_calendar_month-9,
    record_calendar_month%in%c(1:9)~record_calendar_month+3
  )) %>% 
  group_by(record_fiscal_year) %>% 
  arrange(fy_month) %>% 
  mutate(cum_total_month=cumsum(total_day),
         total_year=sum(total_month,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(date=floor_date(record_date,"month")) %>% 
  left_join(readRDS(paste0("Data/Processing/Models/nowcast_","Excise Taxes",".RDS"))$cbo) %>% 
  arrange(record_date) %>% 
  left_join(tax_days,by=c("record_date"="date")) %>% 
  mutate(cum_share=cum_total_day/total_month,
         share=total_day/total_month,
         quarter_end=case_when(
           record_calendar_month==4&tax_day==1~1,
           record_calendar_month%in%c(1,6,9)&record_calendar_day==15&!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))~1,
           record_calendar_month%in%c(1,6,9)&record_calendar_day%in%c(16,17)&(weekdays(record_date,abbreviate = TRUE)%in%c("Mon"))~1 # only use 16 or 17 IF the 15th had fallen on a weekend
         )) %>% 
  group_by(date) %>% 
  fill(tax_day,quarter_end,.direction="down") %>% 
  mutate(tax_day=ifelse(is.na(tax_day),0,tax_day),
         quarter_end=ifelse(is.na(quarter_end),0,quarter_end)) %>% 
  ungroup() %>% 
  group_by(date) %>% 
  mutate(lag_cum_share = case_when(record_date==min(record_date)~0,
                                   TRUE~dplyr::lag(cum_share)),
         lag_share=case_when(record_date==min(record_date)~0,
                             TRUE~dplyr::lag(share)),
         weekend=weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))

# how much do I have to scale the amounts by?
reg_scalar = lm_robust(value~dat,daily_df %>% group_by(date) %>% slice(n()) %>% summarize(value=actual/total_month) %>% ungroup() %>% mutate(dat=1:n()))

# monthly_shares_reg = lm_robust(cum_share~factor(record_calendar_day),daily_df %>% filter(date!="2020-04-01"))
monthly_shares_reg <- ranger(share ~ quarter_end + record_calendar_month + record_calendar_day  + weekend, 
                             data = daily_df, 
                             importance = 'permutation',
                             scale.permutation.importance = TRUE,
                             quantreg = TRUE,
                             mtry = 3,
                             write.forest = TRUE)

daily_df$pred_share=as.numeric(predict(monthly_shares_reg,data=daily_df)$predictions)
daily_df = daily_df %>% 
  group_by(date) %>% 
  mutate(pred_cumshare=cumsum(pred_share))
daily_df$pred_total = daily_df$cum_total_day/daily_df$pred_cumshare

#reg_combine = lm_robust(actual~record_calendar_day_perc*pred_total+record_calendar_day_perc*pred,daily_df)

saveRDS(list(share=monthly_shares_reg,scalar=reg_scalar),file="Data/Processing/Models/nowcast_daily_Excise Taxes.RDS")


#### Individual and Payroll Income Taxes ####
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
  filter(record_date<=(as.Date(run_date_fy) %m+% months(12))) 

refund_shares_reg <- ranger(share ~ quarter_end + month + tax_due, 
                             data = refund_share %>% filter(var=="Refundable"&record_date<=run_date_fy&fiscal_year>2015&record_date!="2020-04-01"), 
                             importance = 'permutation',
                             scale.permutation.importance = TRUE,
                             quantreg = TRUE,
                             mtry = 3,
                             write.forest = TRUE)

daily_df = receipt_daily_df %>% 
  filter(cbo_category%in%c("Individual Income Taxes","Payroll Taxes")&record_date<=run_date_fy) %>% 
  mutate(date=floor_date(record_date,"month")) %>% 
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
    is.na(refund_share)~predict(refund_shares_reg,.)$predictions,
    TRUE~refund_share
  ),
  refund_share=1-refund_share,
  transaction_today_amt=case_when(
    grepl("Individual Tax Refunds|Tax Refunds Individual",transaction_catg)~transaction_today_amt*refund_share,
    TRUE~transaction_today_amt
  )) %>% # non-refundable tax credit is counted in Individual Tax Receipts
  select(-c(quarter_end,tax_due,fiscal_year)) %>% 
  group_by(record_fiscal_year,record_calendar_month,record_calendar_day) %>% 
  summarize(record_date=record_date[1],
            total_day=sum(transaction_today_amt/1000,na.rm=TRUE)) %>% 
  ungroup() %>% 
  complete(record_date = seq.Date(min(record_date), max(record_date), by = "day")) %>% 
  mutate(record_fiscal_year=as.integer(quarter(record_date, with_year = TRUE, fiscal_start = 10)),
         record_calendar_month=month(record_date),
         record_calendar_day=sprintf("%02d", day(record_date)),
         total_day=ifelse(is.na(total_day),0,total_day)) %>% 
  group_by(record_fiscal_year,record_calendar_month) %>% 
  arrange(record_calendar_day) %>% 
  mutate(cum_total_day=cumsum(total_day),
         total_month=sum(total_day,na.rm=TRUE),
         record_calendar_day_perc=(as.numeric(record_calendar_day))/as.numeric(days_in_month(record_date)),
         inv_record_calendar_day=1-record_calendar_day_perc) %>% 
  mutate(fy_month=case_when(
    record_calendar_month%in%c(10:12)~record_calendar_month-9,
    record_calendar_month%in%c(1:9)~record_calendar_month+3
  )) %>% 
  group_by(record_fiscal_year) %>% 
  arrange(fy_month) %>% 
  mutate(cum_total_month=cumsum(total_day),
         total_year=sum(total_month,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(date=floor_date(record_date,"month")) %>% 
  left_join(bind_rows(readRDS(paste0("Data/Processing/Models/nowcast_","Individual Income Taxes",".RDS"))$cbo,
                      readRDS(paste0("Data/Processing/Models/nowcast_","Payroll Taxes",".RDS"))$cbo) %>% 
              group_by(date) %>% 
              summarize_at(vars(cbo_proj,actual),sum),
            by=c("date"="date")) %>% 
  arrange(record_date) %>% 
  left_join(tax_days,by=c("record_date"="date")) %>% 
  mutate(cum_share=cum_total_day/total_month,
         share=total_day/total_month,
         quarter_end=case_when(
           record_calendar_month==4&tax_day==1~1,
           record_calendar_month%in%c(1,6,9)&record_calendar_day==15&!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))~1,
           record_calendar_month%in%c(1,6,9)&record_calendar_day%in%c(16,17)&(weekdays(record_date,abbreviate = TRUE)%in%c("Mon"))~1 # only use 16 or 17 IF the 15th had fallen on a weekend
         )) %>% 
  group_by(date) %>% 
  fill(tax_day,quarter_end,.direction="down") %>% 
  mutate(tax_day=ifelse(is.na(tax_day),0,tax_day),
         quarter_end=ifelse(is.na(quarter_end),0,quarter_end)) %>% 
  ungroup() %>% 
  group_by(date) %>% 
  mutate(lag_cum_share = case_when(record_date==min(record_date)~0,
                                   TRUE~dplyr::lag(cum_share)),
         lag_share=case_when(record_date==min(record_date)~0,
                             TRUE~dplyr::lag(share)),
         weekend=weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))

# how much do I have to scale the amounts by?
reg_scalar = lm_robust(value~factor(date>="2023-01-01")*factor(month),
                       daily_df %>% 
                         filter(date!="2020-04-01") %>% 
                         group_by(date) %>% 
                         slice(n()) %>% 
                         summarize(value=actual/total_month) %>% 
                         ungroup() %>% 
                         mutate(month=month(date),
                                dat=1:n()))

# monthly_shares_reg = lm_robust(cum_share~factor(record_calendar_day)*factor(record_calendar_month)+factor(tax_day)+factor(quarter_end),daily_df %>% filter(date!="2020-04-01"))

monthly_shares_reg <- ranger(share ~ quarter_end + record_calendar_month + record_calendar_day  + weekend + tax_day, 
                             data = daily_df %>% filter(date!="2020-04-01"), 
                             importance = 'permutation',
                             scale.permutation.importance = TRUE,
                             quantreg = TRUE,
                             mtry = 3,
                             write.forest = TRUE)

daily_df$pred_share=as.numeric(predict(monthly_shares_reg,data=daily_df)$predictions)
daily_df = daily_df %>% 
  group_by(date) %>% 
  mutate(pred_cumshare=cumsum(pred_share))
daily_df$pred_total = daily_df$cum_total_day/daily_df$pred_cumshare

reg_combine = lm_robust(actual~record_calendar_day_perc*pred_total+record_calendar_day_perc*cbo_proj,daily_df)

share_df = bind_rows(get_monthly_shares_df_revenue(receipts,"Total -- Individual Income Taxes","revenue","Individual Income Taxes") %>% 
                       select(date,actual=value) %>% 
                       mutate(var="Individual Income Taxes"),
                     get_monthly_shares_df_revenue(receipts,"Total -- Social Insurance and Retirement Receipts","revenue","Payroll Taxes") %>% 
                       select(date,actual=value) %>% 
                       mutate(var="Payroll Taxes")) %>% 
  filter(date<=run_date_fy) %>% 
  group_by(date) %>% 
  mutate(share=actual/sum(actual,na.rm=TRUE)) %>% 
  ungroup() %>% 
  filter(var=="Individual Income Taxes") %>% 
  select(date,share) %>% 
  mutate(month=month(date)) %>% 
  filter(!is.na(share)) %>% 
  mutate(quarter_end=case_when(month%in%c(1,4,6,9)~1,
                               TRUE~0),
         tax_due=case_when(
           !(year(date)%in%c(2020,2021))&month==4~1,
           year(date)==2020&month==7~1,
           year(date)==2021&month==5~1,
           TRUE~0
         ))

share_reg = lm_robust(share~factor(month)+factor(tax_due)+factor(quarter_end),share_df %>% filter(date!="2020-04-01"))

saveRDS(list(share=monthly_shares_reg,scalar=reg_scalar,disagg_reg=share_reg,refund_reg=refund_shares_reg),file="Data/Processing/Models/nowcast_daily_Individual Income Taxes.RDS")
saveRDS(list(share=monthly_shares_reg,scalar=reg_scalar,disagg_reg=share_reg,refund_reg=refund_shares_reg),file="Data/Processing/Models/nowcast_daily_Payroll Taxes.RDS")


#### Corporate Income Taxes ####
daily_df = receipt_daily_df %>% 
  filter(cbo_category%in%c("Corporate Income Taxes")&record_date<=run_date_fy&record_date>="2023-10-01") %>% 
  group_by(record_fiscal_year,record_calendar_month,record_calendar_day) %>% 
  summarize(record_date=record_date[1],
            total_day=sum(transaction_today_amt/1000,na.rm=TRUE)) %>% 
  ungroup() %>% 
  complete(record_date = seq.Date(min(record_date), max(record_date), by = "day")) %>% 
  mutate(record_fiscal_year=as.integer(quarter(record_date, with_year = TRUE, fiscal_start = 10)),
         record_calendar_month=month(record_date),
         record_calendar_day=sprintf("%02d", day(record_date)),
         total_day=ifelse(is.na(total_day),0,total_day)) %>% 
  group_by(record_fiscal_year,record_calendar_month) %>% 
  arrange(record_calendar_day) %>% 
  mutate(cum_total_day=cumsum(total_day),
         total_month=sum(total_day,na.rm=TRUE),
         record_calendar_day_perc=(as.numeric(record_calendar_day))/as.numeric(days_in_month(record_date)),
         inv_record_calendar_day=1-record_calendar_day_perc) %>% 
  mutate(fy_month=case_when(
    record_calendar_month%in%c(10:12)~record_calendar_month-9,
    record_calendar_month%in%c(1:9)~record_calendar_month+3
  )) %>% 
  group_by(record_fiscal_year) %>% 
  arrange(fy_month) %>% 
  mutate(cum_total_month=cumsum(total_day),
         total_year=sum(total_month,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(date=floor_date(record_date,"month")) %>% 
  left_join(readRDS(paste0("Data/Processing/Models/nowcast_","Corporate Income Taxes",".RDS"))$cbo,
            by=c("date"="date")) %>% 
  arrange(record_date) %>% 
  left_join(tax_days,by=c("record_date"="date")) %>% 
  mutate(cum_share=cum_total_day/total_month,
         share=total_day/total_month,
         quarter_end=case_when(
           record_calendar_month==4&tax_day==1~1,
           record_calendar_month%in%c(12,6,9)&record_calendar_day==15&!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))~1,
           record_calendar_month%in%c(12,6,9)&record_calendar_day%in%c(16,17)&(weekdays(record_date,abbreviate = TRUE)%in%c("Mon"))~1 # only use 16 or 17 IF the 15th had fallen on a weekend
         )) %>% 
  group_by(date) %>% 
  fill(tax_day,quarter_end,.direction="down") %>% 
  mutate(tax_day=ifelse(is.na(tax_day),0,tax_day),
         quarter_end=ifelse(is.na(quarter_end),0,quarter_end)) %>% 
  ungroup() %>% 
  group_by(date) %>% 
  mutate(lag_cum_share = case_when(record_date==min(record_date)~0,
                                   TRUE~dplyr::lag(cum_share)),
         lag_share=case_when(record_date==min(record_date)~0,
                             TRUE~dplyr::lag(share)),
         weekend=weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))

# how much do I have to scale the amounts by?
reg_scalar = lm_robust(value~factor(month),daily_df %>% filter(date!="2020-04-01") %>% group_by(date) %>% slice(n()) %>% summarize(value=actual/total_month) %>% ungroup() %>% mutate(month=month(date),dat=1:n()))

# monthly_shares_reg = lm_robust(cum_share~factor(record_calendar_day)+factor(tax_day)+factor(quarter_end),daily_df %>% filter(date!="2020-04-01"))

monthly_shares_reg <- ranger(share ~ quarter_end + record_calendar_month + record_calendar_day  + weekend + tax_day, 
                             data = daily_df %>% filter(date!="2020-04-01"), 
                             importance = 'permutation',
                             scale.permutation.importance = TRUE,
                             quantreg = TRUE,
                             mtry = 3,
                             write.forest = TRUE)

daily_df$pred_share=as.numeric(predict(monthly_shares_reg,data=daily_df)$predictions)
daily_df = daily_df %>% 
  group_by(date) %>% 
  mutate(pred_cumshare=cumsum(pred_share))
daily_df$pred_total = daily_df$cum_total_day/daily_df$pred_cumshare

#reg_combine = lm_robust(actual~record_calendar_day_perc*pred_total+record_calendar_day_perc*pred,daily_df)

saveRDS(list(share=monthly_shares_reg,scalar=reg_scalar),file="Data/Processing/Models/nowcast_daily_Corporate Income Taxes.RDS")


#### Miscellaneous Receipts ####
daily_df = receipt_daily_df %>% 
  filter(cbo_category%in%c("Miscellaneous Receipts")&record_date<=run_date_fy) %>% 
  group_by(record_fiscal_year,record_calendar_month,record_calendar_day) %>% 
  summarize(record_date=record_date[1],
            total_day=sum(transaction_today_amt/1000,na.rm=TRUE)) %>% 
  ungroup() %>% 
  complete(record_date = seq.Date(min(record_date), max(record_date), by = "day")) %>% 
  mutate(record_fiscal_year=as.integer(quarter(record_date, with_year = TRUE, fiscal_start = 10)),
         record_calendar_month=month(record_date),
         record_calendar_day=sprintf("%02d", day(record_date)),
         total_day=ifelse(is.na(total_day),0,total_day)) %>% 
  group_by(record_fiscal_year,record_calendar_month) %>% 
  arrange(record_calendar_day) %>% 
  mutate(cum_total_day=cumsum(total_day),
         total_month=sum(total_day,na.rm=TRUE),
         record_calendar_day_perc=(as.numeric(record_calendar_day))/as.numeric(days_in_month(record_date)),
         inv_record_calendar_day=1-record_calendar_day_perc) %>% 
  mutate(fy_month=case_when(
    record_calendar_month%in%c(10:12)~record_calendar_month-9,
    record_calendar_month%in%c(1:9)~record_calendar_month+3
  )) %>% 
  group_by(record_fiscal_year) %>% 
  arrange(fy_month) %>% 
  mutate(cum_total_month=cumsum(total_day),
         total_year=sum(total_month,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(date=floor_date(record_date,"month")) %>% 
  left_join(readRDS(paste0("Data/Processing/Models/nowcast_","Miscellaneous Receipts",".RDS"))$cbo,
            by=c("date"="date")) %>% 
  arrange(record_date) %>% 
  left_join(tax_days,by=c("record_date"="date")) %>% 
  mutate(cum_share=cum_total_day/total_month,
         share=total_day/total_month,
         quarter_end=case_when(
           record_calendar_month==4&tax_day==1~1,
           record_calendar_month%in%c(1,6,9)&record_calendar_day==15&!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))~1,
           record_calendar_month%in%c(1,6,9)&record_calendar_day%in%c(16,17)&(weekdays(record_date,abbreviate = TRUE)%in%c("Mon"))~1 # only use 16 or 17 IF the 15th had fallen on a weekend
         )) %>% 
  group_by(date) %>% 
  fill(tax_day,quarter_end,.direction="down") %>% 
  mutate(tax_day=ifelse(is.na(tax_day),0,tax_day),
         quarter_end=ifelse(is.na(quarter_end),0,quarter_end)) %>% 
  ungroup() %>% 
  group_by(date) %>% 
  mutate(lag_cum_share = case_when(record_date==min(record_date)~0,
                                   TRUE~dplyr::lag(cum_share)),
         lag_share=case_when(record_date==min(record_date)~0,
                             TRUE~dplyr::lag(share)),
         weekend=weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))

# how much do I have to scale the amounts by?
reg_scalar = lm_robust(value~dat+factor(month),daily_df %>% filter(date!="2020-04-01") %>% group_by(date) %>% slice(n()) %>% summarize(value=actual/total_month) %>% ungroup() %>% mutate(month=month(date),dat=1:n()))

# monthly_shares_reg = lm_robust(cum_share~factor(record_calendar_day)+factor(tax_day)+factor(quarter_end),daily_df %>% filter(date!="2020-04-01"))

monthly_shares_reg <- ranger(share ~ quarter_end + record_calendar_month + record_calendar_day  + weekend + tax_day, 
                             data = daily_df, 
                             importance = 'permutation',
                             scale.permutation.importance = TRUE,
                             quantreg = TRUE,
                             mtry = 3,
                             write.forest = TRUE)

daily_df$pred_share=as.numeric(predict(monthly_shares_reg,data=daily_df)$predictions)
daily_df = daily_df %>% 
  group_by(date) %>% 
  mutate(pred_cumshare=cumsum(pred_share))
daily_df$pred_total = daily_df$cum_total_day/daily_df$pred_cumshare

#reg_combine = lm_robust(actual~record_calendar_day_perc*pred_total+record_calendar_day_perc*pred,daily_df)

saveRDS(list(share=monthly_shares_reg,scalar=reg_scalar),file="Data/Processing/Models/nowcast_daily_Miscellaneous Receipts.RDS")


#### Medicare ####
daily_df = receipt_daily_df %>% 
  filter(cbo_category=="Medicare"&!grepl("from Depositaries",transaction_catg)&record_date<=run_date_fy) %>% 
  group_by(record_fiscal_year,record_calendar_month,record_calendar_day) %>% 
  summarize(record_date=record_date[1],
            total_day=sum(transaction_today_amt/1000,na.rm=TRUE)) %>% 
  ungroup() %>% 
  complete(record_date = seq.Date(min(record_date), max(record_date), by = "day")) %>% 
  mutate(record_fiscal_year=as.integer(quarter(record_date, with_year = TRUE, fiscal_start = 10)),
         record_calendar_month=month(record_date),
         record_calendar_day=sprintf("%02d", day(record_date)),
         total_day=ifelse(is.na(total_day),0,total_day)) %>% 
  group_by(record_fiscal_year,record_calendar_month) %>% 
  arrange(record_calendar_day) %>% 
  mutate(cum_total_day=cumsum(total_day),
         total_month=sum(total_day,na.rm=TRUE),
         record_calendar_day_perc=(as.numeric(record_calendar_day))/as.numeric(days_in_month(record_date)),
         inv_record_calendar_day=1-record_calendar_day_perc) %>% 
  mutate(fy_month=case_when(
    record_calendar_month%in%c(10:12)~record_calendar_month-9,
    record_calendar_month%in%c(1:9)~record_calendar_month+3
  )) %>% 
  group_by(record_fiscal_year) %>% 
  arrange(fy_month) %>% 
  mutate(cum_total_month=cumsum(total_day),
         total_year=sum(total_month,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(date=floor_date(record_date,"month")) %>% 
  left_join(readRDS(paste0("Data/Processing/Models/nowcast_","Medicare",".RDS"))$cbo) %>% 
  arrange(record_date) %>% 
  left_join(tax_days,by=c("record_date"="date")) %>% 
  mutate(cum_share=cum_total_day/total_month,
         share=total_day/total_month,
         quarter_end=case_when(
           record_calendar_month==4&tax_day==1~1,
           record_calendar_month%in%c(1,6,9)&record_calendar_day==15&!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))~1,
           record_calendar_month%in%c(1,6,9)&record_calendar_day%in%c(16,17)&(weekdays(record_date,abbreviate = TRUE)%in%c("Mon"))~1 # only use 16 or 17 IF the 15th had fallen on a weekend
         )) %>% 
  group_by(date) %>% 
  fill(tax_day,quarter_end,.direction="down") %>% 
  mutate(tax_day=ifelse(is.na(tax_day),0,tax_day),
         quarter_end=ifelse(is.na(quarter_end),0,quarter_end)) %>% 
  ungroup() %>% 
  mutate_at(vars(total_day,cum_total_day,total_month,cum_total_month),~.*-1) %>%  # put it in positive terms
  group_by(date) %>% 
  mutate(lag_cum_share = case_when(record_date==min(record_date)~0,
                                  TRUE~dplyr::lag(cum_share)),
         lag_share=case_when(record_date==min(record_date)~0,
                             TRUE~dplyr::lag(share)),
         weekend=weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))
  
  
daily_df = daily_df %>% 
  left_join(daily_df %>% distinct(date) %>% 
              mutate(month=month(date)) %>% 
              rowwise() %>% 
              mutate(first_day_thismonth_weekend=(weekdays(date,abbreviate=TRUE)%in%c("Sat","Sun")|date%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=1,
                     first_day_nextmonth_weekend=(weekdays(date %m+% months(1),abbreviate=TRUE)%in%c("Sat","Sun")|(date %m+% months(1))%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12,
                     last_day_thismonth_weekend=(weekdays((date %m+% months(1) )- 1,abbreviate=TRUE)%in%c("Sat","Sun")|((date %m+% months(1) )- 1)%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12) %>% 
              ungroup())

# how much do I have to scale the amounts by?
reg_scalar = lm_robust(value~dat+factor(month)+factor(first_day_thismonth_weekend)+factor(first_day_nextmonth_weekend)+factor(last_day_thismonth_weekend),
                       daily_df %>% 
                         group_by(date) %>% 
                         slice(n()) %>% 
                         summarize(value=actual/total_month,
                                   first_day_nextmonth_weekend=first_day_nextmonth_weekend[1],
                                   first_day_thismonth_weekend=first_day_thismonth_weekend[1],
                                   last_day_thismonth_weekend=last_day_thismonth_weekend[1]) %>% 
                         ungroup() %>% 
                         mutate(dat=1:n(),
                                month=month(date))
                       )

monthly_shares_reg <- ranger(share ~ quarter_end + record_calendar_month + record_calendar_day  + weekend + first_day_thismonth_weekend + first_day_nextmonth_weekend +last_day_thismonth_weekend, 
                   data = daily_df, 
                   importance = 'permutation',
                   scale.permutation.importance = TRUE,
                   quantreg = TRUE,
                   mtry = 3,
                   write.forest = TRUE)

# monthly_shares_reg = lm_robust(cum_share~lag_cum_share+as.numeric(record_calendar_day)*factor(record_calendar_month)*weekend,daily_df %>% filter(date!="2020-04-01"))

daily_df$pred_share=as.numeric(predict(monthly_shares_reg,data=daily_df)$predictions)
daily_df = daily_df %>% 
  group_by(date) %>% 
  mutate(pred_cumshare=cumsum(pred_share))
daily_df$pred_total = daily_df$cum_total_day/daily_df$pred_cumshare

#reg_combine = lm_robust(actual~record_calendar_day_perc*pred_total+record_calendar_day_perc*pred,daily_df)

saveRDS(list(share=monthly_shares_reg,scalar=reg_scalar),file="Data/Processing/Models/nowcast_daily_Medicare.RDS")

#### Medicaid ####
daily_df = receipt_daily_df %>% 
  filter(cbo_category=="Medicaid"&!grepl("from Depositaries",transaction_catg)&record_date<=run_date_fy) %>% 
  group_by(record_fiscal_year,record_calendar_month,record_calendar_day) %>% 
  summarize(record_date=record_date[1],
            total_day=sum(transaction_today_amt/1000,na.rm=TRUE)) %>% 
  ungroup() %>% 
  complete(record_date = seq.Date(min(record_date), max(record_date), by = "day")) %>% 
  mutate(record_fiscal_year=as.integer(quarter(record_date, with_year = TRUE, fiscal_start = 10)),
         record_calendar_month=month(record_date),
         record_calendar_day=sprintf("%02d", day(record_date)),
         total_day=ifelse(is.na(total_day),0,total_day)) %>% 
  group_by(record_fiscal_year,record_calendar_month) %>% 
  arrange(record_calendar_day) %>% 
  mutate(cum_total_day=cumsum(total_day),
         total_month=sum(total_day,na.rm=TRUE),
         record_calendar_day_perc=(as.numeric(record_calendar_day))/as.numeric(days_in_month(record_date)),
         inv_record_calendar_day=1-record_calendar_day_perc) %>% 
  mutate(fy_month=case_when(
    record_calendar_month%in%c(10:12)~record_calendar_month-9,
    record_calendar_month%in%c(1:9)~record_calendar_month+3
  )) %>% 
  group_by(record_fiscal_year) %>% 
  arrange(fy_month) %>% 
  mutate(cum_total_month=cumsum(total_day),
         total_year=sum(total_month,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(date=floor_date(record_date,"month")) %>% 
  left_join(readRDS(paste0("Data/Processing/Models/nowcast_","Medicaid",".RDS"))$cbo) %>% 
  arrange(record_date) %>% 
  left_join(tax_days,by=c("record_date"="date")) %>% 
  mutate(cum_share=cum_total_day/total_month,
         share=total_day/total_month,
         quarter_end=case_when(
           record_calendar_month==4&tax_day==1~1,
           record_calendar_month%in%c(1,6,9)&record_calendar_day==15&!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))~1,
           record_calendar_month%in%c(1,6,9)&record_calendar_day%in%c(16,17)&(weekdays(record_date,abbreviate = TRUE)%in%c("Mon"))~1 # only use 16 or 17 IF the 15th had fallen on a weekend
         )) %>% 
  group_by(date) %>% 
  fill(tax_day,quarter_end,.direction="down") %>% 
  mutate(tax_day=ifelse(is.na(tax_day),0,tax_day),
         quarter_end=ifelse(is.na(quarter_end),0,quarter_end)) %>% 
  ungroup() %>% 
  mutate_at(vars(total_day,cum_total_day,total_month,cum_total_month),~.*-1) %>%  # put it in positive terms
  group_by(date) %>% 
  mutate(lag_cum_share = case_when(record_date==min(record_date)~0,
                                   TRUE~dplyr::lag(cum_share)),
         lag_share=case_when(record_date==min(record_date)~0,
                             TRUE~dplyr::lag(share)),
         weekend=weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))

daily_df = daily_df %>% 
  left_join(daily_df %>% distinct(date) %>% 
              mutate(month=month(date)) %>% 
              rowwise() %>% 
              mutate(first_day_thismonth_weekend=(weekdays(date,abbreviate=TRUE)%in%c("Sat","Sun")|date%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=1,
                     first_day_nextmonth_weekend=(weekdays(date %m+% months(1),abbreviate=TRUE)%in%c("Sat","Sun")|(date %m+% months(1))%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12,
                     last_day_thismonth_weekend=(weekdays((date %m+% months(1) )- 1,abbreviate=TRUE)%in%c("Sat","Sun")|((date %m+% months(1) )- 1)%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12) %>% 
              ungroup())

# how much do I have to scale the amounts by?
reg_scalar = lm_robust(value~dat*factor(date>"2020-01-01")+factor(month)+factor(first_day_thismonth_weekend)+factor(first_day_nextmonth_weekend)+factor(last_day_thismonth_weekend),
                       daily_df %>% 
                         group_by(date) %>% 
                         slice(n()) %>% 
                         summarize(value=actual/total_month,
                                   first_day_nextmonth_weekend=first_day_nextmonth_weekend[1],
                                   first_day_thismonth_weekend=first_day_thismonth_weekend[1],
                                   last_day_thismonth_weekend=last_day_thismonth_weekend[1]) %>% 
                         ungroup() %>% 
                         mutate(dat=1:n(),
                                month=month(date))
)

monthly_shares_reg <- ranger(share ~ quarter_end + record_calendar_month + record_calendar_day  + weekend + first_day_thismonth_weekend + first_day_nextmonth_weekend +last_day_thismonth_weekend, 
                             data = daily_df, 
                             importance = 'permutation',
                             scale.permutation.importance = TRUE,
                             quantreg = TRUE,
                             mtry = 3,
                             write.forest = TRUE)

# monthly_shares_reg = lm_robust(cum_share~factor(record_calendar_day)*factor(record_calendar_month),daily_df %>% filter(date!="2020-04-01"))

daily_df$pred_share=as.numeric(predict(monthly_shares_reg,data=daily_df)$predictions)
daily_df = daily_df %>% 
  group_by(date) %>% 
  mutate(pred_cumshare=cumsum(pred_share))
daily_df$pred_total = daily_df$cum_total_day/daily_df$pred_cumshare

#reg_combine = lm_robust(actual~record_calendar_day_perc*pred_total+record_calendar_day_perc*pred,daily_df)

saveRDS(list(share=monthly_shares_reg,scalar=reg_scalar),file="Data/Processing/Models/nowcast_daily_Medicaid.RDS")

#### Social Security ####
daily_df = receipt_daily_df %>% 
  filter(cbo_category=="Social Security"&!grepl("from Depositaries",transaction_catg)&record_date<=run_date_fy) %>% 
  group_by(record_fiscal_year,record_calendar_month,record_calendar_day) %>% 
  summarize(record_date=record_date[1],
            total_day=sum(transaction_today_amt/1000,na.rm=TRUE)) %>% 
  ungroup() %>% 
  complete(record_date = seq.Date(min(record_date), max(record_date), by = "day")) %>% 
  mutate(record_fiscal_year=as.integer(quarter(record_date, with_year = TRUE, fiscal_start = 10)),
         record_calendar_month=month(record_date),
         record_calendar_day=sprintf("%02d", day(record_date)),
         total_day=ifelse(is.na(total_day),0,total_day)) %>% 
  group_by(record_fiscal_year,record_calendar_month) %>% 
  arrange(record_calendar_day) %>% 
  mutate(cum_total_day=cumsum(total_day),
         total_month=sum(total_day,na.rm=TRUE),
         record_calendar_day_perc=(as.numeric(record_calendar_day))/as.numeric(days_in_month(record_date)),
         inv_record_calendar_day=1-record_calendar_day_perc) %>% 
  mutate(fy_month=case_when(
    record_calendar_month%in%c(10:12)~record_calendar_month-9,
    record_calendar_month%in%c(1:9)~record_calendar_month+3
  )) %>% 
  group_by(record_fiscal_year) %>% 
  arrange(fy_month) %>% 
  mutate(cum_total_month=cumsum(total_day),
         total_year=sum(total_month,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(date=floor_date(record_date,"month")) %>% 
  left_join(readRDS(paste0("Data/Processing/Models/nowcast_","Social Security",".RDS"))$cbo) %>% 
  arrange(record_date) %>% 
  left_join(tax_days,by=c("record_date"="date")) %>% 
  mutate(cum_share=cum_total_day/total_month,
         share=total_day/total_month,
         quarter_end=case_when(
           record_calendar_month==4&tax_day==1~1,
           record_calendar_month%in%c(1,6,9)&record_calendar_day==15&!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))~1,
           record_calendar_month%in%c(1,6,9)&record_calendar_day%in%c(16,17)&(weekdays(record_date,abbreviate = TRUE)%in%c("Mon"))~1 # only use 16 or 17 IF the 15th had fallen on a weekend
         )) %>% 
  group_by(date) %>% 
  fill(tax_day,quarter_end,.direction="down") %>% 
  mutate(tax_day=ifelse(is.na(tax_day),0,tax_day),
         quarter_end=ifelse(is.na(quarter_end),0,quarter_end)) %>% 
  ungroup() %>% 
  mutate_at(vars(total_day,cum_total_day,total_month,cum_total_month),~.*-1) %>%  # put it in positive terms
  group_by(date) %>% 
  mutate(lag_cum_share = case_when(record_date==min(record_date)~0,
                                   TRUE~dplyr::lag(cum_share)),
         lag_share=case_when(record_date==min(record_date)~0,
                             TRUE~dplyr::lag(share)),
         weekend=weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"),
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
         ))

daily_df = daily_df %>% 
  left_join(daily_df %>% distinct(date) %>% 
              mutate(month=month(date)) %>% 
              rowwise() %>% 
              mutate(first_day_thismonth_weekend=(weekdays(date,abbreviate=TRUE)%in%c("Sat","Sun")|date%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=1,
                     first_day_nextmonth_weekend=(weekdays(date %m+% months(1),abbreviate=TRUE)%in%c("Sat","Sun")|(date %m+% months(1))%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12,
                     last_day_thismonth_weekend=(weekdays((date %m+% months(1) )- 1,abbreviate=TRUE)%in%c("Sat","Sun")|((date %m+% months(1) )- 1)%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12) %>% 
              ungroup())

# how much do I have to scale the amounts by?
reg_scalar = lm_robust(value~dat*factor(date<"2022-01-01")+factor(month),
                       daily_df %>% 
                         group_by(date) %>% 
                         slice(n()) %>% 
                         summarize(value=actual/total_month,
                                   first_day_nextmonth_weekend=first_day_nextmonth_weekend[1],
                                   first_day_thismonth_weekend=first_day_thismonth_weekend[1],
                                   last_day_thismonth_weekend=last_day_thismonth_weekend[1]) %>% 
                         ungroup() %>% 
                         mutate(dat=1:n(),
                                month=month(date))
)

monthly_shares_reg <- ranger(share ~ ssi_day + ss_ssi_day + ss_day + weekend, 
                             data = daily_df, 
                             importance = 'permutation',
                             scale.permutation.importance = TRUE,
                             quantreg = TRUE,
                             mtry = 3,
                             write.forest = TRUE)

# monthly_shares_reg = lm_robust(cum_share~lag_cum_share+as.numeric(record_calendar_day)*factor(record_calendar_month)*weekend,daily_df %>% filter(date!="2020-04-01"))

daily_df$pred_share=as.numeric(predict(monthly_shares_reg,data=daily_df)$predictions)
daily_df = daily_df %>% 
  group_by(date) %>% 
  mutate(pred_cumshare=cumsum(pred_share))
daily_df$pred_total = daily_df$cum_total_day/daily_df$pred_cumshare

#reg_combine = lm_robust(actual~record_calendar_day_perc*pred_total+record_calendar_day_perc*pred,daily_df)

saveRDS(list(share=monthly_shares_reg,scalar=reg_scalar),file="Data/Processing/Models/nowcast_daily_Social Security.RDS")

#### Other Spending ####
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
  filter(record_date<=(as.Date(run_date_fy) %m+% months(12))) 

refund_shares_reg <- ranger(share ~ quarter_end + month + tax_due, 
                            data = refund_share %>% filter(var=="Refundable"&record_date<=run_date_fy&fiscal_year>2015&record_date!="2020-04-01"), 
                            importance = 'permutation',
                            scale.permutation.importance = TRUE,
                            quantreg = TRUE,
                            mtry = 3,
                            write.forest = TRUE)


daily_df = receipt_daily_df %>% 
  filter(((cbo_category=="Other Spending"&!grepl("from Depositaries",transaction_catg))|grepl("Individual Tax Refunds|Tax Refunds Individual",transaction_catg))&record_date<=run_date_fy) %>% 
  mutate(date=floor_date(record_date,"month")) %>% 
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
    is.na(refund_share)~predict(refund_shares_reg,.)$predictions,
    TRUE~refund_share
  ),
  transaction_today_amt=case_when(
    grepl("Individual Tax Refunds|Tax Refunds Individual",transaction_catg)~transaction_today_amt*refund_share,
    TRUE~transaction_today_amt
  )) %>% # refundable tax credit is counted in Other Spending (under IRS subheading)
  select(-c(quarter_end,tax_due,fiscal_year)) %>% 
  group_by(record_fiscal_year,record_calendar_month,record_calendar_day) %>% 
  summarize(record_date=record_date[1],
            total_day=sum(transaction_today_amt/1000,na.rm=TRUE)) %>% 
  ungroup() %>% 
  complete(record_date = seq.Date(min(record_date), max(record_date), by = "day")) %>% 
  mutate(record_fiscal_year=as.integer(quarter(record_date, with_year = TRUE, fiscal_start = 10)),
         record_calendar_month=month(record_date),
         record_calendar_day=sprintf("%02d", day(record_date)),
         total_day=ifelse(is.na(total_day),0,total_day)) %>% 
  group_by(record_fiscal_year,record_calendar_month) %>% 
  arrange(record_calendar_day) %>% 
  mutate(cum_total_day=cumsum(total_day),
         total_month=sum(total_day,na.rm=TRUE),
         record_calendar_day_perc=(as.numeric(record_calendar_day))/as.numeric(days_in_month(record_date)),
         inv_record_calendar_day=1-record_calendar_day_perc) %>% 
  mutate(fy_month=case_when(
    record_calendar_month%in%c(10:12)~record_calendar_month-9,
    record_calendar_month%in%c(1:9)~record_calendar_month+3
  )) %>% 
  group_by(record_fiscal_year) %>% 
  arrange(fy_month) %>% 
  mutate(cum_total_month=cumsum(total_day),
         total_year=sum(total_month,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(date=floor_date(record_date,"month")) %>% 
  left_join(readRDS(paste0("Data/Processing/Models/nowcast_","Other Spending",".RDS"))$cbo) %>% 
  arrange(record_date) %>% 
  left_join(tax_days,by=c("record_date"="date")) %>% 
  mutate(cum_share=cum_total_day/total_month,
         share=total_day/total_month,
         quarter_end=case_when(
           record_calendar_month==4&tax_day==1~1,
           record_calendar_month%in%c(1,6,9)&record_calendar_day==15&!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))~1,
           record_calendar_month%in%c(1,6,9)&record_calendar_day%in%c(16,17)&(weekdays(record_date,abbreviate = TRUE)%in%c("Mon"))~1 # only use 16 or 17 IF the 15th had fallen on a weekend
         )) %>% 
  group_by(date) %>% 
  fill(tax_day,quarter_end,.direction="down") %>% 
  mutate(tax_day=ifelse(is.na(tax_day),0,tax_day),
         quarter_end=ifelse(is.na(quarter_end),0,quarter_end)) %>% 
  ungroup() %>% 
  mutate_at(vars(total_day,cum_total_day,total_month,cum_total_month),~.*-1) %>%  # put it in positive terms
  group_by(date) %>% 
  mutate(lag_cum_share = case_when(record_date==min(record_date)~0,
                                   TRUE~dplyr::lag(cum_share)),
         lag_share=case_when(record_date==min(record_date)~0,
                             TRUE~dplyr::lag(share)),
         weekend=weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))

daily_df = daily_df %>% 
  left_join(daily_df %>% distinct(date) %>% 
              mutate(month=month(date)) %>% 
              rowwise() %>% 
              mutate(first_day_thismonth_weekend=(weekdays(date,abbreviate=TRUE)%in%c("Sat","Sun")|date%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=1,
                     first_day_nextmonth_weekend=(weekdays(date %m+% months(1),abbreviate=TRUE)%in%c("Sat","Sun")|(date %m+% months(1))%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12,
                     last_day_thismonth_weekend=(weekdays((date %m+% months(1) )- 1,abbreviate=TRUE)%in%c("Sat","Sun")|((date %m+% months(1) )- 1)%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12) %>% 
              ungroup())

# how much do I have to scale the amounts by?
reg_scalar = lm_robust(value~dat*factor(date<"2021-01-01")+factor(month),
                       daily_df %>% 
                         group_by(date) %>% 
                         slice(n()) %>% 
                         summarize(value=actual/total_month,
                                   first_day_nextmonth_weekend=first_day_nextmonth_weekend[1],
                                   first_day_thismonth_weekend=first_day_thismonth_weekend[1],
                                   last_day_thismonth_weekend=last_day_thismonth_weekend[1]) %>% 
                         ungroup() %>% 
                         mutate(dat=1:n(),
                                month=month(date))
)

monthly_shares_reg <- ranger(share ~ record_calendar_month + record_calendar_day  + weekend, 
                             data = daily_df %>% filter(date!="2020-04-01"), 
                             importance = 'permutation',
                             scale.permutation.importance = TRUE,
                             quantreg = TRUE,
                             mtry = 3,
                             write.forest = TRUE)

# monthly_shares_reg = lm_robust(cum_share~factor(record_calendar_day)*factor(record_calendar_month),daily_df %>% filter(date!="2020-04-01"))

daily_df$pred_share=as.numeric(predict(monthly_shares_reg,data=daily_df)$predictions)
daily_df = daily_df %>% 
  group_by(date) %>% 
  mutate(pred_cumshare=cumsum(pred_share))
daily_df$pred_total = daily_df$cum_total_day/daily_df$pred_cumshare

#reg_combine = lm_robust(actual~record_calendar_day_perc*pred_total+record_calendar_day_perc*pred,daily_df)

saveRDS(list(share=monthly_shares_reg,scalar=reg_scalar,refund_reg=refund_shares_reg),file="Data/Processing/Models/nowcast_daily_Other Spending.RDS")

#### National Defense ####
daily_df = receipt_daily_df %>% 
  filter(cbo_category=="National Defense"&!grepl("from Depositaries",transaction_catg)&record_date<=run_date_fy) %>% 
  group_by(record_fiscal_year,record_calendar_month,record_calendar_day) %>% 
  summarize(record_date=record_date[1],
            total_day=sum(transaction_today_amt/1000,na.rm=TRUE)) %>% 
  ungroup() %>% 
  complete(record_date = seq.Date(min(record_date), max(record_date), by = "day")) %>% 
  mutate(record_fiscal_year=as.integer(quarter(record_date, with_year = TRUE, fiscal_start = 10)),
         record_calendar_month=month(record_date),
         record_calendar_day=sprintf("%02d", day(record_date)),
         total_day=ifelse(is.na(total_day),0,total_day)) %>% 
  group_by(record_fiscal_year,record_calendar_month) %>% 
  arrange(record_calendar_day) %>% 
  mutate(cum_total_day=cumsum(total_day),
         total_month=sum(total_day,na.rm=TRUE),
         record_calendar_day_perc=(as.numeric(record_calendar_day))/as.numeric(days_in_month(record_date)),
         inv_record_calendar_day=1-record_calendar_day_perc) %>% 
  mutate(fy_month=case_when(
    record_calendar_month%in%c(10:12)~record_calendar_month-9,
    record_calendar_month%in%c(1:9)~record_calendar_month+3
  )) %>% 
  group_by(record_fiscal_year) %>% 
  arrange(fy_month) %>% 
  mutate(cum_total_month=cumsum(total_day),
         total_year=sum(total_month,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(date=floor_date(record_date,"month")) %>% 
  left_join(readRDS(paste0("Data/Processing/Models/nowcast_","National Defense",".RDS"))$cbo) %>% 
  arrange(record_date) %>% 
  left_join(tax_days,by=c("record_date"="date")) %>% 
  mutate(cum_share=cum_total_day/total_month,
         share=total_day/total_month,
         quarter_end=case_when(
           record_calendar_month==4&tax_day==1~1,
           record_calendar_month%in%c(1,6,9)&record_calendar_day==15&!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))~1,
           record_calendar_month%in%c(1,6,9)&record_calendar_day%in%c(16,17)&(weekdays(record_date,abbreviate = TRUE)%in%c("Mon"))~1 # only use 16 or 17 IF the 15th had fallen on a weekend
         )) %>% 
  group_by(date) %>% 
  fill(tax_day,quarter_end,.direction="down") %>% 
  mutate(tax_day=ifelse(is.na(tax_day),0,tax_day),
         quarter_end=ifelse(is.na(quarter_end),0,quarter_end)) %>% 
  ungroup() %>% 
  mutate_at(vars(total_day,cum_total_day,total_month,cum_total_month),~.*-1) %>%  # put it in positive terms
  group_by(date) %>% 
  mutate(lag_cum_share = case_when(record_date==min(record_date)~0,
                                   TRUE~dplyr::lag(cum_share)),
         lag_share=case_when(record_date==min(record_date)~0,
                             TRUE~dplyr::lag(share)),
         weekend=weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))

daily_df = daily_df %>% 
  left_join(daily_df %>% distinct(date) %>% 
              mutate(month=month(date)) %>% 
              rowwise() %>% 
              mutate(first_day_thismonth_weekend=(weekdays(date,abbreviate=TRUE)%in%c("Sat","Sun")|date%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=1,
                     first_day_nextmonth_weekend=(weekdays(date %m+% months(1),abbreviate=TRUE)%in%c("Sat","Sun")|(date %m+% months(1))%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12,
                     last_day_thismonth_weekend=(weekdays((date %m+% months(1) )- 1,abbreviate=TRUE)%in%c("Sat","Sun")|((date %m+% months(1) )- 1)%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12) %>% 
              ungroup()) %>% 
  mutate(date_group=case_when(
    date<="2020-03-01"~"Before 2020-4",
    date<="2023-11-01"~"Before 2023-12",
    date>"2023-11-01"~"After 2023-12"
  ))

# how much do I have to scale the amounts by?
reg_scalar = lm_robust(value~dat*factor(date_group)+factor(month),
                       daily_df %>% 
                         group_by(date) %>% 
                         slice(n()) %>% 
                         summarize(value=actual/total_month,
                                   date_group=date_group[1],
                                   first_day_nextmonth_weekend=first_day_nextmonth_weekend[1],
                                   first_day_thismonth_weekend=first_day_thismonth_weekend[1],
                                   last_day_thismonth_weekend=last_day_thismonth_weekend[1]) %>% 
                         ungroup() %>% 
                         mutate(dat=1:n(),
                                month=month(date))
)

monthly_shares_reg <- ranger(share ~ quarter_end + record_calendar_month + record_calendar_day  + weekend, 
                             data = daily_df, 
                             importance = 'permutation',
                             scale.permutation.importance = TRUE,
                             quantreg = TRUE,
                             mtry = 3,
                             write.forest = TRUE)

# monthly_shares_reg = lm_robust(cum_share~factor(record_calendar_day)*factor(record_calendar_month),daily_df %>% filter(date!="2020-04-01"))

daily_df$pred_share=as.numeric(predict(monthly_shares_reg,data=daily_df)$predictions)
daily_df = daily_df %>% 
  group_by(date) %>% 
  mutate(pred_cumshare=cumsum(pred_share))
daily_df$pred_total = daily_df$cum_total_day/daily_df$pred_cumshare

#reg_combine = lm_robust(actual~record_calendar_day_perc*pred_total+record_calendar_day_perc*pred,daily_df)

saveRDS(list(share=monthly_shares_reg,scalar=reg_scalar),file="Data/Processing/Models/nowcast_daily_National Defense.RDS")

#### Net Interest ####
daily_df = receipt_daily_df %>% 
  filter(cbo_category=="Net Interest"&!grepl("from Depositaries",transaction_catg)&record_date<=run_date_fy) %>% 
  group_by(record_fiscal_year,record_calendar_month,record_calendar_day) %>% 
  summarize(record_date=record_date[1],
            total_day=sum(transaction_today_amt/1000,na.rm=TRUE)) %>% 
  ungroup() %>% 
  complete(record_date = seq.Date(min(record_date), max(record_date), by = "day")) %>% 
  mutate(record_fiscal_year=as.integer(quarter(record_date, with_year = TRUE, fiscal_start = 10)),
         record_calendar_month=month(record_date),
         record_calendar_day=sprintf("%02d", day(record_date)),
         total_day=ifelse(is.na(total_day),0,total_day)) %>% 
  group_by(record_fiscal_year,record_calendar_month) %>% 
  arrange(record_calendar_day) %>% 
  mutate(cum_total_day=cumsum(total_day),
         total_month=sum(total_day,na.rm=TRUE),
         record_calendar_day_perc=(as.numeric(record_calendar_day))/as.numeric(days_in_month(record_date)),
         inv_record_calendar_day=1-record_calendar_day_perc) %>% 
  mutate(fy_month=case_when(
    record_calendar_month%in%c(10:12)~record_calendar_month-9,
    record_calendar_month%in%c(1:9)~record_calendar_month+3
  )) %>% 
  group_by(record_fiscal_year) %>% 
  arrange(fy_month) %>% 
  mutate(cum_total_month=cumsum(total_day),
         total_year=sum(total_month,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(date=floor_date(record_date,"month")) %>% 
  left_join(readRDS(paste0("Data/Processing/Models/nowcast_","Net Interest",".RDS"))$cbo) %>% 
  arrange(record_date) %>% 
  left_join(tax_days,by=c("record_date"="date")) %>% 
  mutate(cum_share=cum_total_day/total_month,
         share=total_day/total_month,
         quarter_end=case_when(
           record_calendar_month==4&tax_day==1~1,
           record_calendar_month%in%c(1,6,9)&record_calendar_day==15&!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))~1,
           record_calendar_month%in%c(1,6,9)&record_calendar_day%in%c(16,17)&(weekdays(record_date,abbreviate = TRUE)%in%c("Mon"))~1 # only use 16 or 17 IF the 15th had fallen on a weekend
         )) %>% 
  group_by(date) %>% 
  fill(tax_day,quarter_end,.direction="down") %>% 
  mutate(tax_day=ifelse(is.na(tax_day),0,tax_day),
         quarter_end=ifelse(is.na(quarter_end),0,quarter_end)) %>% 
  ungroup() %>% 
  mutate_at(vars(total_day,cum_total_day,total_month,cum_total_month),~.*-1) %>%  # put it in positive terms
  group_by(date) %>% 
  mutate(lag_cum_share = case_when(record_date==min(record_date)~0,
                                   TRUE~dplyr::lag(cum_share)),
         lag_share=case_when(record_date==min(record_date)~0,
                             TRUE~dplyr::lag(share)),
         weekend=weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))

daily_df = daily_df %>% 
  left_join(daily_df %>% distinct(date) %>% 
              mutate(month=month(date)) %>% 
              rowwise() %>% 
              mutate(first_day_thismonth_weekend=(weekdays(date,abbreviate=TRUE)%in%c("Sat","Sun")|date%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=1,
                     first_day_nextmonth_weekend=(weekdays(date %m+% months(1),abbreviate=TRUE)%in%c("Sat","Sun")|(date %m+% months(1))%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12,
                     last_day_thismonth_weekend=(weekdays((date %m+% months(1) )- 1,abbreviate=TRUE)%in%c("Sat","Sun")|((date %m+% months(1) )- 1)%in%as.Date(as.character(tis::holidays(year(date))),format="%Y%m%d"))&month!=12) %>% 
              ungroup()) %>% 
  group_by(date) %>% 
  mutate(date_group=case_when(
    date<="2020-03-01"~"Before 2020-4",
    date<="2023-11-01"~"Before 2023-12",
    date>"2023-11-01"~"After 2023-12"
  ),
  settlement_period=case_when(
    record_date==max(record_date[!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))])~"EOM",
    record_date==min(record_date[day(record_date)>=15&!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))])~"Second Settlement",
    record_date==min(record_date[day(record_date)<=7&!(weekdays(record_date,abbreviate = TRUE)%in%c("Sat","Sun"))])~"First Settlement",
    TRUE~"Regular Day"
  ))

# how much do I have to scale the amounts by?
reg_scalar = lm_robust(value~factor(month)+factor(first_day_nextmonth_weekend)+factor(first_day_thismonth_weekend)+factor(last_day_thismonth_weekend),
                       daily_df %>% 
                         group_by(date) %>% 
                         slice(n()) %>% 
                         summarize(value=abs(actual/total_month),
                                   date_group=date_group[1],
                                   first_day_nextmonth_weekend=first_day_nextmonth_weekend[1],
                                   first_day_thismonth_weekend=first_day_thismonth_weekend[1],
                                   last_day_thismonth_weekend=last_day_thismonth_weekend[1]) %>% 
                         ungroup() %>% 
                         mutate(dat=1:n(),
                                month=month(date))
)

monthly_shares_reg <- ranger(share ~ record_calendar_month  + weekend + settlement_period, 
                             data = daily_df, 
                             importance = 'permutation',
                             scale.permutation.importance = TRUE,
                             quantreg = TRUE,
                             mtry = 3,
                             write.forest = TRUE)

# monthly_shares_reg = lm_robust(cum_share~factor(record_calendar_month)*factor(settlement_period),daily_df %>% filter(date!="2020-04-01"))

daily_df$pred_share=as.numeric(predict(monthly_shares_reg,data=daily_df)$predictions)
daily_df = daily_df %>% 
  group_by(date) %>% 
  mutate(pred_cumshare=cumsum(pred_share))
daily_df$pred_total = daily_df$cum_total_day/daily_df$pred_cumshare

#reg_combine = lm_robust(actual~record_calendar_day_perc*pred_total+record_calendar_day_perc*pred,daily_df)

saveRDS(list(share=monthly_shares_reg,scalar=reg_scalar),file="Data/Processing/Models/nowcast_daily_Net Interest.RDS")



