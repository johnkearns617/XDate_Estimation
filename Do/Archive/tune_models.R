# Tune Nowcast Models
set.seed(178)
dat = "2025-12-29"
cols_to_check = colnames(read_csv(paste0("Data/Processing/raw_data/data_asof","2025-05-16",".csv")) )
models = list()




#### Corporate Income Taxes ####
monthly_shares = get_monthly_shares_df(receipts,"Corporation Income Taxes","revenue","Corporate Income Taxes") %>% 
  mutate(tax_due=case_when(
    !(fiscal_year%in%c(2020))&month==4~1,
    fiscal_year==2020&month==7~1,
    TRUE~0
  ),
  quarter_end=ifelse(month%in%c(4,6,9,12),1,0))

monthly_shares_reg = lm_robust(cum_share~factor(fy_month)+factor(tax_due)+factor(quarter_end),monthly_shares)

monthly_shares$pred_cumshare=as.numeric(predict(monthly_shares_reg,monthly_shares))
monthly_shares$pred_total = monthly_shares$cum_total/monthly_shares$pred_cumshare

total_fy_reg = lm_robust(total~pred_total:factor(fy_month)+cbo_proj:factor(fy_month)-1,monthly_shares)
monthly_shares$final_pred = as.numeric(predict(total_fy_reg,monthly_shares))
monthly_shares$final_pred_month = monthly_shares$final_pred*monthly_shares$pred_cumshare

monthly_shares1 = monthly_shares %>% 
  left_join(x_data,by="date")

monthly_shares1 = monthly_shares1 %>% 
  mutate(cbo_proj=cbo_proj*pred_cumshare) %>% 
  group_by(fiscal_year) %>% 
  mutate(tst1=final_pred_month-dplyr::lag(final_pred_month,1),
         final_pred_month=ifelse(fy_month==1,final_pred_month,tst1),
         tst1=cbo_proj-dplyr::lag(cbo_proj,1),
         cbo_proj_month=ifelse(fy_month==1,cbo_proj,tst1)) %>% 
  ungroup()

X = model.matrix(as.formula(paste0("total","~",paste(c(colnames(monthly_shares1)[c(which(colnames(monthly_shares1)=="PAYEMS"):which(colnames(monthly_shares1)=="gt_999_ch1m_lag4"))]),collapse="+"))),
                 monthly_shares1)[, -1]
y = (monthly_shares1)[['value']]

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

res = lm_robust(as.formula(paste0("value~cbo_proj_month+final_pred_month+",paste(rownames(selected_coefs_state),collapse="+"))),monthly_shares1)

models$`Corporate Income Taxes` = list(share=monthly_shares_reg,total=total_fy_reg,rf=res)


#### Payroll Taxes ####
monthly_shares = get_monthly_shares_df(receipts,"Total -- Social Insurance and Retirement Receipts","revenue","Payroll Taxes")

monthly_shares_reg = lm_robust(cum_share~factor(fy_month),monthly_shares)

monthly_shares$pred_cumshare=as.numeric(predict(monthly_shares_reg,monthly_shares))
monthly_shares$pred_total = monthly_shares$cum_total/monthly_shares$pred_cumshare

total_fy_reg = lm_robust(total~pred_total:factor(fy_month)+cbo_proj:factor(fy_month)-1,monthly_shares)
monthly_shares$final_pred = as.numeric(predict(total_fy_reg,monthly_shares))
monthly_shares$final_pred_month = monthly_shares$final_pred*monthly_shares$pred_cumshare

monthly_shares1 = monthly_shares %>% 
  left_join(x_data,by="date")

monthly_shares1 = monthly_shares1 %>% 
  mutate(cbo_proj=cbo_proj*pred_cumshare) %>% 
  group_by(fiscal_year) %>% 
  mutate(tst1=final_pred_month-dplyr::lag(final_pred_month,1),
         final_pred_month=ifelse(fy_month==1,final_pred_month,tst1),
         tst1=cbo_proj-dplyr::lag(cbo_proj,1),
         cbo_proj_month=ifelse(fy_month==1,cbo_proj,tst1)) %>% 
  ungroup()

X = model.matrix(as.formula(paste0("total","~",paste(c(colnames(monthly_shares1)[c(which(colnames(monthly_shares1)=="PAYEMS"):which(colnames(monthly_shares1)=="gt_999_ch1m_lag4"))]),collapse="+"))),
                 monthly_shares1)[, -1]
y = (monthly_shares1)[['value']]

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

res = lm_robust(as.formula(paste0("value~cbo_proj_month+final_pred_month+",paste(rownames(selected_coefs_state),collapse="+"))),monthly_shares1)

models$`Payroll Taxes` = list(share=monthly_shares_reg,total=total_fy_reg,rf=res)

#### Excise Taxes ####
monthly_shares = get_monthly_shares_df(receipts,"Total -- Excise Taxes","revenue","Excise Taxes") %>% 
  mutate(tax_due=case_when(
    fiscal_year==2020&month==9~1,
    TRUE~0
  ))

monthly_shares_reg = lm_robust(cum_share~factor(fy_month)+factor(tax_due),monthly_shares)

monthly_shares$pred_cumshare=as.numeric(predict(monthly_shares_reg,monthly_shares))
monthly_shares$pred_total = monthly_shares$cum_total/monthly_shares$pred_cumshare

total_fy_reg = lm_robust(total~pred_total:factor(fy_month)+cbo_proj:factor(fy_month)-1,monthly_shares)
monthly_shares$final_pred = as.numeric(predict(total_fy_reg,monthly_shares))
monthly_shares$final_pred_month = monthly_shares$final_pred*monthly_shares$pred_cumshare

monthly_shares1 = monthly_shares %>% 
  left_join(x_data,by="date")

monthly_shares1 = monthly_shares1 %>% 
  mutate(cbo_proj=cbo_proj*pred_cumshare) %>% 
  group_by(fiscal_year) %>% 
  mutate(tst1=final_pred_month-dplyr::lag(final_pred_month,1),
         final_pred_month=ifelse(fy_month==1,final_pred_month,tst1),
         tst1=cbo_proj-dplyr::lag(cbo_proj,1),
         cbo_proj_month=ifelse(fy_month==1,cbo_proj,tst1)) %>% 
  ungroup()

X = model.matrix(as.formula(paste0("total","~",paste(c(colnames(monthly_shares1)[c(which(colnames(monthly_shares1)=="PAYEMS"):which(colnames(monthly_shares1)=="gt_999_ch1m_lag4"))]),collapse="+"))),
                 monthly_shares1)[, -1]
y = (monthly_shares1)[['value']]

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

res = lm_robust(as.formula(paste0("value~cbo_proj_month+final_pred_month+",paste(rownames(selected_coefs_state),collapse="+"))),monthly_shares1)

models$`Excise Taxes` = list(share=monthly_shares_reg,total=total_fy_reg,rf=res)

#### Estate Taxes ####
monthly_shares = get_monthly_shares_df(receipts,"Estate and Gift Taxes","revenue","Estate and Gift Taxes")

monthly_shares_reg = lm_robust(cum_share~factor(fy_month),monthly_shares)

monthly_shares$pred_cumshare=as.numeric(predict(monthly_shares_reg,monthly_shares))
monthly_shares$pred_total = monthly_shares$cum_total/monthly_shares$pred_cumshare

total_fy_reg = lm_robust(total~pred_total:factor(fy_month)+cbo_proj:factor(fy_month)-1,monthly_shares)
monthly_shares$final_pred = as.numeric(predict(total_fy_reg,monthly_shares))
monthly_shares$final_pred_month = monthly_shares$final_pred*monthly_shares$pred_cumshare

monthly_shares1 = monthly_shares %>% 
  left_join(x_data,by="date")

monthly_shares1 = monthly_shares1 %>% 
  mutate(cbo_proj=cbo_proj*pred_cumshare) %>% 
  group_by(fiscal_year) %>% 
  mutate(tst1=final_pred_month-dplyr::lag(final_pred_month,1),
         final_pred_month=ifelse(fy_month==1,final_pred_month,tst1),
         tst1=cbo_proj-dplyr::lag(cbo_proj,1),
         cbo_proj_month=ifelse(fy_month==1,cbo_proj,tst1)) %>% 
  ungroup()

X = model.matrix(as.formula(paste0("total","~",paste(c(colnames(monthly_shares1)[c(which(colnames(monthly_shares1)=="PAYEMS"):which(colnames(monthly_shares1)=="gt_999_ch1m_lag4"))]),collapse="+"))),
                 monthly_shares1)[, -1]
y = (monthly_shares1)[['value']]

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

res = lm_robust(as.formula(paste0("value~cbo_proj_month+final_pred_month+",paste(rownames(selected_coefs_state),collapse="+"))),monthly_shares1)

models$`Estate and Gift Taxes` = list(share=monthly_shares_reg,total=total_fy_reg,rf=res)

#### Customs Taxes ####
monthly_shares = get_monthly_shares_df(receipts,"Customs Duties","revenue","Customs Duties")

monthly_shares_reg = lm_robust(cum_share~factor(fy_month),monthly_shares)

monthly_shares$pred_cumshare=as.numeric(predict(monthly_shares_reg,monthly_shares))
monthly_shares$pred_total = monthly_shares$cum_total/monthly_shares$pred_cumshare

total_fy_reg = lm_robust(total~pred_total:factor(fy_month)+cbo_proj:factor(fy_month)-1,monthly_shares)
monthly_shares$final_pred = as.numeric(predict(total_fy_reg,monthly_shares))
monthly_shares$final_pred_month = monthly_shares$final_pred*monthly_shares$pred_cumshare

monthly_shares1 = monthly_shares %>% 
  left_join(x_data,by="date")

monthly_shares1 = monthly_shares1 %>% 
  mutate(cbo_proj=cbo_proj*pred_cumshare) %>% 
  group_by(fiscal_year) %>% 
  mutate(tst1=final_pred_month-dplyr::lag(final_pred_month,1),
         final_pred_month=ifelse(fy_month==1,final_pred_month,tst1),
         tst1=cbo_proj-dplyr::lag(cbo_proj,1),
         cbo_proj_month=ifelse(fy_month==1,cbo_proj,tst1)) %>% 
  ungroup()

X = model.matrix(as.formula(paste0("total","~",paste(c(colnames(monthly_shares1)[c(which(colnames(monthly_shares1)=="PAYEMS"):which(colnames(monthly_shares1)=="gt_999_ch1m_lag4"))]),collapse="+"))),
                 monthly_shares1)[, -1]
y = (monthly_shares1)[['value']]

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

res = lm_robust(as.formula(paste0("value~cbo_proj_month+final_pred_month+",paste(rownames(selected_coefs_state),collapse="+"))),monthly_shares1)

models$`Customs Duties` = list(share=monthly_shares_reg,total=total_fy_reg,rf=res)

#### Misc Receipts ####
monthly_shares = get_monthly_shares_df(receipts,"Total -- Miscellaneous Receipts","revenue","Miscellaneous Receipts")

monthly_shares_reg = lm_robust(cum_share~factor(fy_month),monthly_shares)

monthly_shares$pred_cumshare=as.numeric(predict(monthly_shares_reg,monthly_shares))
monthly_shares$pred_total = monthly_shares$cum_total/monthly_shares$pred_cumshare

total_fy_reg = lm_robust(total~pred_total:factor(fy_month)+cbo_proj:factor(fy_month)-1,monthly_shares)
monthly_shares$final_pred = as.numeric(predict(total_fy_reg,monthly_shares))
monthly_shares$final_pred_month = monthly_shares$final_pred*monthly_shares$pred_cumshare

monthly_shares1 = monthly_shares %>% 
  left_join(x_data,by="date")

monthly_shares1 = monthly_shares1 %>% 
  mutate(cbo_proj=cbo_proj*pred_cumshare) %>% 
  group_by(fiscal_year) %>% 
  mutate(tst1=final_pred_month-dplyr::lag(final_pred_month,1),
         final_pred_month=ifelse(fy_month==1,final_pred_month,tst1),
         tst1=cbo_proj-dplyr::lag(cbo_proj,1),
         cbo_proj_month=ifelse(fy_month==1,cbo_proj,tst1)) %>% 
  ungroup()

X = model.matrix(as.formula(paste0("total","~",paste(c(colnames(monthly_shares1)[c(which(colnames(monthly_shares1)=="PAYEMS"):which(colnames(monthly_shares1)=="gt_999_ch1m_lag4"))]),collapse="+"))),
                 monthly_shares1)[, -1]
y = (monthly_shares1)[['value']]

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

res = lm_robust(as.formula(paste0("value~cbo_proj_month+final_pred_month+",paste(rownames(selected_coefs_state),collapse="+"))),monthly_shares1)

models$`Miscellaneous Receipts` = list(share=monthly_shares_reg,total=total_fy_reg,rf=res)

#### Medicare ####
monthly_shares = get_monthly_shares_df_spending("Medicare","Medicare") %>% 
  filter(num==12) %>%
  select(-num)

monthly_shares_reg = lm_robust(cum_share~factor(fy_month)+factor(first_day_weekend)+factor(last_day_weekend),monthly_shares)
monthly_shares$pred_cumshare=as.numeric(predict(monthly_shares_reg,monthly_shares))
monthly_shares$pred_total = monthly_shares$cum_total/monthly_shares$pred_cumshare

total_fy_reg = lm_robust(total~pred_total:factor(fy_month)+cbo_proj:factor(fy_month)-1,monthly_shares)
monthly_shares$final_pred = as.numeric(predict(total_fy_reg,monthly_shares))
monthly_shares$final_pred_month = monthly_shares$final_pred*monthly_shares$pred_cumshare

monthly_shares1 = monthly_shares %>% 
  left_join(x_data,by="date")

monthly_shares1 = monthly_shares1 %>% 
  mutate(cbo_proj=cbo_proj*pred_cumshare) %>% 
  group_by(fiscal_year) %>% 
  mutate(tst1=final_pred_month-dplyr::lag(final_pred_month,1),
         final_pred_month=ifelse(fy_month==1,final_pred_month,tst1),
         tst1=cbo_proj-dplyr::lag(cbo_proj,1),
         cbo_proj_month=ifelse(fy_month==1,cbo_proj,tst1)) %>% 
  ungroup()

X = model.matrix(as.formula(paste0("total","~",paste(c(colnames(monthly_shares1)[c(which(colnames(monthly_shares1)=="PAYEMS"):which(colnames(monthly_shares1)=="gt_999_ch1m_lag4"))]),collapse="+"))),
                 monthly_shares1)[, -1]
y = (monthly_shares1)[['value']]

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

res = lm_robust(as.formula(paste0("value~cbo_proj_month+final_pred_month+",paste(rownames(selected_coefs_state),collapse="+"))),monthly_shares1)

models$`Medicare` = list(share=monthly_shares_reg,total=total_fy_reg,rf=res)

#### Medicaid ####
monthly_shares = get_monthly_shares_df_spending("Grants to States for Medicaid","Medicaid") %>% 
  filter(num==12) %>%
  select(-num)

monthly_shares_reg = lm_robust(cum_share~factor(fy_month),monthly_shares)
monthly_shares$pred_cumshare=as.numeric(predict(monthly_shares_reg,monthly_shares))
monthly_shares$pred_total = monthly_shares$cum_total/monthly_shares$pred_cumshare

total_fy_reg = lm_robust(total~pred_total:factor(fy_month)+cbo_proj:factor(fy_month)-1,monthly_shares)
monthly_shares$final_pred = as.numeric(predict(total_fy_reg,monthly_shares))
monthly_shares$final_pred_month = monthly_shares$final_pred*monthly_shares$pred_cumshare

monthly_shares1 = monthly_shares %>% 
  left_join(x_data,by="date")

monthly_shares1 = monthly_shares1 %>% 
  mutate(cbo_proj=cbo_proj*pred_cumshare) %>% 
  group_by(fiscal_year) %>% 
  mutate(tst1=final_pred_month-dplyr::lag(final_pred_month,1),
         final_pred_month=ifelse(fy_month==1,final_pred_month,tst1),
         tst1=cbo_proj-dplyr::lag(cbo_proj,1),
         cbo_proj_month=ifelse(fy_month==1,cbo_proj,tst1)) %>% 
  ungroup()

X = model.matrix(as.formula(paste0("total","~",paste(c(colnames(monthly_shares1)[c(which(colnames(monthly_shares1)=="PAYEMS"):which(colnames(monthly_shares1)=="gt_999_ch1m_lag4"))]),collapse="+"))),
                 monthly_shares1)[, -1]
y = (monthly_shares1)[['value']]

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

res = lm_robust(as.formula(paste0("value~cbo_proj_month+final_pred_month+",paste(rownames(selected_coefs_state),collapse="+"))),monthly_shares1)

models$`Medicaid` = list(share=monthly_shares_reg,total=total_fy_reg,rf=res)


#### Social Security ####
monthly_shares = get_monthly_shares_df_spending("Social Security","Social Security") %>% 
  filter(num==12) %>%
  select(-num)

monthly_shares_reg = lm_robust(cum_share~factor(fy_month),monthly_shares)
monthly_shares$pred_cumshare=as.numeric(predict(monthly_shares_reg,monthly_shares))
monthly_shares$pred_total = monthly_shares$cum_total/monthly_shares$pred_cumshare

total_fy_reg = lm_robust(total~pred_total:factor(fy_month)+cbo_proj:factor(fy_month)-1,monthly_shares)
monthly_shares$final_pred = as.numeric(predict(total_fy_reg,monthly_shares))
monthly_shares$final_pred_month = monthly_shares$final_pred*monthly_shares$pred_cumshare

monthly_shares1 = monthly_shares %>% 
  left_join(x_data,by="date")

monthly_shares1 = monthly_shares1 %>% 
  mutate(cbo_proj=cbo_proj*pred_cumshare) %>% 
  group_by(fiscal_year) %>% 
  mutate(tst1=final_pred_month-dplyr::lag(final_pred_month,1),
         final_pred_month=ifelse(fy_month==1,final_pred_month,tst1),
         tst1=cbo_proj-dplyr::lag(cbo_proj,1),
         cbo_proj_month=ifelse(fy_month==1,cbo_proj,tst1)) %>% 
  ungroup()

X = model.matrix(as.formula(paste0("total","~",paste(c(colnames(monthly_shares1)[c(which(colnames(monthly_shares1)=="PAYEMS"):which(colnames(monthly_shares1)=="gt_999_ch1m_lag4"))]),collapse="+"))),
                 monthly_shares1)[, -1]
y = (monthly_shares1)[['value']]

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

res = lm_robust(as.formula(paste0("value~cbo_proj_month+final_pred_month+",paste(rownames(selected_coefs_state),collapse="+"))),monthly_shares1)

models$`Social Security` = list(share=monthly_shares_reg,total=total_fy_reg,rf=res)

#### Other Spending ####
monthly_shares = get_monthly_shares_df_spending("Other Spending","Other Spending") %>% 
  filter(num==12) %>%
  select(-num)

monthly_shares_reg = lm_robust(cum_share~factor(fy_month),monthly_shares)
monthly_shares$pred_cumshare=as.numeric(predict(monthly_shares_reg,monthly_shares))
monthly_shares$pred_total = monthly_shares$cum_total/monthly_shares$pred_cumshare

total_fy_reg = lm_robust(total~pred_total:factor(fy_month)+cbo_proj:factor(fy_month)-1,monthly_shares)
monthly_shares$final_pred = as.numeric(predict(total_fy_reg,monthly_shares))
monthly_shares$final_pred_month = monthly_shares$final_pred*monthly_shares$pred_cumshare

monthly_shares1 = monthly_shares %>% 
  left_join(x_data,by="date")

monthly_shares1 = monthly_shares1 %>% 
  mutate(cbo_proj=cbo_proj*pred_cumshare) %>% 
  group_by(fiscal_year) %>% 
  mutate(tst1=final_pred_month-dplyr::lag(final_pred_month,1),
         final_pred_month=ifelse(fy_month==1,final_pred_month,tst1),
         tst1=cbo_proj-dplyr::lag(cbo_proj,1),
         cbo_proj_month=ifelse(fy_month==1,cbo_proj,tst1)) %>% 
  ungroup()

X = model.matrix(as.formula(paste0("total","~",paste(c(colnames(monthly_shares1)[c(which(colnames(monthly_shares1)=="PAYEMS"):which(colnames(monthly_shares1)=="gt_999_ch1m_lag4"))]),collapse="+"))),
                 monthly_shares1)[, -1]
y = (monthly_shares1)[['value']]

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

res = lm_robust(as.formula(paste0("value~cbo_proj_month+final_pred_month+",paste(rownames(selected_coefs_state),collapse="+"))),monthly_shares1)

models$`Other Spending` = list(share=monthly_shares_reg,total=total_fy_reg,rf=res)







save(models,file="Data/Final/models.RDS")
