test_df = outlay_daily_df_groups %>% 
  filter(date>="2015-03-01"&date<floor_date(min(feb_forecast$date,na.rm=TRUE),"month")) %>% 
  mutate(record_calendar_month=as.numeric(record_calendar_month)) %>% 
  group_by(group,record_fiscal_year,record_calendar_month) %>% 
  left_join(deficit_summary %>% 
              filter(!grepl("FY |Year-to-Date",classification_desc)&line_code_nbr>=160) %>% 
              rowwise() %>% 
              mutate(outlay=as.numeric(current_month_gross_outly_amt)/1000000000,
                     receipt=as.numeric(current_month_gross_rcpt_amt)/1000000000,
                     record_calendar_month=which(month.name==classification_desc)) %>% 
              select(record_fiscal_year,record_calendar_month,outlay,receipt) %>% 
              group_by(record_fiscal_year,record_calendar_month) %>% 
              slice(1) %>% 
              select(record_fiscal_year,record_calendar_month,actual_check=outlay)) %>% 
  mutate(actual=ifelse(is.na(actual),pred,actual),
         actual_total=ifelse(is.na(actual_total),pred_total,actual_total),
         actual_check=ifelse(is.na(actual_check),pred,actual_check),
         scaled_daily=total_day*(actual/sum(total_day))*(actual_check/actual_total)) %>%
  ungroup() %>% 
  select(group,actual_date,scaled_daily)

feb_adj = outlay_daily_df_groups %>% 
  filter(date>=floor_date(min(feb_forecast$date,na.rm=TRUE),"month")) %>% 
  mutate(record_calendar_month=as.numeric(record_calendar_month)) %>% 
  full_join(feb_forecast %>% select(outlay_day_amt,receipt_day_amt,record_calendar_month,record_calendar_day)) %>% 
  group_by(record_calendar_day,record_calendar_month) %>% 
  mutate(scalar=total_day/sum(total_day)) %>% 
  ungroup() %>% 
  mutate(scaled_daily=outlay_day_amt*scalar/1000) %>% 
  select(group,actual_date,scaled_daily) %>% 
  filter(!is.na(group))

test_df = bind_rows(
  test_df,
  feb_adj
)

#daily_receipts_all = data.frame()
# for(month in unique(forecast_list1 %>% filter(is.na(mean)) %>% select(date) %>% pull())){
#   
#   dat = spending_by_function %>% 
#     mutate(var=case_when(
#       classification_desc=="Individual Income Taxes"~"revenue_Individual Income Taxes",
#       classification_desc=="Corporation Income Taxes"~"revenue_Corporate Income Taxes",
#       classification_desc=="Excise Taxes"~"revenue_Excise Taxes",
#       classification_desc%in%c("Employment and General Retirement","Unemployment Insurance","Other Retirement")~"revenue_Payroll Taxes",
#       classification_desc=="Estate and Gift Taxes"~"revenue_Estate and Gift Taxes",
#       classification_desc=="Customs Duties"~"revenue_Customs Duties",
#       classification_desc=="Miscellaneous Receipts"~"revenue_Miscellaneous Receipts"
#     )) %>% 
#     filter(!is.na(var)) %>% 
#     group_by(var,record_date) %>%
#     summarize(actual=sum(as.numeric(current_month_rcpt_outly_amt))/1000000000,
#               date=floor_date(record_date[1],"month"),
#               record_fiscal_year=record_fiscal_year[1],
#               record_calendar_month=record_calendar_month[1]) %>% 
#     select(date,actual,record_fiscal_year,record_calendar_month)
#   
#   for(var1 in colnames(cbo_monthly_proj)[3:9]){
#     
#     dat1 = dat %>% 
#       ungroup() %>% 
#       filter(var==var1)
#     
#     var_forecast = data.frame(date=as.Date(month),
#                               record_calendar_day=as.numeric(day(seq(as.Date(month),ceiling_date(as.Date(month,by=1),"month")-1,by=1)))) %>% 
#       left_join(dat1) %>% 
#       left_join(receipt_daily_df %>% 
#                   filter(record_calendar_month==month(as.Date(month))) %>% 
#                   distinct(record_calendar_day,avg_share) %>% 
#                   rename(avg_share_receipt=avg_share)%>% 
#                   mutate(record_calendar_day=as.numeric(record_calendar_day))) %>% 
#       fill(avg_share_receipt,.direction="down") %>% 
#       mutate(avg_share_receipt=ifelse(is.na(avg_share_receipt),0,avg_share_receipt),
#              avg_share_receipt=avg_share_receipt/avg_share_receipt[n()],
#              receipt_mtd_amt=actual*avg_share_receipt,
#              receipt_day_amt=receipt_mtd_amt-lag(receipt_mtd_amt,1),
#              receipt_day_amt=ifelse(record_calendar_day==min(record_calendar_day),receipt_mtd_amt,receipt_day_amt),
#              record_calendar_month=as.numeric(record_calendar_month)) %>% 
#       select(date,record_calendar_day,receipt_day_amt,record_fiscal_year,record_calendar_month) %>% 
#       mutate(var=var1)
#     
#     daily_receipts_all = bind_rows(daily_receipts_all,var_forecast)
#     
#   }
# }
# 
# daily_receipts_all = daily_receipts_all %>% 
#   mutate(actual_date=as.Date(paste0(year(date),"-",month(date),"-",record_calendar_day))) %>% 
#   left_join(deficit_summary %>% 
#               filter(!grepl("FY |Year-to-Date",classification_desc)&line_code_nbr>=160) %>% 
#               rowwise() %>% 
#               mutate(outlay=as.numeric(current_month_gross_outly_amt)/1000000000,
#                      receipt=as.numeric(current_month_gross_rcpt_amt)/1000000000,
#                      record_calendar_month=which(month.name==classification_desc)) %>% 
#               select(record_fiscal_year,record_calendar_month,outlay,receipt) %>% 
#               group_by(record_fiscal_year,record_calendar_month) %>% 
#               slice(1) %>% 
#               select(record_fiscal_year,record_calendar_month,actual_check=receipt) %>% 
#               ungroup()) %>%  
#   group_by(record_fiscal_year,record_calendar_month) %>% 
#   mutate(total=sum(receipt_day_amt),
#         receipt_day_amt=receipt_day_amt*(actual_check/total)) %>% 
#   select(-c(actual_check,total))

daily_receipts_all = imputed_daily_receipts %>% 
  mutate(record_calendar_month=as.numeric(record_calendar_month)) %>% 
  right_join(receipt_daily_df %>% 
               mutate(record_calendar_day=as.numeric(record_calendar_day),
                      record_calendar_year=year(date)),
             by=c("record_calendar_year","record_calendar_month", "record_calendar_day")) %>% 
  left_join(actual_receipt %>% 
              rowwise() %>% 
              mutate(total=sum(c(misc,corp,payroll,individ,excise,estate,customs)),
                     month=month(date))) %>% 
  arrange(date) %>% 
  rowwise() %>% 
  mutate(misc1=`Miscellaneous Receipts`*total_day/1000,
         corp1=`Corporate Income Taxes`*total_day/1000,
         payroll1=`Payroll Taxes`*total_day/1000,
         individ1=`Individual Income Taxes`*total_day/1000,
         excise1=`Excuse Taxes`*total_day/1000,
         estate1=`Estate and Gift Taxes`*total_day/1000,
         customs1=`Customs Duties`*total_day/1000) %>% 
  group_by(date) %>% 
  mutate(individ1=individ1+(individ[n()]-sum(individ1))/n(),
         corp1=corp1+(corp[n()]-sum(corp1))/n(),
         misc1=misc1+(misc[n()]-sum(misc1))/n(),
         payroll1=payroll1+(payroll[n()]-sum(payroll1))/n(),
         excise1=excise1+(excise[n()]-sum(excise1))/n(),
         estate1=estate1+(estate[n()]-sum(estate1))/n(),
         customs1=customs1+(customs[n()]-sum(customs1))/n()) %>% 
  rowwise() %>% 
  mutate(total_check=sum(sum(c(misc1,corp1,payroll1,individ1,excise1,estate1,customs1)))) %>% 
  select(record_calendar_year,record_calendar_month,record_calendar_day,misc1:customs1) %>% 
  pivot_longer(cols=misc1:customs1,names_to="var",values_to="receipt_day_amt") %>% 
  mutate(actual_date=as.Date(paste0(record_calendar_year,"-",record_calendar_month,"-",record_calendar_day))) %>% 
  ungroup() %>% 
  filter(date<floor_date(min(feb_forecast$date,na.rm=TRUE),"month")) %>% 
  select(-date) 
  

feb_adj = imputed_daily_receipts %>% 
  mutate(record_calendar_month=as.numeric(record_calendar_month)) %>% 
  right_join(receipt_daily_df %>% 
               mutate(record_calendar_day=as.numeric(record_calendar_day),
                      record_calendar_year=year(date)),
             by=c("record_calendar_year","record_calendar_month", "record_calendar_day")) %>% 
  filter(date>=floor_date(min(feb_forecast$date,na.rm=TRUE),"month")) %>% 
  left_join(feb_forecast %>% select(outlay_day_amt,receipt_day_amt,record_calendar_month,record_calendar_day)) %>% 
  arrange(date) %>% 
  rowwise() %>% 
  mutate(misc1=`Miscellaneous Receipts`*total_day/1000,
         corp1=`Corporate Income Taxes`*total_day/1000,
         payroll1=`Payroll Taxes`*total_day/1000,
         individ1=`Individual Income Taxes`*total_day/1000,
         excise1=`Excuse Taxes`*total_day/1000,
         estate1=`Estate and Gift Taxes`*total_day/1000,
         customs1=`Customs Duties`*total_day/1000) %>% 
  rowwise() %>% 
  mutate_at(vars(misc1:customs1),~.*(receipt_day_amt/1000/sum(c(misc1,corp1,payroll1,individ1,excise1,estate1,customs1)))) %>% 
  select(record_calendar_year,record_calendar_month,record_calendar_day,misc1:customs1) %>% 
  pivot_longer(cols=misc1:customs1,names_to="var",values_to="receipt_day_amt") %>% 
  mutate(actual_date=as.Date(paste0(record_calendar_year,"-",record_calendar_month,"-",record_calendar_day))) %>% 
  ungroup()

daily_receipts_all = bind_rows(
  daily_receipts_all,
  feb_adj
)

daily_chart_df = bind_rows(
  test_df %>% 
    mutate(scaled_daily=-1*scaled_daily),
  daily_receipts_all %>% 
    rename(group=var,
           scaled_daily=receipt_day_amt) %>% 
    select(group,actual_date,scaled_daily)
) %>% 
  group_by(actual_date) %>% 
  mutate(daily_deficit=sum(scaled_daily)) %>% 
  ungroup()

colors_df = bind_rows(
  data.frame(group=daily_receipts_all %>% group_by(var) %>% summarize(value=median(receipt_day_amt,na.rm=TRUE)) %>% arrange(value) %>% pull(var),
             cols=RColorBrewer::brewer.pal(7, "Greens")),
  data.frame(group=test_df %>% group_by(group) %>% summarize(value=median(scaled_daily,na.rm=TRUE)) %>% arrange(value) %>% pull(group),
             cols=RColorBrewer::brewer.pal(6, "Reds"))
)

monthly_chart_df = daily_chart_df %>% 
  mutate(year=year(actual_date),
         month=month(actual_date)) %>% 
  group_by(year,month,group) %>% 
  summarize(actual_date=actual_date[1],
            scaled_monthly=sum(scaled_daily,na.rm=TRUE)) %>% 
  group_by(year,month) %>% 
  mutate(monthly_deficit=sum(scaled_monthly,na.rm=TRUE)) %>% 
  ungroup() %>% 
  select(-c(year,month))

yearly_chart_df = monthly_chart_df %>% 
  mutate(year=as.integer(quarter(actual_date, with_year = TRUE, fiscal_start = 10))) %>% 
  group_by(year,group) %>% 
  summarize(scaled_yearly=sum(scaled_monthly,na.rm=TRUE)) %>% 
  group_by(year) %>% 
  mutate(yearly_deficit=sum(scaled_yearly,na.rm=TRUE)) %>% 
  ungroup()

plotly::ggplotly(
  ggplot(yearly_chart_df,aes(x=year,y=scaled_yearly,fill=group)) +
    geom_bar(stat="identity") +
    geom_line(inherit.aes = FALSE,aes(x=year,y=yearly_deficit)) +
    geom_point(inherit.aes = FALSE,aes(x=year,y=yearly_deficit)) +
    theme_bw() +
    labs(x="",y="Outlays/Receipts ($B)") +
    scale_fill_manual(values=colors_df$cols,
                      breaks=colors_df$group)
)

plotly::ggplotly(
  ggplot(monthly_chart_df %>% filter(year(actual_date)>=2024),aes(x=as.yearmon(actual_date),y=scaled_monthly,fill=group)) +
    geom_bar(stat="identity") +
    geom_line(inherit.aes = FALSE,aes(x=as.yearmon(actual_date),y=monthly_deficit)) +
    geom_point(inherit.aes = FALSE,aes(x=as.yearmon(actual_date),y=monthly_deficit)) +
    theme_bw() +
    labs(x="",y="Outlays/Receipts ($B)") +
    scale_fill_manual(values=colors_df$cols,
                      breaks=colors_df$group)
)

plotly::ggplotly(
  ggplot(daily_chart_df %>% filter(year(actual_date)>=2025),aes(x=actual_date,y=scaled_daily,fill=group)) +
  geom_bar(stat="identity") +
  geom_line(inherit.aes = FALSE,aes(x=actual_date,y=daily_deficit)) +
  geom_point(inherit.aes = FALSE,aes(x=actual_date,y=daily_deficit)) +
  theme_bw() +
  labs(x="",y="Outlays/Receipts ($B)") +
  scale_fill_manual(values=colors_df$cols,
                    breaks=colors_df$group)
)

test_df = outlay_daily_df_groups %>% 
  filter(date>="2015-03-01"&date<floor_date(min(feb_forecast$date,na.rm=TRUE),"month")) %>% 
  mutate(record_calendar_month=as.numeric(record_calendar_month)) %>% 
  group_by(group,record_fiscal_year,record_calendar_month) %>% 
  left_join(deficit_summary %>% 
              filter(!grepl("FY |Year-to-Date",classification_desc)&line_code_nbr>=160) %>% 
              rowwise() %>% 
              mutate(outlay=as.numeric(current_month_gross_outly_amt)/1000000000,
                     receipt=as.numeric(current_month_gross_rcpt_amt)/1000000000,
                     record_calendar_month=which(month.name==classification_desc)) %>% 
              select(record_fiscal_year,record_calendar_month,outlay,receipt) %>% 
              group_by(record_fiscal_year,record_calendar_month) %>% 
              slice(1) %>% 
              select(record_fiscal_year,record_calendar_month,actual_check=outlay)) %>% 
  mutate(actual=ifelse(is.na(actual),pred,actual),
         actual_total=ifelse(is.na(actual_total),pred_total,actual_total),
         actual_check=ifelse(is.na(actual_check),pred,actual_check),
         scaled_daily=total_day*(actual/sum(total_day))*(actual_check/actual_total)) %>%
  ungroup() %>% 
  select(group,actual_date,scaled_daily)

feb_adj = outlay_daily_df_groups %>% 
  filter(date>=floor_date(min(feb_forecast$date,na.rm=TRUE),"month")) %>% 
  mutate(record_calendar_month=as.numeric(record_calendar_month)) %>% 
  full_join(feb_forecast %>% select(outlay_day_amt,receipt_day_amt,record_calendar_month,record_calendar_day)) %>% 
  group_by(record_calendar_day,record_calendar_month) %>% 
  mutate(scalar=total_day/sum(total_day)) %>% 
  ungroup() %>% 
  mutate(scaled_daily=outlay_day_amt*scalar/1000) %>% 
  select(group,actual_date,scaled_daily) %>% 
  filter(!is.na(group))

test_df = bind_rows(
  test_df,
  feb_adj
)

#daily_receipts_all = data.frame()
# for(month in unique(forecast_list1 %>% filter(is.na(mean)) %>% select(date) %>% pull())){
#   
#   dat = spending_by_function %>% 
#     mutate(var=case_when(
#       classification_desc=="Individual Income Taxes"~"revenue_Individual Income Taxes",
#       classification_desc=="Corporation Income Taxes"~"revenue_Corporate Income Taxes",
#       classification_desc=="Excise Taxes"~"revenue_Excise Taxes",
#       classification_desc%in%c("Employment and General Retirement","Unemployment Insurance","Other Retirement")~"revenue_Payroll Taxes",
#       classification_desc=="Estate and Gift Taxes"~"revenue_Estate and Gift Taxes",
#       classification_desc=="Customs Duties"~"revenue_Customs Duties",
#       classification_desc=="Miscellaneous Receipts"~"revenue_Miscellaneous Receipts"
#     )) %>% 
#     filter(!is.na(var)) %>% 
#     group_by(var,record_date) %>%
#     summarize(actual=sum(as.numeric(current_month_rcpt_outly_amt))/1000000000,
#               date=floor_date(record_date[1],"month"),
#               record_fiscal_year=record_fiscal_year[1],
#               record_calendar_month=record_calendar_month[1]) %>% 
#     select(date,actual,record_fiscal_year,record_calendar_month)
#   
#   for(var1 in colnames(cbo_monthly_proj)[3:9]){
#     
#     dat1 = dat %>% 
#       ungroup() %>% 
#       filter(var==var1)
#     
#     var_forecast = data.frame(date=as.Date(month),
#                               record_calendar_day=as.numeric(day(seq(as.Date(month),ceiling_date(as.Date(month,by=1),"month")-1,by=1)))) %>% 
#       left_join(dat1) %>% 
#       left_join(receipt_daily_df %>% 
#                   filter(record_calendar_month==month(as.Date(month))) %>% 
#                   distinct(record_calendar_day,avg_share) %>% 
#                   rename(avg_share_receipt=avg_share)%>% 
#                   mutate(record_calendar_day=as.numeric(record_calendar_day))) %>% 
#       fill(avg_share_receipt,.direction="down") %>% 
#       mutate(avg_share_receipt=ifelse(is.na(avg_share_receipt),0,avg_share_receipt),
#              avg_share_receipt=avg_share_receipt/avg_share_receipt[n()],
#              receipt_mtd_amt=actual*avg_share_receipt,
#              receipt_day_amt=receipt_mtd_amt-lag(receipt_mtd_amt,1),
#              receipt_day_amt=ifelse(record_calendar_day==min(record_calendar_day),receipt_mtd_amt,receipt_day_amt),
#              record_calendar_month=as.numeric(record_calendar_month)) %>% 
#       select(date,record_calendar_day,receipt_day_amt,record_fiscal_year,record_calendar_month) %>% 
#       mutate(var=var1)
#     
#     daily_receipts_all = bind_rows(daily_receipts_all,var_forecast)
#     
#   }
# }
# 
# daily_receipts_all = daily_receipts_all %>% 
#   mutate(actual_date=as.Date(paste0(year(date),"-",month(date),"-",record_calendar_day))) %>% 
#   left_join(deficit_summary %>% 
#               filter(!grepl("FY |Year-to-Date",classification_desc)&line_code_nbr>=160) %>% 
#               rowwise() %>% 
#               mutate(outlay=as.numeric(current_month_gross_outly_amt)/1000000000,
#                      receipt=as.numeric(current_month_gross_rcpt_amt)/1000000000,
#                      record_calendar_month=which(month.name==classification_desc)) %>% 
#               select(record_fiscal_year,record_calendar_month,outlay,receipt) %>% 
#               group_by(record_fiscal_year,record_calendar_month) %>% 
#               slice(1) %>% 
#               select(record_fiscal_year,record_calendar_month,actual_check=receipt) %>% 
#               ungroup()) %>%  
#   group_by(record_fiscal_year,record_calendar_month) %>% 
#   mutate(total=sum(receipt_day_amt),
#         receipt_day_amt=receipt_day_amt*(actual_check/total)) %>% 
#   select(-c(actual_check,total))

daily_receipts_all = imputed_daily_receipts %>% 
  mutate(record_calendar_month=as.numeric(record_calendar_month)) %>% 
  right_join(receipt_daily_df %>% 
               mutate(record_calendar_day=as.numeric(record_calendar_day),
                      record_calendar_year=year(date)),
             by=c("record_calendar_year","record_calendar_month", "record_calendar_day")) %>% 
  left_join(actual_receipt %>% 
              rowwise() %>% 
              mutate(total=sum(c(misc,corp,payroll,individ,excise,estate,customs)),
                     month=month(date))) %>% 
  arrange(date) %>% 
  rowwise() %>% 
  mutate(misc1=`Miscellaneous Receipts`*total_day/1000,
         corp1=`Corporate Income Taxes`*total_day/1000,
         payroll1=`Payroll Taxes`*total_day/1000,
         individ1=`Individual Income Taxes`*total_day/1000,
         excise1=`Excuse Taxes`*total_day/1000,
         estate1=`Estate and Gift Taxes`*total_day/1000,
         customs1=`Customs Duties`*total_day/1000) %>% 
  group_by(date) %>% 
  mutate(individ1=individ1+(individ[n()]-sum(individ1))/n(),
         corp1=corp1+(corp[n()]-sum(corp1))/n(),
         misc1=misc1+(misc[n()]-sum(misc1))/n(),
         payroll1=payroll1+(payroll[n()]-sum(payroll1))/n(),
         excise1=excise1+(excise[n()]-sum(excise1))/n(),
         estate1=estate1+(estate[n()]-sum(estate1))/n(),
         customs1=customs1+(customs[n()]-sum(customs1))/n()) %>% 
  rowwise() %>% 
  mutate(total_check=sum(sum(c(misc1,corp1,payroll1,individ1,excise1,estate1,customs1)))) %>% 
  select(record_calendar_year,record_calendar_month,record_calendar_day,misc1:customs1) %>% 
  pivot_longer(cols=misc1:customs1,names_to="var",values_to="receipt_day_amt") %>% 
  mutate(actual_date=as.Date(paste0(record_calendar_year,"-",record_calendar_month,"-",record_calendar_day))) %>% 
  ungroup() %>% 
  filter(date<floor_date(min(feb_forecast$date,na.rm=TRUE),"month")) %>% 
  select(-date) 


feb_adj = imputed_daily_receipts %>% 
  mutate(record_calendar_month=as.numeric(record_calendar_month)) %>% 
  right_join(receipt_daily_df %>% 
               mutate(record_calendar_day=as.numeric(record_calendar_day),
                      record_calendar_year=year(date)),
             by=c("record_calendar_year","record_calendar_month", "record_calendar_day")) %>% 
  filter(date>=floor_date(min(feb_forecast$date,na.rm=TRUE),"month")) %>% 
  left_join(feb_forecast %>% select(outlay_day_amt,receipt_day_amt,record_calendar_month,record_calendar_day)) %>% 
  arrange(date) %>% 
  rowwise() %>% 
  mutate(misc1=`Miscellaneous Receipts`*total_day/1000,
         corp1=`Corporate Income Taxes`*total_day/1000,
         payroll1=`Payroll Taxes`*total_day/1000,
         individ1=`Individual Income Taxes`*total_day/1000,
         excise1=`Excuse Taxes`*total_day/1000,
         estate1=`Estate and Gift Taxes`*total_day/1000,
         customs1=`Customs Duties`*total_day/1000) %>% 
  rowwise() %>% 
  mutate_at(vars(misc1:customs1),~.*(receipt_day_amt/1000/sum(c(misc1,corp1,payroll1,individ1,excise1,estate1,customs1)))) %>% 
  select(record_calendar_year,record_calendar_month,record_calendar_day,misc1:customs1) %>% 
  pivot_longer(cols=misc1:customs1,names_to="var",values_to="receipt_day_amt") %>% 
  mutate(actual_date=as.Date(paste0(record_calendar_year,"-",record_calendar_month,"-",record_calendar_day))) %>% 
  ungroup()

daily_receipts_all = bind_rows(
  daily_receipts_all,
  feb_adj
)

daily_chart_df = bind_rows(
  test_df %>% 
    mutate(scaled_daily=-1*scaled_daily),
  daily_receipts_all %>% 
    rename(group=var,
           scaled_daily=receipt_day_amt) %>% 
    select(group,actual_date,scaled_daily)
) %>% 
  group_by(actual_date) %>% 
  mutate(daily_deficit=sum(scaled_daily)) %>% 
  ungroup()

colors_df = bind_rows(
  data.frame(group=daily_receipts_all %>% group_by(var) %>% summarize(value=median(receipt_day_amt,na.rm=TRUE)) %>% arrange(value) %>% pull(var),
             cols=RColorBrewer::brewer.pal(7, "Greens")),
  data.frame(group=test_df %>% group_by(group) %>% summarize(value=median(scaled_daily,na.rm=TRUE)) %>% arrange(value) %>% pull(group),
             cols=RColorBrewer::brewer.pal(6, "Reds"))
)

monthly_chart_df = daily_chart_df %>% 
  mutate(year=year(actual_date),
         month=month(actual_date)) %>% 
  group_by(year,month,group) %>% 
  summarize(actual_date=actual_date[1],
            scaled_monthly=sum(scaled_daily,na.rm=TRUE)) %>% 
  group_by(year,month) %>% 
  mutate(monthly_deficit=sum(scaled_monthly,na.rm=TRUE)) %>% 
  ungroup() %>% 
  select(-c(year,month))

yearly_chart_df = monthly_chart_df %>% 
  mutate(year=as.integer(quarter(actual_date, with_year = TRUE, fiscal_start = 10))) %>% 
  group_by(year,group) %>% 
  summarize(scaled_yearly=sum(scaled_monthly,na.rm=TRUE)) %>% 
  group_by(year) %>% 
  mutate(yearly_deficit=sum(scaled_yearly,na.rm=TRUE)) %>% 
  ungroup()

plotly::ggplotly(
  ggplot(yearly_chart_df,aes(x=year,y=scaled_yearly,fill=group)) +
    geom_bar(stat="identity") +
    geom_line(inherit.aes = FALSE,aes(x=year,y=yearly_deficit)) +
    geom_point(inherit.aes = FALSE,aes(x=year,y=yearly_deficit)) +
    theme_bw() +
    labs(x="",y="Outlays/Receipts ($B)") +
    scale_fill_manual(values=colors_df$cols,
                      breaks=colors_df$group)
)

plotly::ggplotly(
  ggplot(monthly_chart_df %>% filter(year(actual_date)>=2024),aes(x=as.yearmon(actual_date),y=scaled_monthly,fill=group)) +
    geom_bar(stat="identity") +
    geom_line(inherit.aes = FALSE,aes(x=as.yearmon(actual_date),y=monthly_deficit)) +
    geom_point(inherit.aes = FALSE,aes(x=as.yearmon(actual_date),y=monthly_deficit)) +
    theme_bw() +
    labs(x="",y="Outlays/Receipts ($B)") +
    scale_fill_manual(values=colors_df$cols,
                      breaks=colors_df$group)
)

plotly::ggplotly(
  ggplot(daily_chart_df %>% filter(year(actual_date)>=2025),aes(x=actual_date,y=scaled_daily,fill=group)) +
    geom_bar(stat="identity") +
    geom_line(inherit.aes = FALSE,aes(x=actual_date,y=daily_deficit)) +
    geom_point(inherit.aes = FALSE,aes(x=actual_date,y=daily_deficit)) +
    theme_bw() +
    labs(x="",y="Outlays/Receipts ($B)") +
    scale_fill_manual(values=colors_df$cols,
                      breaks=colors_df$group)
)

save(breakdown_df,gdp_pred_df,yearly_chart_df,monthly_chart_df,daily_chart_df,colors_df,my_chart,outlay_daily_df_groups,feb_forecast,deficit_summary,imputed_daily_receipts,receipt_daily_df,actual_receipt,daily_forecast,daily_forecast_upper,daily_forecast_lower,national_econ,file=paste0("Data/Processing/image_saves/data_asof_",Sys.Date(),".RData"))
save(breakdown_df,gdp_pred_df,yearly_chart_df,monthly_chart_df,daily_chart_df,colors_df,my_chart,outlay_daily_df_groups,feb_forecast,deficit_summary,imputed_daily_receipts,receipt_daily_df,actual_receipt,daily_forecast,daily_forecast_upper,daily_forecast_lower,national_econ,file=paste0("Data/Processing/image_saves/chart_data.RData"))

