test_df = outlay_daily_df_groups %>% 
  filter(date>="2015-03-01"&date<floor_date(min(feb_forecast$date,na.rm=TRUE),"month")) %>% 
  select(group,actual_date,scaled_daily=scaled_total_day)

feb_adj = outlay_daily_df_groups %>% 
  filter(date>=floor_date(min(feb_forecast$date,na.rm=TRUE),"month")) %>% 
  mutate(record_calendar_month=as.numeric(record_calendar_month)) %>% 
  full_join(feb_forecast %>% select(outlay_day_amt,receipt_day_amt,record_calendar_month,record_calendar_day)) %>% 
  group_by(record_calendar_day,record_calendar_month) %>% 
  select(group,actual_date,scaled_daily=total_day) %>% 
  mutate(scaled_daily=scaled_daily/1000) %>% 
  filter(!is.na(group))

test_df = bind_rows(
  test_df,
  feb_adj
)

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
  mutate_at(vars(misc1:customs1),~.*(receipt_day_amt/sum(c(misc1,corp1,payroll1,individ1,excise1,estate1,customs1)))) %>% 
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
    mutate(scaled_daily=scaled_daily),
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
  ggplot(daily_chart_df %>% filter(as.yearmon(actual_date)==as.yearmon("2025-04")),aes(x=actual_date,y=scaled_daily,fill=group)) +
    geom_bar(stat="identity") +
    geom_line(inherit.aes = FALSE,aes(x=actual_date,y=daily_deficit)) +
    geom_point(inherit.aes = FALSE,aes(x=actual_date,y=daily_deficit)) +
    theme_bw() +
    labs(x="",y="Outlays/Receipts ($B)") +
    scale_fill_manual(values=colors_df$cols,
                      breaks=colors_df$group)
)

dat_value=Sys.Date()

save(dat_value,yearly_chart_df,monthly_chart_df,daily_chart_df,colors_df,my_chart,outlay_daily_df_groups,feb_forecast,deficit_summary,imputed_daily_receipts,receipt_daily_df,outlay_daily_df,actual_receipt,daily_forecast,daily_forecast_upper,daily_forecast_lower,national_econ,file=paste0("Data/Processing/image_saves/data_asof_",Sys.Date(),".RData"))
save(dat_value,yearly_chart_df,monthly_chart_df,daily_chart_df,colors_df,my_chart,outlay_daily_df_groups,feb_forecast,deficit_summary,imputed_daily_receipts,receipt_daily_df,outlay_daily_df,actual_receipt,daily_forecast,daily_forecast_upper,daily_forecast_lower,national_econ,file=paste0("Data/Processing/image_saves/chart_data.RData"))

