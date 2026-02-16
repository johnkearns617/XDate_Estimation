

daily_outlays_all = bind_rows(lapply(list(nowcast_medicare_outlay,nowcast_medicaid_outlay,
                                       nowcast_ss_outlay,nowcast_other_outlay,nowcast_defense_outlay,
                                       nowcast_interest_outlay),`[[`, "daily_df")) %>% 
  select(record_date,cbo_category,final_pred_day:final_pred_day_upper,cbo_proj) %>% 
  mutate_at(vars(final_pred_day:cbo_proj),~.*-1)

daily_receipts_all = bind_rows(lapply(list(nowcast_misc_receipts,nowcast_corporate_receipts,
                                           nowcast_payroll_receipts,nowcast_individual_receipts,
                                           nowcast_excise_receipts,nowcast_estate_receipts,
                                           nowcast_customs_receipts),`[[`, "daily_df")) %>% 
  select(record_date,cbo_category,final_pred_day:final_pred_day_upper,cbo_proj)

# No fix needed, somewhere I already scaled the receipts so they equal actual values



daily_chart_df = bind_rows(
  daily_outlays_all,
  daily_receipts_all
) %>% 
  group_by(record_date) %>% 
  mutate(daily_deficit=sum(final_pred_day)) %>% 
  ungroup() %>% 
  mutate(month=floor_date(record_date,"month")) %>% 
  group_by(month) %>% 
  mutate(deficit=sum(final_pred_day),
         share=final_pred_day/deficit) %>% 
  left_join(deficit_fred %>% mutate(value=value/1000) %>% select(date,value),by=c("month"="date")) %>% 
  mutate(final_pred_day=case_when(record_date<="2025-09-30"~share*value, # ensures it adds up to monthly 
                                  TRUE~final_pred_day)) %>% 
  ungroup() %>% 
  select(-c(month,deficit,value,share))
  

colors_df = bind_rows(
  data.frame(group=daily_receipts_all %>% group_by(cbo_category) %>% summarize(value=median(final_pred_day,na.rm=TRUE)) %>% arrange(value) %>% pull(cbo_category),
             cols=RColorBrewer::brewer.pal(7, "Greens")),
  data.frame(group=daily_outlays_all %>% group_by(cbo_category) %>% summarize(value=median(final_pred_day,na.rm=TRUE)) %>% arrange(value) %>% pull(cbo_category),
             cols=RColorBrewer::brewer.pal(6, "Reds"))
)

monthly_chart_df = daily_chart_df %>% 
  mutate(year=year(record_date),
         month=month(record_date)) %>% 
  group_by(year,month,cbo_category) %>% 
  summarize(record_date=record_date[1],
            scaled_monthly=sum(final_pred_day,na.rm=TRUE)) %>% 
  group_by(year,month) %>% 
  mutate(monthly_deficit=sum(scaled_monthly,na.rm=TRUE)) %>% 
  ungroup() %>% 
  select(-c(year,month))

yearly_chart_df = monthly_chart_df %>% 
  mutate(year=as.integer(quarter(record_date, with_year = TRUE, fiscal_start = 10))) %>% 
  group_by(year,cbo_category) %>% 
  summarize(scaled_yearly=sum(scaled_monthly,na.rm=TRUE)) %>% 
  group_by(year) %>% 
  mutate(yearly_deficit=sum(scaled_yearly,na.rm=TRUE)) %>% 
  ungroup()

plotly::ggplotly(
  ggplot(yearly_chart_df,aes(x=year,y=scaled_yearly,fill=cbo_category)) +
    geom_bar(stat="identity") +
    geom_line(inherit.aes = FALSE,aes(x=year,y=yearly_deficit)) +
    geom_point(inherit.aes = FALSE,aes(x=year,y=yearly_deficit)) +
    theme_bw() +
    labs(x="",y="Outlays/Receipts ($B)") +
    scale_fill_manual(values=colors_df$cols,
                      breaks=colors_df$group)
)

plotly::ggplotly(
  ggplot(monthly_chart_df %>% filter(year(record_date)>=2024),aes(x=as.yearmon(record_date),y=scaled_monthly,fill=cbo_category)) +
    geom_bar(stat="identity") +
    geom_line(inherit.aes = FALSE,aes(x=as.yearmon(record_date),y=monthly_deficit)) +
    geom_point(inherit.aes = FALSE,aes(x=as.yearmon(record_date),y=monthly_deficit)) +
    theme_bw() +
    labs(x="",y="Outlays/Receipts ($B)") +
    scale_fill_manual(values=colors_df$cols,
                      breaks=colors_df$group)
)

plotly::ggplotly(
  ggplot(daily_chart_df %>% filter(as.yearmon(record_date)==as.yearmon(substr(end_date,1,7))),aes(x=record_date,y=final_pred_day,fill=cbo_category)) +
    geom_bar(stat="identity") +
    geom_line(inherit.aes = FALSE,aes(x=record_date,y=daily_deficit)) +
    geom_point(inherit.aes = FALSE,aes(x=record_date,y=daily_deficit)) +
    theme_bw() +
    labs(x="",y="Outlays/Receipts ($B)") +
    scale_fill_manual(values=colors_df$cols,
                      breaks=colors_df$group)
)

dat_value=Sys.Date()

save(dat_value,yearly_chart_df,monthly_chart_df,daily_chart_df,colors_df,my_chart,daily_outlays_all,daily_receipts_all,deficit_summary,actuals,nowcast_outlay,nowcast_receipt,nowcast_deficit,national_econ,file=paste0("Data/Processing/image_saves/data_asof_",Sys.Date(),".RData"))
save(dat_value,yearly_chart_df,monthly_chart_df,daily_chart_df,colors_df,my_chart,daily_outlays_all,daily_receipts_all,deficit_summary,actuals,nowcast_outlay,nowcast_receipt,nowcast_deficit,national_econ,file=paste0("Data/Processing/image_saves/chart_data.RData"))

