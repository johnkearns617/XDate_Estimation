charts = list()
for(dat in c("05","09","10","11","12","13","14","15","17","18","19","20","21","25","26")){
  
  load(paste0("Data/Processing/image_saves/data_asof_2025-03-",dat,".RData"))
  
  charts[[dat]] = my_chart %>% 
    mutate(date_run=dat)
  
}

charts = data.table::rbindlist(charts)

ggplot(charts %>% filter(date<="2025-09-30"),aes(x=date,color=as.numeric(date_run),group=date_run)) + 
  #geom_ribbon(aes(ymin=running_bal_lower,ymax=running_bal_upper),alpha=.3) +
  geom_line(aes(y=running_bal)) +
  scale_color_gradient(low='red',high='green') +
  theme_bw() +
  labs(x="",y="Fiscal Space Remaining ($B)")
