#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# eventually i will need an RData or some other file that gives all the historic information that I need

library(shiny)
library(plotly)
library(zoo)
library(tidyverse)
library(funspotr)
library(data.table)

conflicted::conflicts_prefer(lubridate::year)
conflicted::conflicts_prefer(lubridate::month)

load(url("https://github.com/johnkearns617/XDate_Estimation/raw/refs/heads/main/Data/Processing/image_saves/chart_data.RData"))

charts = list()
for(dat in tail(list_files_github_repo(
  "johnkearns617/XDate_Estimation",
  branch = NULL,
  pattern = stringr::regex("(rdata)$", ignore_case = TRUE),
) %>% 
select(absolute_paths) %>% 
filter(grepl("image_saves",absolute_paths)&grepl("data_asof_",absolute_paths)) %>%
pull(absolute_paths),
15)){
  
  if(substr(dat,109,118)<"2026-01-01"){ next }
  
  load(url(dat))
  
  gsub(".*https://raw.githubusercontent.com/johnkearns617/XDate_Estimation/main/Data/Processing/image_saves/data_asof_ (.+) .RData.*", "\\1", dat)
  res <- str_match(dat, "https://raw.githubusercontent.com/johnkearns617/XDate_Estimation/main/Data/Processing/image_saves/data_asof_\\s*(.*?)\\s*.RData")[,2]
  
  charts[[dat]] = my_chart %>% 
    mutate(date_run=res)
  
}

charts = data.table::rbindlist(charts)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("XDate Estimation Charts"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h5(paste0("Data as of ",dat_value)),
            h5("This is where model explanation and links would go")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tabsetPanel(
             tabPanel("XDate",
                      textOutput("xdate"),
                      plotlyOutput('xdate_chart'),
                      plotlyOutput('historical_chart')),
             tabPanel("Government Deficits",
                      plotlyOutput('yearly_chart'),
                      plotlyOutput('monthly_chart'),
                      plotlyOutput('daily_chart'))
           )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$xdate_chart <- renderPlotly({
      ggplotly(ggplot(my_chart %>% filter(record_date<=(record_date[1] %m+% years(5))) %>% mutate(label=paste0("$",round(running_bal,2),"B")),
                      aes(x=record_date,group=1,text = paste("Date:", record_date,
                                                       "<br>Fiscal space remaining ($B):", round(running_bal,2),
                                                       "<br>Upper bound:", round(running_bal_upper,2),
                                                       "<br>Lower bound:",round(running_bal_lower,2),
                                                       "<br>Estimated date to hit debt ceiling:",exmeasures_date,
                                                       "<br>Estimated X-date:",my_chart %>% filter(record_date>=dat_value&running_bal<=0) %>% slice(1) %>% pull(record_date)))) + 
        geom_ribbon(aes(ymin=running_bal_lower,ymax=running_bal_upper),alpha=.3) +
        geom_line(aes(y=running_bal)) +
        geom_vline(xintercept=as.Date(exmeasures_date),color="red") +
        theme_bw() +
        labs(x="",y="Fiscal Space Remaining ($B)")
        ,tooltip="text")
    })
    
    output$xdate = renderText({
      
      paste0("The estimated X-Date is: ",my_chart %>% filter(record_date>=dat_value&running_bal<=0) %>% slice(1) %>% pull(record_date),"\n",
             "and as early as: ",my_chart %>% filter(record_date>=dat_value&running_bal_lower<=0) %>% slice(1) %>% pull(record_date))
      
    })
    
    output$historical_chart = renderPlotly({
      
      ggplotly(
        ggplot(charts,aes(x=record_date,color=as.Date(date_run),group=date_run)) + 
        #geom_ribbon(aes(ymin=running_bal_lower,ymax=running_bal_upper),alpha=.3) +
        geom_line(aes(y=running_bal,alpha=as.Date(date_run))) +
        geom_line(data=charts %>% mutate(date_run=as.Date(date_run)) %>% filter(date_run==max(date_run)),aes(x=record_date,y=running_bal),color="black") +
        scale_color_gradient(low='red',high='green') +
        theme_bw() +
        labs(x="",y="Fiscal Space Remaining ($B)") +
        theme(legend.position="none")
      )
      
    })
    
    output$yearly_chart = renderPlotly({
      ggplotly(
        ggplot(yearly_chart_df %>% filter(year>=2015),aes(x=year,y=scaled_yearly,fill=cbo_category)) +
          geom_bar(stat="identity") +
          geom_line(inherit.aes = FALSE,aes(x=year,y=yearly_deficit)) +
          geom_point(inherit.aes = FALSE,aes(x=year,y=yearly_deficit)) +
          theme_bw() +
          labs(x="",y="Outlays/Receipts ($B)") +
          scale_fill_manual(values=colors_df$cols,
                            breaks=colors_df$group),
      source='yearly_chart')
    })
    
    monthly_chart_val <- eventReactive(event_data("plotly_click", source = "yearly_chart"),{
      d <- event_data("plotly_click", source = "yearly_chart")
      if(!is.null(d$x)){
      
        plotly::ggplotly(
          ggplot(monthly_chart_df %>% filter((year(record_date)==d$x&month(record_date)<=9)|(year(record_date)==(d$x-1)&month(record_date)>9)),aes(x=as.yearmon(record_date),y=scaled_monthly,fill=cbo_category)) +
            geom_bar(stat="identity") +
            geom_line(inherit.aes = FALSE,aes(x=as.yearmon(record_date),y=monthly_deficit)) +
            geom_point(inherit.aes = FALSE,aes(x=as.yearmon(record_date),y=monthly_deficit)) +
            theme_bw() +
            labs(x="",y="Outlays/Receipts ($B)",title=paste0("Fiscal year: ",d$x)) +
            scale_fill_manual(values=colors_df$cols,
                              breaks=colors_df$group),
        source='monthly_chart')
        
    }
    })
    
    output$monthly_chart = renderPlotly({
      monthly_chart_val()
    })
    
    daily_chart_val <- eventReactive(event_data("plotly_click", source = "monthly_chart"),{
      d <- event_data("plotly_click", source = "monthly_chart")
      if(!is.null(d$x)){
        
        plotly::ggplotly(
          ggplot(daily_chart_df %>% filter(as.yearmon(record_date)==d$x),aes(x=record_date,y=final_pred_day,fill=cbo_category)) +
            geom_bar(stat="identity") +
            geom_line(inherit.aes = FALSE,aes(x=record_date,y=daily_deficit)) +
            geom_point(inherit.aes = FALSE,aes(x=record_date,y=daily_deficit)) +
            theme_bw() +
            labs(x="",y="Outlays/Receipts ($B)") +
            scale_fill_manual(values=colors_df$cols,
                              breaks=colors_df$group)
        )
        
      }
    })
    
    output$daily_chart = renderPlotly({
      daily_chart_val()
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
