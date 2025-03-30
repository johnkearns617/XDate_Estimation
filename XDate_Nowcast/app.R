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
            h5(paste0("Data as of ",max(breakdown_df$prediction_date))),
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
                      plotlyOutput('daily_chart')),
             tabPanel("GDP Nowcast",
                      plotlyOutput("gdp_nowcast"),
                      dataTableOutput("released_today"))
           )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$xdate_chart <- renderPlotly({
      ggplotly(ggplot(my_chart %>% filter(date<="2025-09-30") %>% mutate(label=paste0("$",round(running_bal,2),"B")),aes(x=date)) + 
        geom_ribbon(aes(ymin=running_bal_lower,ymax=running_bal_upper),alpha=.3) +
        geom_line(aes(y=running_bal)) +
        theme_bw() +
        labs(x="",y="Fiscal Space Remaining ($B)")
        )
    })
    
    output$xdate = renderText({
      
      paste0("The estimated X-Date is: ",my_chart %>% filter(running_bal==0) %>% ungroup() %>% slice(1) %>% pull(date),"\n",
             "and as early as: ",my_chart %>% filter(running_bal_lower==0) %>% ungroup() %>% slice(1) %>% pull(date))
      
    })
    
    output$historical_chart = renderPlotly({
      
      ggplotly(
        ggplot(charts,aes(x=date,color=as.Date(date_run),group=date_run)) + 
        #geom_ribbon(aes(ymin=running_bal_lower,ymax=running_bal_upper),alpha=.3) +
        geom_line(aes(y=running_bal,alpha=as.Date(date_run))) +
        geom_line(data=charts %>% mutate(date_run=as.Date(date_run)) %>% filter(date_run==max(date_run)),aes(x=date,y=running_bal),color="black") +
        scale_color_gradient(low='red',high='green') +
        theme_bw() +
        labs(x="",y="Fiscal Space Remaining ($B)") +
        theme(legend.position="none")
      )
      
    })
    
    output$yearly_chart = renderPlotly({
      ggplotly(
        ggplot(yearly_chart_df %>% filter(year>=2015),aes(x=year,y=scaled_yearly,fill=group)) +
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
          ggplot(monthly_chart_df %>% filter((year(actual_date)==d$x&month(actual_date)<=9)|(year(actual_date)==(d$x-1)&month(actual_date)>9)),aes(x=as.yearmon(actual_date),y=scaled_monthly,fill=group)) +
            geom_bar(stat="identity") +
            geom_line(inherit.aes = FALSE,aes(x=as.yearmon(actual_date),y=monthly_deficit)) +
            geom_point(inherit.aes = FALSE,aes(x=as.yearmon(actual_date),y=monthly_deficit)) +
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
          ggplot(daily_chart_df %>% filter(as.yearmon(actual_date)==d$x),aes(x=actual_date,y=scaled_daily,fill=group)) +
            geom_bar(stat="identity") +
            geom_line(inherit.aes = FALSE,aes(x=actual_date,y=daily_deficit)) +
            geom_point(inherit.aes = FALSE,aes(x=actual_date,y=daily_deficit)) +
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
    
    output$gdp_nowcast = renderPlotly({
      plotly::ggplotly(ggplot(breakdown_df %>% filter(date>=(Sys.Date() %m-% years(1))) %>% mutate(prediction_date=as.Date(prediction_date)), aes(x = prediction_date, y = contribution, fill = variable_name)) +
                         geom_bar(stat = "identity", position = "stack") +
                         geom_line(data=gdp_data %>% filter(date>=(Sys.Date() %m-% years(1))) %>% ungroup() %>% mutate(prediction_date=as.Date(prediction_date)),
                                   aes(x=prediction_date,y=(gdp)),inherit.aes = FALSE) +
                         geom_point(data=gdp_data %>% filter(date>=(Sys.Date() %m-% years(1))) %>% ungroup()  %>% mutate(prediction_date=as.Date(prediction_date)),
                                    aes(x=prediction_date,y=(gdp)),inherit.aes = FALSE) +
                         geom_hline(data=gdp_data %>% filter(date>=(Sys.Date() %m-% years(1))) %>% ungroup()  %>% mutate(prediction_date=as.Date(prediction_date)),
                                    aes(yintercept=(actual)),color="darkblue",inherit.aes = FALSE) +
                         labs(title = "Variable Contribution Over Time",
                              x = "Observation (Time)", 
                              y = "Contribution to Fitted Value") +
                         theme_minimal() +
                         facet_wrap(~date,scales="free")
      ) 
    })
    
    output$released_today = renderDataTable({
      
      national_econ %>% 
        filter(release_date>=(Sys.Date()-7)) %>% 
        select(title,series_id,release_date) %>% 
        full_join(breakdown_df %>% 
                    filter((prediction_date>=(Sys.Date()-8)|length(unique(prediction_date))<7)&
                             variable_name%in%((national_econ %>% 
                                                  filter(release_date>=(Sys.Date()-7)) %>% 
                                                  select(title,series_id,release_date))$series_id)) %>% 
                    group_by(date,variable_name) %>% 
                    slice(1,n()) %>% 
                    group_by(date,variable_name) %>% 
                    summarize(impact=contribution[2]-contribution[1]) %>% 
                    rename(projection_quarter=date),
                  by=c("series_id"="variable_name")) %>% 
        arrange(impact)
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
