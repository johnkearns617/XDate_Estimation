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

load(url("https://github.com/johnkearns617/XDate_Estimation/raw/refs/heads/main/Data/Processing/image_saves/chart_data.RData"))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("XDate Estimation Charts"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h5("This is where model explanation and links would go")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tabsetPanel(
             tabPanel("XDate",
                      plotlyOutput('xdate_chart')),
             tabPanel("Government Deficits",
                      plotlyOutput('yearly_chart'),
                      plotlyOutput('monthly_chart'),
                      plotlyOutput('daily_chart')),
             tabPanel("GDP Nowcast",
                      plotlyOutput("gdp_nowcast"))
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
      plotly::ggplotly(ggplot(breakdown_df %>% filter(variable!="prediction"), aes(x = eval_date, y = contribution, fill = variable_name)) +
                         geom_bar(stat = "identity", position = "stack") +
                         geom_line(data=gdp_pred_df %>% ungroup() %>% filter(date>="2025-01-01") %>% mutate(date=1:n()),
                                   aes(x=date,y=pred),inherit.aes = FALSE) +
                         geom_point(data=gdp_pred_df %>% ungroup() %>% filter(date>="2025-01-01") %>% mutate(date=1:n()),
                                    aes(x=date,y=pred),inherit.aes = FALSE) +
                         labs(title = "Variable Contribution Over Time",
                              x = "Observation (Time)", 
                              y = "Contribution to Fitted Value") +
                         theme_minimal()
      )
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
