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

setwd("/Users/johnkearns/Documents/GitHub/XDate_Estimation")

load("Data/Processing/image_saves/data_asof_2025-03-05.RData")

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
                      ),
             tabPanel("GDP Nowcast",
                      )
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
}

# Run the application 
shinyApp(ui = ui, server = server)
