library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
# new
library(plotly)


# read data from Subsurface
 ss_dives <- read_csv('/Users/jking/Projects/Subsurface/Subsurface summary.csv') %>%
   drop_na(., `watertemp [F]`) %>%
   separate_wider_delim(location, " - ", names = c("Location", "Site"))
 ss_dives$month <- format(ss_dives$date, "%B")
 ss_dives$day <- format(ss_dives$date, "%d")
 
 Water <- ss_dives %>%
   group_by(month, day, Location) %>%
   summarize(mean = mean(`watertemp [F]`),
             n = n()) %>%
   arrange(Location)

library(shiny)

ui <- fluidPage(
  titlePanel("Interactive Site-Water Temp Graph"),
  sidebarLayout(
    sidebarPanel(
      selectInput("siteSelect", "Choose Site:", choices = unique(Water$Location)),
      selectInput("monthSelect", "Choose Month:", choices = c("January","February","March",
                                                              "April","May","June","July",
                                                              "August","September","October",
                                                              "November","December"))
    ),
    mainPanel(
#      plotOutput("sitePlot")
      plotlyOutput("sitePlot")
    )
  )
)

server <- function(input, output, session) {
#  output$sitePlot <- renderPlot({
  output$sitePlot <- renderPlotly({
    selectedSite <- Water[Water$Location == input$siteSelect &
                            Water$month == input$monthSelect,]
    ggplot(selectedSite, aes(x = day, y = mean)) +
      geom_point(colour = 2, aes(size = n)) +
      labs(title = paste("Data for", input$siteSelect, input$monthSelect),
           x = "Day of the Month",
           y = "Mean Water Temp")
  })
}

shinyApp(ui = ui, server = server)



